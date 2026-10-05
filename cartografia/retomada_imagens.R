# Retomada consulta somente caches declarados e execuções da própria pasta de saída.
mq_resume_caches <- function(c,report) {
  roots<-base::unique(c(c$caches_adicionais,c$saida))
  configs<-unlist(lapply(roots,function(p)list.files(p,pattern='^configuracao.json$',recursive=TRUE,full.names=TRUE)))
  recovered<-character()
  for(f in configs) {
    d<-tryCatch(jsonlite::read_json(f),error=function(e)NULL)
    if(!is.null(d$cache_dir)&&dir.exists(d$cache_dir))recovered<-c(recovered,d$cache_dir)
  }
  c$caches_adicionais<-base::unique(c(c$caches_adicionais,recovered))
  c$caches_adicionais<-c$caches_adicionais[dir.exists(c$caches_adicionais)]
  mq_json(list(principal=c$cache_dir,adicionais=c$caches_adicionais,regra='Reutilizar fontes íntegras e compatíveis; somente faltantes podem ser adquiridos.'),file.path(report,'retomada_cache.json'))
  message('CACHE: ',c$cache_dir,'; ',length(c$caches_adicionais),' pastas adicionais recuperadas/configuradas; renovar_imagens=',c$renovar_imagens,'.')
  c
}
mq_sentinel_missing <- function(context,paths) {
  missing<-sf::st_union(sf::st_transform(sf::st_zm(sf::st_geometry(context)),3857))
  for(p in paths) {
    r<-terra::rast(p);if(terra::nlyr(r)<3)next
    area<-terra::project(terra::vect(sf::st_sf(geometry=missing)),terra::crs(r))
    overlap<-tryCatch(terra::intersect(terra::ext(r),terra::ext(area)),error=function(e)NULL)
    if(is.null(overlap))next
    z<-terra::crop(r,overlap,snap='out')
    valid<-if(terra::nlyr(z)>=4)!is.na(z[[4]])&z[[4]]>0 else !is.na(z[[1]])
    covered<-terra::as.polygons(terra::ifel(valid,1,NA),dissolve=TRUE,na.rm=TRUE)
    if(!nrow(covered))next
    g<-sf::st_transform(sf::st_as_sf(covered),3857)
    missing<-suppressWarnings(sf::st_difference(missing,sf::st_union(sf::st_geometry(g))))
    if(all(sf::st_is_empty(missing)))break
  }
  missing
}
mq_sentinel_candidates <- function(c,cache) {
  roots<-base::unique(c(cache,c$caches_adicionais,c$saida))
  files<-unlist(lapply(roots,function(d)list.files(d,pattern='\\.(mbtiles|tif)$',recursive=TRUE,full.names=TRUE)))
  base::unique(c(c$sentinel_arquivo,files))
}
mq_sentinel_cached <- function(context,c,scratch,report) {
  cache<-file.path(c$cache_dir,'sentinel');dir.create(cache,recursive=TRUE,showWarnings=FALSE)
  paths<-character();evidence<-list();audit<-list()
  for(p in mq_sentinel_candidates(c,cache)) {
    if(!file.exists(p))next
    ev<-tryCatch(mq_sentinel_evidence(p),error=function(e)NULL)
    explicit<-!is.null(c$sentinel_arquivo)&&base::identical(p,c$sentinel_arquivo)
    valid<-!is.null(ev)&&(base::identical(ev$versao_acervo,c$versao_acervo)||explicit)&&!base::isTRUE(c$renovar_imagens)
    if(is.null(ev)) {
      if(file.exists(paste0(p,'.fonte.json'))) {
        audit[[length(audit)+1L]]<-list(arquivo=p,status='proveniencia_ou_integridade_invalida_preservado')
        message('SENTINEL: fonte preservada, mas não reutilizada por falha de integridade/proveniência: ',p)
      }
      next
    }
    if(valid) {
      paths<-c(paths,p);evidence[[length(evidence)+1L]]<-ev
      audit[[length(audit)+1L]]<-list(arquivo=p,status='integro_compativel')
      if(mq_sentinel_covers(p,context)) {
        ev$acao<-'reutilizado';ev$arquivo<-p;mq_json(ev,file.path(report,'sentinel_fonte.json'))
        mq_json(audit,file.path(report,'sentinel_cache.json'))
        message('SENTINEL: cobertura integral reutilizada; nenhum download: ',p)
        return(list(complete=p,paths=p,evidence=list(ev)))
      }
    }else audit[[length(audit)+1L]]<-list(arquivo=p,status='acervo_incompativel_ou_renovacao_solicitada')
  }
  mq_json(audit,file.path(report,'sentinel_cache.json'))
  list(complete=NULL,paths=paths,evidence=evidence)
}
mq_sentinel <- function(context,points,c,scratch,report) {
  local<-mq_sentinel_cached(context,c,scratch,report)
  if(!is.null(local$complete))return(local$complete)
  missing<-mq_sentinel_missing(context,local$paths)
  original_area<-sum(as.numeric(sf::st_area(sf::st_transform(context,3857))))
  missing_area<-sum(as.numeric(sf::st_area(missing)))
  # Guardar fontes antes de qualquer consulta remota; retomada não depende do projeto final.
  paths<-local$paths;evs<-local$evidence
  estimate<-list(area_contexto_m2=original_area,area_faltante_m2=missing_area,area_reutilizada_m2=max(0,original_area-missing_area),fontes_locais=length(paths),
                 observacao='Aquisição limitada às lacunas. Blocos COG do provedor podem incluir pixels vizinhos; não se promete tráfego byte a byte exclusivo.')
  mq_json(estimate,file.path(report,'plano_sentinel.json'))
  if(!all(sf::st_is_empty(missing))) {
    message(sprintf('SENTINEL: %.1f%% do contexto disponível; %.1f%% faltante. Cache preservado; baixar somente complemento.',100*(1-missing_area/original_area),100*missing_area/original_area))
    allowed<-c$confirmar_sentinel
    if(is.null(allowed)&&interactive())allowed<-toupper(trimws(readline('Autorizar complemento Sentinel (pode levar minutos)? [S/N]: ')))=='S'
    if(!base::isTRUE(allowed))mq_stop('Sentinel sem cobertura completa. Preserve cache_dir; informe caches_adicionais/sentinel_arquivo ou autorize somente o complemento com confirmar_sentinel=TRUE. Nenhum cache removido.')
    pieces<-suppressWarnings(sf::st_cast(sf::st_collection_extract(missing,'POLYGON',warn=FALSE),'POLYGON'))
    for(i in seq_along(pieces)) {
      sub<-file.path(scratch,paste0('sentinel_complemento_',i));dir.create(sub,showWarnings=FALSE)
      a<-sf::st_sf(geometry=pieces[i]);p<-suppressWarnings(sf::st_point_on_surface(a))
      message('SENTINEL: complemento ',i,'/',length(pieces),'; cenas recebidas serão preservadas para retomada.')
      acquired<-monitora_qfield_sentinel(p,sub,limite=a,cache_dir=c$cache_dir,versao_acervo=c$versao_acervo)
      # Os TIFFs individuais persistem mesmo se o mosaico/MBTiles posterior falhar.
      paths<-c(paths,acquired$rgb)
      evs[[length(evs)+1L]]<-list(cenas=base::as.data.frame(acquired$metadados))
    }
  }else message('SENTINEL: fontes parciais complementares cobrem todo o contexto; nenhum download.')
  if(!length(paths))mq_stop('Sentinel sem fontes utilizáveis após conferência de cache.')
  vrt<-file.path(scratch,'sentinel_cache_combinado.vrt')
  sf::gdal_utils('buildvrt',base::rev(paths),vrt,options=c('-resolution','highest','-addalpha'),quiet=TRUE)
  if(!mq_sentinel_covers(vrt,context))mq_stop('Sentinel combinado ainda tem lacunas; arquivos íntegros preservados para retomada.')
  key<-digest::digest(list(sf::st_as_binary(sf::st_geometry(context)),vapply(paths,mq_hash,character(1)),c$versao_acervo,'sentinel_cache_v3'))
  target<-file.path(c$cache_dir,'sentinel',paste0(key,'.mbtiles'))
  if(!mq_verified(target)) {
    if(file.exists(target))mq_stop('Sentinel em cache sem integridade: ',target)
    partial<-file.path(scratch,'sentinel_contexto.mbtiles');monitora_qfield_mbtiles(vrt,partial,'Sentinel-2 L2A RGB nativo 10 m; fontes locais preservadas e lacunas complementadas.')
    if(!mq_sentinel_covers(partial,context))mq_stop('Sentinel exportado com lacuna; cache preservado.')
    if(!file.copy(partial,target,overwrite=FALSE))mq_stop('Falha ao armazenar Sentinel.');mq_seal(target)
  }
  scenes<-data.table::rbindlist(lapply(evs,function(e)e$cenas),fill=TRUE)
  ev<-list(produto='Sentinel-2 L2A',resolucao_nativa_m=10,versao_acervo=c$versao_acervo,sha256=mq_hash(target),cenas=base::as.data.frame(scenes),
           acao=if(missing_area>0)'complementado'else 'mosaico_local',fontes=paths,arquivo=target,nota='Contexto completo; fontes íntegras reaproveitadas, sem restrição ao raio de detalhe.')
  mq_json(ev,paste0(target,'.fonte.json'));mq_json(ev,file.path(report,'sentinel_fonte.json'));mq_csv(scenes,file.path(report,'cenas_sentinel.csv'))
  target
}
mq_cached_product <- function(c,folder,filename) {
  paths<-file.path(base::unique(c(c$cache_dir,c$caches_adicionais)),folder,filename)
  for(p in paths)if(mq_verified(p)){message('CACHE: ',folder,' reutilizado: ',p);return(p)}
  NULL
}
