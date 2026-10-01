# Helpers mantidos em fonte legível e incorporados ao script autossuficiente.
mq_files <- function(root, pattern=NULL, relative=FALSE) {
  all<-sort(list.files(root,recursive=TRUE,full.names=FALSE,all.files=FALSE))
  all<-all[!file.info(file.path(root,all))$isdir]
  if(length(all))invisible(vapply(all,monitora_qfield_caminho_local,character(1),raiz=root))
  if(!is.null(pattern))all<-all[grepl(pattern,all,ignore.case=TRUE)]
  if(relative)all else file.path(root,all)
}
mq_resolve_paths <- function(c) {
  base<-c$pasta_base
  if(is.null(base))base<-if(!is.na(MQ_SCRIPT_ARQUIVO))dirname(MQ_SCRIPT_ARQUIVO)else NULL
  absolute<-function(p)grepl('^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)',p)
  if(is.null(base)&&any(!vapply(c(c$entrada,c$saida),absolute,logical(1))))mq_stop('Não foi possível localizar o script. Informe pasta_base ou caminhos absolutos.')
  if(!is.null(base))base<-normalizePath(base,winslash='/',mustWork=TRUE)
  for(n in c('entrada','saida','cache_dir','referencia_anterior','sentinel_arquivo','qgis_python'))if(!is.null(c[[n]])&&!absolute(c[[n]]))c[[n]]<-file.path(base,c[[n]])
  c$caches_adicionais<-vapply(c$caches_adicionais,function(p)if(absolute(p))p else file.path(base,p),character(1));c$pasta_base<-base;c
}
mq_inventory_status <- function(inv,layers,c) {
  used<-unique(vapply(layers,function(l)strsplit(l$fonte,' | ',fixed=TRUE)[[1]][1],character(1)))
  ext<-tolower(tools::file_ext(inv$arquivo));raster<-ext%in%c('mbtiles','tif','tiff','img','asc')
  config<-inv$arquivo%in%c('camadas_qfield.csv',c$cotas_atributos_arquivo,c$estratos_arquivo)
  aux<-ext%in%c('shx','dbf','prj','cpg','qix','sbn','sbx','tfw','wld','xml','ovr','ige','rrd')
  inv$estado<-ifelse(inv$arquivo%in%used|raster,'processado',ifelse(config,'configuracao',ifelse(aux,'auxiliar','ignorado')))
  inv$motivo<-ifelse(inv$estado=='ignorado','Arquivo sem papel de entrada reconhecido',ifelse(aux,'Auxiliar disponível ao driver da fonte','Leitura ou configuração registrada'))
  inv
}
mq_id_contexts <- function(contexts,ae,cr,c,old=NULL) {
  top<-if(!is.null(old)&&'id_malha'%in%names(old))max(old$id_grade,na.rm=TRUE)else 0
  for(z in contexts)if(!is.null(z$base_id))top<-max(top,z$base_id+z$ncol_ref*z$nlin_ref)
  for(i in seq_along(contexts)) {
    z<-contexts[[i]]
    if(is.null(z$ncol_ref)) {
      b<-sf::st_bbox(if(is.null(z$wkt))sf::st_transform(ae,cr)else sf::st_as_sfc(z$wkt,crs=cr))
      z$ncol_ref<-max(1,floor((b[3]-z$origem[1])/c$grade_m[1]+1e-9)+1)
      z$nlin_ref<-max(1,floor((b[4]-z$origem[2])/c$grade_m[2]+1e-9)+1)
      z$base_id<-top;top<-top+z$ncol_ref*z$nlin_ref
      contexts[[i]]<-z
    }
  }
  if(top>2^52)mq_stop('Domínio numérico da malha excedido.')
  contexts
}
mq_grid_identify <- function(g,contexts,old=NULL) {
  g$id_malha<-NA_real_;candidate<-rep(NA_real_,nrow(g));reserved<-0
  for(z in contexts) {
    k<-which(g$malha==z$chave & g$coluna>=0 & g$coluna<z$ncol_ref & g$linha>=0 & g$linha<z$nlin_ref)
    g$id_malha[k]<-g$linha[k]*z$ncol_ref+g$coluna[k]+1
    candidate[k]<-z$base_id+g$id_malha[k];reserved<-max(reserved,z$base_id+z$ncol_ref*z$nlin_ref)
  }
  g$id_grade<-candidate;g$categoria<-'grade';ix<-if(is.null(old))rep(NA_integer_,nrow(g))else match(g$chave_grade,old$chave_grade)
  known<-which(!is.na(ix));fresh<-which(is.na(ix))
  if(length(known)){g$id_grade[known]<-old$id_grade[ix[known]];g$categoria[known]<-old$categoria[ix[known]]}
  used<-if(is.null(old))numeric()else old$id_grade
  nextid<-max(c(reserved,used,candidate),na.rm=TRUE)
  for(k in fresh)if(is.na(g$id_grade[k]) || g$id_grade[k]%in%used){nextid<-nextid+1;g$id_grade[k]<-nextid;used<-c(used,nextid)}else used<-c(used,g$id_grade[k])
  if(anyDuplicated(g$id_grade))mq_stop('Colisão no cadastro da grade.')
  base<-ceiling(max(reserved,10000)/10000)*10000
  alias_id<-ifelse(is.na(candidate),g$id_grade,candidate)
  codes<-ifelse(alias_id<10000,base+alias_id,alias_id)
  previous_codes<-if(!is.null(old)&&'codigo_pa'%in%names(old))old$codigo_pa else character()
  g$codigo_pa<-paste0('PA',format(codes,scientific=FALSE,trim=TRUE))
  if(length(previous_codes)&&length(known)){valid<-known[!is.na(previous_codes[ix[known]])&grepl('^PA[0-9]{5,}$',previous_codes[ix[known]])];g$codigo_pa[valid]<-previous_codes[ix[valid]]}
  allcodes<-c(stats::na.omit(previous_codes),g$codigo_pa[known]);nextcode<-max(c(10000,suppressWarnings(as.numeric(sub('^PA','',c(g$codigo_pa,allcodes))))),na.rm=TRUE)
  for(k in fresh){if(g$codigo_pa[k]%in%allcodes){nextcode<-nextcode+1;g$codigo_pa[k]<-paste0('PA',format(nextcode,scientific=FALSE,trim=TRUE))};allcodes<-c(allcodes,g$codigo_pa[k])}
  # Migração v0.3: mantém PA e id_grade, acrescentando código de exibição.
  g$PA<-g$codigo_pa
  if(length(known))g$PA[known]<-old$PA[ix[known]]
  if(anyDuplicated(g$codigo_pa)||any(!grepl('^PA[0-9]{5,}$',g$codigo_pa)))mq_stop('Código PA inválido ou repetido.')
  attr(g,'contextos')<-contexts;g
}
mq_pa_aliases <- function(layers,report) {
  ids<-which(vapply(layers,function(l)l$papel%in%c('PA_priorit','PA_altern','grade_amostral'),logical(1)))
  if(!length(ids))return(layers)
  # Mesma chave espacial identifica o mesmo ponto na grade e na camada selecionada.
  rows<-list();cmp_crs<-sf::st_crs(layers[[ids[1]]]$x);if(isTRUE(cmp_crs$IsGeographic))cmp_crs<-sf::st_crs(3857)
  for(i in ids){l<-layers[[i]];x<-l$x;if(!nrow(x))next
    geom<-sf::st_as_binary(sf::st_geometry(x));original<-if(l$label=='codigo_pa'&&'PA'%in%names(x))as.character(x$PA)else if(nzchar(l$label))as.character(x[[l$label]])else rep('',nrow(x))
    key<-if('chave_grade'%in%names(x))as.character(x$chave_grade)else paste(original,vapply(geom,digest::digest,character(1),algo='sha256'),sep=':')
    code<-if('codigo_pa'%in%names(x))as.character(x$codigo_pa)else ifelse(grepl('^PA[0-9]{5,}$',original),original,NA_character_)
    previous<-code;code[is.na(code)|!grepl('^PA[0-9]{5,}$',code)]<-NA_character_;xy<-sf::st_coordinates(sf::st_transform(x,cmp_crs))
    rows[[length(rows)+1]]<-data.frame(i=i,row=seq_len(nrow(x)),chave=key,original=original,codigo_anterior=previous,codigo=code,x=xy[,1],y=xy[,2])
  }
  if(!length(rows))return(layers)
  d<-do.call(rbind,rows);keys<-sort(unique(d$chave));assigned<-character();used<-unique(stats::na.omit(d$codigo));counter<-max(c(10000,suppressWarnings(as.numeric(sub('^PA','',used)))),na.rm=TRUE)
  for(k in keys){dk<-d[d$chave==k,];if(diff(range(dk$x))>.01||diff(range(dk$y))>.01)mq_stop('Chave PA com posições conflitantes (>1 cm): ',k);v<-unique(stats::na.omit(d$codigo[d$chave==k]));if(length(v)>1)mq_stop('Códigos conflitantes para a mesma chave PA.')
    code<-if(length(v))v else NA_character_;if(is.na(code)||code%in%assigned){counter<-counter+1;while(paste0('PA',counter)%in%used)counter<-counter+1;code<-paste0('PA',counter)}
    assigned<-c(assigned,code);used<-c(used,code);d$codigo[d$chave==k]<-code
  }
  for(i in ids){r<-d[d$i==i,];if(!nrow(r))next;if(any(!is.na(r$codigo_anterior)&r$codigo_anterior!=r$codigo)&&!'codigo_pa_anterior'%in%names(layers[[i]]$x))layers[[i]]$x$codigo_pa_anterior<-r$codigo_anterior;layers[[i]]$x$codigo_pa<-r$codigo;layers[[i]]$label<-'codigo_pa'}
  d$camada<-vapply(d$i,function(i)layers[[i]]$nome,character(1));mq_csv(d[,c('camada','chave','original','codigo_anterior','codigo')],file.path(report,'correspondencia_codigos_PA.csv'))
  layers
}
mq_entry <- function(root,c,mode,status) {
  e<-monitora_qfield_xml
  links<-c('01_qfield/pacote_qfield.zip'='Projeto para QField','01_qfield/projeto.qgs'='Projeto de navegação no QGIS','02_relatorio/relatorio_execucao.html'='Relatório de execução','03_vetores'='Vetores GPKG, KML e KMZ','04_csv'='Tabelas CSV')
  if(c$gerar_cartografia)links<-c(links,'05_qgis/projeto_edicao.qgz'='Projeto QGIS editável e quatro layouts','mapas_pdf'='Quatro mapas PDF georreferenciados','mapas_png'='Quatro mapas PNG')
  writeLines(c('<!doctype html><html lang="pt-BR"><meta charset="utf-8"><title>Monitora — entrega</title><style>body{font:18px/1.6 sans-serif;max-width:900px;margin:40px auto;padding:20px;color:#174b3b}</style>',paste0('<h1>',e(c$projeto),'</h1><p>v0.4.2 · ',e(mode),' · ',e(status),'</p><ul>'),paste0('<li><a href="',names(links),'">',links,'</a></li>'),'</ul><p>Edite os vetores em 05_qgis. Essas alterações não modificam o pacote QField, os CSV/KML ou mapas já exportados. Reexporte os layouts após editar. Preserve a pasta completa para manter as imagens compartilhadas e os caminhos relativos.</p></html>'),file.path(root,'ABRA_AQUI.html'))
}
# Composição source-over: a primeira fonte local prevalece; transparências são preenchidas.
mq_alpha_over <- function(front,back) {
  af<-front[,,4];ab<-back[,,4];alpha<-af+ab*(1-af);out<-front
  for(k in 1:3)out[,,k]<-ifelse(alpha>0,(front[,,k]*af+back[,,k]*ab*(1-af))/pmax(alpha,1e-20),0)
  out[,,4]<-alpha;out
}
mq_mosaic_mb <- function(paths,c,report,mask=NULL) {
  paths<-unique(paths);hashes<-vapply(paths,mq_hash,character(1));key<-digest::digest(list(hashes,if(!is.null(mask))list(sf::st_as_binary(mask),sf::st_crs(mask)$wkt), 'alpha_local_first_normalize256_v3'))
  target<-file.path(c$cache_dir,'mosaicos',paste0(key,'.mbtiles'));dir.create(dirname(target),recursive=TRUE,showWarnings=FALSE)
  sources<-lapply(paths,function(p){db<-DBI::dbConnect(RSQLite::SQLite(),p,flags=RSQLite::SQLITE_RO);d<-DBI::dbGetQuery(db,'SELECT zoom_level z,tile_column x,tile_row t FROM tiles');m<-DBI::dbGetQuery(db,'SELECT name,value FROM metadata');list(db=db,d=d,meta=m)})
  on.exit(for(v in sources)if(DBI::dbIsValid(v$db))DBI::dbDisconnect(v$db),add=TRUE)
  provenance<-lapply(seq_along(paths),function(i)list(prioridade=i,arquivo=paths[i],sha256=hashes[i],metadados=sources[[i]]$meta))
  mq_json(list(regra='Fonte local primeiro; complemento somente via alpha; fontes ordenadas por caminho relativo. Pixel exportado não comprova resolução nativa.',fontes=provenance,cache=target),file.path(report,'mosaico_fontes.json'))
  if(mq_verified(target))return(target)
  if(file.exists(target))mq_stop('Mosaico em cache sem integridade: ',target)
  sizes<-vapply(sources,function(v){b<-DBI::dbGetQuery(v$db,'SELECT tile_data FROM tiles LIMIT 1')$tile_data[[1]];a<-mq_rgba(b);if(dim(a)[1]!=dim(a)[2]||!dim(a)[1]%in%c(256,512))mq_stop('Tile deve ser quadrado de 256 ou 512 pixels.');dim(a)[1]},integer(1))
  levels<-lapply(sources,function(v)sort(unique(v$d$z)));same<-all(sizes==256)&&length(levels[[1]])==1&&all(vapply(levels,identical,logical(1),levels[[1]]))
  tmp<-paste0(target,'.parcial');if(file.exists(tmp))unlink(tmp)
  db<-DBI::dbConnect(RSQLite::SQLite(),tmp);on.exit(if(DBI::dbIsValid(db))DBI::dbDisconnect(db),add=TRUE)
  DBI::dbExecute(db,'CREATE TABLE tiles(zoom_level INTEGER,tile_column INTEGER,tile_row INTEGER,tile_data BLOB,PRIMARY KEY(zoom_level,tile_column,tile_row))');DBI::dbExecute(db,'CREATE TABLE metadata(name TEXT PRIMARY KEY,value TEXT)')
  tile<-function(v,z,x,t)DBI::dbGetQuery(v$db,'SELECT tile_data FROM tiles WHERE zoom_level=? AND tile_column=? AND tile_row=?',params=list(z,x,t))$tile_data
  mask3857<-if(is.null(mask))NULL else terra::vect(sf::st_transform(mask,3857))
  put<-function(z,x,t,a) {
    if(!is.null(mask3857)){h<-20037508.342789244;w<-2*h/2^z;r<-terra::rast(nrows=256,ncols=256,xmin=-h+x*w,xmax=-h+(x+1)*w,ymin=-h+t*w,ymax=-h+(t+1)*w,crs='EPSG:3857');a[,,4]<-a[,,4]*as.matrix(terra::rasterize(mask3857,r,field=1,background=0),wide=TRUE)}
    if(any(a[,,4]>0))DBI::dbExecute(db,'INSERT INTO tiles VALUES(?,?,?,?)',params=list(z,x,t,list(png::writePNG(a,target=raw()))))}
  DBI::dbBegin(db)
  if(same) {
    keys<-unique(do.call(rbind,lapply(sources,`[[`,'d')));keys<-keys[order(keys$z,keys$x,keys$t),]
    bar<-mq_progress('Mosaico único',nrow(keys));on.exit(mq_progress_done(bar),add=TRUE)
    for(i in seq_len(nrow(keys))) {
      k<-keys[i,];a<-array(0,c(256,256,4))
      for(v in sources){raw<-tile(v,k$z,k$x,k$t);if(length(raw)){b<-mq_rgba(raw[[1]]);if(!identical(dim(b),c(256L,256L,4L)))mq_stop('Dimensões de tiles variam dentro da fonte.');a<-mq_alpha_over(a,b)}}
      put(k$z,k$x,k$t,a);mq_progress_update(bar,i)
    }
  } else {
    # Normalização esparsa: usa pixels nativos de 512 e expande apenas tiles existentes.
    zmax<-max(vapply(levels,max,numeric(1))+log2(sizes/256));zmin<-min(unlist(levels))
    keys<-list()
    for(j in seq_along(sources)) {
      d<-sources[[j]]$d
      if(sum(4^(zmax-d$z))>c$max_tiles*20)mq_stop('Normalização excede volume seguro; escolha fontes de resolução compatível.')
      for(i in seq_len(nrow(d))){k<-d[i,];factor<-2^(zmax-k$z);if(factor^2>c$max_tiles)mq_stop('Normalização excede max_tiles.');keys[[length(keys)+1]]<-expand.grid(x=k$x*factor+0:(factor-1),t=k$t*factor+0:(factor-1))}
    }
    keys<-unique(do.call(rbind,keys));if(nrow(keys)>c$max_tiles)mq_stop('Mosaico excede max_tiles.');bar<-mq_progress('Mosaico/resoluções',nrow(keys));on.exit(mq_progress_done(bar),add=TRUE)
    read_at<-function(v,z,x,t) {
      out<-array(0,c(256,256,4))
      for(zz in sort(unique(v$d$z[v$d$z<=z]),decreasing=TRUE)) {
        f<-2^(z-zz);xx<-floor(x/f);tt<-floor(t/f);raw<-tile(v,zz,xx,tt)
        if(length(raw)) {
          a<-mq_rgba(raw[[1]]);n<-dim(a)[1];if(dim(a)[2]!=n||!n%in%c(256,512))mq_stop('Tile inconsistente.')
          # Amostragem nearest preserva cores/alpha: tamanho de saída explicitamente reamostrado.
          cols<-pmin(n,floor(((x%%f)+(seq_len(256)-.5)/256)*n/f)+1)
          rows<-pmin(n,floor(((f-1-t%%f)+(seq_len(256)-.5)/256)*n/f)+1)
          out<-mq_alpha_over(out,a[rows,cols,,drop=FALSE]);if(all(out[,,4]>=1))return(out)
        }
      };out
    }
    for(i in seq_len(nrow(keys))){a<-array(0,c(256,256,4));for(v in sources)a<-mq_alpha_over(a,read_at(v,zmax,keys$x[i],keys$t[i]));put(zmax,keys$x[i],keys$t[i],a);mq_progress_update(bar,i)}
    if(zmax>zmin)for(z in seq(zmax-1,zmin)) {
      d<-DBI::dbGetQuery(db,'SELECT DISTINCT cast(tile_column/2 as integer) x,cast(tile_row/2 as integer) t FROM tiles WHERE zoom_level=?',params=list(z+1))
      for(i in seq_len(nrow(d))) {
        a<-array(0,c(512,512,4))
        for(dx in 0:1)for(dt in 0:1){raw<-DBI::dbGetQuery(db,'SELECT tile_data FROM tiles WHERE zoom_level=? AND tile_column=? AND tile_row=?',params=list(z+1,2*d$x[i]+dx,2*d$t[i]+dt))$tile_data;if(length(raw))a[(1:256)+(1-dt)*256,(1:256)+dx*256,]<-mq_rgba(raw[[1]])}
        # Média premultiplicada evita franjas pretas nas bordas transparentes.
        prem<-a;for(k in 1:3)prem[,,k]<-a[,,k]*a[,,4]
        b<-(prem[seq(1,512,2),seq(1,512,2),]+prem[seq(2,512,2),seq(1,512,2),]+prem[seq(1,512,2),seq(2,512,2),]+prem[seq(2,512,2),seq(2,512,2),])/4
        for(k in 1:3)b[,,k]<-ifelse(b[,,4]>0,b[,,k]/pmax(b[,,4],1e-20),0)
        put(z,d$x[i],d$t[i],b)
      }
    }
  }
  d<-DBI::dbGetQuery(db,'SELECT min(zoom_level) zmin,max(zoom_level) zmax,count(*) n FROM tiles');if(d$n==0)mq_stop('Mosaico vazio.')
  # Bounds são a união declarada dos recortes, sem reutilizar descrição de uma fonte isolada.
  bounds<-lapply(sources,function(v)as.numeric(strsplit(v$meta$value[match('bounds',v$meta$name)],',',fixed=TRUE)[[1]]));b<-do.call(rbind,bounds)
  attribution<-unique(unlist(lapply(sources,function(v)v$meta$value[v$meta$name=='attribution'])))
  meta<-data.frame(name=c('name','format','type','version','minzoom','maxzoom','bounds','description','attribution','recorte_circular_m'),value=c('sat_escala_local','png','overlay','1.1',d$zmin,d$zmax,paste(c(min(b[,1]),min(b[,2]),max(b[,3]),max(b[,4])),collapse=','),'Mosaico de fontes locais e complementares; raio 500 m; detalhes em mosaico_fontes.json',paste(attribution,collapse='; '),'500'))
  DBI::dbWriteTable(db,'metadata',meta,append=TRUE);DBI::dbCommit(db);if(DBI::dbGetQuery(db,'PRAGMA integrity_check')[[1]]!='ok')mq_stop('Mosaico inválido.');DBI::dbDisconnect(db)
  if(!file.rename(tmp,target))mq_stop('Falha ao finalizar mosaico.');mq_seal(target)
}
mq_qgis_call <- function(runtime,script,mode,arg,log) {
  if(runtime$windows) {
    conv<-function(p)if(.Platform$OS.type=='windows')normalizePath(p,winslash='\\',mustWork=FALSE)else system2('wslpath',c('-w',shQuote(p)),stdout=TRUE)
    bat<-file.path(dirname(script),paste0('qgis_',mode,'.cmd'))
    q<-function(p)paste0('"',p,'"')
    # Nenhum segredo é inserido no comando; caminhos com metacaracteres de cmd são rejeitados.
    vals<-vapply(c(runtime$python,script,arg,log),conv,character(1));if(any(grepl('[%\r\n!]',vals)))mq_stop('Caminho com caractere incompatível com o launcher QGIS.')
    writeLines(c('@echo off','set QT_QPA_PLATFORM=windows',paste('call',q(vals[1]),q(vals[2]),mode,q(vals[3]),'>',q(vals[4]),'2>&1'),'exit /b %errorlevel%'),bat,useBytes=TRUE)
    launcher<-if(.Platform$OS.type=='windows')Sys.getenv('COMSPEC','cmd.exe')else '/mnt/c/Windows/System32/cmd.exe'
    rc<-system2(launcher,c('/d','/c',shQuote(conv(bat))),stdout=FALSE,stderr=FALSE)
  }else rc<-system2(runtime$python,c(shQuote(script),mode,shQuote(arg)),stdout=log,stderr=log,env='QT_QPA_PLATFORM=offscreen')
  if(file.exists(log)){lines<-readLines(log,warn=FALSE);for(line in lines[grepl('^Cartografia',lines)])message(line)}
  if(rc!=0)mq_stop('QGIS falhou; consulte ',log,' (código ',rc,').')
}
mq_qgis_prepare <- function(c,root,scratch) {
  for(n in names(MQ_RECURSOS))writeBin(jsonlite::base64_dec(MQ_RECURSOS[[n]]),file.path(scratch,n))
  python<-c$qgis_python
  windows<-.Platform$OS.type=='windows'
  if(is.null(python)) {
    search<-if(windows)'C:/Program Files/QGIS*/bin/python-qgis*.bat'else '/mnt/c/Program Files/QGIS*/bin/python-qgis*.bat'
    candidates<-sort(Sys.glob(search),decreasing=TRUE)
    if(length(candidates)){python<-candidates[1];windows<-TRUE}else python<-Sys.which('python3')
  }else windows<-grepl('\\.bat$',python,ignore.case=TRUE)
  if(!length(python)||!nzchar(python))mq_stop('QGIS/PyQGIS não localizado. Instale QGIS ou informe qgis_python antes de gerar a cartografia.')
  runtime<-list(python=python,windows=windows)
  mq_qgis_call(runtime,file.path(scratch,'cartografia_qgis.py'),'probe',file.path(root,'02_relatorio','qgis_capacidades.json'),file.path(root,'02_relatorio','qgis_probe.log'))
  runtime
}
mq_locator_base <- function(c,root) {
  # Bases oficiais de contexto; primeira obtenção ~27 MiB, reutilizadas com SHA256.
  cache<-file.path(c$cache_dir,'cartografia_ibge_2025');dir.create(cache,recursive=TRUE,showWarnings=FALSE)
  sources<-c(estados='https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2025/Brasil/BR_UF_2025.zip',biomas='https://geoftp.ibge.gov.br/informacoes_ambientais/estudos_ambientais/biomas/vetores/2025_Biomas-e-Sistema-Costeiro-Marinho-do-Brasil-1-250000_shp.zip')
  gp<-file.path(cache,'contexto_simplificado_1000m_v2.gpkg');meta<-file.path(cache,'fontes.json')
  if(!mq_verified(gp)||!file.exists(meta)) {
    audit<-list();tmpgp<-file.path(cache,'contexto_ibge.parcial.gpkg');if(file.exists(tmpgp))unlink(tmpgp);if(file.exists(gp))mq_stop('Base de contexto sem integridade; confira o cache: ',gp)
    for(n in names(sources)) {
      z<-file.path(cache,paste0(n,'.zip'))
      if(!mq_verified(z)) {
        message('Localizador: obtendo base oficial IBGE de ',n,' (primeira execução; cache persistente).')
        tmp<-paste0(z,'.parcial');res<-httr::GET(sources[[n]],httr::write_disk(tmp,overwrite=TRUE),httr::timeout(max(120,c$timeout_s)));httr::stop_for_status(res)
        if(!file.rename(tmp,z))mq_stop('Falha ao materializar base IBGE.');mq_seal(z)
      }
      folder<-tempfile('ibge_');dir.create(folder);entries<-utils::unzip(z,list=TRUE)$Name
      if(any(grepl('(^/|(^|/)\\.\\.(/|$)|:|\\\\)',entries)))mq_stop('Caminho inseguro no ZIP IBGE.')
      utils::unzip(z,exdir=folder);shp<-list.files(folder,'\\.shp$',recursive=TRUE,full.names=TRUE,ignore.case=TRUE)
      if(length(shp)!=1)mq_stop('Base IBGE com seleção de camada ambígua: ',n)
      x<-sf::st_read(shp,quiet=TRUE);if(is.na(sf::st_crs(x)))mq_stop('Base IBGE sem CRS.')
      field<-intersect(if(n=='estados')c('SIGLA_UF','SIGLA')else c('Bioma','BIOMA','NOM_BIOMA','NM_BIOMA'),names(x))[1]
      if(is.na(field))mq_stop('Campo de identificação ausente na base IBGE: ',n,'; campos: ',paste(names(x),collapse=', '))
      x<-sf::st_sf(nome=as.character(x[[field]]),geometry=sf::st_geometry(x));if(n=='estados')sf::st_write(sf::st_transform(sf::st_make_valid(x),4674),tmpgp,layer='estados_identificacao',quiet=TRUE,append=FALSE);x<-sf::st_transform(sf::st_simplify(sf::st_make_valid(sf::st_transform(x,5880)),dTolerance=1000,preserveTopology=TRUE),4674)
      if(n=='estados'&&nrow(x)!=27)mq_stop('Malha estadual incompleta.')
      sf::st_write(x,tmpgp,layer=n,quiet=TRUE,append=FALSE)
      audit[[n]]<-list(autoridade='IBGE',edicao='2025',fonte=sources[[n]],sha256_zip=mq_hash(z),simplificacao_m=1000,uso='Somente contexto cartográfico; não classifica pontos nem delimita áreas elegíveis.')
      unlink(folder,recursive=TRUE)
    }
    if(!file.rename(tmpgp,gp))mq_stop('Falha ao finalizar base do localizador.');mq_seal(gp);mq_json(audit,meta)
  }
  dir.create(file.path(root,'05_qgis/contexto'),recursive=TRUE,showWarnings=FALSE)
  if(!file.copy(gp,file.path(root,'05_qgis/contexto/contexto_ibge.gpkg'),overwrite=TRUE))mq_stop('Falha ao copiar base do localizador.')
  file.copy(meta,file.path(root,'02_relatorio/fontes_localizador.json'),overwrite=TRUE)
  invisible(gp)
}
mq_cartography <- function(layers,ras,ae,c,root,scratch) {
  mq_locator_base(c,root)
  message('Cartografia QGIS: quatro layouts, PDFs georreferenciados e PNGs. Aguarde a renderização.')
  project_root<-if(c$qgis_runtime$windows&&.Platform$OS.type!='windows')system2('wslpath',c('-w',shQuote(root)),stdout=TRUE)else root
  config<-file.path(scratch,'cartografia.json');mq_json(list(root=project_root,projeto=c$projeto,bioma_min_mm2=c$localizador_bioma_min_mm2,dpi=c$mapas_dpi,papel=c$mapas_papel,elaboracao=c$elaboracao,camadas=lapply(layers,function(l)list(nome=l$nome,papel=l$papel))),config)
  mq_qgis_call(c$qgis_runtime,file.path(scratch,'cartografia_qgis.py'),'build',config,file.path(root,'02_relatorio','qgis_cartografia.log'))
  evidence<-jsonlite::read_json(file.path(root,'02_relatorio/cartografia.json'));if(evidence$status!='PASS'||length(evidence$mapas)!=4)mq_stop('Cartografia não validada.')
}
