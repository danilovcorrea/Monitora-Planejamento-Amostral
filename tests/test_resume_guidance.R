options(monitora.qfield.somente_funcoes=TRUE);source('monitora_planejamento_amostral.R',encoding='UTF-8');MQ_CONFIG<-mq_legacy_defaults() # Regressão da interface pública 1.0.2
n<-0;ok<-function(name,v){stopifnot(isTRUE(v));n<<-n+1;cat('PASS',name,'\n')}
c<-MQ_CONFIG;c$perfil<-'personalizado';c$parametros_protocolo<-list(transecto_m=50,grade_m=c(100,100),direcoes=character(),distancias_viarias_m=numeric());c$grade_m<-c(77,77)
report<-tempfile();dir.create(report);effective<-mq_protocol(c)
q<-mq_config_summary(c,effective,report,'planejar')
ok('precedencia_e_orientacao',q$efetivo[q$variavel=='grade_m']=='100, 100'&&grepl('parametros_protocolo$grade_m',q$orientacao[q$variavel=='grade_m'],fixed=TRUE))
a<-sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(rbind(c(1,1),c(2,1),c(2,2),c(1,2),c(1,1)))),crs=3857))
ctx<-list(list(chave='EXTERNO',origem=c(0,0),wkt=NULL,uc=''))
err<-tryCatch(mq_grid(a,ctx,3857,effective,report=report),error=conditionMessage)
ok('grade_vazia_variavel_certa',grepl('parametros_protocolo$grade_m',err,fixed=TRUE)&&file.exists(file.path(report,'diagnostico_grade.json')))
# Cobertura parcial é conferida por pixels/alpha, não apenas por envelope.
d<-tempfile();dir.create(d);c<-MQ_CONFIG;c$cache_dir<-d;c$saida<-tempfile();c$caches_adicionais<-character();c$confirmar_sentinel<-FALSE
dir.create(file.path(d,'sentinel'))
r<-terra::rast(nrows=10,ncols=10,nlyrs=4,xmin=500000,xmax=500100,ymin=8000000,ymax=8000100,crs='EPSG:3857')
area<-sf::st_as_sf(terra::as.polygons(terra::ext(r),crs=terra::crs(r)));xy<-terra::xyFromCell(r,1:terra::ncell(r))
make<-function(name,mask,value){
 p<-file.path(d,'sentinel',paste0(name,'.tif'));z<-r;terra::values(z)<-cbind(rep(value,100),rep(value,100),rep(value,100),ifelse(mask,255,0))
 tmp<-tempfile(fileext='.tif');terra::writeRaster(z,tmp,datatype='INT1U',overwrite=TRUE);sf::gdal_utils('translate',tmp,p,options=c('-a_nodata','none','-colorinterp','red,green,blue,alpha'),quiet=TRUE);mq_seal(p)
 mq_json(list(produto='Sentinel-2 L2A',resolucao_nativa_m=10,versao_acervo='acervo_01',sha256=mq_hash(p),cenas=data.frame(item=name,data='2026-01-01',fonte='fixture')),paste0(p,'.fonte.json'));p
}
left<-make('left',xy[,1]<500060,80);right<-make('right',xy[,1]>=500050,150)
miss<-mq_sentinel_missing(area,left)
ok('lacuna_parcial_exata',abs(sum(as.numeric(sf::st_area(miss)))-4000)<.1)
ok('uniao_local_completa',all(sf::st_is_empty(mq_sentinel_missing(area,c(left,right)))))
# Força rede proibida; exportador local mantido leve, sem alterar avaliação de cobertura.
env<-new.env(parent=globalenv());evalq({
 mq_sentinel<-get('mq_sentinel',envir=globalenv());environment(mq_sentinel)<-environment()
 monitora_qfield_sentinel<-function(...)stop('REDE_NAO_PERMITIDA')
 monitora_qfield_mbtiles<-function(rgb,path,nota)sf::gdal_utils('translate',rgb,path,options=c('-of','GTiff'),quiet=TRUE)
},env)
scratch<-tempfile();dir.create(scratch)
target<-env$mq_sentinel(area,NULL,c,scratch,report)
ev<-jsonlite::read_json(file.path(report,'sentinel_fonte.json'))
ok('sentinel_mosaico_sem_rede',file.exists(target)&&ev$acao=='mosaico_local'&&mq_sentinel_covers(target,area))
again<-env$mq_sentinel(area,NULL,c,scratch,report)
ok('retomada_sem_download',identical(target,again))
# Retirada do cache composto somente no fixture; gap legítimo deve bloquear sem autorização.
unlink(c(target,paste0(target,c('.sha256','.fonte.json'))))
unlink(c(right,paste0(right,c('.sha256','.fonte.json'))))
gap<-tryCatch(env$mq_sentinel(area,NULL,c,scratch,report),error=conditionMessage)
ok('lacuna_sem_autorizacao_preserva_cache',grepl('complemento',gap)&&mq_verified(left))
# Adquirir somente o complemento; falha posterior não pode invalidar fonte anterior.
c$confirmar_sentinel<-TRUE
env$monitora_qfield_sentinel<-function(pontos,scratch,limite,cache_dir,versao_acervo){
 stopifnot(abs(sum(as.numeric(sf::st_area(limite)))-4000)<.1)
 p<-make('new_right',xy[,1]>=500060,190)
 list(rgb=p,metadados=data.frame(item='new',data='2026-01-01',fonte='fixture'))
}
target<-env$mq_sentinel(area,NULL,c,scratch,report)
ok('download_restrito_a_lacuna',mq_sentinel_covers(target,area)&&mq_verified(left))
cat('TOTAL_RETOMADA',n,'PASS\n')
# Entrada ambígua não vira AE automaticamente.
input<-tempfile();dir.create(input);work<-tempfile();dir.create(work)
sf::st_write(a,file.path(input,'area_elegivel.gpkg'),quiet=TRUE)
before<-mq_hash(file.path(input,'area_elegivel.gpkg'))
err<-tryCatch(mq_read(input,work),error=conditionMessage)
ok('nome_incorreto_orientado_e_preservado',grepl('plural',err)&&grepl('area_elegivel.gpkg',err,fixed=TRUE)&&identical(before,mq_hash(file.path(input,'area_elegivel.gpkg'))))
# Seleção automática considera teste real; seleção explícita nunca troca a instalação.
e<-new.env(parent=globalenv())
evalq({
 mq_qgis_prepare<-get('mq_qgis_prepare',envir=globalenv());environment(mq_qgis_prepare)<-environment()
 Sys.glob<-function(...)c('C:/Program Files/QGIS 3.44.9/bin/python-qgis-ltr.bat','C:/Program Files/QGIS 4.2.3/bin/python-qgis.bat')
 calls<-character()
 mq_qgis_call<-function(runtime,script,verb,output,log){
   calls<<-c(calls,runtime$python);writeLines('fixture',log)
   if(grepl('4.2.3',runtime$python,fixed=TRUE))stop('fixture incompatível')
   mq_json(list(status='PASS',QGIS='3.44.9'),output)
 }
},e)
root<-tempfile();dir.create(root);dir.create(file.path(root,'02_relatorio'));tmp<-tempfile();dir.create(tmp)
cc<-MQ_CONFIG
chosen<-e$mq_qgis_prepare(cc,root,tmp)
ok('qgis_fallback_aprovado',grepl('3.44.9',chosen$python,fixed=TRUE)&&grepl('4.2.3',e$calls[1],fixed=TRUE))
cc$qgis_python<-'C:/Program Files/QGIS 4.2.3/bin/python-qgis.bat';e$calls<-character()
err<-tryCatch(e$mq_qgis_prepare(cc,root,tmp),error=conditionMessage)
ok('qgis_explicito_sem_substituicao',length(e$calls)==1&&grepl('nenhuma substituição',err))
cat('TOTAL_RETOMADA_FINAL',n,'PASS\n')
