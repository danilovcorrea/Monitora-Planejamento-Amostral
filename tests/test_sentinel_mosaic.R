options(monitora.qfield.somente_funcoes=TRUE);source('monitora_planejamento_amostral.R',encoding='UTF-8');MQ_CONFIG<-mq_legacy_defaults() # Regressão da interface pública 1.0.2
library(sf);library(terra)
d<-tempfile();dir.create(d);n<-0L;ok<-function(name,v){stopifnot(isTRUE(v));n<<-n+1L;cat('PASS',name,'\n')}
r<-rast(nrows=10,ncols=10,nlyrs=4,xmin=500000,xmax=500100,ymin=8000000,ymax=8000100,crs='EPSG:3857');a<-st_as_sf(as.polygons(ext(r),crs=crs(r)));xy<-xyFromCell(r,1:ncell(r))
make<-function(path,mask,value){x<-r;values(x)<-cbind(rep(value,100),rep(value,100),rep(value,100),ifelse(mask,255,0));tmp<-tempfile(fileext='.tif');writeRaster(x,tmp,datatype='INT1U',overwrite=TRUE);sf::gdal_utils('translate',tmp,path,options=c('-a_nodata','none','-colorinterp','red,green,blue,alpha'),quiet=TRUE)}
left<-file.path(d,'left.tif');right<-file.path(d,'right.tif');vrt<-file.path(d,'both.vrt')
make(left,xy[,1]<500060,80);make(right,xy[,1]>=500050,150)
ok('cena_parcial_rejeitada',!mq_sentinel_covers(left,a))
sf::gdal_utils('buildvrt',c(right,left),vrt,options=c('-resolution','highest'),quiet=TRUE)
ok('cenas_complementares_cobrem_contexto',mq_sentinel_covers(vrt,a))
z<-rast(vrt);v<-values(z);ok('prioridade_visual_e_preenchimento',all(v[xy[,1]<500060,1]==80)&&all(v[xy[,1]>=500060,1]==150))
make(right,xy[,1]>=500070,150);unlink(vrt);sf::gdal_utils('buildvrt',c(right,left),vrt,options=c('-resolution','highest'),quiet=TRUE)
ok('lacuna_interna_rejeitada',!mq_sentinel_covers(vrt,a))
cat('TOTAL_SENTINEL',n,'PASS\n')
# Muitas cenas da mesma passagem não podem ocultar a complementar depois do limite.
poly<-function(x1,x2)st_polygon(list(rbind(c(x1,0),c(x2,0),c(x2,100),c(x1,100),c(x1,0))))
f<-st_sf(geometry=st_sfc(c(rep(list(poly(0,60)),17),list(poly(40,100))),crs=3857));target<-st_sfc(poly(0,100),crs=3857)
ok('complementar_antes_de_16_redundantes',mq_sentinel_choose(f,2:18,target,1L)==18L)
cat('TOTAL_SENTINEL_FINAL',n,'PASS\n')
