# Reproduz entrada KML/KMZ XYZ combinada aos raios XY das imagens.
source('tests/test_core.R',encoding='UTF-8');start_dimensions<-length(checks)
input_z<-tempfile();dir.create(input_z)
aez<-st_sf(geometry=st_sfc(st_polygon(list(cbind(matrix(c(198000,8432000,199000,8432000,199000,8433000,198000,8433000,198000,8432000),ncol=2,byrow=TRUE),z=100))),crs=31983))
zfile<-file.path(input_z,'areas_elegiveis.gpkg');st_write(aez,zfile,quiet=TRUE);hash_z<-mq_hash(zfile)
cz<-co;cz$entrada<-input_z;cz$saida<-tempfile();rz<-e$monitora_planejamento_amostral(cz)
ok('pipeline_AE_XYZ_raios_XY',file.exists(file.path(rz$pasta,'01_qfield/pacote_qfield.zip')))
ok('arquivo_XYZ_original_preservado',identical(hash_z,mq_hash(zfile))&&class(st_geometry(st_read(zfile,quiet=TRUE))[[1]])[1]=='XYZ')
context<-st_read(file.path(rz$pasta,'02_relatorio/contexto_imagens.gpkg'),quiet=TRUE)
ok('contexto_derivado_XY_valido',class(st_geometry(context)[[1]])[1]=='XY'&&all(st_is_valid(context)))
mask<-st_geometry(st_read(file.path(rz$pasta,'02_relatorio/centros_recorte.gpkg'),layer='uniao_raios',quiet=TRUE));expected<-st_buffer(st_union(c(st_zm(st_geometry(aez)),mask)),co$margem_contexto_m)
ok('contexto_cobre_AE_e_raios_com_margem',lengths(st_equals(st_geometry(context),expected))==1L)
# Combinações com/sem UC e Z/M; nenhuma coordenada original é modificada.
for(dim in c('XY','XYZ','XYM','XYZM')){
 xy<-matrix(c(0,0,100,0,100,100,0,100,0,0),ncol=2,byrow=TRUE)
 coords<-switch(dim,XY=xy,XYZ=cbind(xy,9),XYM=cbind(xy,7),XYZM=cbind(xy,9,7))
 geom<-st_sfc(st_polygon(list(coords),dim=dim),crs=31983);area<-st_sf(geometry=geom);saved<-st_as_binary(geom)
 uc<-st_sf(geometry=st_buffer(st_zm(geom),100));mk<-st_buffer(st_sfc(st_point(c(50,50)),crs=31983),500)
 for(has_uc in c(FALSE,TRUE)){
  bound<-if(has_uc)uc else mq_empty(31983);ctx<-mq_image_context(area,bound,mk,500)
  expected<-st_buffer(st_union(c(st_zm(geom),if(has_uc)st_geometry(uc)else st_sfc(crs=31983),mk)),500)
  ok(paste0('dimensao_',dim,'_UC_',has_uc),all(st_is_valid(ctx))&&lengths(st_equals(ctx,expected))==1&&identical(saved,st_as_binary(st_geometry(area))))
 }
}
cat('TOTAL_DIMENSOES_IMAGENS ',length(checks)-start_dimensions,' PASS\n',sep='')
