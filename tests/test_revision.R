options(monitora.qfield.somente_funcoes=TRUE);source('monitora_planejamento_amostral.R',encoding='UTF-8');MQ_CONFIG<-mq_legacy_defaults() # Regressão da interface pública 1.0.2
suppressPackageStartupMessages(library(sf))
count<-0L;ok<-function(n,v){stopifnot(isTRUE(v));count<<-count+1L;cat('PASS',n,'\n')};fails<-function(expr)inherits(try(force(expr),silent=TRUE),'try-error')
rect<-function(a,b,c,d)st_polygon(list(matrix(c(a,b,c,b,c,d,a,d,a,b),ncol=2,byrow=TRUE)))
cr<-st_crs(31983);uc<-st_sf(cnuc='UC1',geometry=st_sfc(rect(500000,8000000,510000,8010000),crs=cr));ae<-st_sf(geometry=st_sfc(rect(501000,8001000,502000,8002000),crs=cr));cfg<-MQ_CONFIG;cfg$grade_m<-c(100,100);cfg$estratificar_vegetacao<-FALSE
ct<-mq_contexts(ae,uc,cr,cfg$grade_m);g<-mq_grid(ae,ct,cr,cfg);ct<-attr(g,'contextos');ok('id_anterior_ao_corte',all(g$id_malha==g$linha*101+g$coluna+1)&&min(g$id_grade)>1)
ok('PA_cinco_digitos',all(grepl('^PA[0-9]{5,}$',g$codigo_pa))&&!anyDuplicated(g$codigo_pa))
a2<-st_sf(geometry=st_sfc(rect(500500,8000500,503000,8003000),crs=cr));g2<-mq_grid(a2,ct,cr,cfg,g);ii<-match(g$chave_grade,g2$chave_grade);ok('expansao_preserva_todos_identificadores',identical(g$PA,g2$PA[ii])&&identical(g$id_grade,g2$id_grade[ii])&&identical(g$codigo_pa,g2$codigo_pa[ii]))
z<-g;z$id_grade<-seq_len(nrow(g));z$PA<-paste0('PA',z$id_grade);z$codigo_pa<-NULL;migrated<-mq_grid(ae,ct,cr,cfg,z);ok('migracao_legada_preserva_PA_id',identical(migrated$PA,z$PA)&&all(migrated$id_grade==z$id_grade)&&all(grepl('^PA[0-9]{5,}$',migrated$codigo_pa)))
# Fora do retângulo inicial, índices não podem colidir com a linha seguinte.
external<-list(list(chave='EXTERNO',origem=c(500000,8000000),uc='',wkt=NULL,ncol_ref=2,nlin_ref=2,base_id=0))
xa<-st_sf(geometry=st_sfc(rect(500000,8000000,500300,8000200),crs=cr));ex<-mq_grid(xa,external,cr,cfg);ok('expansao_externa_sem_colisao',!anyDuplicated(ex$id_grade)&&all(ex$id_grade[is.na(ex$id_malha)]>4))
# Mesma entidade em grade e prioritário usa o mesmo alias.
r<-tempfile();dir.create(r);pa<-list(x=z[1:2,],nome='PA_priorit',papel='PA_priorit',label='PA');gr<-list(x=z,nome='grade_amostral',papel='grade_amostral',label='PA');layers<-mq_pa_aliases(list(pa,gr),r);ok('alias_compartilhado_grade_PA',identical(layers[[1]]$x$codigo_pa,layers[[2]]$x$codigo_pa[1:2]))
# Entrada aninhada e dois arquivos homônimos identificados por caminho relativo.
inp<-tempfile();dir.create(file.path(inp,'ae'),recursive=TRUE);dir.create(file.path(inp,'apoio'),recursive=TRUE);st_write(ae,file.path(inp,'ae/areas_elegiveis.gpkg'),layer='areas_elegiveis',quiet=TRUE);st_write(z[1:2,],file.path(inp,'apoio/dados.gpkg'),layer='PA_priorit',quiet=TRUE);scratch<-tempfile();dir.create(scratch);rd<-mq_read(inp,scratch);ok('vetores_recursivos',length(rd$camadas)==2&&any(grepl('apoio/dados.gpkg',vapply(rd$camadas,`[[`,character(1),'fonte'),fixed=TRUE)))
writeLines('observacao',file.path(inp,'nota.txt'));inv<-data.frame(arquivo=mq_files(inp,relative=TRUE));invr<-mq_inventory_status(inv,rd$camadas,cfg);ok('inventario_ignorado_explicito',invr$estado[invr$arquivo=='nota.txt']=='ignorado')
base<-tempfile();dir.create(base);pc<-cfg;pc$pasta_base<-base;pc$entrada<-'entrada';pc$saida<-'saida';resolved<-mq_resolve_paths(pc);ok('caminhos_relativos_pasta_script',resolved$entrada==file.path(normalizePath(base,winslash='/'),'entrada')&&resolved$saida==file.path(normalizePath(base,winslash='/'),'saida'))
# Apoio individual vazio mantém esquema, FID e tipo.
support<-monitora_qfield_apoio_vazio();for(n in names(support))mq_write_gpkg(support[[n]],file.path(inp,'apoio',paste0(n,'.gpkg')),n)
rr<-mq_read(inp,scratch);ok('apoio_individual_vazio_reimportavel',all(c('pontos_interesse','trajeto')%in%vapply(rr$camadas,`[[`,character(1),'papel')))
# Mosaico com sobreposição real, transparência parcial e cache imutável.
create<-function(file,tiles,z=18L,size=256L){db<-DBI::dbConnect(RSQLite::SQLite(),file);on.exit(DBI::dbDisconnect(db));DBI::dbExecute(db,'CREATE TABLE tiles(zoom_level INTEGER,tile_column INTEGER,tile_row INTEGER,tile_data BLOB)');DBI::dbExecute(db,'CREATE TABLE metadata(name TEXT,value TEXT)');DBI::dbWriteTable(db,'metadata',data.frame(name=c('format','bounds','attribution'),value=c('png','-44,-18,-43,-17','fixture')),append=TRUE);for(i in seq_along(tiles))DBI::dbExecute(db,'INSERT INTO tiles VALUES(?,?,?,?)',params=list(z,100+i,200L,list(png::writePNG(tiles[[i]],target=raw()))))}
a<-array(0,c(256,256,4));a[,,1]<-1;a[,,4]<-.5;b<-a;b[,,1]<-0;b[,,3]<-1;b[,,4]<-1
p1<-file.path(r,'a.mbtiles');p2<-file.path(r,'b.mbtiles');create(p1,list(a));create(p2,list(b,b));cfg$cache_dir<-file.path(r,'cache');m<-mq_mosaic_mb(c(p1,p2),cfg,r);db<-DBI::dbConnect(RSQLite::SQLite(),m);d<-DBI::dbGetQuery(db,'SELECT * FROM tiles ORDER BY tile_column');DBI::dbDisconnect(db);pixel<-mq_rgba(d$tile_data[[1]])
ok('mosaico_uniao_2_tiles',nrow(d)==2);ok('alpha_parcial_source_over',abs(pixel[1,1,1]-.5)<.01&&abs(pixel[1,1,3]-.5)<.01&&pixel[1,1,4]==1)
stamp<-file.info(m)$mtime;ok('cache_mosaico_reutilizado',identical(m,mq_mosaic_mb(c(p1,p2),cfg,r))&&identical(stamp,file.info(m)$mtime))
rev<-mq_mosaic_mb(c(p2,p1),cfg,r);ok('prioridade_invalida_cache',rev!=m)
# 512 pixels normaliza para 4 tiles de 256 no zoom seguinte, preservando quadrantes.
a512<-array(0,c(512,512,4));a512[,,4]<-1;a512[1:256,1:256,1]<-1;a512[257:512,257:512,3]<-1;p3<-file.path(r,'512.mbtiles');create(p3,list(a512),size=512L)
mm<-mq_mosaic_mb(p3,cfg,r);db<-DBI::dbConnect(RSQLite::SQLite(),mm);d<-DBI::dbGetQuery(db,'SELECT * FROM tiles WHERE zoom_level=19 ORDER BY tile_column,tile_row');DBI::dbDisconnect(db);ok('normalizacao_512_para256',nrow(d)==4&&all(vapply(d$tile_data,function(x)dim(mq_rgba(x))[1]==256,logical(1))))
ok('normalizacao_orientacao_TMS',mq_rgba(d$tile_data[[2]])[1,1,1]==1&&mq_rgba(d$tile_data[[3]])[1,1,3]==1)
cat('TOTAL_REVISAO',count,'PASS\n')
# Grade inclui a mesma entidade selecionada; identidade contraditória deve ser diagnosticada.
legacy<-z;legacy$codigo_pa<-NA_character_;gx<-mq_grid(ae,ct,cr,cfg,legacy);ok('historico_sem_alias_preenchido',!anyNA(gx$codigo_pa)&&all(grepl('^PA[0-9]{5,}$',gx$codigo_pa)))
# Origens diferentes têm faixas distintas, inclusive em expansão para UC nova.
uc2<-rbind(uc,st_sf(cnuc='UC2',geometry=st_sfc(rect(520000,8000000,530000,8010000),crs=cr)));ae2<-rbind(ae,st_sf(geometry=st_sfc(rect(521000,8001000,522000,8002000),crs=cr)));multi<-mq_grid(ae2,mq_contexts(ae2,uc2,cr,cfg$grade_m),cr,cfg);ok('multiplas_UCs_codigos_unicos',!anyDuplicated(multi$id_grade)&&!anyDuplicated(multi$codigo_pa)&&length(unique(multi$malha))==2)
# Ordem reversa do inventário não muda a atribuição dos aliases fornecidos.
ll<-mq_pa_aliases(list(gr,pa),r);ok('alias_independe_ordem_camadas',identical(ll[[2]]$x$codigo_pa,layers[[1]]$x$codigo_pa))
# Resoluções z17 e z18: lacunas de nível alto recebem pixels do nível mais baixo.
p4<-file.path(r,'z17.mbtiles');create(p4,list(b),z=17L);mixed<-mq_mosaic_mb(c(p4,p1),cfg,r);db<-DBI::dbConnect(RSQLite::SQLite(),mixed);ns<-DBI::dbGetQuery(db,'SELECT zoom_level,count(*) n FROM tiles GROUP BY zoom_level');DBI::dbDisconnect(db);ok('mosaico_niveis_mistos',all(c(17,18)%in%ns$zoom_level)&&ns$n[ns$zoom_level==18]==5)
cat('TOTAL_REVISAO_FINAL',count,'PASS\n')

# Listas de zoom iguais não implicam pirâmides completas; preservar também alpha.
local_sparse<-file.path(r,'local_sparse.mbtiles');remote_sparse<-file.path(r,'remote_sparse.mbtiles')
create(local_sparse,list(b),z=17L);green<-b;green[,,3]<-0;green[,,2]<-1;create(remote_sparse,list(green),z=17L)
for(p in c(local_sparse,remote_sparse)) {
 db<-DBI::dbConnect(RSQLite::SQLite(),p)
 DBI::dbExecute(db,'INSERT INTO tiles VALUES(18,202,400,?)',params=list(list(png::writePNG(if(p==local_sparse)a else green,target=raw()))))
 DBI::dbDisconnect(db)
}
ms<-mq_mosaic_mb(c(local_sparse,remote_sparse),cfg,r);db<-DBI::dbConnect(RSQLite::SQLite(),ms)
dd<-DBI::dbGetQuery(db,'SELECT * FROM tiles WHERE zoom_level=18');DBI::dbDisconnect(db)
ok('mesmos_zooms_descendencia_parcial',nrow(dd)==4)
lo<-mq_rgba(dd$tile_data[[which(dd$tile_column==203&dd$tile_row==401)]])
ok('prioridade_local_inferior_sobre_remoto',all(lo[,,3]==1)&&all(lo[,,2]==0))
hi<-mq_rgba(dd$tile_data[[which(dd$tile_column==202&dd$tile_row==400)]])
ok('alpha_superior_complementado_inferior',abs(hi[1,1,1]-.5)<.01&&abs(hi[1,1,3]-.5)<.01&&hi[1,1,4]==1)
# A máscara final evita que reamostragem amplie pixels para fora do raio.
h<-20037508.342789244;w<-2*h/2^18;cx<- -h+202*w-400;cy<- -h+400.5*w
mask500<-st_buffer(st_sfc(st_point(c(cx,cy)),crs=3857),500,nQuadSegs=90)
masked<-mq_mosaic_mb(c(local_sparse,remote_sparse),cfg,r,mask500);db<-DBI::dbConnect(RSQLite::SQLite(),masked)
md<-DBI::dbGetQuery(db,'SELECT * FROM tiles WHERE zoom_level=18');DBI::dbDisconnect(db)
outside<-0;valid<-0
for(i in seq_len(nrow(md))) {
 aa<-mq_rgba(md$tile_data[[i]]);rr<-terra::rast(nrows=256,ncols=256,xmin=-h+md$tile_column[i]*w,xmax=-h+(md$tile_column[i]+1)*w,ymin=-h+md$tile_row[i]*w,ymax=-h+(md$tile_row[i]+1)*w,crs='EPSG:3857')
 # Oráculo independente: interseção vetorial dos centros dos pixels visíveis.
 cells<-which(as.vector(t(aa[,,4]))>0);xy<-terra::xyFromCell(rr,cells);pts<-st_as_sf(data.frame(x=xy[,1],y=xy[,2]),coords=c('x','y'),crs=3857)
 outside<-outside+sum(lengths(st_intersects(pts,mask500))==0);valid<-valid+length(cells)
}
ok('mascara_500m_reaplicada_apos_reamostrar',outside==0&&valid>0)
cat('TOTAL_REVISAO_FINAL',count,'PASS\n')

# Pirâmide esparsa: um tile z17 sem descendentes e outro z18 distante na mesma fonte.
db<-DBI::dbConnect(RSQLite::SQLite(),p4);DBI::dbExecute(db,'INSERT INTO tiles VALUES(18,110,210,?)',params=list(list(png::writePNG(b,target=raw()))));DBI::dbDisconnect(db)
sp<-mq_mosaic_mb(c(p4,p3),cfg,r);db<-DBI::dbConnect(RSQLite::SQLite(),sp);spkeys<-DBI::dbGetQuery(db,'SELECT * FROM tiles WHERE zoom_level=19 AND tile_column BETWEEN 404 AND 407 AND tile_row BETWEEN 800 AND 803');DBI::dbDisconnect(db)
ok('piramide_esparsa_preserva_zoom_inferior',nrow(spkeys)==16)
cat('TOTAL_REVISAO_FINAL',count,'PASS\n')

short<-pa;short$x$codigo_pa<-c('PA7','PA12');ss<-mq_pa_aliases(list(short),r);ok('codigo_pa_preexistente_curto_corrigido',all(grepl('^PA[0-9]{5,}$',ss[[1]]$x$codigo_pa))&&identical(ss[[1]]$x$codigo_pa_anterior,c('PA7','PA12')))
bad<-pa;st_geometry(bad$x)<-st_geometry(bad$x)+c(1,0);st_crs(bad$x)<-cr;ok('chave_grade_conflito_espacial_bloqueado',fails(mq_pa_aliases(list(gr,bad),r)))
cat('TOTAL_REVISAO_FINAL',count,'PASS\n')
# Região inteira exclusivamente no nível inferior, apesar de ambas as fontes terem z17/z18.
db<-DBI::dbConnect(RSQLite::SQLite(),local_sparse);DBI::dbExecute(db,'INSERT INTO tiles VALUES(17,102,200,?)',params=list(list(png::writePNG(b,target=raw()))));DBI::dbDisconnect(db)
whole<-mq_mosaic_mb(c(local_sparse,remote_sparse),cfg,r);db<-DBI::dbConnect(RSQLite::SQLite(),whole);nw<-DBI::dbGetQuery(db,'SELECT count(*) n FROM tiles WHERE zoom_level=18 AND tile_column IN (204,205) AND tile_row IN (400,401)')$n;DBI::dbDisconnect(db)
ok('mesmos_zooms_area_so_inferior',nw==4)
cat('TOTAL_REVISAO_FINAL',count,'PASS\n')
