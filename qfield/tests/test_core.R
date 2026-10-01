options(monitora.qfield.somente_funcoes=TRUE)
source('qfield/monitora_criar_qfield.R',encoding='UTF-8')
suppressPackageStartupMessages(library(sf))
checks<-character();ok<-function(name,v){if(!isTRUE(v))stop(name);checks<<-c(checks,name);cat('PASS ',name,'\n',sep='')}
fails<-function(expr)inherits(try(force(expr),silent=TRUE),'try-error')
rect<-function(x1,y1,x2,y2)st_polygon(list(matrix(c(x1,y1,x2,y1,x2,y2,x1,y2,x1,y1),ncol=2,byrow=TRUE)))
cr<-st_crs(31983);a<-st_sf(geometry=st_sfc(rect(0,0,300,300),rect(100,100,400,400),crs=cr));c<-MQ_CONFIG;c$estratificar_vegetacao<-FALSE;c$grade_m<-c(100,100);c$mapbiomas<-FALSE
ctx<-list(list(chave='EXTERNO',origem=c(0,0),wkt=NULL,uc=''))
g<-mq_grid(a,ctx,cr,c);ok('union_vertices_unicos',nrow(g)==23 && !anyDuplicated(g$chave_grade));ok('vertices_nao_centroides',all(g$x_m%%100==0 & g$y_m%%100==0))
sel<-mq_select(g,c);ok('cotas_globais',sum(sel$categoria=='prioritario')==5 && sum(sel$categoria=='alternativo')==10)
ok('ordem_nao_altera_selecao',identical(mq_select(g[rev(seq_len(nrow(g))),],c)$categoria[order(g$id_grade[rev(seq_len(nrow(g)))])],sel$categoria))
ex<-mq_grid(st_sf(geometry=st_sfc(rect(-200,-200,500,500),crs=cr)),ctx,cr,c,sel);ix<-match(sel$chave_grade,ex$chave_grade);ok('expansao_identidade_posicao',identical(sel$id_grade,ex$id_grade[ix]) && all(st_coordinates(sel)==st_coordinates(ex[ix,])))
ok('expansao_preserva_selecao',identical(sel$categoria,ex$categoria[ix]));ok('ids_novos_nao_colidem',!anyDuplicated(ex$id_grade))
for(N in 1:2)ok(paste0('cota_insuficiente_',N),fails(mq_select(g[seq_len(N),],c)))
ok('contagem_explicita',mq_quantity(list(n=3,percentual=NULL),23)==3);ok('dupla_quantidade_rejeitada',fails(mq_quantity(list(n=3,percentual=20),23)))
outer<-matrix(c(0,0,400,0,400,400,0,400,0,0),ncol=2,byrow=TRUE);hole<-matrix(c(50,50,50,350,350,350,350,50,50,50),ncol=2,byrow=TRUE)
h<-mq_grid(st_sf(geometry=st_sfc(st_polygon(list(outer,hole)),crs=cr)),ctx,cr,c);ok('buraco_excluido',nrow(h)==16)
uc<-st_sf(cnuc='A',nomeuc='UC',geometry=st_sfc(rect(0,0,200,400),crs=cr));ae<-st_sf(geometry=st_sfc(rect(0,0,400,400),crs=cr));ct<-mq_contexts(ae,uc,cr,c$grade_m);gg<-mq_grid(ae,ct,cr,c);ok('UC_parcial_sem_duplicata_fronteira',nrow(gg)==25 && !anyDuplicated(paste(gg$x_m,gg$y_m)))
ok('EPSG_automatico',mq_crs(st_transform(st_sf(geometry=st_sfc(rect(198000,8432000,199000,8433000),crs=cr)),4326))$epsg==31983)
p<-tempfile();dir.create(p);st_write(st_sf(geometry=st_sfc(rect(198000,8432000,199000,8433000),crs=cr)),file.path(p,'areas_elegiveis.gpkg'),quiet=TRUE);sc<-tempfile();dir.create(sc);rd<-mq_read(p,sc);ok('entrada_AE_padrao',length(rd$camadas)==1)
wrong<-tempfile();dir.create(wrong);st_write(a,file.path(wrong,'outros_poligonos.gpkg'),quiet=TRUE);ok('nao_inferir_AE_generica',fails(mq_read(wrong,sc)))
# Integração local usa resposta federal sintética somente no ambiente deste teste; produto normal sempre consulta rede.
e<-new.env(parent=globalenv());sys.source('qfield/monitora_criar_qfield.R',envir=e)
e$mq_uc<-function(ae,c)list(x=mq_empty(4326),fonte='fixture',camada='fixture',titulo='fixture sem UC',consulta='teste',total_bbox=0,status='consulta_completa')
# Fundo sintético amplo: valida o fluxo obrigatório sem consultar imagens externas.
br<-st_bbox(st_transform(st_buffer(st_read(file.path(p,'areas_elegiveis.gpkg'),quiet=TRUE),2000),3857))
rr<-terra::rast(nrows=128,ncols=128,nlyrs=3,xmin=br[1],xmax=br[3],ymin=br[2],ymax=br[4],crs='EPSG:3857');terra::values(rr)<-100
rt<-tempfile(fileext='.tif');terra::writeRaster(rr,rt,datatype="INT1U");rs<-tempfile(fileext='.mbtiles');monitora_qfield_mbtiles(rt,rs,'fixture sintética')
mq_json(list(produto='Sentinel-2 L2A',resolucao_nativa_m=10,sha256=mq_hash(rs),versao_acervo='fixture',origem='TESTE SINTÉTICO; sem imagem real'),paste0(rs,'.fonte.json'))
co<-MQ_CONFIG;co$gerar_cartografia<-FALSE;co$estratificar_vegetacao<-FALSE;co$sentinel_arquivo<-rs;co$baixar_imagem_detalhe<-FALSE;co$cache_dir<-tempfile();co$entrada<-p;co$saida<-tempfile();co$mapbiomas<-FALSE;co$projeto<-'Teste sintético'
r<-e$monitora_criar_qfield(co);ok('pacote_final_existe',file.exists(file.path(r$pasta,'01_qfield','pacote_qfield.zip')))
reg<-st_read(file.path(r$pasta,'01_qfield','dados','grade_amostral.gpkg'),layer='grade_amostral',quiet=TRUE);ok('PA_id_grade',identical(reg$PA,reg$codigo_pa)&&all(grepl('^PA[0-9]{5,}$',reg$codigo_pa)))
# Teste de falha remota: nenhuma promoção final.
e$mq_uc<-function(ae,c)stop('consulta indisponível');co$saida<-tempfile();ok('falha_UC_bloqueia',fails(e$monitora_criar_qfield(co)));ok('falha_relatorio_preservado',length(list.files(co$saida,pattern='resultado.json',recursive=TRUE))==1)
cat('TOTAL ',length(checks),' PASS\n',sep='')
# Regressões da revisão independente.
zero<-c;zero$prioritarios<-list(n=0,percentual=NULL);zero$alternativos<-list(n=0,percentual=NULL);ok('cotas_zero_sem_PA',all(mq_select(g,zero)$categoria=='grade'))
# Nova UC só altera malha na área nunca processada; vértices antigos externos permanecem.
orig<-st_sf(geometry=st_sfc(rect(0,0,100,100),crs=cr));prior<-list(grade_m=c$grade_m,epsg=31983,contextos=ctx,dominio_processado_wkt=st_as_text(st_union(orig)))
newuc<-st_sf(cnuc='NOVA',nomeuc='Nova',geometry=st_sfc(rect(50,0,450,450),crs=cr))
newctx<-mq_contexts(ae,newuc,cr,c$grade_m,prior);ok('expansao_nova_UC',any(vapply(newctx,function(z)z$chave=='UC_NOVA',logical(1))))
oldg<-mq_select(mq_grid(orig,ctx,cr,c),zero);newg<-mq_grid(ae,newctx,cr,c,oldg);ix<-match(oldg$chave_grade,newg$chave_grade);ok('nova_UC_preserva_malha_preexistente',!anyNA(ix)&&identical(oldg$id_grade,newg$id_grade[ix]))
# Formatos de entrada e pares de distâncias reais.
kdir<-tempfile();dir.create(kdir);xp<-g[1:3,];st_geometry(xp)<-st_set_crs(st_geometry(xp)+c(198000,8432000),cr);xp$PA<-paste0('PA',xp$id_grade);mq_kml(xp,'PA',file.path(kdir,'PA_priorit.kml'));zip::zipr(file.path(kdir,'PA_priorit.kmz'),'PA_priorit.kml',root=kdir)
unlink(file.path(kdir,'PA_priorit.kml'));file.copy(file.path(p,'areas_elegiveis.gpkg'),kdir);ok('entrada_KMZ',length(mq_read(kdir,sc)$camadas)==2)
sdir<-tempfile();dir.create(sdir);st_write(st_transform(st_set_crs(st_geometry(a)+c(198000,8432000),cr),4326),file.path(sdir,'areas_elegiveis.shp'),quiet=TRUE);zdir<-tempfile();dir.create(zdir);zip::zipr(file.path(zdir,'areas_elegiveis.zip'),list.files(sdir),root=sdir);ok('entrada_ZIP_shapefile',length(mq_read(zdir,sc)$camadas)==1)
a1<-st_sfc(st_linestring(matrix(c(0,0,50,0),2,2,byrow=TRUE)),crs=cr);a2<-st_sfc(st_linestring(matrix(c(156.25,0,106.25,0),2,2,byrow=TRUE)),crs=cr);ok('grade_nao_garante_100m',abs(as.numeric(st_distance(a1,a2))-56.25)<1e-8)
cat('TOTAL_FINAL ',length(checks),' PASS\n',sep='')

# Camadas vazias precisam aceitar captura; GEOMETRY genérica impede o QGIS de gravar.
su<-rbind(data.frame(st_layers(file.path(r$pasta,'01_qfield/dados/pontos_interesse.gpkg'))[c('name','geomtype')]),data.frame(st_layers(file.path(r$pasta,'01_qfield/dados/trajeto.gpkg'))[c('name','geomtype')]))
ok('apoio_vazio_tipado',identical(as.character(su$geomtype),c('Point','Multi Line String')))
zco<-co;zco$saida<-tempfile();zco$prioritarios<-list(n=0,percentual=NULL);zco$alternativos<-list(n=0,percentual=NULL)
e$mq_uc<-function(ae,c)list(x=mq_empty(4326),fonte='fixture',camada='fixture',titulo='fixture',consulta='teste',total_bbox=0,status='consulta_completa')
zr<-e$monitora_criar_qfield(zco);zg<-list(name=c('PA_priorit','PA_altern'),features=vapply(c('PA_priorit','PA_altern'),function(n)st_layers(file.path(zr$pasta,'01_qfield/dados',paste0(n,'.gpkg')))$features,numeric(1)))
ok('exportacao_cotas_zero',all(zg$features[match(c('PA_priorit','PA_altern'),zg$name)]==0))
exco<-co;exco$modo<-'expandir';exco$saida<-tempfile();exco$referencia_anterior<-file.path(r$pasta,'02_relatorio')
er<-e$monitora_criar_qfield(exco);eg<-st_read(file.path(er$pasta,'01_qfield/dados/grade_amostral.gpkg'),layer='grade_amostral',quiet=TRUE)
ok('expansao_integral_idempotente',identical(reg$PA,eg$PA)&&identical(reg$categoria,eg$categoria)&&all(st_coordinates(reg)==st_coordinates(eg)))
# Exportações entregues: atributos e coordenadas após reabrir KML/CSV.
kfile<-list.files(file.path(r$pasta,'03_vetores/kml'),pattern='grade',full.names=TRUE)
kdoc<-xml2::read_xml(kfile);kn<-xml2::xml_find_all(kdoc,'//*[local-name()="Placemark"]')
ok('kml_atributos_completos',length(kn)==nrow(reg)&&length(xml2::xml_find_all(kn[[1]],'.//*[local-name()="Data"]'))==ncol(st_drop_geometry(reg)))
cat('TOTAL_HOMOLOGACAO ',length(checks),' PASS\n',sep='')

prevuc<-prior;prevuc$contextos<-list(list(chave='UC_ANTIGA',uc='ANTIGA',origem=c(0,0),wkt=st_as_text(st_sfc(rect(0,0,300,400),crs=cr))),ctx[[1]])
ok('expansao_bloqueia_UCs_sobrepostas',fails(mq_contexts(ae,newuc,cr,c$grade_m,prevuc)))
cat('TOTAL_FINAL_VALIDADO ',length(checks),' PASS\n',sep='')
