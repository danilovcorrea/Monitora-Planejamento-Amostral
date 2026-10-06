# Exportação completa com fontes sintéticas locais; nenhum acesso remoto neste teste.
source('tests/test_core.R',encoding='UTF-8');start<-length(checks)
e$mq_uc<-function(ae,c)list(x=mq_empty(4326),fonte='fixture',camada='fixture',titulo='fixture sem UC',consulta='teste',total_bbox=0,status='consulta_completa')
x<-st_read(file.path(p,'areas_elegiveis.gpkg'),quiet=TRUE);x$formacao<-'fora_alvo';x$condicao<-'restauracao'
st_write(x,file.path(p,'areas_elegiveis.gpkg'),delete_dsn=TRUE,quiet=TRUE)
cfg<-e$MQ_CONFIG;cfg$entrada<-p;cfg$saida<-tempfile();cfg$perfil<-'treinamento_navegacao';cfg$grade_m<-c(100,100)
cfg$gerar_cartografia<-FALSE;cfg$baixar_imagem_detalhe<-FALSE;cfg$confirmar_download<-FALSE;cfg$confirmar_sentinel<-FALSE;cfg$sentinel_arquivo<-rs;cfg$cache_dir<-tempfile()
cfg$confirmar_desvios<-TRUE
r<-e$monitora_criar_qfield(cfg);nav<-st_read(file.path(r$pasta,'02_relatorio/cadastro_grade.gpkg'),quiet=TRUE)
ok('navegacao_pacote_completo',file.exists(file.path(r$pasta,'01_qfield/pacote_qfield.zip'))&&sum(nav$categoria!='grade')>0)
ok('navegacao_sem_mapbiomas',!file.exists(file.path(r$pasta,'02_relatorio/fonte_mb_30m.json')))
inc<-cfg;inc$operacao<-'incrementar';inc$referencia_anterior<-file.path(r$pasta,'02_relatorio');inc$prioritarios<-list(n=2,percentual=NULL);inc$alternativos<-list(n=1,percentual=NULL)
ir<-e$monitora_criar_qfield(inc);ig<-st_read(file.path(ir$pasta,'02_relatorio/cadastro_grade.gpkg'),quiet=TRUE)
ok('incrementar_total_correto',sum(ig$categoria=='prioritario')==sum(nav$categoria=='prioritario')+2&&sum(ig$categoria=='alternativo')==sum(nav$categoria=='alternativo')+1)
ok('incrementar_geometria_identica',identical(st_geometry(nav),st_geometry(ig))&&identical(nav$id_grade,ig$id_grade)&&all(nav$categoria[nav$categoria!='grade']==ig$categoria[nav$categoria!='grade']))
for(profile in c('treinamento_campestre','monitoramento_campestre','treinamento_ilha','monitoramento_ilha')) {
 c<-cfg;c$perfil<-profile;c$mapbiomas<-FALSE;c$formacao_campo<-'formacao';c$condicao_campo<-'condicao';c$aplicar_cotas<-FALSE
 c$aplicar_afastamentos<-FALSE
 rr<-e$monitora_criar_qfield(c);g<-st_read(file.path(rr$pasta,'02_relatorio/cadastro_grade.gpkg'),quiet=TRUE)
 ok(paste0(profile,'_restauracao'),sum(g$categoria!='grade')>0&&all(g$mq_condicao=='restauracao')&&all(g$mq_formacao=='fora_alvo'))
 ok(paste0(profile,'_decisao_no_html'),grepl('ajustes_perfil',paste(readLines(file.path(rr$pasta,'02_relatorio/relatorio_execucao.html')),collapse=' ')))
}
# Real small urban AOI: selection only, no redownload or modification of source.
path<-'/mnt/c/R/AE_adarquia/entrada/areas_elegiveis.kmz'
if(file.exists(path)) {
 sc<-tempfile();dir.create(sc);d<-mq_read(dirname(path),sc);c<-mq_prepare_config(cfg);c$grade_m<-c(10,10);cr<-mq_crs(d$ae);ct<-mq_contexts(d$ae,mq_empty(4326),cr,c$grade_m);g<-mq_grid(d$ae,ct,cr,c);g<-mq_select_reviewed(g,c,sc)
 ok('AE_adarquia_real_170_vertices',nrow(g)==170&&sum(g$categoria=='prioritario')==34&&sum(g$categoria=='alternativo')==68)
}
cat('TOTAL_PROFILES_INTEGRACAO',length(checks)-start,'PASS\n')
