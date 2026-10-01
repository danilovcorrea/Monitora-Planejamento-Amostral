source('tests/test_core.R',encoding='UTF-8');start<-length(checks)
for(profile in c('campestre_savanico','ilha')) {
 input<-tempfile();dir.create(input);ae<-st_read(file.path(co$entrada,'areas_elegiveis.gpkg'),quiet=TRUE);ae$formacao<-if(profile=='ilha')'florestal'else 'pastagem';st_write(ae,file.path(input,'areas_elegiveis.gpkg'),quiet=TRUE)
 cfg<-co;cfg$entrada<-input;cfg$saida<-tempfile();cfg$perfil<-profile;cfg$estratificar_vegetacao<-TRUE;cfg$formacao_campo<-'formacao';cfg$prioritarios<-list(n=NULL,percentual=100);cfg$politica_insuficiencia<-'usar_disponiveis';cfg$parametros_protocolo<-list(transecto_m=25,grade_m=c(100,100))
 result<-e$monitora_criar_qfield(cfg);out<-result$pasta;q<-data.table::fread(file.path(out,'02_relatorio/selecao_quantidades.csv'))
 ok(paste0(profile,'_pacote_promovido'),file.exists(file.path(out,'01_qfield/pacote_qfield.zip')))
 ok(paste0(profile,'_adaptado_sem_alternativos'),q$realizado[q$categoria=='alternativos']==0&&q$deficit[q$categoria=='alternativos']>0)
 html<-paste(readLines(file.path(out,'02_relatorio/relatorio_execucao.html'),warn=FALSE),collapse=' ')
 ok(paste0(profile,'_deficit_no_html'),grepl('quantitativos reduzidos',html,fixed=TRUE))
 if(profile=='campestre_savanico')ok('pastagem_html_ocorrencias',grepl('PAs em pastagem/degradada',html,fixed=TRUE)&&file.exists(file.path(out,'02_relatorio/ocorrencias_vegetacao.csv')))
 else ok('ilha_sem_procedimento_campestre',!file.exists(file.path(out,'02_relatorio/diagnosticos/simulacoes_direcoes.csv')))
}
cat('TOTAL_ADAPTATIVO_INTEGRACAO',length(checks)-start,'PASS\n')
