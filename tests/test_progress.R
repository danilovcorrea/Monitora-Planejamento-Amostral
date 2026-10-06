# Força o backend interativo mesmo no Rscript/CI; não depende de terminal físico.
options(monitora.qfield.somente_funcoes=TRUE)
source('monitora_planejamento_amostral.R',encoding='UTF-8');MQ_CONFIG<-mq_legacy_defaults() # Regressão da interface pública 1.0.2
checks<-character();ok<-function(n,v){stopifnot(isTRUE(v));checks<<-c(checks,n);cat('PASS ',n,'\n',sep='')}
run<-function(){
 old<-options(cli.dynamic=TRUE,cli.progress_show_after=0,cli.progress_clear=FALSE);on.exit(options(old),add=TRUE)
 baseline<-cli::cli_progress_num()
 b<-mq_progress('Teste interativo',3);on.exit(mq_progress_done(b),add=TRUE)
 ok('barra_sobrevive_retorno_da_funcao_criadora',cli::cli_progress_num()==baseline+1L)
 mq_progress_update(b,1,'primeira etapa');cli::cli_progress_update(id=b$id,set=1,force=TRUE)
 ok('atualizacao_e_renderizacao_funcionam',b$state$current==1)
 mq_progress_update(b,3,'fim');mq_progress_update(b,3,'fim repetido')
 ok('cem_porcento_aguarda_encerramento_explicito',cli::cli_progress_num()==baseline+1L)
 mq_progress_done(b);mq_progress_done(b)
 ok('encerramento_repetido_seguro',cli::cli_progress_num()==baseline)
 nested<-function(){
   a<-mq_progress('Externa',2);on.exit(mq_progress_done(a),add=TRUE)
   inner<-function(){z<-mq_progress('Interna',1);on.exit(mq_progress_done(z),add=TRUE);mq_progress_update(z,1)}
   inner();mq_progress_update(a,1);mq_progress_update(a,2)
 }
 nested();ok('barras_aninhadas_independentes',cli::cli_progress_num()==baseline)
 failed<-tryCatch((function(){a<-mq_progress('Erro operacional',3);on.exit(mq_progress_done(a),add=TRUE);mq_progress_update(a,1);stop('erro operacional preservado')})(),error=identity)
 ok('erro_original_e_limpeza_preservados',inherits(failed,'error')&&conditionMessage(failed)=='erro operacional preservado'&&cli::cli_progress_num()==baseline)
 for(dynamic in c(TRUE,FALSE))for(n in c(NA,0)){
  options(cli.dynamic=dynamic);a<-mq_progress('Total desconhecido ou vazio',n);mq_progress_update(a,1,'atividade');mq_progress_done(a)
  ok(paste0('total_',n,'_dinamico_',dynamic),cli::cli_progress_num()==baseline)
 }
 options(cli.dynamic=FALSE);a<-mq_progress('Texto',2);mq_progress_update(a,1);mq_progress_update(a,2);mq_progress_done(a)
 ok('backend_textual_preservado',a$state$current==2&&cli::cli_progress_num()==baseline)
}
run();cat('TOTAL ',length(checks),' PASS\n',sep='')
