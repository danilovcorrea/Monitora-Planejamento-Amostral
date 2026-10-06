options(monitora.qfield.somente_funcoes=TRUE)
source('monitora_planejamento_amostral.R',encoding='UTF-8')
suppressPackageStartupMessages(library(sf))
n<-0;ok<-function(label,x){if(!isTRUE(x))stop(label);n<<-n+1;cat('PASS',label,'\n')}
fails<-function(e)inherits(try(force(e),silent=TRUE),'try-error')
r<-tempfile();dir.create(r)
for(p in c('treinamento_navegacao','treinamento_campestre','treinamento_ilha','monitoramento_campestre','monitoramento_ilha','personalizado')){
 cfg<-MQ_CONFIG;cfg$perfil<-p;c<-mq_prepare_config(cfg);mq_validate_config(c)
 ok(p,isTRUE(c$.config_v2)&&c$perfil_escolhido==p)
 cfg$grade_m<-c(17,23);cfg$transecto_m<-7;c<-mq_prepare_config(cfg)
 ok(paste0(p,'_override'),identical(c$grade_m,c(17,23))&&c$transecto_m==7)
}
cfg<-MQ_CONFIG;cfg$perfil<-'personalizado';c<-mq_prepare_config(cfg)
ok('personalizado_sem_filtros',!mq_design_active(c)&&!c$mapbiomas&&!any(unlist(c[c('aplicar_afastamentos','usar_estradas_terra','usar_formacao_florestal','usar_uas_existentes')])))
cfg$usar_estradas_terra<-TRUE;c<-mq_prepare_config(cfg);ok('controle_individual',c$usar_estradas_terra&&!c$usar_estradas_pavimentadas)
cfg$aplicar_afastamentos<-TRUE;cfg$usar_estradas_terra<-FALSE;c<-mq_prepare_config(cfg);ok('individual_prevalece',!c$usar_estradas_terra&&c$usar_uas_existentes)
cfg<-MQ_CONFIG;cfg$perfil<-'monitoramento_ilha';cfg$incluir_formacao_florestal<-FALSE;c<-mq_prepare_config(cfg);ok('ilha_nao_forca_floresta',!c$incluir_formacao_florestal)
cfg<-MQ_CONFIG;cfg$filtrar_vegetacao<-FALSE;cfg$aplicar_cotas<-FALSE;c<-mq_prepare_config(cfg);ok('campestre_desliga_filtro',!mq_design_active(c))
cfg$aplicar_cotas<-TRUE;c<-mq_prepare_config(cfg);ok('classifica_para_cotas_sem_filtrar',mq_design_active(c)&&!c$filtrar_vegetacao)
cfg<-MQ_CONFIG;cfg$perfil<-'personalizado';cfg$confirmar_desvios<-TRUE;c<-mq_prepare_config(cfg)
N<-12;g<-st_as_sf(data.frame(chave_grade=letters[1:N],id_grade=1:N,categoria='grade',x=198000+seq_len(N)*100,y=8432000,mb_codigo=24),coords=c('x','y'),crs=31983)
z<-mq_select_reviewed(g,c,r);ok('urbano_sem_restricoes',sum(z$categoria=='prioritario')==3&&sum(z$categoria=='alternativo')==6)
# Classification for quotas must not silently reactivate eligibility.
cc<-c;cc$aplicar_cotas<-TRUE;cc$estratificar_vegetacao<-TRUE;cc$mapbiomas<-TRUE
z<-mq_design_classify(g,list(),cc,r);ok('urbanos_classificados_sem_exclusao',all(z$mq_apto)&&all(z$mq_formacao=='fora_alvo'))
# Consent is explicit, logged and refused by default in batch.
cc<-c;cc$confirmar_desvios<-NULL;ok('batch_nao_autoriza',fails(mq_decide(cc,r,'teste_recusa','Teste sem resposta.')))
g$mq_apto<-FALSE;cc$confirmar_desvios<-FALSE;ok('zero_elegiveis_recusa',fails(mq_select_reviewed(g,cc,r)))
cc$confirmar_desvios<-TRUE;z<-mq_select_reviewed(g,cc,r);ok('zero_elegiveis_aceite_registrado',sum(z$categoria!='grade')==9&&all(!z$mq_apto_original))
g$mq_apto<-TRUE;g$categoria[1:3]<-'prioritario';g$categoria[4:5]<-'alternativo'
cc$operacao<-'incrementar';cc$quantidade_incremento<-'novos';cc$prioritarios<-list(n=2,percentual=NULL);cc$alternativos<-list(n=1,percentual=NULL)
ci<-mq_increment_config(g,cc,r);z<-mq_select_reviewed(g,ci,r)
ok('incremento_novos_preserva',sum(z$categoria=='prioritario')==5&&sum(z$categoria=='alternativo')==3&&identical(z$categoria[1:5],g$categoria[1:5]))
ok('incremento_posicoes_ids',identical(st_geometry(z),st_geometry(g))&&identical(z$id_grade,g$id_grade))
# Missing sources create reviewable decisions, rather than false clearance.
cc<-mq_prepare_config(MQ_CONFIG);cc$confirmar_desvios<-TRUE;t<-mq_road_sources(list(),cc,r);ok('fontes_ausentes_pendentes',length(t)==0&&any(data.table::fread(file.path(r,'decisoes_metodologicas.csv'))$codigo=='fontes_ausentes'))
# Complete segment, not just initial point, must pass forest/UA distance.
line<-st_sf(geometry=st_sfc(st_linestring(matrix(c(197990,8432100,199210,8432100),ncol=2,byrow=TRUE)),crs=31983))
cc$usar_estradas_pavimentadas<-FALSE;cc$usar_estradas_terra<-FALSE;cc$usar_trilhas_preexistentes<-FALSE;cc$usar_formacao_florestal<-FALSE;cc$direcoes_campo<-'N';cc$transecto_m<-50
layers<-list(list(papel='transectos',x=line,nome='transectos',fonte='sintetica'))
t<-mq_road_sources(layers,cc,r);z<-mq_road_screen(g,t,st_buffer(st_union(g),1000),cc,r)
ok('afastamento_UA_linha_completa',all(!z$mq_viavel_vias))
# Legacy calls preserve exactly their protocol behaviour.
l<-mq_legacy_defaults();l$perfil<-'ilha';l$parametros_protocolo<-list(transecto_m=31,grade_m=c(34,35))
a<-mq_prepare_config(l);b<-mq_protocol(l);ok('legado_sem_migracao_silenciosa',identical(a,b))
cat('TOTAL',n,'PASS\n')
# Exceções devem ser seletivas e não apagar evidências da solicitação.
cc<-mq_prepare_config(MQ_CONFIG);cc$confirmar_desvios<-TRUE;cc$politica_insuficiencia<-'bloquear'
cc$prioritarios<-list(n=3,percentual=NULL);cc$alternativos<-list(n=6,percentual=NULL)
h<-g;h$categoria<-'grade';h$mq_apto<-TRUE;h$mq_formacao<-c('campestre',rep('savanica',11));h$mq_viavel_vias<-TRUE
z<-mq_select_reviewed(h,cc,r,design=TRUE)
ok('cotas_inviaveis_oferecem_adaptacao',sum(z$categoria=='prioritario')==3&&sum(z$categoria=='alternativo')==6)
h$mq_formacao<-'campestre';cc$politica_insuficiencia<-'usar_disponiveis';cc$cotas_formacao<-data.frame(classe=c('campestre','savanica'),percentual=c(0,100))
z<-mq_select_reviewed(h,cc,r,design=TRUE);ok('zero_explicito_exige_decisao',sum(z$categoria!='grade')==9&&any(data.table::fread(file.path(r,'decisoes_metodologicas.csv'))$codigo=='cotas_excluem_todos'))
cc$confirmar_desvios<-FALSE;ok('zero_explicito_recusa_preserva',fails(mq_select_reviewed(h,cc,r,design=TRUE))&&all(h$categoria=='grade'))
cc<-mq_prepare_config(MQ_CONFIG);cc$aplicar_afastamentos<-FALSE;cc$confirmar_desvios<-TRUE
h$categoria[1]<-'prioritario';h$mq_apto[1]<-FALSE;cc$estratificar_vegetacao<-FALSE
z<-mq_select_reviewed(h,cc,r);ok('historico_excecao_auditavel',z$categoria[1]=='prioritario'&&file.exists(file.path(r,'historico_excecoes.csv')))
# API inválida não deve ser silenciosamente interpretada como autorização.
bad<-MQ_CONFIG;bad$grade_m<-c(0,10);ok('grade_zero_erro_tecnico',fails(mq_validate_config(mq_prepare_config(bad))))
bad<-MQ_CONFIG;bad$perfil<-'personalizado';bad$usar_estradas_terra<-NA;ok('booleano_NA_rejeitado',fails(mq_prepare_config(bad)))
bad<-MQ_CONFIG;bad$perfil<-'monitoramento_ilha';bad$parametros_protocolo<-list(grade_m=c(10,10));ok('duas_fontes_parametros_rejeitadas',fails(mq_prepare_config(bad)))
cat('TOTAL_PERFIS_FINAL',n,'PASS\n')
cc<-mq_prepare_config(MQ_CONFIG);cc$confirmar_desvios<-TRUE;cc$prioritarios<-list(n=3,percentual=NULL);cc$alternativos<-list(n=6,percentual=NULL)
h$mq_apto<-TRUE;h$mq_formacao<-'campestre';old<-h;old$mq_estrato<-NULL
z<-mq_select_reviewed(h,cc,r,old,TRUE)
ok('historico_sem_estratos_pede_decisao',z$categoria[1]=='prioritario'&&file.exists(file.path(r,'reclassificacao_historico.csv')))
cc$confirmar_desvios<-FALSE;ok('historico_sem_estratos_recusa',fails(mq_select_reviewed(h,cc,r,old,TRUE)))
cat('TOTAL_PERFIS_FINAL',n,'PASS\n')
