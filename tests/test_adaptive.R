options(monitora.qfield.somente_funcoes=TRUE);source('monitora_planejamento_amostral.R',encoding='UTF-8');suppressPackageStartupMessages(library(sf))
r<-tempfile();dir.create(r);tests<-0L;ok<-function(n,v){if(!isTRUE(v))stop(n);tests<<-tests+1L;cat('PASS',n,'\n')};fails<-function(x)inherits(try(force(x),silent=TRUE),'try-error')
grid<-function(codes){n<-length(codes);st_as_sf(data.frame(id_grade=seq_len(n),chave_grade=paste0('g',seq_len(n)),categoria='grade',mb_codigo=codes,x=seq_len(n)*100,y=8000000),coords=c('x','y'),crs=31983)}
run<-function(codes,c=MQ_CONFIG){g<-mq_design_classify(grid(codes),list(),c,r);mq_design_select(g,c,r)}
c<-MQ_CONFIG
ok('padrao_adaptavel',c$politica_insuficiencia=='usar_disponiveis')
z<-run(12);ok('um_candidato_sem_reserva_impossivel',identical(z$categoria,'prioritario'))
z<-run(c(12,12));ok('dois_candidatos',sum(z$categoria=='prioritario')==1&&sum(z$categoria=='alternativo')==1)
cc<-c;cc$prioritarios<-list(n=9,percentual=NULL);z<-run(c(12,15),cc);ok('pedido_maior_que_area',sum(z$categoria=='prioritario')==2&&sum(z$categoria=='alternativo')==0)
q<-data.table::fread(file.path(r,'selecao_quantidades.csv'));ok('deficits_auditados',identical(q$deficit,c(7L,18L))&&all(q$denominador_grade_AE==2))
ok('campestre_bloqueia_so_floresta_agua',fails(run(c(3,33,3))))
ci<-c;ci$perfil<-'ilha';z<-run(rep(3,20),ci);ok('ilha_100_florestal',sum(z$categoria=='prioritario')==4&&sum(z$categoria=='alternativo')==8&&all(z$mq_apto))
z<-run(rep(c(3,4,12),20),ci);ok('ilha_mista',length(unique(z$mq_formacao[z$categoria=='prioritario']))==3)
z<-run(rep(15,10));ok('pastagem_identificada_sem_falsa_formacao',all(z$mq_formacao=='pastagem')&&sum(z$categoria!='grade')==6);notes<-mq_selection_occurrences(z,c,r);ok('pastagem_nas_ocorrencias',length(notes)>0&&nrow(data.table::fread(file.path(r,'ocorrencias_vegetacao.csv')))==10)
z<-run(c(rep(12,16),rep(15,14),3));ok('complemento_pastagem',sum(z$categoria!='grade')==21&&sum(z$categoria!='grade'&z$mq_formacao=='pastagem')==5&&all(z$categoria[z$mq_formacao=='florestal']=='grade'))
z<-run(c(rep(12,40),rep(15,10)));ok('preferir_nativas_quando_suficientes',all(z$categoria[z$mq_formacao=='pastagem']=='grade'))
z<-run(c(12,29,NA,25,33));ok('pendentes_nao_bloqueiam_area_parcial',sum(z$categoria!='grade')==1&&all(z$categoria[2:5]=='grade')&&z$mq_formacao[2]=='nao_resolvida')
ok('sem_classificacao_suficiente_bloqueia',fails(run(c(29,NA))))
cc<-c;cc$estratificar_vegetacao<-FALSE;z<-run(c(12,3,33),cc);ok('desabilitar_cotas_nao_desabilita_elegibilidade_perfil',all(z$categoria[2:3]=='grade'))
cc<-c;cc$incluir_antropizadas<-FALSE;ok('antropizadas_opcionais',fails(run(rep(15,4),cc)))
# Metas cumulativas impossíveis: reportar desvios, manter zero explícito e não inventar candidatos.
g<-mq_design_classify(grid(rep(c(12,4),10)),list(),c,r);g$atr_setor<-ifelse(g$mq_formacao=='campestre','A','B');cc<-c;cc$prioritarios<-list(n=6,percentual=NULL);cc$alternativos<-list(n=6,percentual=NULL);cc$estratificar_por_atributos<-TRUE;cc$cotas_atributos<-list(setor=data.frame(classe=c('A','B'),percentual=c(100,0)))
z<-mq_design_select(g,cc,r);ok('zero_explicito_e_deficit_conjunto',all(z$categoria[z$atr_setor=='B']=='grade')&&sum(z$categoria=='prioritario')==6&&sum(z$categoria=='alternativo')==4)
q<-data.table::fread(file.path(r,'cotas_realizadas.csv'));ok('margens_nao_cumpridas_explicitas',any(q$fora_margem)&&any(q$deficit_solicitado>0))
cc$politica_insuficiencia<-'bloquear';ok('estrito_disponivel',fails(mq_design_select(g,cc,r)))
cc$politica_insuficiencia<-'usar_disponiveis';z<-mq_design_select(g[nrow(g):1,],cc,r);zz<-mq_design_select(g,cc,r);ok('determinismo_ordem',identical(z$categoria[nrow(g):1],zz$categoria))
h<-g;h$categoria[1:2]<-'alternativo';cc$cotas_atributos<-list(setor=data.frame(classe=c('A','B'),percentual=c(50,50)));zz<-mq_design_select(h,cc,r);ok('historicos_preservados',all(zz$categoria[1:2]=='alternativo'))
cc$cotas_atributos$setor$percentual<-c(100,0);ok('zero_conflitante_historico_bloqueia',fails(mq_design_select(h,cc,r)))
# Degradação só declarada, não inferida de área não vegetada.
g<-grid(25);g$mb_codigo<-NULL;ae<-st_sf(tipo='degradada',geometry=st_buffer(st_geometry(g),100));cc<-c;cc$formacao_campo<-'tipo';zz<-mq_design_classify(g,list(list(x=ae,papel='areas_elegiveis',fonte='teste')),cc,r);ok('degradada_declarada',zz$mq_apto&&zz$mq_formacao=='degradada')
# Contexto UC não interfere nas regras de seleção; malhas pequenas/grandes e recortadas.
rect<-function(x,y,w)st_polygon(list(rbind(c(x,y),c(x+w,y),c(x+w,y+w),c(x,y+w),c(x,y))))
for(uc in c(FALSE,TRUE))for(w in c(150,1000)){
 ae<-st_sf(geometry=st_sfc(rect(500000,8000000,w),crs=31983));u<-if(uc)st_sf(cnuc='TESTE',geometry=st_sfc(rect(499900,7999900,2000),crs=31983))else mq_empty(31983);cc<-c;cc$grade_m<-c(100,100);ctx<-mq_contexts(ae,u,st_crs(ae),cc$grade_m);gg<-mq_grid(ae,ctx,st_crs(ae),cc);gg$mb_codigo<-12;zz<-mq_design_select(mq_design_classify(gg,list(),cc,r),cc,r);ok(paste('UC_area',uc,w),sum(zz$categoria!='grade')>0&&all(lengths(st_intersects(zz,ae))>0))
}
cc<-c;cc$prioritarios<-list(n=3,percentual=NULL);cc$alternativos<-list(n=6,percentual=NULL);z<-run(c(rep(12,3),rep(15,9)),cc);ok('nativas_primeiro_nos_prioritarios',all(z$mq_formacao[z$categoria=='prioritario']=='campestre'))
cc<-c;cc$estratificar_vegetacao<-FALSE;cc$politica_insuficiencia<-'bloquear';cc$alternativos<-list(n=0,percentual=NULL);z<-run(c(12,3),cc);ok('estrito_sem_cotas_mantem_filtro',z$categoria[1]=='prioritario'&&z$categoria[2]=='grade')
cat('TOTAL_ADAPTATIVO',tests,'PASS\n')
