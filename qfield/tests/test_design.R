options(monitora.qfield.somente_funcoes=TRUE)
source('qfield/monitora_criar_qfield.R',encoding='UTF-8')
suppressPackageStartupMessages(library(sf))
checks<-character();ok<-function(n,v){if(!isTRUE(v))stop(n);checks<<-c(checks,n);cat('PASS ',n,'\n',sep='')};fails<-function(x)inherits(try(force(x),silent=TRUE),'try-error')
report<-tempfile();dir.create(report)
c<-MQ_CONFIG;c$politica_insuficiencia<-'bloquear';c$prioritarios<-list(n=20,percentual=NULL);c$mapbiomas<-FALSE;c$estratificar_por_atributos<-TRUE
c$cotas_atributos<-list(fogo=data.frame(classe=c('0','1'),percentual=c(70,30)),setor=data.frame(classe=c('N','S'),percentual=c(40,60)))
d<-expand.grid(mq_formacao=c('campestre','savanica'),atr_fogo=c('0','1'),atr_setor=c('N','S'),rep=1:16,stringsAsFactors=FALSE)
g<-st_as_sf(transform(d,x=seq_len(nrow(d))*100,y=8000000),coords=c('x','y'),crs=31983);g$id_grade<-seq_len(nrow(g));g$PA<-paste0('PA',g$id_grade);g$chave_grade<-g$PA;g$categoria<-'grade';g$mq_apto<-TRUE
s<-mq_design_select(g,c,report);p<-s[s$categoria=='prioritario',];a<-s[s$categoria=='alternativo',]
ok('margens_cumulativas',nrow(p)==20&&sum(p$mq_formacao=='campestre')==10&&sum(p$atr_fogo=='1')==6&&sum(p$atr_setor=='N')==8)
ok('alternativos_dobro_combinacao',all(table(factor(a$mq_estrato,levels=unique(s$mq_estrato)))==2*table(factor(p$mq_estrato,levels=unique(s$mq_estrato)))))
s2<-mq_design_select(g[rev(seq_len(nrow(g))),],c,report);ok('ordem_independente',identical(s$categoria,s2$categoria[match(s$PA,s2$PA)]))
s3<-mq_design_select(s,c,report,s);ok('expansao_idempotente',identical(s$categoria,s3$categoria))
changed<-s;changed$atr_fogo[which(changed$categoria=='prioritario')[1]]<-'X';ok('mudanca_estrato_bloqueada',fails(mq_design_select(changed,c,report,s)))
cp<-c;cp$alternativos<-list(n=10,percentual=NULL);sp<-mq_design_select(g,cp,report);ap<-sp[sp$categoria=='alternativo',];ok('alternativos_personalizados_margens',nrow(ap)==10&&sum(ap$atr_fogo=='1')==3&&sum(ap$atr_setor=='N')==4)
cn<-c;cn$cotas_atributos$fogo<-data.frame(classe=c('0','1'),n=c(14L,6L));sn<-mq_design_select(g,cn,report);ok('cotas_n',sum(sn$categoria=='prioritario'&sn$atr_fogo=='1')==6)
cc<-c;cc$cotas_atributos$fogo<-data.frame(classe=c('0','1'),percentual=c(40,40));ok('soma_incorreta',fails(mq_design_select(g,cc,report)))
cc<-c;cc$cotas_atributos$fogo$n<-c(14,6);ok('n_e_percentual',fails(mq_design_select(g,cc,report)))
cc<-c;cc$cotas_atributos$fogo<-data.frame(classe='0',percentual=100);ok('classe_nao_contemplada',fails(mq_design_select(g,cc,report)))
# Correlação perfeita: margens incompatíveis mesmo com capacidade global abundante.
x<-g[g$atr_fogo==ifelse(g$mq_formacao=='campestre','0','1'),];ok('inviabilidade_cruzada',fails(mq_design_select(x,c,report)))
status<-jsonlite::read_json(file.path(report,'solucao_cotas.json'));ok('inviabilidade_identificada',status$status_solver==2)
# Empate de arredondamento com rótulos em ordem oposta: somente arredondamento conjunto resolve.
tie<-g[g$atr_fogo==ifelse(g$mq_formacao=='campestre','1','0'),];ct<-c;ct$prioritarios<-list(n=1,percentual=NULL);ct$cotas_atributos<-list(fogo=data.frame(classe=c('0','1'),percentual=c(50,50)))
st<-mq_design_select(tie,ct,report);ok('arredondamento_conjunto',sum(st$categoria=='prioritario')==1&&sum(st$categoria=='alternativo')==2)
cs<-c;cs$cotas_atributos<-list();cs$estratificar_por_atributos<-FALSE
single<-g[g$mq_formacao=='campestre',];ss<-mq_design_select(single,cs,report);ok('formacao_unica',all(ss$mq_formacao[ss$categoria=='prioritario']=='campestre'))
forest<-g;forest$mq_formacao[forest$mq_formacao=='savanica']<-'florestal';cf<-cs;cf$incluir_formacao_florestal<-TRUE;cf$perfil<-'ilha';ok('floresta_cotas_automaticas',sum(mq_design_select(forest,cf,report)$categoria=='prioritario')==20);cf$cotas_formacao<-data.frame(classe=c('campestre','florestal'),percentual=c(50,50));fs<-mq_design_select(forest,cf,report);ok('floresta_com_cota',sum(fs$categoria=='prioritario'&fs$mq_formacao=='florestal')==10)
cz<-c;cz$prioritarios<-list(n=0,percentual=NULL);zs<-mq_design_select(g,cz,report);ok('zero_prioritarios',all(zs$categoria=='grade'))
# Classificação cartográfica: afloramento não é automaticamente campo rupestre.
m<-g[1:4,];m$mq_formacao<-NULL;m$mq_apto<-NULL;m$mb_codigo<-c(12,4,3,15);cm<-MQ_CONFIG;cl<-mq_design_classify(m,list(),cm,report);ok('classes_principais',identical(cl$mq_formacao,c('campestre','savanica','florestal','pastagem'))&&identical(cl$mq_apto,c(TRUE,TRUE,FALSE,TRUE)))
m$mb_codigo[1]<-29;ok('afloramento_exige_validacao',!mq_design_classify(m,list(),cm,report)$mq_apto[1])
# Atributos em polígonos: conflito real versus sobreposição concordante.
rect<-function(x1,y1,x2,y2)st_polygon(list(matrix(c(x1,y1,x2,y1,x2,y2,x1,y2,x1,y1),ncol=2,byrow=TRUE)))
v<-st_sf(formacao=c('campestre','campestre'),fogo=c('1','1'),geometry=st_sfc(rect(0,7999000,500,8001000),rect(0,7999000,500,8001000),crs=31983));ls<-list(list(x=v,papel='areas_elegiveis',fonte='teste | AE'))
cv<-cs;cv$formacao_campo<-'formacao';vg0<-g[1:2,];vg0$mq_formacao<-NULL;vg0$mq_apto<-NULL;vg<-mq_design_classify(vg0,ls,cv,report);ok('sobreposicao_concordante',nrow(vg)==2&&all(vg$mq_formacao=='campestre'))
ls[[1]]$x$formacao[2]<-'savanica';ok('sobreposicao_conflitante',fails(mq_design_classify(g[1:2,],ls,cv,report)))
ls[[1]]$x$formacao<-NA_character_;ok('atributo_na',fails(mq_design_classify(g[1:2,],ls,cv,report)))
cv$formacao_campo<-'ausente';ok('campo_inexistente',fails(mq_design_classify(g[1:2,],ls,cv,report)))
# População percentual permanece toda a grade AE mesmo com formações excluídas.
extra<-g;extra$mq_apto[1:16]<-FALSE;cg<-c;cg$prioritarios<-list(n=NULL,percentual=10);sg<-mq_design_select(extra,cg,report);ok('denominador_grade_global',sum(sg$categoria=='prioritario')==ceiling(.1*nrow(extra)))
# Oráculo exaustivo em tabelas 2x2 pequenas; existência não depende da solução escolhida.
caps<-expand.grid(a=0:2,b=0:2,c=0:2,d=0:2);tested<-0
for(ii in seq(1,nrow(caps),by=4)) {
 cap<-as.integer(caps[ii,]);if(sum(cap)<2)next
 base<-expand.grid(mq_formacao=c('campestre','savanica'),atr_fogo=c('0','1'),stringsAsFactors=FALSE);base<-base[rep(1:4,cap),]
 z<-g[seq_len(nrow(base)),];z$mq_formacao<-base$mq_formacao;z$atr_fogo<-base$atr_fogo
 co<-cs;co$prioritarios<-list(n=2,percentual=NULL);co$alternativos<-list(n=0,percentual=NULL);co$cotas_formacao<-data.frame(classe=c('campestre','savanica'),n=c(1,1));co$estratificar_por_atributos<-TRUE;co$cotas_atributos<-list(fogo=data.frame(classe=c('0','1'),n=c(1,1)))
 cand<-expand.grid(lapply(cap,function(n)0:n));feasible<-any(rowSums(cand)==2&cand[[1]]+cand[[3]]==1&cand[[1]]+cand[[2]]==1)
 got<-!fails(mq_design_select(z,co,report));if(got!=feasible)stop('oraculo_',ii);tested<-tested+1
}
ok('oraculo_exaustivo_pequeno',tested>=15)
cat('TOTAL_DESIGN ',length(checks),' PASS; ',tested,' capacidades contra oráculo\n',sep='')

historical<-g[1:2,];historical$mb_codigo<-c(4L,12L);ok('classificacao_historica_nao_sobrescrita',fails(mq_design_classify(historical,list(),MQ_CONFIG,report)))
ct1<-MQ_CONFIG;ct2<-ct1;ct2$mb_ano<-2024;ok('contrato_detecta_ano',mq_design_contract(ct1)$sha256!=mq_design_contract(ct2)$sha256)
ct2<-ct1;ct2$formacao_campo<-'vegetacao';ok('contrato_detecta_campo',mq_design_contract(ct1)$sha256!=mq_design_contract(ct2)$sha256)
cat('TOTAL_DESIGN_FINAL ',length(checks),' PASS\n',sep='')
# Fitofisionomias são estratos distintos; formação agrega seu esforço.
fito<-g;fito$mq_fitofisionomia<-ifelse(fito$mq_formacao=='savanica','savana',ifelse(fito$atr_fogo=='0','campo_limpo','campo_sujo'))
cfi<-cs;cfi$fitofisionomia_campo<-'fito';cfi$prioritarios<-list(n=18,percentual=NULL);sfito<-mq_design_select(fito,cfi,report)
ok('balanceamento_fitofisionomias',all(table(sfito$mq_fitofisionomia[sfito$categoria=='prioritario'])==6)&&sum(sfito$categoria=='prioritario'&sfito$mq_formacao=='campestre')==12)
zeroq<-c;zeroq$prioritarios<-list(n=16,percentual=NULL);zeroq$cotas_atributos$fogo<-data.frame(classe=c('0','1'),percentual=c(100,0));sz<-mq_design_select(g,zeroq,report);ok('cota_zero_explicita',!any(sz$categoria!='grade'&sz$atr_fogo=='1'))
cfld<-c;cfld$max_estratos<-1;ok('limite_combinacoes',fails(mq_design_select(g,cfld,report)))
fi<-tempfile();dir.create(fi);writeLines(c('atributo;classe;percentual','fogo;0;70','fogo;1;30'),file.path(fi,'cotas.csv'));cfile<-c;cfile$cotas_atributos<-list();cfile$cotas_atributos_arquivo<-'cotas.csv';loaded<-mq_design_load(cfile,fi,report);ok('CSV_longo_margens',identical(loaded$cotas_atributos$fogo$classe,c('0','1'))&&identical(loaded$cotas_atributos$fogo$percentual,c(70,30)))
# Formação florestal não é obstáculo para o alvo florestal, mas continua para o campestre.
pp<-g[1:2,];st_geometry(pp)<-st_sfc(st_point(c(198000,8432000)),st_point(c(198200,8432000)),crs=31983);pp$mq_formacao<-c('campestre','florestal');pp$categoria<-'prioritario'
fg<-st_sf(geometry=st_sfc(rect(197000,8431000,199000,8433000),crs=31983));fc<-MQ_CONFIG;fc$perfil<-'ilha';fc$incluir_formacao_florestal<-TRUE;mq_diagnostics(pp,list(list(x=fg,papel='formacao_florestal')),fg,fc,report);ok('ilha_sem_procedimentos_campestres',file.exists(file.path(report,'procedimento_campo_nao_aplicado.txt')))
cat('TOTAL_DESIGN_VALIDADO ',length(checks),' PASS\n',sep='')
ua<-vg0;ua$UA<-paste0('UA',seq_len(nrow(ua)));ua$PA<-NULL;cv$formacao_campo<-'formacao'
stopifnot(fails(mq_design_classify(ua,ls,cv,report)));conf<-data.table::fread(file.path(report,'conflitos_atributos.csv'))
ok('relatorio_UA_nao_rotulado_PA',identical(conf$campo_identificador,rep('UA',nrow(conf)))&&identical(conf$identificador,ua$UA)&&!'PA'%in%names(conf))
cat('TOTAL_DESIGN_ENTREGA ',length(checks),' PASS\n',sep='')
