# Resolução de perfis: padrões primeiro, valores explícitos por último.
mq_profile_defaults <- function(profile) {
  choices<-c('treinamento_navegacao','treinamento_campestre','treinamento_ilha','monitoramento_campestre','monitoramento_ilha','personalizado')
  if(length(profile)!=1||is.na(profile)||!profile%in%choices)mq_stop('perfil: escolha ',paste(choices,collapse=', '))
  camp<-grepl('campestre$',profile);island<-grepl('ilha$',profile);protocol<-camp||island
  list(perfil=if(camp)'campestre_savanico'else if(island)'ilha'else 'personalizado',
       transecto_m=if(island)25 else 50,grade_m=rep(if(camp)156.25 else if(island)30 else 50,2),
       distancia_min_m=if(camp)100 else if(island)30 else 100,deslocamento_max_m=if(camp)10 else 0,
       direcoes_campo=c('N','L','S','O'),filtrar_vegetacao=protocol,aplicar_cotas=protocol,
       incluir_formacao_florestal=island,incluir_antropizadas=protocol,estratificar_por_atributos=FALSE,
       aplicar_afastamentos=protocol,usar_estradas_pavimentadas=camp,usar_estradas_terra=camp,
       usar_trilhas_preexistentes=camp,usar_formacao_florestal=camp,usar_uas_existentes=protocol,
       distancias_viarias_m=c(estradas_pavimentadas=100,estradas_terra=50,trilhas_preexistentes=5),
       distancia_floresta_m=100,mapbiomas=protocol)
}
mq_prepare_config <- function(config) {
  modern<-identical(as.integer(config$configuracao_versao),2L)||(!is.null(config$perfil)&&config$perfil%in%c('treinamento_navegacao','treinamento_campestre','treinamento_ilha','monitoramento_campestre','monitoramento_ilha'))
  if(!modern)return(mq_protocol(utils::modifyList(mq_legacy_defaults(),config,keep.null=TRUE)))
  if(!is.null(config$parametros_protocolo)||!is.null(config$padrao_ilha))mq_stop('Configuração 2: use grade_m/transecto_m diretamente; remova parametros_protocolo e padrao_ilha para evitar duas fontes de valores.')
  p<-if(is.null(config$perfil))'monitoramento_campestre'else config$perfil;defaults<-mq_profile_defaults(p)
  c<-mq_legacy_defaults();origins<-list()
  for(n in names(config))c[n]<-config[n]
  for(n in names(defaults))if(n!='perfil'){
    if(is.null(config[[n]])){c[[n]]<-defaults[[n]];origins[[n]]<-'perfil'}else origins[[n]]<-'usuario'
  }
  c$.config_v2<-TRUE;c$perfil_escolhido<-p;c$perfil<-defaults$perfil;c$origem_valores<-origins
  c$operacao<-if(is.null(config$operacao))'planejar'else config$operacao
  if(length(c$operacao)!=1||!c$operacao%in%c('planejar','montar','incrementar','expandir','auto'))mq_stop('operacao inválida: planejar, montar, incrementar ou expandir.')
  if(!is.null(config$modo)&&!identical(config$modo,'auto')&&!identical(config$modo,c$operacao))mq_stop('Use somente operacao na configuração 2; modo legado conflita com operacao.')
  c$modo<-if(c$operacao=='incrementar')'expandir'else c$operacao
  c$quantidade_incremento<-if(is.null(config$quantidade_incremento))'novos'else config$quantidade_incremento
  if(!c$quantidade_incremento%in%c('novos','total'))mq_stop('quantidade_incremento: novos ou total.')
  for(n in c('usar_estradas_pavimentadas','usar_estradas_terra','usar_trilhas_preexistentes','usar_formacao_florestal','usar_uas_existentes')) {
    if(is.null(config[[n]])&&!is.null(config$aplicar_afastamentos)) {c[[n]]<-config$aplicar_afastamentos;c$origem_valores[[n]]<-'aplicar_afastamentos'}
  }
  c$estratificar_vegetacao<-c$aplicar_cotas
  if(!is.null(config$estratificar_vegetacao))mq_stop('Configuração 2: use aplicar_cotas; filtrar_vegetacao controla elegibilidade separadamente.')
  bools<-c('filtrar_vegetacao','aplicar_cotas','aplicar_afastamentos','usar_estradas_pavimentadas','usar_estradas_terra','usar_trilhas_preexistentes','usar_formacao_florestal','usar_uas_existentes','mapbiomas')
  for(n in bools)if(length(c[[n]])!=1||!is.logical(c[[n]])||is.na(c[[n]]))mq_stop(n,': NULL herda, TRUE ativa ou FALSE desativa.')
  if(!is.null(c$confirmar_desvios)&&(length(c$confirmar_desvios)!=1||!is.logical(c$confirmar_desvios)||is.na(c$confirmar_desvios)))mq_stop('confirmar_desvios deve ser NULL, TRUE ou FALSE.')
  for(n in c('distancia_min_m','distancia_floresta_m','deslocamento_max_m'))if(length(c[[n]])!=1||!is.numeric(c[[n]])||!is.finite(c[[n]])||c[[n]]<0)mq_stop('Valor métrico inválido: ',n)
  d<-c$distancias_viarias_m;roles<-c('estradas_pavimentadas','estradas_terra','trilhas_preexistentes')
  if(!is.numeric(d)||!setequal(names(d),roles)||anyDuplicated(names(d))||any(!is.finite(d)|d<0))mq_stop('distancias_viarias_m exige valores não negativos para estradas_pavimentadas, estradas_terra e trilhas_preexistentes.')
  if(!length(c$direcoes_campo)||anyNA(c$direcoes_campo)||any(!c$direcoes_campo%in%c('N','L','S','O'))||anyDuplicated(c$direcoes_campo))mq_stop('direcoes_campo: uma ou mais direções únicas entre N, L, S, O.')
  if(c$perfil=='ilha')c$referencia_ilha<-list(transecto_m=c$transecto_m,grade_m=c$grade_m,distancia_referencia_m=c$distancia_min_m,direcoes_referencia=c$direcoes_campo,fonte='Pré-projeto piloto Noronha 2026',natureza='referências experimentais; não certificam conformidade do protocolo em construção')
  c
}
mq_decide <- function(c,report,code,text) {
  message('ATENÇÃO [',code,']: ',text)
  response<-c$confirmar_desvios;source<-'configuracao'
  if(is.null(response)) {
    if(!interactive()) {response<-FALSE;source<-'sem_console_interativo'}else {
      source<-'console';a<-toupper(trimws(readline('Continuar com a alteração descrita? [S] sim / [N] cancelar: ')))
      while(!a%in%c('S','N'))a<-toupper(trimws(readline('Responda S ou N: ')))
      response<-a=='S'
    }
  }
  if(!is.null(report)) {
    path<-file.path(report,'decisoes_metodologicas.csv')
    row<-data.frame(data=format(Sys.time(),tz='UTC',usetz=TRUE),codigo=code,descricao=text,aceito=isTRUE(response),origem=source)
    if(file.exists(path))row<-rbind(as.data.frame(data.table::fread(path)),row)
    mq_csv(row,path)
  }
  if(!isTRUE(response))mq_stop('Execução cancelada: ',code,'. Revise as variáveis indicadas; para execução sem console, configurar confirmar_desvios=TRUE aceita os avisos descritos. Cache preservado.')
  invisible(TRUE)
}
mq_profile_review <- function(c,report) {
  if(!isTRUE(c$.config_v2))return(invisible(NULL))
  d<-mq_profile_defaults(c$perfil_escolhido);warnings<-character()
  if(c$perfil!='personalizado') {
    fields<-c('transecto_m','distancia_min_m','deslocamento_max_m','direcoes_campo','filtrar_vegetacao','incluir_formacao_florestal','distancia_floresta_m','distancias_viarias_m','usar_estradas_pavimentadas','usar_estradas_terra','usar_trilhas_preexistentes','usar_formacao_florestal','usar_uas_existentes')
    for(n in fields)if(!isTRUE(all.equal(c[[n]],d[[n]])))warnings<-c(warnings,paste0(n,' = ',paste(c[[n]],collapse=', '),' (referência: ',paste(d[[n]],collapse=', '),')'))
    if(min(c$grade_m)<c$distancia_min_m+c$transecto_m)warnings<-c(warnings,paste0('grade_m = ',paste(c$grade_m,collapse=' x '),' m: candidatos próximos podem não admitir UAs simultâneas. distancia_min_m refere-se às linhas completas; validação final em campo.'))
  }
  if(length(warnings))mq_decide(c,report,'ajustes_perfil',paste(warnings,collapse='; '))
  invisible(NULL)
}
mq_selection_review <- function(c,report) {
  if(!isTRUE(c$.config_v2))return(invisible(NULL))
  warnings<-character();f<-file.path(report,'selecao_quantidades.csv')
  if(file.exists(f)){q<-data.table::fread(f);if(any(q$deficit>0))warnings<-c(warnings,paste0('prioritarios/alternativos: ',paste(paste0(q$categoria,' ',q$realizado,'/',q$solicitado),collapse='; '),'. Ajuste as quantidades, grade_m ou filtros para rever o desenho.'))}
  f<-file.path(report,'cotas_realizadas.csv')
  if(file.exists(f)){q<-data.table::fread(f);if('fora_margem'%in%names(q)&&any(q$fora_margem))warnings<-c(warnings,paste(sum(q$fora_margem),'metas de cotas_formacao/cotas_atributos não atendidas; veja cotas_realizadas.csv.'))}
  if(length(warnings))mq_decide(c,report,'metas_adaptadas',paste(warnings,collapse=' '))
}
mq_decision_notes <- function(report) {
  f<-file.path(report,'decisoes_metodologicas.csv');if(!file.exists(f))return(character())
  q<-data.table::fread(f);paste0('Decisão ',q$codigo,': ',q$descricao,' Aceita: ',q$aceito,'.')
}
mq_increment_config <- function(g,c,report) {
  if(!isTRUE(c$.config_v2)||c$operacao!='incrementar'||c$quantidade_incremento!='novos')return(c)
  np<-mq_quantity(c$prioritarios,nrow(g),ceiling(.2*nrow(g)));na<-mq_quantity(c$alternativos,nrow(g),2*np)
  old<-c(sum(g$categoria=='prioritario'),sum(g$categoria=='alternativo'))
  mq_csv(data.frame(categoria=c('prioritarios','alternativos'),preservados=old,novos_solicitados=c(np,na),total_solicitado=old+c(np,na),denominador_grade_AE=nrow(g)),file.path(report,'incremento_quantidades.csv'))
  c$prioritarios<-list(n=old[1]+np,percentual=NULL);c$alternativos<-list(n=old[2]+na,percentual=NULL)
  if(c$estratificar_vegetacao||c$estratificar_por_atributos)message('INCREMENTO: cotas incidem no total final (histórico + novos). Tabelas de n devem somar esse total; percentuais usam esse total.')
  c
}
mq_select_reviewed <- function(g,c,report,old=NULL,design=FALSE) {
  fun<-function(g,c,old){
    result<-if(design)mq_design_select(g,c,report,old)else mq_select(g,c,report)
    if(isTRUE(c$.config_v2))mq_json(list(perfil=c$perfil_escolhido,operacao=c$operacao,filtrar_vegetacao=c$filtrar_vegetacao,cotas_formacao=c$estratificar_vegetacao,cotas_atributos=c$estratificar_por_atributos,politica_insuficiencia=c$politica_insuficiencia,prioritarios=c$prioritarios,alternativos=c$alternativos,nota='Efeitos das decisões nesta seleção; configuração solicitada preservada em configuracao.json. Exceções de histórico em decisoes_metodologicas.csv.'),file.path(report,'configuracao_selecao.json'))
    result
  }
  if(!isTRUE(c$.config_v2))return(fun(g,c,old))
  viable<-mq_road_available(g);if('mq_apto'%in%names(g))viable<-viable&g$mq_apto
  if(!any(viable)&&nrow(g)>0) {
    mq_decide(c,report,'sem_candidatos','Nenhum vértice atende aos filtros. Prosseguir usará todos os vértices das AEs, sem filtros de vegetação, afastamentos ou cotas nesta seleção. Para manter critérios, cancele e revise filtrar_vegetacao, aplicar_cotas, controles de afastamento ou áreas de entrada.')
    g$mq_apto_original<-if('mq_apto'%in%names(g))g$mq_apto else TRUE;g$mq_viavel_vias_original<-mq_road_available(g)
    g$mq_apto<-TRUE;g$mq_viavel_vias<-TRUE;g$mq_excecao_selecao<-'filtros e cotas dispensados por decisão explícita'
    c$filtrar_vegetacao<-FALSE;c$estratificar_vegetacao<-FALSE;c$estratificar_por_atributos<-FALSE;design<-FALSE
  }
  if(design&&(c$estratificar_vegetacao||c$estratificar_por_atributos)) {
    criteria<-mq_design_criteria(g,c,mq_quantity(c$prioritarios,nrow(g),ceiling(.2*nrow(g))))
    effective<-mq_road_available(g)&g$mq_apto
    for(f in names(criteria)){q<-criteria[[f]];explicit<-if('explicita'%in%names(q))q$explicita else rep(TRUE,nrow(q));effective<-effective&!g[[f]]%in%q$classe[explicit&q$valor==0]}
    if(!any(effective)&&any(mq_road_available(g)&g$mq_apto)) {
      mq_decide(c,report,'cotas_excluem_todos','Cotas explicitamente zero excluem todos os candidatos disponíveis. Prosseguir selecionará sem cotas nesta rodada, mantendo os filtros de vegetação e afastamentos. Para manter as cotas, cancele e revise cotas_formacao/cotas_atributos.')
      c$estratificar_vegetacao<-FALSE;c$estratificar_por_atributos<-FALSE;design<-FALSE
    }
    if(any(g$categoria!='grade'&!effective)&&any(effective)){
      mq_decide(c,report,'historico_cotas','PAs históricos fora dos critérios/cotas atuais serão preservados como exceções; novos candidatos continuarão sujeitos aos critérios.');c$preservar_historico_excecao<-TRUE
    }
  }
  if(design&&!is.null(old)&&any(g$categoria!='grade')&&(c$estratificar_vegetacao||c$estratificar_por_atributos)) {
    fields<-names(mq_design_criteria(g,c,mq_quantity(c$prioritarios,nrow(g),ceiling(.2*nrow(g)))))
    tuples<-do.call(paste,c(lapply(sf::st_drop_geometry(g)[,fields,drop=FALSE],function(v){v<-as.character(v);paste0(nchar(v,type='bytes'),':',v)}),sep='|'))
    keys<-vapply(tuples,digest::digest,character(1),algo='sha256',serialize=FALSE)
    ix<-match(g$chave_grade,old$chave_grade);sel<-which(!is.na(ix)&g$categoria!='grade')
    previous<-if('mq_estrato'%in%names(old))old$mq_estrato[ix[sel]]else rep(NA_character_,length(sel))
    changed<-is.na(previous)|previous!=keys[sel]
    if(any(changed)){
      if(!isTRUE(c$revisao_historico))mq_decide(c,report,'estratos_historicos','A classificação dos PAs históricos mudou ou não estava disponível. Prosseguir registrará os estratos atuais na nova saída, preservando posições, IDs, categorias e o cadastro original.')
      mq_csv(data.frame(PA=if('PA'%in%names(g))g$PA[sel]else g$chave_grade[sel],estrato_anterior=previous,estrato_atual=keys[sel]),file.path(report,'reclassificacao_historico.csv'))
      c$revisao_historico<-TRUE
    }
    if(isTRUE(c$revisao_historico))old<-NULL
  }
  historical<-g$categoria!='grade'
  viable<-mq_road_available(g);if('mq_apto'%in%names(g))viable<-viable&g$mq_apto
  if(any(historical&!viable)) {
    mq_decide(c,report,'historico_fora_criterios','PAs históricos conflitam com os critérios atuais. Prosseguir preservará esses PAs como exceções; os filtros continuarão aplicados aos novos candidatos. Consulte historico_excecoes.csv.')
    mq_csv(g[historical&!viable,],file.path(report,'historico_excecoes.csv'))
    if('mq_apto'%in%names(g))g$mq_apto[historical]<-TRUE
    g$mq_viavel_vias[historical]<-TRUE;c$preservar_historico_excecao<-TRUE
  }
  wanted<-c(mq_quantity(c$prioritarios,nrow(g),ceiling(.2*nrow(g))))
  wanted<-c(wanted,mq_quantity(c$alternativos,nrow(g),2*wanted[1]));previous<-c(sum(g$categoria=='prioritario'),sum(g$categoria=='alternativo'))
  if(any(previous>wanted)) {
    mq_decide(c,report,'quantidade_inferior_historico',paste0('Quantidades solicitadas inferiores ao histórico: ',paste(wanted,collapse='/'),' versus ',paste(previous,collapse='/'),'. Prosseguir manterá ao menos todos os pontos anteriores; revise prioritarios/alternativos.'))
    wanted<-pmax(wanted,previous);c$prioritarios<-list(n=wanted[1],percentual=NULL);c$alternativos<-list(n=wanted[2],percentual=NULL)
  }
  tryCatch(fun(g,c,old),error=function(e){
    msg<-conditionMessage(e)
    # Só desvios metodológicos conhecidos podem ser aceitos; nunca solver, geometria ou corrupção.
    if(!grepl('Quantidade solicitada excede|cotas.*invi|Cotas.*invi|metas.*invi|Sem solução|inviável|Cotas cumulativas inviáveis',msg))stop(e)
    if(c$politica_insuficiencia!='bloquear')stop(e)
    mq_decide(c,report,'metas_estritas',paste(msg,'Prosseguir substituirá politica_insuficiencia por usar_disponiveis nesta seleção, preservando filtros e histórico.'))
    c$politica_insuficiencia<-'usar_disponiveis';fun(g,c,old)
  })
}
