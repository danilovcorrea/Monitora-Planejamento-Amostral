# Diagnósticos sem modificar configuração nem desenho.
mq_grade_variable <- function(c) {
  if(c$perfil=='campestre_savanico')return('grade_m')
  if(c$perfil=='personalizado'||!is.null(c$parametros_protocolo$grade_m))return('parametros_protocolo$grade_m')
  'padrao_ilha$grade_m'
}
mq_config_summary <- function(original,c,report,mode=c$modo) {
  val<-function(x)if(is.null(x))'NULL'else if(is.list(x))as.character(jsonlite::toJSON(x,auto_unbox=TRUE,null='null'))else paste(as.character(unlist(x)),collapse=', ')
  rows<-list()
  add<-function(variable,requested,effective,action)rows[[length(rows)+1L]]<<-data.frame(variavel=variable,informado=val(requested),efetivo=val(effective),orientacao=action)
  add('perfil',original$perfil,c$perfil,'Define regras metodológicas; não alterar somente para contornar exclusões.')
  add('modo',original$modo,mode,'auto escolhe montar com pontos/UAs fornecidos; planejar cria a grade.')
  add('grade_m',original$grade_m,c$grade_m,paste('Espaçamento efetivo controlado por',mq_grade_variable(c),'; alterar manualmente para um novo desenho.'))
  add('transecto_m',original$transecto_m,c$transecto_m,'Campestre usa transecto_m; demais perfis usam os parâmetros do protocolo.')
  add('estratificar_vegetacao',original$estratificar_vegetacao,c$estratificar_vegetacao,paste('Filtro de elegibilidade vegetacional:',if(mq_vegetation_active(c))'ATIVO, mesmo sem balanceamento de cotas.'else 'DESATIVADO; MapBiomas é informativo.'))
  add('estratificar_por_atributos',original$estratificar_por_atributos,c$estratificar_por_atributos,'Habilite e configure cotas_atributos para margens cumulativas por atributo.')
  for(n in c('prioritarios','alternativos'))add(n,original[[n]],c[[n]],paste('Use',paste0(n,' = list(n=..., percentual=NULL)'),'OU percentual; o resultado depende dos candidatos disponíveis.'))
  add('politica_insuficiencia',original$politica_insuficiencia,c$politica_insuficiencia,'usar_disponiveis registra déficits; bloquear exige metas. Não cria candidatos ausentes.')
  add('distancia_min_m',original$distancia_min_m,c$distancia_min_m,'Não aplicada automaticamente nos perfis Ilha/personalizado; viabilidade real exige conferência em campo.')
  for(n in c('usar_estradas_pavimentadas','usar_estradas_terra','usar_trilhas_preexistentes'))add(n,original[[n]],c[[n]],'NULL detecta camada; TRUE exige camada; FALSE somente exibe. Distâncias efetivas em restricoes_viarias.json.')
  for(n in c('incluir_formacao_florestal','incluir_antropizadas','formacao_campo','cotas_formacao','cotas_atributos'))
    add(n,original[[n]],c[[n]],'Interpretar junto ao perfil e habilitação das cotas; resultados em ocorrencias_vegetacao.csv e cotas_realizadas.csv.')
  add('centros_detalhe',original$centros_detalhe,c$centros_detalhe,'auto usa UAs quando presentes; senão PAs. Centros efetivos em centros_recorte.gpkg e plano_download.json.')
  add('raio_detalhe_m',original$raio_detalhe_m,c$raio_detalhe_m,'Raio das imagens locais; não limita o contexto Sentinel.')
  add('baixar_imagem_detalhe',original$baixar_imagem_detalhe,c$baixar_imagem_detalhe,'TRUE permite planejar complemento; cache/local têm prioridade. confirmar_download controla aquisição.')
  add('qgis_python',original$qgis_python,c$qgis_python,'NULL testa instalações; versão escolhida em qgis_capacidades.json e qgis_instalacoes.json.')
  add('renovar_imagens',original$renovar_imagens,c$renovar_imagens,'Mantenha FALSE para reaproveitar. TRUE solicita novo acervo e pode exigir downloads.')
  add('cache_dir',original$cache_dir,c$cache_dir,'Cache persistente; preserve esta pasta. caches_adicionais aponta caches/projetos anteriores.')
  tab<-do.call(rbind,rows);mq_csv(tab,file.path(report,'configuracao_efetiva.csv'))
  message('CONFIGURAÇÃO: perfil=',c$perfil,'; modo efetivo=',mode,'; grade=',paste(c$grade_m,collapse=' x '),' m (alterar ',mq_grade_variable(c),'); transecto=',c$transecto_m,' m.')
  message('Vegetação: cotas=',c$estratificar_vegetacao,'; filtro=',mq_vegetation_active(c),'; atributos=',c$estratificar_por_atributos,'. Detalhes e variáveis a revisar: configuracao_efetiva.csv.')
  invisible(tab)
}
mq_grid_diagnostic <- function(ae,contexts,cr,c,N,report=NULL) {
  a<-sf::st_transform(ae,cr);b<-sf::st_bbox(a)
  msg<-sprintf('Área elegível: %.4f ha; envelope: %.2f x %.2f m; grade utilizada: %.2f x %.2f m; vértices nas AEs: %d. ',
               sum(as.numeric(sf::st_area(a)))/10000,b[3]-b[1],b[4]-b[2],c$grade_m[1],c$grade_m[2],N)
  action<-paste0('Para avaliar uma grade mais densa, reduza os valores de ',mq_grade_variable(c),
    ' no bloco inicial, conforme o desenho pretendido. Alterar prioritarios/alternativos não cria vértices. Espaçamento e origem não foram alterados automaticamente.',
    if(c$modo=='expandir')' Em expandir, preserve a malha anterior; espaçamento diferente exige novo planejamento separado.'else '')
  if(!is.null(report))mq_json(list(area_ha=sum(as.numeric(sf::st_area(a)))/10000,envelope_m=as.numeric(c(b[3]-b[1],b[4]-b[2])),grade_m=c$grade_m,pontos=N,variavel=mq_grade_variable(c),origens=lapply(contexts,function(x)list(chave=x$chave,origem=x$origem)),orientacao=action),file.path(report,'diagnostico_grade.json'))
  if(N==0)mq_stop('Nenhum vértice da grade nas Áreas Elegíveis. ',msg,action)
  message(msg)
}
mq_selection_summary <- function(report) {
  f<-file.path(report,'selecao_quantidades.csv')
  if(!file.exists(f))return(invisible(NULL))
  q<-data.table::fread(f)
  for(i in seq_len(nrow(q)))message('SELEÇÃO: ',q$categoria[i],': solicitado=',q$solicitado[i],'; realizado=',q$realizado[i],'; déficit=',q$deficit[i],'.')
  if(any(q$deficit>0))message('Para rever metas, altere prioritarios/alternativos (n OU percentual); para criar mais candidatos, revise a variável de espaçamento indicada em diagnostico_grade.json, os polígonos e as exclusões registradas. Cotas de classes: cotas_formacao/cotas_atributos. Nenhuma alteração automática da malha.')
}
