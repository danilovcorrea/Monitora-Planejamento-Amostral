# Regras geométricas explícitas; nenhuma interpretação automática de imagem.
mq_protocol <- function(c) {
  if(!c$perfil%in%c('campestre_savanico','ilha','personalizado'))mq_stop('Perfil inválido.')
  if(c$perfil=='campestre_savanico') {
    c$direcoes_campo<-c('N','L','S','O')
    c$distancias_viarias_m<-c(estradas_pavimentadas=100,estradas_terra=50,trilhas_preexistentes=5)
  } else {
    p<-c$parametros_protocolo
    if(c$perfil=='ilha'){
      if(!is.list(c$padrao_ilha))mq_stop('padrao_ilha exige lista de parâmetros.')
      if(!is.null(p)&&!is.list(p))mq_stop('parametros_protocolo exige lista.')
      p<-utils::modifyList(c$padrao_ilha,if(is.null(p))list()else p,keep.null=TRUE)
      if(length(p$distancia_referencia_m)!=1||!is.numeric(p$distancia_referencia_m)||!is.finite(p$distancia_referencia_m)||p$distancia_referencia_m<=0||!length(p$direcoes_referencia)||any(!p$direcoes_referencia%in%c('N','L','S','O')))mq_stop('Referência Ilha inválida.')
      c$referencia_ilha<-c(p,list(fonte='Projeto piloto Noronha, setembro/2026',natureza='parâmetros experimentais; não substituem validação de campo'))
    }
    if(!is.list(p)||!all(c('transecto_m','grade_m')%in%names(p)))mq_stop('Perfil ',c$perfil,': informe parametros_protocolo com transecto_m e grade_m; padrões campestres não são herdados.')
    c$transecto_m<-p$transecto_m;c$grade_m<-p$grade_m
    c$distancia_min_m<-NA_real_;c$deslocamento_max_m<-NA_real_
    c$direcoes_campo<-character();c$distancias_viarias_m<-numeric()
    if(c$perfil=='personalizado') {
      if(!all(c('direcoes','distancias_viarias_m')%in%names(p))||any(!p$direcoes%in%c('N','L','S','O'))||anyDuplicated(p$direcoes))mq_stop('Personalizado: declare direcoes e distancias_viarias_m.')
      d<-p$distancias_viarias_m
      if(!is.numeric(d)||(length(d)>0&&(is.null(names(d))||anyDuplicated(names(d))||any(!names(d)%in%c('estradas_pavimentadas','estradas_terra','trilhas_preexistentes'))||any(!is.finite(d)|d<0)||!length(p$direcoes))))mq_stop('Distâncias viárias personalizadas inválidas.')
      c$direcoes_campo<-p$direcoes;c$distancias_viarias_m<-d
    }
  }
  for(n in c('usar_estradas_pavimentadas','usar_estradas_terra','usar_trilhas_preexistentes'))
    if(!is.null(c[[n]])&&(length(c[[n]])!=1||!is.logical(c[[n]])||is.na(c[[n]])))mq_stop('Use NULL, TRUE ou FALSE em ',n)
  c
}
mq_segments <- function(points,c) {
  if(!base::nrow(points))return(list())
  xy<-sf::st_coordinates(points)[,1:2,drop=FALSE];ll<-sf::st_coordinates(sf::st_transform(points,4326));ll[,2]<-ll[,2]+1e-5
  north<-sf::st_coordinates(sf::st_transform(sf::st_as_sf(data.frame(lon=ll[,1],lat=ll[,2]),coords=c('lon','lat'),crs=4326),sf::st_crs(points)))-xy
  north<-north/sqrt(base::rowSums(north^2))
  ans<-lapply(c$direcoes_campo,function(k){v<-switch(k,N=north,L=cbind(north[,2],-north[,1]),S=-north,O=cbind(-north[,2],north[,1]));sf::st_sfc(lapply(seq_len(base::nrow(points)),function(i)sf::st_linestring(rbind(xy[i,],xy[i,]+c$transecto_m*v[i,]))),crs=sf::st_crs(points))})
  stats::setNames(ans,c$direcoes_campo)
}
mq_road_sources <- function(layers,c,report) {
  audit<-list();targets<-list()
  for(role in c('estradas_pavimentadas','estradas_terra','trilhas_preexistentes')) {
    flag<-c[[paste0('usar_',role)]];src<-Filter(function(l)l$papel==role,layers);count<-sum(vapply(src,function(l)base::nrow(l$x),integer(1)))
    if(base::isTRUE(flag)&&!count)mq_stop('Camada exigida ausente ou vazia: ',role)
    active<-count>0&&!base::identical(flag,FALSE)&&role%in%names(c$distancias_viarias_m)
    if(active) {
      items<-lapply(src,function(l){x<-l$x;line<-as.character(sf::st_geometry_type(x))%in%c('LINESTRING','MULTILINESTRING');width<-rep(0,base::nrow(x));
        if('largura_m'%in%names(x)){w<-suppressWarnings(as.numeric(x$largura_m));if(any(line&(!is.finite(w)|w<0)))mq_stop('largura_m inválida em ',role);width[line]<-w[line]}
        # Limiar por feição: distância ao eixo acrescida de metade da largura, quando declarada.
        sf::st_sf(limite=c$distancias_viarias_m[[role]]+width/2,geometry=sf::st_geometry(x))})
      targets[[role]]<-do.call(rbind,items)
    }
    audit[[role]]<-list(configuracao=if(is.null(flag))'auto'else flag,feicoes=count,aplicada=active,distancia_m=if(role%in%names(c$distancias_viarias_m))c$distancias_viarias_m[[role]]else NULL,fontes=vapply(src, function(l)if(is.null(l$fonte))l$papel else l$fonte,character(1)),nota=if(!count)'Não fornecida/vazia: ausência de conflito não comprovada.'else if(c$perfil=='ilha')'Referência visual: procedimento campestre não se aplica ao perfil Ilha.'else if(base::identical(flag,FALSE))'Desabilitada explicitamente; somente referência visual.'else 'Polígonos: borda; linhas: eixo, com largura_m/2 quando informada. Sem largura, limitação de distância ao eixo. Cobertura fora das AEs deve ser fornecida.')
    message('Vias — ',role,': ',count,' feições; ',if(active)'restrição aplicada'else 'restrição não aplicada')
  }
  legacy<-Filter(function(l)l$papel%in%c('rodovias','estradas','trilhas'),layers)
  if(length(legacy))message('Vias com nomes legados: somente referência. Declare papel explícito em camadas_qfield.csv para aplicar restrições.')
  mq_json(list(perfil=c$perfil,camadas=audit,legadas_sem_classificacao=vapply(legacy,`[[`,character(1),'nome')),file.path(report,'restricoes_viarias.json'))
  targets
}
mq_road_screen <- function(g,targets,ae,c,report,tag='grade') {
  g$mq_viavel_vias<-rep(TRUE,base::nrow(g));g$mq_direcoes_vias<-rep(NA_character_,base::nrow(g));g$mq_status_vias<-rep('não avaliado: sem restrições viárias ativas',base::nrow(g))
  if(!length(targets)||!base::nrow(g))return(g)
  segments<-mq_segments(g,c);domain<-sf::st_union(ae);rows<-list();viable<-matrix(FALSE,base::nrow(g),length(segments),dimnames=list(NULL,names(segments)))
  progress<-mq_progress('Triagem de restrições viárias',length(segments));on.exit(mq_progress_done(progress),add=TRUE)
  for(k in seq_along(segments)) {
    line<-segments[[k]];inside<-lengths(sf::st_covered_by(line,domain))>0
    d<-data.frame(id=if('chave_grade'%in%names(g))g$chave_grade else seq_len(base::nrow(g)),direcao=names(segments)[k],inteiro_na_AE=inside)
    conflict<-rep(FALSE,base::nrow(g))
    for(role in names(targets)) {
      t<-targets[[role]];near<-sf::st_is_within_distance(line,t,dist=max(t$limite))
      v<-vapply(seq_along(near),function(i)length(near[[i]])>0&&any(as.numeric(sf::st_distance(line[i],t[near[[i]],]))<t$limite[near[[i]]]-1e-7),logical(1))
      d[[paste0('conflito_',role)]]<-v;conflict<-conflict|v
    }
    viable[,k]<-!conflict;d$viavel<-viable[,k];rows[[k]]<-d;mq_progress_update(progress,k,names(segments)[k])
  }
  g$mq_viavel_vias<-base::rowSums(viable)>0;g$mq_direcoes_vias<-apply(viable,1,function(v)paste(colnames(viable)[v],collapse=','))
  g$mq_status_vias<-ifelse(g$mq_viavel_vias,'sem conflito detectado nas fontes ativas','indisponível no PA original; eventual deslocamento exige avaliação de campo')
  mq_csv(do.call(rbind,rows),file.path(report,paste0('restricoes_direcoes_',tag,'.csv')))
  mq_csv(sf::st_drop_geometry(g),file.path(report,paste0('restricoes_pontos_',tag,'.csv')))
  g
}
mq_road_available <- function(g) if('mq_viavel_vias'%in%names(g))g$mq_viavel_vias else rep(TRUE,base::nrow(g))
