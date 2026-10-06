# Metas cumulativas adaptáveis: nunca transformar falta de capacidade em falsa conformidade.
mq_design_select <- function(g,c,report,old=NULL) {
  if(!c$estratificar_vegetacao&&!c$estratificar_por_atributos)return(mq_select(g,c,report))
  if(base::identical(c$politica_insuficiencia,'bloquear'))return(mq_design_select_estrito(g,c,report,old))
  if(!base::requireNamespace('lpSolve',quietly=TRUE))mq_stop('Instale lpSolve para resolver metas cumulativas.')
  N<-base::nrow(g);requested<-c(prioritarios=mq_quantity(c$prioritarios,N,ceiling(.2*N)))
  requested<-c(requested,alternativos=mq_quantity(c$alternativos,N,unname(2L*requested[1])))
  criteria<-mq_design_criteria(g,c,requested[1]);fields<-names(criteria)
  if(!length(fields))return(mq_select(g,c,report))
  viable<-g$mq_apto&mq_road_available(g)
  # Zero declarado é exclusão. Zero de meta automática para antropizadas permite complemento.
  for(f in fields){q<-criteria[[f]];explicit<-if('explicita'%in%names(q))q$explicita else rep(TRUE,base::nrow(q));zero<-q$classe[explicit&q$valor==0];viable<-viable&!g[[f]]%in%zero}
  if(isTRUE(c$preservar_historico_excecao))viable[g$categoria!='grade']<-TRUE
  if(!any(viable))mq_stop('Nenhum candidato elegível/classificado após exclusões e restrições. Consulte classificacao_vegetacao.csv e vegetacao_pendente.csv; ausência de classificação não prova ausência de alvo.')
  if(any(g$categoria!='grade'&!viable))mq_stop('PA histórico incompatível com elegibilidade, exclusão explícita ou vias; migração/revisão necessária.')
  opn<-sum(g$categoria=='prioritario');oan<-sum(g$categoria=='alternativo')
  if(opn>requested[1]||oan>requested[2])mq_stop('Solicitação inferior ao histórico preservado; revisão explícita necessária.')
  np<-min(requested[1],sum(viable)-oan);na<-min(requested[2],sum(viable)-np)
  quantities<-data.frame(categoria=names(requested),solicitado=as.integer(requested),realizado=c(np,na),deficit=as.integer(requested)-c(np,na),denominador_grade_AE=N,candidatos_disponiveis=sum(viable),politica=c$politica_insuficiencia)
  mq_csv(quantities,file.path(report,'selecao_quantidades.csv'))
  tuples<-do.call(paste,c(lapply(sf::st_drop_geometry(g)[,fields,drop=FALSE],function(v){v<-as.character(v);paste0(nchar(v,type='bytes'),':',v)}),sep='|'))
  keys<-vapply(tuples,digest::digest,character(1),algo='sha256',serialize=FALSE);g$mq_estrato<-keys;g$mq_estrato[!g$mq_apto]<-'fora_alvo'
  if(!is.null(old)&&!isTRUE(c$revisao_historico)) {
    ix<-base::match(g$chave_grade,old$chave_grade);sel<-which(!is.na(ix)&g$categoria!='grade')
    if(length(sel)&&(!'mq_estrato'%in%names(old)||anyNA(old$mq_estrato[ix[sel]])||any(old$mq_estrato[ix[sel]]!=g$mq_estrato[sel])))mq_stop('Expansão mudaria estrato de PA histórico; migração explícita necessária.')
  }
  cells<-base::sort(base::unique(keys[viable]),method='radix');H<-length(cells)
  if(H>c$max_estratos)mq_stop('Quantidade de combinações acima de max_estratos: ',H)
  cell<-base::match(keys,cells);cap<-tabulate(cell[viable],H);op<-tabulate(cell[g$categoria=='prioritario'],H);oa<-tabulate(cell[g$categoria=='alternativo'],H)
  combos<-sf::st_drop_geometry(g[base::match(cells,keys),fields,drop=FALSE]);combos$estrato<-cells;combos$disponiveis<-cap;combos$preservados_p<-op;combos$preservados_a<-oa
  mq_csv(combos,file.path(report,'combinacoes_disponiveis.csv'))
  bands<-list()
  for(f in fields)for(j in seq_len(base::nrow(criteria[[f]])))for(k in 0:1) {
    q<-criteria[[f]][j,];share<-if(q$medida=='percentual')q$valor/100 else if(requested[1]>0)q$alvo/requested[1] else 0
    wanted<-if(k==0)q$alvo else requested[2]*share
    target<-if(q$medida=='percentual')c(np,na)[k+1]*share else wanted
    bands[[length(bands)+1L]]<-list(campo=f,classe=q$classe,grupo=c('prioritario','alternativo')[k+1],ix=k*H+which(combos[[f]]==q$classe),alvo=target,solicitado=wanted,medida=if(k==0)q$medida else 'proporcao_dos_prioritarios',valor=if(k==0)q$valor else 100*share)
  }
  B<-length(bands);V<-6*H+2*B;rows<-list();dirs<-character();rhs<-numeric()
  add<-function(ix,val,dir,b){r<-length(rhs)+1L;rows[[r]]<<-if(length(ix))cbind(r,ix,val)else matrix(numeric(),0,3);dirs[r]<<-dir;rhs[r]<<-b}
  add(seq_len(H),rep(1,H),'=',np);add(H+seq_len(H),rep(1,H),'=',na)
  for(h in seq_len(H)) {
    add(c(h,H+h),c(1,1),'<=',cap[h]);add(h,1,'>=',op[h]);add(H+h,1,'>=',oa[h])
    for(k in 0:1)add(c(k*H+h,2*H+2*B+2*(k*H+h)-1:0),c(1,-1,1),'=',c(np,na)[k+1]*cap[h]/sum(cap))
  }
  for(j in seq_along(bands)){b<-bands[[j]];add(c(b$ix,2*H+2*j-1:0),c(rep(1,length(b$ix)),-1,1),'=',b$alvo)}
  solve<-function(obj){s<-lpSolve::lp('min',obj,const.dir=dirs,const.rhs=rhs,dense.const=do.call(rbind,rows),int.vec=seq_len(2*H),timeout=as.integer(c$solver_timeout_s),scale=0);if(s$status!=0){mq_json(list(status_solver=s$status,interpretacao=if(s$status==2)'inviabilidade_comprovada'else 'sem_otimo_comprovado'),file.path(report,'solucao_cotas.json'));mq_stop('Solver sem ótimo comprovado (status ',s$status,'); não é diagnóstico automático de ausência de áreas.')} ;s}
  progress<-mq_progress('Adaptar metas aos candidatos disponíveis',3);on.exit(mq_progress_done(progress),add=TRUE)
  # Sem cotas explícitas, usar primeiro a vegetação nativa e complementar com antropizadas.
  if(c$estratificar_vegetacao&&is.null(c$cotas_formacao)&&!c$estratificar_por_atributos) {
    native<-which(combos$mq_formacao%in%c('campestre','savanica','florestal'));ix<-c(native,H+native)
    if(length(native))for(prefer in list(native,ix)){obj<-numeric(V);obj[prefer]<- -1;s<-solve(obj);add(prefer,rep(1,length(prefer)),'>=',round(sum(s$solution[prefer])))}
  }
  mq_progress_update(progress,1,'Elegibilidade, capacidades e histórico')
  margvars<-2*H+seq_len(2*B);obj<-numeric(V);obj[margvars]<-1;s<-solve(obj);add(margvars,rep(1,length(margvars)),'<=',s$objval+1e-7)
  mq_progress_update(progress,2,'Metas cumulativas e desvios')
  obj[]<-0;obj[(2*H+2*B+1):V]<-1;s<-solve(obj)
  counts<-round(s$solution[seq_len(2*H)]);p<-counts[seq_len(H)];a<-counts[H+seq_len(H)]
  if(!all(abs(counts-s$solution[seq_len(2*H)])<1e-5)||sum(p)!=np||sum(a)!=na||any(p<op)||any(a<oa)||any(p+a>cap)||any(counts<0))mq_stop('Falha na verificação independente da solução inteira adaptável.')
  actual<-vapply(bands,function(b)sum(counts[b$ix]),numeric(1))
  marginal<-do.call(rbind,lapply(seq_along(bands),function(j){b<-bands[[j]];data.frame(atributo=b$campo,classe=b$classe,grupo=b$grupo,medida=b$medida,valor=b$valor,solicitado=b$solicitado,alvo_real=b$alvo,minimo=floor(b$alvo+1e-8),maximo=ceiling(b$alvo-1e-8),realizado=actual[j],desvio=actual[j]-b$alvo,deficit_solicitado=max(0,b$solicitado-actual[j]))}))
  marginal$fora_margem<-marginal$realizado<marginal$minimo|marginal$realizado>marginal$maximo
  mq_csv(marginal[,base::setdiff(names(marginal),c('realizado','desvio','deficit_solicitado','fora_margem'))],file.path(report,'cotas_solicitadas.csv'));mq_csv(marginal,file.path(report,'cotas_realizadas.csv'))
  combos$prioritarios<-p;combos$alternativos<-a;mq_csv(combos,file.path(report,'alocacao_combinacoes.csv'))
  rank<-vapply(g$chave_grade,function(k)digest::digest(paste(c$semente,k,sep=':'),algo='sha256',serialize=FALSE),character(1))
  for(h in seq_len(H)) {
    available<-which(viable&!is.na(cell)&cell==h&g$categoria=='grade');available<-available[order(rank[available],g$id_grade[available])]
    pp<-utils::head(available,p[h]-op[h]);if(length(pp))g$categoria[pp]<-'prioritario'
    aa<-utils::head(base::setdiff(available,pp),a[h]-oa[h]);if(length(aa))g$categoria[aa]<-'alternativo'
  }
  if(any(g$categoria!='grade'&!viable)||sum(g$categoria=='prioritario')!=np||sum(g$categoria=='alternativo')!=na)mq_stop('Seleção final divergiu da solução verificada.')
  adjusted<-any(quantities$deficit>0)||any(marginal$fora_margem)
  if(adjusted)message('ATENÇÃO: metas adaptadas à disponibilidade. Confira selecao_quantidades.csv e cotas_realizadas.csv; não representam cumprimento integral do desenho solicitado.')
  mq_json(list(status=if(adjusted)'verificado_com_ocorrencias'else 'verificado',politica='usar_disponiveis',denominador_grade_AE=N,candidatos_habilitados=sum(cap),prioritarios=np,alternativos=na,metas_ajustadas=adjusted,alternativos_regra='até o solicitado; dobro é meta global, não obrigação por combinação',campos=fields,semente=c$semente,limite_inferencia='desenho efetivamente executado nas AEs; não comprova representatividade da UC nem conformidade de campo'),file.path(report,'solucao_cotas.json'))
  mq_progress_update(progress,3,'Seleção verificada')
  g
}
mq_selection_occurrences <- function(g,c,report) {
  notes<-character()
  if('mq_condicao'%in%names(g)){mq_csv(g,file.path(report,'condicoes_vegetacao.csv'));ncond<-sum(g$categoria!='grade'&g$mq_condicao%in%c('degradada','restauracao','restaurada'));if(ncond)notes<-c(notes,paste(ncond,'PAs em degradação/restauração declarada no vetor; formação e cobertura preservadas separadamente. Consulte condicoes_vegetacao.csv.'))}
  if(!'mq_formacao'%in%names(g)&&'mb_codigo'%in%names(g))g$mq_formacao<-ifelse(g$mb_codigo%in%15,'pastagem','nao_estratificada')
  if('mq_formacao'%in%names(g)) {
    g$mq_ocorrencia<-ifelse(g$mq_formacao=='pastagem','Pastagem incluída; não é formação nativa. Confirmar aptidão em campo.',ifelse(g$mq_formacao=='degradada','Degradação declarada no vetor; não inferida automaticamente da imagem.',ifelse(g$mq_formacao=='nao_resolvida','Classificação não resolvida; elegibilidade depende do filtro efetivo e de decisões registradas.','')))
    out<-g[nzchar(g$mq_ocorrencia),];mq_csv(out,file.path(report,'ocorrencias_vegetacao.csv'))
    n<-sum(nzchar(g$mq_ocorrencia)&g$categoria!='grade');pending<-sum(g$mq_formacao=='nao_resolvida'&!g$mq_apto)
    if(n||pending)notes<-c(notes,paste('Ocorrências vegetacionais:',n,'PAs em pastagem/degradada;',pending,'pontos sem classificação excluídos. Consulte ocorrencias_vegetacao.csv; não comprova conformidade de campo.'))
    mq_csv(base::as.data.frame(table(categoria=g$categoria,formacao=g$mq_formacao)),file.path(report,'distribuicao_formacoes.csv'))
  }
  if(c$perfil=='campestre_savanico'&&'mq_formacao'%in%names(g)&&any(g$mq_apto)) {
    field<-if('mq_fitofisionomia'%in%names(g))'mq_fitofisionomia'else 'mq_formacao'
    effort<-data.frame(estrato=base::sort(base::unique(g[[field]][g$mq_apto])))
    effort$PA_prioritarios<-vapply(effort$estrato,function(v)sum(g$categoria=='prioritario'&g[[field]]==v),integer(1))
    effort$observacao<-'PA candidato não comprova UA instalada nem esforço protocolar consolidado.'
    mq_csv(effort,file.path(report,'esforco_planejado.csv'))
  }
  f<-file.path(report,'cotas_realizadas.csv')
  if(file.exists(f)){q<-data.table::fread(f);if('fora_margem'%in%names(q)&&any(q$fora_margem))notes<-c(notes,paste('ATENÇÃO:',sum(q$fora_margem),'metas de classes fora das margens solicitadas; déficits e desvios em cotas_realizadas.csv. A análise deve considerar o desenho realizado.'))}
  notes
}
