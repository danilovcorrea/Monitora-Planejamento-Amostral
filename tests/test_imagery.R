options(monitora.qfield.somente_funcoes=TRUE)
source('monitora_planejamento_amostral.R',encoding='UTF-8');MQ_CONFIG<-mq_legacy_defaults() # Regressão da interface pública 1.0.2
suppressPackageStartupMessages(library(sf))
check<-character();ok<-function(n,v){stopifnot(isTRUE(v));check<<-c(check,n);cat('PASS ',n,'\n',sep='')};fails<-function(x)inherits(try(force(x),silent=TRUE),'try-error')
cr<-st_crs(31983);pt<-function(x,y)st_sf(PA=paste0('PA',seq_along(x)),geometry=st_sfc(lapply(seq_along(x),function(i)st_point(c(x[i],y[i]))),crs=cr))
pa<-pt(620000,8020000);p<-list(x=pa,papel='PA_priorit',nome='PA_priorit',label='PA');a<-p;a$papel<-'verg_ini';a$nome<-'verg_ini_2026';a$ano<-'2026';b<-a;b$papel<-'verg_fin';b$nome<-'verg_fin_2026';b$x<-pt(620000,8020050)
c<-MQ_CONFIG;c$cache_dir<-tempfile();dir.create(c$cache_dir);c$zooms_detalhe<-18L;c$confirmar_download<-TRUE;c$url_xyz<-'https://fixture.invalid/{z}/{x}/{y}';c$caches_adicionais<-character()
ok('sem_UA_seleciona_PA',nrow(mq_centers(list(p),c,cr))==1)
u<-mq_centers(list(p,a,b),c,cr);ok('com_UA_prioriza_ponto_medio',identical(attr(u,'abrangencia'),'UAs')&&abs(st_coordinates(u)[1,2]-8020025)<1e-9)
c$centros_detalhe<-'UAs_e_PAs';ok('ambos_explicitamente',nrow(mq_centers(list(p,a,b),c,cr))==2);c$centros_detalhe<-'auto'
ok('UA_sem_par_bloqueia',fails(mq_centers(list(a),c,cr)))
q<-p;q$papel<-'grade_amostral';ok('nao_baixa_grade_inteira',nrow(mq_centers(list(q),c,cr))==0)
x<-mq_centers(list(p),c,cr);mask<-mq_mask(x,c);ok('raio_500_metrico',abs(as.numeric(st_area(mask))-pi*500^2)/(pi*500^2)<0.0001)
# Transporte simulado: contagem de chamadas e PNG com orientação conhecida. Nenhum servidor acessado.
e<-new.env(parent=globalenv());sys.source('monitora_planejamento_amostral.R',envir=e);counter<-0L
rgb<-array(1,c(256,256,4));rgb[,,1]<-matrix(rep((0:255)/255,256),256,256);rgb[,,2]<-0;rgb[,,3]<-1-rgb[,,1];blob<-png::writePNG(rgb,target=raw())
e$qfi_buscar<-function(c,z,x,y){counter<<-counter+1L;list(ok=TRUE,raw=blob,formato='png',largura=256L,altura=256L)}
run<-function(cfg){s<-tempfile();r<-tempfile();dir.create(s);dir.create(r);e$mq_download(x,character(),cfg,s,r)}
r<-run(c);n<-counter;ok('download_confirmado',n>0&&length(r$paths)==1)
c$confirmar_download<-NULL;r2<-run(c);ok('cache_completo_zero_requisicoes_sem_prompt',counter==n&&length(r2$paths)==1)
disabled<-c;disabled$baixar_imagem_detalhe<-FALSE;reuse<-run(disabled);ok('download_desativado_reutiliza_cache',counter==n&&length(reuse$paths)==1)
cc<-c;cc$cache_dir<-tempfile();dir.create(cc$cache_dir);ok('nao_interativo_exige_confirmacao',fails(run(cc))&&counter==n)
cc$confirmar_download<-FALSE;rr<-run(cc);ok('recusa_nao_baixa',counter==n&&identical(rr$status,'download_nao_autorizado'))
cc$caches_adicionais<-c$cache_dir;cc$confirmar_download<-NULL;rr<-run(cc);ok('reutiliza_cache_outra_pasta',counter==n&&length(rr$paths)==1)
report<-tempfile();dir.create(report);cut<-mq_clip_mb(r$paths[1],mask,c,report);ok('recorte_integridade',mq_verified(cut))
db<-DBI::dbConnect(RSQLite::SQLite(),cut,flags=RSQLite::SQLITE_RO);tiles<-DBI::dbGetQuery(db,'SELECT tile_data FROM tiles');alphas<-lapply(tiles$tile_data,function(z)png::readPNG(z)[,,4]);ok('recorte_transparencia_borda',any(vapply(alphas,function(v)any(v==0)&&any(v>0),logical(1))))
for(z in tiles$tile_data){aa<-png::readPNG(z);good<-aa[,,4]>0;stopifnot(max(abs(aa[,,1][good]-rgb[,,1][good]))<1/255+1e-8)};ok('recorte_preserva_orientacao_e_cor',TRUE);DBI::dbDisconnect(db)
stamp<-file.info(cut)$mtime;cut2<-mq_clip_mb(r$paths[1],mask,c,report);ok('recorte_cache_reutilizado',identical(cut,cut2)&&identical(stamp,file.info(cut2)$mtime))
# Falha interrompe a transferência e a nova execução reutiliza os sucessos anteriores.
cc<-c;cc$cache_dir<-tempfile();dir.create(cc$cache_dir);cc$confirmar_download<-TRUE;first<-TRUE;attempt<-0L
e$qfi_buscar<-function(c,z,x,y){attempt<<-attempt+1L;if(first&&attempt==2L){first<<-FALSE;return(list(ok=FALSE,motivo='HTTP429 fixture',interromper=TRUE))};list(ok=TRUE,raw=blob,formato='png',largura=256L,altura=256L)}
z<-run(cc);ok('falha_preserva_cache',identical(z$status,'download_incompleto'));before<-attempt;z<-run(cc);ok('retomada_nao_rebaixa_sucesso',attempt-before==n-1&&identical(z$status,'download_completo'))
cat('TOTAL ',length(check),' PASS\n')
# Sentinel: fonte arbitrária não é aceita; confirmação e renovação respeitadas.
sc<-c;sc$confirmar_sentinel<-FALSE;sc$sentinel_arquivo<-r$paths[1];scratch<-tempfile();dir.create(scratch)
ok('raster_arbitrario_nao_vira_Sentinel',fails(mq_sentinel(mask,x,sc,scratch,report)))
mq_json(list(produto='Sentinel-2 L2A',resolucao_nativa_m=10,versao_acervo=sc$versao_acervo,sha256=mq_hash(sc$sentinel_arquivo),origem='fixture sintética'),paste0(sc$sentinel_arquivo,'.fonte.json'))
ok('Sentinel_local_documentado_reutilizado',identical(mq_sentinel(mask,x,sc,scratch,report),sc$sentinel_arquivo))
sc$renovar_imagens<-TRUE;ok('renovar_nao_reutiliza_Sentinel_antigo',fails(mq_sentinel(mask,x,sc,scratch,report)))
# MapBiomas: nova chamada consulta apenas coordenadas inéditas.
mbe<-new.env(parent=globalenv());sys.source('monitora_planejamento_amostral.R',envir=mbe);nmb<-0L
mbe$mq_mb_fresh<-function(x,c,report){nmb<<-nmb+nrow(x);x$mb_codigo<-3L;x$mb_classe<-'fixture';x$mb_status<-'obtido';x}
mc<-c;mc$cache_dir<-tempfile();mbe$mq_mb(pa,mc,report);mbe$mq_mb(pa,mc,report);ok('MapBiomas_cache_sem_reconsulta',nmb==1L)
mbe$mq_mb(rbind(pa,pt(620100,8020000)),mc,report);ok('MapBiomas_expansao_somente_novos',nmb==2L)
cat('TOTAL_FINAL ',length(check),' PASS\n')

# SQL por tile deve coincidir com a leitura raster, inclusive transparência e fora da máscara.
probe<-st_as_sf(expand.grid(x=seq(619400,620600,length.out=17),y=seq(8019400,8020600,length.out=17)),coords=c('x','y'),crs=cr)
raster<-terra::rast(cut);values<-terra::extract(raster,terra::project(terra::vect(probe),terra::crs(raster)));expected<-!is.na(values[[5]])&values[[5]]>0
ok('cobertura_SQL_igual_GDAL_289_pontos',identical(mq_mb_visible(cut,probe),expected))
cat('TOTAL_FINAL ',length(check),' PASS\n')

sc$renovar_imagens<-FALSE
ok('Sentinel_aceita_geometria_sfc',mq_sentinel_covers(sc$sentinel_arquivo,sf::st_geometry(mask)))
cat('TOTAL_FINAL ',length(check),' PASS\n')
