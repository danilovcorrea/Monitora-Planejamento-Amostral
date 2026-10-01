# Reutiliza a infraestrutura real de exportação/Sentinel sintético do teste principal.
source('qfield/tests/test_core.R',encoding='UTF-8')
start_checks<-length(checks)
x<-st_read(file.path(p,'areas_elegiveis.gpkg'),quiet=TRUE);x$formacao<-'campestre';x$manejo<-'referencia';st_write(x,file.path(p,'areas_elegiveis.gpkg'),delete_dsn=TRUE,quiet=TRUE)
e$mq_uc<-function(ae,c)list(x=mq_empty(4326),fonte='fixture',camada='fixture',titulo='fixture',consulta='teste',total_bbox=0,status='consulta_completa')
dc<-co;dc$saida<-tempfile();dc$estratificar_vegetacao<-TRUE;dc$formacao_campo<-'formacao';dc$estratificar_por_atributos<-TRUE;dc$cotas_atributos<-list(manejo=data.frame(classe='referencia',percentual=100))
dr<-e$monitora_criar_qfield(dc);gd<-st_read(file.path(dr$pasta,'02_relatorio/cadastro_grade.gpkg'),quiet=TRUE)
ok('integracao_cotas_cadastro',all(c('mq_estrato','mq_formacao','atr_manejo')%in%names(gd))&&file.exists(file.path(dr$pasta,'02_relatorio/cotas_realizadas.csv')))
dx<-dc;dx$modo<-'expandir';dx$referencia_anterior<-file.path(dr$pasta,'02_relatorio');dx$saida<-tempfile();er<-e$monitora_criar_qfield(dx);ge<-st_read(file.path(er$pasta,'02_relatorio/cadastro_grade.gpkg'),quiet=TRUE)
ok('integracao_expansao_estratos',identical(gd$mq_estrato,ge$mq_estrato)&&identical(gd$categoria,ge$categoria))
dx$formacao_mapa<-data.frame(classe='campestre',formacao='campestre');dx$saida<-tempfile();ok('integracao_migracao_contrato_bloqueada',fails(e$monitora_criar_qfield(dx)))
# Montagem conflitante deve preservar o campo histórico e não produzir margens parciais.
mi<-tempfile();dir.create(mi);file.copy(file.path(p,'areas_elegiveis.gpkg'),mi)
pr<-gd[gd$categoria=='prioritario',];al<-gd[gd$categoria=='alternativo',];pr$mq_formacao<-'savanica'
st_write(pr,file.path(mi,'PA_priorit.gpkg'),layer='PA_priorit',quiet=TRUE);st_write(al,file.path(mi,'PA_altern.gpkg'),layer='PA_altern',quiet=TRUE)
mc<-dc;mc$entrada<-mi;mc$saida<-tempfile();mc$modo<-'montar';mr<-e$monitora_criar_qfield(mc)
mp<-st_read(file.path(mr$pasta,'01_qfield/dados/PA_priorit.gpkg'),layer='PA_priorit',quiet=TRUE)
ok('montagem_preserva_classificacao_conflitante',all(mp$mq_formacao=='savanica')&&identical(mp$PA,pr$PA))
ok('montagem_nao_audita_denominador_parcial',!file.exists(file.path(mr$pasta,'02_relatorio/cotas_fornecidas_auditoria.csv'))&&grepl('pendências',mr$status))
# Expansão com retirada de área conserva os registros históricos fora das cotas atuais.
xsmall<-st_sf(formacao='campestre',manejo='referencia',geometry=st_geometry(x));st_geometry(xsmall)<-st_sfc(rect(198000,8432000,198900,8433000),crs=31983)
ei<-tempfile();dir.create(ei);st_write(xsmall,file.path(ei,'areas_elegiveis.gpkg'),layer='areas_elegiveis',quiet=TRUE)
dropc<-dc;dropc$entrada<-ei;dropc$saida<-tempfile();dropc$modo<-'expandir';dropc$referencia_anterior<-file.path(dr$pasta,'02_relatorio');inside<-lengths(st_intersects(gd,xsmall))>0;need<-max(sum(inside&gd$categoria=='prioritario'),ceiling(sum(inside&gd$categoria=='alternativo')/2));dropc$prioritarios<-list(n=need,percentual=NULL)
# Se as categorias preservadas forem incompatíveis, o bloqueio é comportamento esperado; cadastro de origem continua íntegro.
origin_hash<-mq_hash(file.path(dr$pasta,'02_relatorio/cadastro_grade.gpkg'));shrunk<-e$monitora_criar_qfield(dropc);history<-st_read(file.path(shrunk$pasta,'02_relatorio/cadastro_grade.gpkg'),quiet=TRUE);ix<-match(gd$PA,history$PA);ok('historico_fora_AE_preservado',!anyNA(ix)&&all(history$categoria[ix][!inside]==gd$categoria[!inside])&&all(history$mq_estrato[ix][!inside]==gd$mq_estrato[!inside]))
ok('historico_origem_inalterado',identical(origin_hash,mq_hash(file.path(dr$pasta,'02_relatorio/cadastro_grade.gpkg'))))
cat('TOTAL_INTEGRACAO_DESIGN ',length(checks)-start_checks,' PASS\n',sep='')
# Vias efetivamente aplicadas antes das cotas em execução completa.
vi<-tempfile();dir.create(vi);xx<-st_read(file.path(p,'areas_elegiveis.gpkg'),quiet=TRUE);st_write(xx,file.path(vi,'areas_elegiveis.gpkg'),quiet=TRUE)
bb<-st_bbox(xx);road<-st_sf(geometry=st_sfc(st_linestring(matrix(c(bb[['xmin']],bb[['ymin']]-100,bb[['xmin']],bb[['ymax']]+100),ncol=2,byrow=TRUE)),crs=st_crs(xx)))
st_write(road,file.path(vi,'estradas_pavimentadas.gpkg'),layer='estradas_pavimentadas',quiet=TRUE)
vc<-dc;vc$entrada<-vi;vc$saida<-tempfile();vr<-e$monitora_criar_qfield(vc);vg<-st_read(file.path(vr$pasta,'02_relatorio/cadastro_grade.gpkg'),quiet=TRUE)
ok('integracao_viaria_antes_cotas',any(!vg$mq_viavel_vias)&&all(vg$mq_viavel_vias[vg$categoria!='grade']))
ok('integracao_viaria_auditoria',file.exists(file.path(vr$pasta,'02_relatorio/restricoes_direcoes_grade.csv'))&&nrow(vg)==nrow(gd))
# Montagem preserva todos os pontos, mesmo quando há conflito viário.
file.copy(file.path(vi,'estradas_pavimentadas.gpkg'),mi);mc$saida<-tempfile();vm<-e$monitora_criar_qfield(mc);vp<-st_read(file.path(vm$pasta,'01_qfield/dados/PA_priorit.gpkg'),quiet=TRUE)
ok('montagem_vias_preserva_PAs',nrow(vp)==nrow(pr)&&identical(vp$PA,pr$PA))
cat('TOTAL_INTEGRACAO_V042 ',length(checks)-start_checks,' PASS\n',sep='')
