# Compatibilidade exata com a configuração pública 1.0.2.
mq_legacy_defaults <- function() list(
  entrada = 'entrada', saida = 'saida', projeto = 'Monitora', pasta_base = NULL,
  gerar_cartografia = TRUE, qgis_python = NULL, # NULL testa instalações QGIS 3/4; caminho explícito fixa uma instalação.
  mapas_dpi = 300L, mapas_papel = 'A4',
  elaboracao = 'Programa Monitora', # edite para registrar a autoria efetiva
  modo = 'auto', # auto: montar se houver PAs/UAs/grade fornecidos; senão planejar.
  referencia_anterior = NULL, # pasta 02_relatorio de uma execução; obrigatória em expandir
  transecto_m = 50, distancia_min_m = 100, deslocamento_max_m = 10,
  # Campestre: altere grade_m. Personalizado: parametros_protocolo$grade_m. Ilha: padrao_ilha ou sobrescrita em parametros_protocolo.
  grade_m = c(156.25, 156.25), epsg = NULL, semente = 20261001L,
  politica_insuficiencia = 'usar_disponiveis', # padrão adaptável; 'bloquear' exige cotas exatas
  prioritarios = list(n=NULL, percentual=20),
  alternativos = list(n=NULL, percentual=NULL), # ambos NULL: 2 x prioritários
  # Cotas cumulativas incidem no MESMO conjunto de PAs; déficits seguem politica_insuficiencia.
  estratificar_vegetacao = TRUE, incluir_formacao_florestal = FALSE, # Ilha inclui floresta automaticamente
  incluir_antropizadas = TRUE, # pastagem (MapBiomas 15) e degradada declarada no vetor; registrar ocorrências
  perfil = 'campestre_savanico', # ilha: sem procedimentos de campo campestres
  parametros_protocolo = NULL, # Ilha usa padrao_ilha; lista permite sobrescrever dimensões. Personalizado exige parâmetros próprios.
  padrao_ilha = list(transecto_m=25, grade_m=c(30,30), distancia_referencia_m=30, direcoes_referencia=c('N','L','S','O')), # piloto Noronha 2026; valores experimentais, não regras campestres
  contexto_uc = 'componentes_com_AE', # componentes inteiros da UC; 'integral' inclui também setores remotos
  usar_estradas_pavimentadas = NULL, usar_estradas_terra = NULL, usar_trilhas_preexistentes = NULL, # NULL detecta; TRUE exige; FALSE somente exibe
  localizador_bioma_min_mm2 = 0.5, # generalização só do localizador e de sua legenda
  formacao_campo = NULL, # NULL: MapBiomas; ou nome exato do campo no vetor
  formacao_mapa = NULL, # data.frame(classe=c('Campo','Savana'),formacao=c('campestre','savanica'))
  fitofisionomia_campo = NULL, # quando informado, balanceia fitofisionomias; não apenas formações
  cotas_formacao = NULL, # NULL: balanceado; ou data.frame(classe=..., percentual=...) OU n=...
  estratificar_por_atributos = FALSE,
  estratos_arquivo = NULL, estratos_camada = NULL, # ambos NULL: polígonos das AEs
  # list(queima_pre=data.frame(classe=c('0','1'),percentual=c(60,40)), setor=...)
  cotas_atributos = list(), cotas_atributos_arquivo = NULL, # CSV longo: atributo;classe;n OU percentual
  solver_timeout_s = 60L, max_estratos = 2000L,
  mapbiomas = TRUE, mb_produto = '30m', mb_colecao = 11L, mb_ano = 2025L,
  # Detalhe pode levar dezenas de minutos e centenas de MiB. TRUE prepara; não autoriza download.
  baixar_imagem_detalhe = TRUE, confirmar_download = NULL, confirmar_sentinel = NULL, # NULL pergunta; TRUE autoriza; FALSE usa somente fontes locais
  centros_detalhe = 'auto', # auto: UAs quando presentes; senão PAs. PAs/UAs/UAs_e_PAs também aceitos.
  raio_detalhe_m = 500, zooms_detalhe = 16:18, margem_contexto_m = 500,
  cache_dir = tools::R_user_dir('Monitora_QField','cache'), caches_adicionais = character(),
  sentinel_arquivo = NULL, # opcional: MBTiles Sentinel anterior; só reutilizado se cobrir o contexto
  versao_acervo = 'acervo_01', renovar_imagens = FALSE,
  url_xyz = 'https://mt1.google.com/vt/lyrs=s&x={x}&y={y}&z={z}', cabecalhos_xyz = character(),
  fonte_xyz = 'Google Satellite', atribuicao_xyz = 'Google', licenca_xyz = 'não declarada',
  intervalo_download_s = 0.5, tentativas_download = 3L, max_tiles = 250000L,
  mb_obrigatorio = FALSE, max_pontos = 500000L, timeout_s = 60,
  uc_url = 'https://geoservicos.inde.gov.br/geoserver/ICMBio/ows',
  uc_camada = 'ICMBio:limiteucsfederais_a'
)
