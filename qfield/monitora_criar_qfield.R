# Monitora — criação independente de projetos QField
# Versão 0.4.2 — revisão após campo e cartografia editável, 01/10/2026.
# Leitores adaptados da versão pública v3.0.6; SHA256 da fonte:
# 454cb8f1d6f74ec8df2695236add1206080f869a3209c8a09aa92af9b6186d45
# Arquivo autossuficiente: não carrega o script biológico do Monitora.


# CONFIGURAÇÃO — edite este bloco antes de executar no RStudio.
# Para fornecer número, use list(n=40, percentual=NULL).
MQ_CONFIG <- list(
  entrada = 'entrada', saida = 'saida', projeto = 'Monitora', pasta_base = NULL,
  gerar_cartografia = TRUE, qgis_python = NULL, mapas_dpi = 300L, mapas_papel = 'A4',
  elaboracao = 'Programa Monitora', # edite para registrar a autoria efetiva
  modo = 'auto', # auto: montar se houver PAs/UAs/grade fornecidos; senão planejar.
  referencia_anterior = NULL, # pasta 02_relatorio de uma execução; obrigatória em expandir
  transecto_m = 50, distancia_min_m = 100, deslocamento_max_m = 10,
  grade_m = c(156.25, 156.25), epsg = NULL, semente = 20261001L,
  prioritarios = list(n=NULL, percentual=20),
  alternativos = list(n=NULL, percentual=NULL), # ambos NULL: 2 x prioritários
  # Cotas são cumulativas: todas devem ser satisfeitas pelo MESMO conjunto de PAs.
  estratificar_vegetacao = TRUE, incluir_formacao_florestal = FALSE,
  perfil = 'campestre_savanico', # ilha: sem procedimentos de campo campestres
  parametros_protocolo = NULL, # ilha: list(transecto_m=..., grade_m=c(...,...)); personalizado: acrescentar direcoes e distancias_viarias_m
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


MQ_SCRIPT_ARQUIVO <- local({
  fs <- lapply(sys.frames(), function(f) f$ofile)
  fs <- Filter(function(x) !is.null(x) && length(x)==1, fs)
  arg <- commandArgs(FALSE); arg <- sub('^--file=', '', arg[grepl('^--file=',arg)])
  z <- if(length(fs)) tail(fs,1)[[1]] else if(length(arg)) arg[1] else NA_character_
  if(!is.na(z) && file.exists(z)) normalizePath(z,winslash='/',mustWork=TRUE) else NA_character_
})

monitora_qfield_utf8 <- function(x, contexto = "QField") {
  x <- as.character(x)
  presentes <- which(!is.na(x))
  if (length(presentes)) {
    invalidos <- presentes[!validUTF8(x[presentes])]
    if (length(invalidos)) {
    stop(
      contexto, ": texto com bytes inválidos em UTF-8 nas posições ",
      paste(utils::head(invalidos, 10L), collapse = ", "),
      if (length(invalidos) > 10L) " ..." else "",
      ".",
      call. = FALSE
    )
    }
    Encoding(x[presentes]) <- "UTF-8"
  }
  x
}

monitora_qfield_xml <- function(x) {
  x <- monitora_qfield_utf8(x, "QField XML"); x[is.na(x)] <- ""
  for (par in list(c("&", "&amp;"), c("<", "&lt;"), c(">", "&gt;"), c('"', "&quot;"), c("'", "&apos;"))) x <- gsub(par[1], par[2], x, fixed = TRUE)
  monitora_qfield_utf8(x, "QField XML escapado")
}

monitora_qfield_slug <- function(x) {
  x <- monitora_qfield_utf8(x, "QField identificador")
  y <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  y <- tolower(gsub("[^A-Za-z0-9]+", "_", y))
  y <- gsub("^_+|_+$", "", y)
  if (!length(y) || anyNA(y) || any(!nzchar(y))) stop("QField: identificador vazio.", call. = FALSE)
  substr(y, 1, 60)
}

monitora_qfield_caminho_local <- function(path, raiz, deve_existir = TRUE) {
  if (length(path) != 1L || is.na(path) || !nzchar(path) || grepl("(^[/\\\\]|^[A-Za-z]:|:|[\\\\]|(^|/)\\.\\.(/|$))", path)) stop("QField: caminho relativo inseguro.", call. = FALSE)
  raiz <- normalizePath(raiz, winslash = "/", mustWork = TRUE)
  alvo <- normalizePath(file.path(raiz, path), winslash = "/", mustWork = deve_existir)
  if (!startsWith(tolower(alvo), paste0(tolower(raiz), "/"))) stop("QField: caminho fora da pasta de entrada.", call. = FALSE)
  componentes <- strsplit(path, "/", fixed = TRUE)[[1]]
  atual <- raiz
  for (parte in componentes) {
    atual <- file.path(atual, parte)
    link <- Sys.readlink(atual)
    if (!is.na(link) && nzchar(link)) stop("QField: links de sistema de arquivos não são aceitos.", call. = FALSE)
  }
  alvo
}

monitora_qfield_geometria <- function(x, fonte) {
  if (!inherits(x, "sf") || !nrow(x) || is.na(sf::st_crs(x))) stop("QField: camada vazia ou sem CRS: ", fonte, call. = FALSE)
  valida_origem <- if (!sf::st_is_longlat(x)) sf::st_is_valid(x) else rep(TRUE, nrow(x))
  if (any(sf::st_is_empty(x)) || anyNA(valida_origem) || any(!valida_origem)) stop("QField: geometria vazia/inválida na origem: ", fonte, call. = FALSE)
  y <- sf::st_transform(x, 4326)
  bb <- sf::st_bbox(y)
  if (any(!is.finite(bb)) || bb[[1]] < -180 || bb[[3]] > 180 || bb[[2]] < -85 || bb[[4]] > 85) stop("QField: extensão inválida para mapa Web Mercator: ", fonte, call. = FALSE)
  tipos <- unique(as.character(sf::st_geometry_type(y)))
  if (any(!tipos %in% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON"))) stop("QField: tipo geométrico não suportado: ", fonte, call. = FALSE)
  if (any(!is.finite(sf::st_coordinates(y)))) stop("QField: coordenada geométrica não finita.", call. = FALSE)
  valida_mapa <- sf::st_is_valid(sf::st_transform(y, 3857))
  if (anyNA(valida_mapa) || any(!valida_mapa)) stop("QField: geometria inválida para exibição cartográfica GEOS: ", fonte, call. = FALSE)
  attr(y, "qfield_validade_s2") <- suppressMessages(sf::st_is_valid(y, reason = TRUE))
  y
}

monitora_qfield_descompactar <- function(arquivo, destino) {
  z <- zip::zip_list(arquivo)
  if (!"type" %in% names(z)) stop("QField: atualizar pacote zip para versão que informa tipos de entradas.", call. = FALSE)
  if (any(!z$type %in% c("file", "directory"))) stop("QField: ZIP contém link ou tipo especial proibido.", call. = FALSE)
  n <- gsub("\\\\", "/", z$filename)
  if (!nrow(z) || nrow(z) > 2000L || anyNA(z$uncompressed_size) || sum(z$uncompressed_size) > 512 * 1024^2 || any(z$uncompressed_size > 256 * 1024^2)) stop("QField: limite de descompactação excedido.", call. = FALSE)
  if (any(grepl("(^/|:|(^|/)\\.\\.(/|$)|[\\\\])", z$filename)) || anyDuplicated(tolower(n)) || any(lengths(strsplit(n, "/", fixed = TRUE)) > 6L)) stop("QField: nomes inseguros/duplicados no ZIP.", call. = FALSE)
  nomes <- n[!endsWith(n, "/")]
  ext <- tolower(tools::file_ext(nomes))
  permitidos <- if (tolower(tools::file_ext(arquivo)) == "kmz") c("kml", "png", "jpg", "jpeg") else c("shp", "shx", "dbf", "prj", "cpg", "qix", "sbn", "sbx")
  if (any(!ext %in% permitidos)) stop("QField: conteúdo não permitido no arquivo compactado.", call. = FALSE)
  zip::unzip(arquivo, exdir = destino)
  arquivos <- vapply(nomes, monitora_qfield_caminho_local, character(1), raiz = destino)
  if (tolower(tools::file_ext(arquivo)) == "kmz") return(arquivos[ext == "kml"])
  shp <- arquivos[ext == "shp"]
  if (!length(shp)) stop("QField: ZIP sem shapefile.", call. = FALSE)
  for (f in shp) if (!all(tolower(paste0(tools::file_path_sans_ext(f), c(".shx", ".dbf", ".prj"))) %in% tolower(arquivos))) stop("QField: shapefile incompleto, inclusive PRJ obrigatório.", call. = FALSE)
  shp
}

monitora_qfield_tracks_kml <- function(doc, arquivo, scratch) {
  ns <- c(k="http://www.opengis.net/kml/2.2", g="http://www.google.com/kml/ext/2.2")
  marks <- xml2::xml_find_all(doc, ".//k:Placemark", ns)
  if (length(xml2::xml_find_all(doc,".//*[local-name()='Placemark']"))!=length(marks)) stop("QField: namespace KML não suportado; nenhuma feição será omitida.",call.=FALSE)
  extras <- list(); linhas <- list(); geoms <- list(); remover <- list(); camadas <- character()
  sha <- digest::digest(file=arquivo, algo="sha256")
  for (i in seq_along(marks)) {
    pm <- marks[[i]]; tracks <- xml2::xml_find_all(pm, ".//g:Track", ns)
    if (!length(tracks)) next
    if (length(xml2::xml_find_all(pm, ".//k:Point|.//k:LineString|.//k:Polygon", ns))) stop("QField: Placemark mistura Track e geometria convencional; separar na fonte.", call.=FALSE)
    partes <- lapply(tracks, function(tr) {
    cc <- xml2::xml_text(xml2::xml_find_all(tr, "./g:coord", ns))
    tt <- xml2::xml_text(xml2::xml_find_all(tr, "./k:when", ns))
    aa <- xml2::xml_find_all(tr, "./g:angles", ns)
    if (length(cc)<2L || length(tt)!=length(cc) || any(!nzchar(trimws(cc))) || any(!nzchar(trimws(tt))) || !length(aa) %in% c(0L,length(cc))) stop("QField: Track com lacunas ou contagens incompatíveis; conversão sem interpolação não suportada.", call.=FALSE)
    v <- strsplit(trimws(cc), "[[:space:]]+")
    if (any(lengths(v)!=3L)) stop("QField: Track exige coordenadas longitude latitude altitude explícitas.", call.=FALSE)
    m <- matrix(suppressWarnings(as.numeric(unlist(v))), ncol=3L, byrow=TRUE)
    if (any(!is.finite(m)) || any(abs(m[,1])>180) || any(abs(m[,2])>85)) stop("QField: coordenadas Track fora do domínio de navegação.", call.=FALSE)
    m
    })
    pasta <- xml2::xml_find_first(pm,"ancestor::*[self::k:Folder or self::k:Document][1]/k:name",ns)
    camada <- xml2::xml_text(pasta)
    if (is.na(camada) || !nzchar(trimws(camada))) camada <- "doc"
    camadas <- c(camadas,camada)
    nome <- xml2::xml_text(xml2::xml_find_first(pm, "./k:name", ns)); if (is.na(nome)) nome <- ""
    descricao <- xml2::xml_text(xml2::xml_find_first(pm, "./k:description", ns)); if (is.na(descricao)) descricao <- ""
    linhas[[length(linhas)+1L]] <- data.frame(Name=nome, Description=descricao,
    kml_fonte_sha256=sha, kml_placemark=i, kml_camada=camada, kml_pasta_xml=xml2::xml_path(xml2::xml_parent(pasta)), kml_track_partes=length(partes), kml_vertices=sum(vapply(partes,nrow,integer(1))),
    kml_placemark_xml=as.character(pm), stringsAsFactors=FALSE)
    geoms[[length(geoms)+1L]] <- sf::st_multilinestring(partes, dim="XYZ")
    remover[[length(remover)+1L]] <- pm
  }
  if (length(linhas)) {
    x <- sf::st_sf(do.call(rbind,linhas), geometry=sf::st_sfc(geoms,crs=4326))
    for (camada in unique(camadas)) {
    z <- x[camadas==camada,]
    if (length(unique(z$kml_pasta_xml))>1L) stop("QField: pastas KML homônimas com Tracks; renomear explicitamente na fonte.",call.=FALSE)
    attr(z,"qfield_kml_original") <- arquivo
    extras[[camada]] <- z
    }
    for (pm in remover) xml2::xml_remove(pm)
  }
  restantes <- xml2::xml_find_all(doc, ".//k:Placemark", ns)
  suportados <- vapply(restantes,function(pm)length(xml2::xml_find_all(pm,".//k:Point|.//k:LineString|.//k:Polygon",ns))>0L,logical(1))
  if (any(!suportados)) stop("QField: KML contém Placemark sem geometria vetorial suportada; nenhuma feição será omitida silenciosamente.",call.=FALSE)
  leitura <- arquivo
  if (length(linhas) && length(restantes)) {
    leitura <- tempfile("kml_sem_tracks_",tmpdir=scratch,fileext=".kml")
    xml2::write_xml(doc,leitura)
  }
  list(arquivo=leitura, extras=extras, restantes=length(restantes))
}

monitora_qfield_ler_adicionais <- function(entrada, scratch) {
  if (!dir.exists(entrada)) return(list())
  fontes <- setdiff(mq_files(entrada, "\\.(kml|kmz|gpkg|zip|shp)$", relative=TRUE), "recorte_imagens_500m.gpkg")
  if (length(fontes) > 100L) stop("QField: mais de 100 fontes adicionais.", call. = FALSE)
  saida <- list()
  for (nome in fontes) {
    f <- monitora_qfield_caminho_local(nome, entrada)
    if (file.info(f)$size > 512 * 1024^2) stop("QField: fonte vetorial maior que 512 MiB.", call. = FALSE)
    ext <- tolower(tools::file_ext(f)); arquivos <- f
    if (ext == "shp") {
    nomes <- list.files(dirname(f),full.names=FALSE)
    req <- paste0(tolower(tools::file_path_sans_ext(basename(nome))),c(".shp",".shx",".dbf",".prj"))
    if (any(vapply(req,function(z)sum(tolower(nomes)==z),integer(1))!=1L)) stop("QField: shapefile direto incompleto/ambíguo; SHP, SHX, DBF e PRJ obrigatórios.",call.=FALSE)
    for (z in nomes[tolower(nomes) %in% req]) monitora_qfield_caminho_local(z,dirname(f))
    }
    if (ext %in% c("kmz", "zip")) {
    dst <- tempfile("vetor_", tmpdir = scratch); dir.create(dst)
    arquivos <- monitora_qfield_descompactar(f, dst)
    }
    for (arq in arquivos) {
    esperado_kml <- NULL; importadas_kml <- 0L
    ext_arq <- tolower(tools::file_ext(arq))
    if (ext_arq == "gpkg") {
      cabecalho <- readBin(arq, what = "raw", n = 100L)
      if (length(cabecalho) != 100L || !identical(cabecalho[1:16], c(charToRaw("SQLite format 3"), as.raw(0))) || !rawToChar(cabecalho[69:72]) %in% c("GPKG", "GP10", "GP11")) stop("QField: assinatura GeoPackage inválida; nenhum driver foi aberto.", call. = FALSE)
    }
    if (ext_arq == "shp" && !identical(readBin(arq, what = "integer", n = 1L, size = 4L, endian = "big"), 9994L)) stop("QField: assinatura shapefile inválida.", call. = FALSE)
    if (tolower(tools::file_ext(arq)) == "kml") {
      if (file.info(arq)$size > 32 * 1024^2) stop("QField: KML excede limite XML.", call. = FALSE)
      txt <- paste(readLines(arq, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      if (grepl("<!DOCTYPE|<!ENTITY|<([A-Za-z0-9_]+:)?NetworkLink", txt, ignore.case = TRUE)) stop("QField: KML contém referências externas/entidades proibidas.", call. = FALSE)
      doc <- xml2::read_xml(txt, options = "NONET")
      if (xml2::xml_name(doc) != "kml" || length(xml2::xml_find_all(doc, '//*[local-name()="NetworkLink"]'))) stop("QField: documento não é KML local permitido.", call. = FALSE)
    }
    if (ext_arq == "kml") {
      normalizado <- monitora_qfield_tracks_kml(doc,arq,scratch)
      for (cn in names(normalizado$extras)) {
        x <- monitora_qfield_geometria(normalizado$extras[[cn]],paste(nome,cn))
        attr(x,"qfield_kml_original") <- attr(normalizado$extras[[cn]],"qfield_kml_original")
        attr(x,"qfield_fonte") <- paste0(nome," | ",cn,"; gx:Track preservado em XYZ, sem interpolação")
        attr(x,"qfield_arquivo") <- nome; attr(x,"qfield_camada") <- cn
        saida[[paste0("adicional_",length(saida)+1L,"_",cn)]] <- x
      }
      esperado_kml <- normalizado$restantes
      if (!esperado_kml) next
      arq <- normalizado$arquivo
    }
    driver <- switch(tolower(tools::file_ext(arq)), gpkg = "GPKG", kml = "LIBKML", shp = "ESRI Shapefile")
    if (identical(driver, "LIBKML") && !"LIBKML" %in% sf::st_drivers()$name) driver <- "KML"
    ls <- sf::st_layers(arq, do_count = FALSE)
    if (length(ls$name) > 100L) stop("QField: excesso de camadas por fonte.", call. = FALSE)
    arquivo_apoio <- basename(nome) %in% c("apoio_campo.gpkg", "pontos_interesse.gpkg", "trajeto.gpkg")
    if (arquivo_apoio && !setequal(ls$name, if(basename(nome)=="apoio_campo.gpkg")c("pontos_interesse", "trajeto")else tools::file_path_sans_ext(basename(nome)))) stop("QField: apoio_campo.gpkg deve conter somente pontos_interesse e trajeto.", call. = FALSE)
    for (camada in ls$name) {
      x <- if (arquivo_apoio) sf::st_read(arq, layer = camada, quiet = TRUE, drivers = driver, stringsAsFactors = FALSE, fid_column_name = "fid") else sf::st_read(arq, layer = camada, quiet = TRUE, drivers = driver, stringsAsFactors = FALSE)
      if (ext_arq=="kml" && !arquivo_apoio && !nrow(x)) next
      if (!is.null(esperado_kml)) importadas_kml <- importadas_kml + nrow(x)
      if (arquivo_apoio) {
        esperado <- if (camada == "pontos_interesse") "POINT" else "MULTILINESTRING"
        campo <- if (camada == "pontos_interesse") "ponto_interesse" else "trajeto"
        campos <- names(sf::st_drop_geometry(x))
        tipo_declarado <- toupper(gsub("[ _]", "", ls$geomtype[[match(camada, ls$name)]]))
        if (!all(c("fid", campo, "obs", "data_hora") %in% campos) || !is.character(x[[campo]]) || !is.character(x$obs) || !inherits(x$data_hora, "POSIXt") || is.na(sf::st_crs(x)) || !identical(tipo_declarado, esperado) || (nrow(x) && !identical(class(sf::st_geometry(x))[1], paste0("sfc_", esperado)))) stop("QField: estrutura de apoio_campo incompatível; nenhum campo será descartado.", call. = FALSE)
        if (anyDuplicated(x$fid) || anyNA(x$fid)) stop("QField: identificador de apoio inválido.", call. = FALSE)
        if (nrow(x)) x <- monitora_qfield_geometria(x, paste(nome, camada)) else sf::st_geometry(x) <- sf::st_geometry(monitora_qfield_apoio_vazio()[[camada]])
        attr(x, "qfield_tipo") <- esperado
      } else x <- monitora_qfield_geometria(x, paste(nome, camada))
      if (nrow(x) > 500000L) stop("QField: camada excede 500 mil feições.", call. = FALSE)
      listcols <- names(x)[vapply(x, is.list, logical(1)) & !vapply(x, inherits, logical(1), what = "sfc")]
      if (length(listcols)) stop("QField: atributos complexos não suportados: ", paste(listcols, collapse = ", "), call. = FALSE)
      id <- paste0("adicional_", length(saida) + 1L, "_", monitora_qfield_slug(camada))
      attr(x, "qfield_fonte") <- paste0(nome, " | ", camada)
      attr(x, "qfield_arquivo") <- nome; attr(x, "qfield_camada") <- camada
      saida[[id]] <- x
    }
    if (!is.null(esperado_kml) && importadas_kml != esperado_kml) stop("QField: leitura KML perdeu ou duplicou Placemarks; geração bloqueada.",call.=FALSE)
    }
  }
  saida
}

monitora_qfield_apoio_vazio <- function() {
  p <- sf::st_sf(ponto_interesse = character(), obs = character(), data_hora = as.POSIXct(character(), tz = "UTC"), geometry = sf::st_sfc(sf::st_point(), crs = 4326)[FALSE])
  t <- sf::st_sf(trajeto = character(), obs = character(), data_hora = as.POSIXct(character(), tz = "UTC"), geometry = sf::st_sfc(sf::st_multilinestring(list(matrix(numeric(), ncol = 2))), crs = 4326)[FALSE])
  attr(p, "qfield_tipo") <- "POINT"; attr(t, "qfield_tipo") <- "MULTILINESTRING"
  list(pontos_interesse = p, trajeto = t)
}

mq_transform <- function(x,cr) if(nrow(x))sf::st_transform(x,cr)else sf::st_set_crs(sf::st_set_crs(x,NA),cr)

mq_stop <- function(...) stop(..., call.=FALSE)
mq_hash <- function(f) digest::digest(file=f, algo='sha256')
mq_json <- function(x, f) jsonlite::write_json(x, f, pretty=TRUE, auto_unbox=TRUE, null='null', na='null', digits=16)
mq_csv <- function(x, f) data.table::fwrite(sf::st_drop_geometry(x), f, sep=';', bom=TRUE, na='', quote='auto')
mq_empty <- function(crs=4326) sf::st_sf(geometry=sf::st_sfc(crs=crs))
mq_deps <- function() {
  p <- c('sf','terra','xml2','zip','jsonlite','digest','httr','data.table','DBI','RSQLite','cli','curl','png','jpeg')
  miss <- p[!vapply(p, requireNamespace, logical(1), quietly=TRUE)]
  if(length(miss)) mq_stop('Instale os pacotes: ',paste(miss,collapse=', '))
}
mq_validate_config <- function(c) {
  mq_design_validate(c)
  for(n in c('transecto_m',if(c$perfil=='campestre_savanico')c('distancia_min_m','deslocamento_max_m'),'max_pontos','timeout_s'))
    if(length(c[[n]])!=1L || !is.numeric(c[[n]]) || !is.finite(c[[n]]) || c[[n]]<0 || (n!='deslocamento_max_m' && c[[n]]==0)) mq_stop('Parâmetro inválido: ',n)
  if(length(c$grade_m)!=2 || any(!is.finite(c$grade_m)) || any(c$grade_m<=0)) mq_stop('grade_m exige largura e altura positivas.')
  if(!c$modo %in% c('auto','planejar','montar','expandir')) mq_stop('Modo inválido.')
  if(length(c$semente)!=1 || !is.finite(c$semente) || c$semente!=as.integer(c$semente)) mq_stop('Semente inválida.')
  if(!c$mb_produto %in% c('30m','10m')) mq_stop('MapBiomas: produto deve ser 30m ou 10m.')
  if(isTRUE(c$mapbiomas) && !((c$mb_produto=='30m' && c$mb_colecao==11 && c$mb_ano %in% 1985:2025) || (c$mb_produto=='10m' && c$mb_colecao==4 && c$mb_ano %in% 2017:2025))) mq_stop('Produto/coleção/ano MapBiomas não homologado; use 30m/11 ou 10m/4 até 2025.')
  if(!c$centros_detalhe%in%c('auto','PAs','UAs','UAs_e_PAs'))mq_stop('centros_detalhe inválido.')
  for(n in c('baixar_imagem_detalhe','renovar_imagens','gerar_cartografia'))if(length(c[[n]])!=1||!is.logical(c[[n]])||is.na(c[[n]]))mq_stop('Opção lógica inválida: ',n)
  if(!is.null(c$confirmar_download)&&(length(c$confirmar_download)!=1||!is.logical(c$confirmar_download)||is.na(c$confirmar_download)))mq_stop('confirmar_download deve ser NULL, TRUE ou FALSE.')
  if(!identical(as.numeric(c$raio_detalhe_m),500))mq_stop('Versão atual homologa raio_detalhe_m=500.')
  if(length(c$margem_contexto_m)!=1||!is.numeric(c$margem_contexto_m)||!is.finite(c$margem_contexto_m)||c$margem_contexto_m<0)mq_stop('Margem de contexto inválida.')
  for(n in c('confirmar_download','confirmar_sentinel'))if(!is.null(c[[n]])&&(length(c[[n]])!=1||!is.logical(c[[n]])||is.na(c[[n]])))mq_stop('Confirmação inválida: ',n)
  if(length(c$cache_dir)!=1||!nzchar(c$cache_dir))mq_stop('Informe cache_dir persistente.')
  if(!c$mapas_papel%in%c('A4','A3')||length(c$mapas_dpi)!=1||!is.finite(c$mapas_dpi)||c$mapas_dpi<96||c$mapas_dpi>600)mq_stop('Cartografia: papel A4/A3 e dpi entre 96 e 600.')
  if(length(c$localizador_bioma_min_mm2)!=1||!is.finite(c$localizador_bioma_min_mm2)||c$localizador_bioma_min_mm2<0)mq_stop('Área mínima de legenda inválida.')
  invisible(c)
}
mq_crs <- function(ae, epsg=NULL) {
  if(is.null(epsg)) {
    b <- sf::st_bbox(sf::st_transform(ae,4326))
    if(b[['xmax']]-b[['xmin']]>6 || (b[['ymin']]<0 && b[['ymax']]>0)) mq_stop('Área extensa/multifuso ou cruzando Equador: informe EPSG métrico apropriado.')
    z <- min(60, floor((mean(b[c('xmin','xmax')])+180)/6)+1)
    epsg <- if(mean(b[c('ymin','ymax')])<0) {if(!z%in%17:25)mq_stop('Fuso fora do domínio SIRGAS suportado; informe EPSG.');31960+z} else {if(!z%in%11:23)mq_stop('Fuso fora do domínio SIRGAS suportado; informe EPSG.');if(z==23)6210 else 31954+z}
  }
  r <- sf::st_crs(epsg)
  if(is.na(r) || isTRUE(r$IsGeographic) || !r$units_gdal %in% c('metre','meter','metres','meters','m')) mq_stop('O CRS de processamento precisa ter unidades em metros.')
  r
}

mq_read <- function(entrada,scratch) {
  raw <- monitora_qfield_ler_adicionais(entrada,scratch)
  mf <- file.path(entrada,'camadas_qfield.csv')
  m <- if(file.exists(mf)) as.data.frame(data.table::fread(mf,colClasses='character',encoding='UTF-8')) else data.frame()
  if(nrow(m) && (!all(c('arquivo','camada','papel') %in% names(m)) || anyDuplicated(paste(m$arquivo,m$camada)))) mq_stop('Manifesto inválido: arquivo, camada e papel obrigatórios e únicos.')
  roles <- c('areas_elegiveis','limites_uc','grade_amostral','PA_priorit','PA_altern','verg_ini','verg_fin','UAs','transectos','estradas','rodovias','acessos','trilhas','formacao_florestal','estradas_pavimentadas','estradas_terra','trilhas_preexistentes','pontos_interesse','trajeto','adicional')
  out <- list(); used <- integer()
  for(x in raw) {
    ar <- attr(x,'qfield_arquivo'); la <- attr(x,'qfield_camada')
    ii <- if(nrow(m)) which(m$arquivo==ar & m$camada==la) else integer()
    hits <- roles[tolower(roles) %in% tolower(c(la,tools::file_path_sans_ext(basename(ar))))]
    role <- if(length(ii)) m$papel[ii] else if(length(hits)==1) hits else 'adicional'
    if(length(hits)>1 && !length(ii)) mq_stop('Papel ambíguo; declare camadas_qfield.csv: ',ar,' / ',la)
    if(!role %in% roles) mq_stop('Papel não reconhecido: ',role)
    used <- c(used,ii)
    tipos <- as.character(sf::st_geometry_type(x))
    if(role %in% c('areas_elegiveis','limites_uc','formacao_florestal') && any(!tipos %in% c('POLYGON','MULTIPOLYGON'))) mq_stop('Polígonos exigidos para ',role)
    if(role %in% c('grade_amostral','PA_priorit','PA_altern','verg_ini','verg_fin','UAs') && any(tipos!='POINT')) mq_stop('Pontos simples exigidos para ',role)
    if(role %in% c('estradas','rodovias','acessos','trilhas','transectos','trajeto') && any(!tipos %in% c('LINESTRING','MULTILINESTRING'))) mq_stop('Linhas exigidas para ',role)
    if(role%in%c('estradas_pavimentadas','estradas_terra','trilhas_preexistentes')&&any(!tipos%in%c('LINESTRING','MULTILINESTRING','POLYGON','MULTIPOLYGON')))mq_stop('Via exige linhas ou polígonos: ',role)
    label <- if(length(ii) && 'campo_rotulo' %in% names(m) && !is.na(m$campo_rotulo[ii]) && nzchar(m$campo_rotulo[ii])) m$campo_rotulo[ii] else intersect(c('PA','UA','Name','nome','ponto_interesse','trajeto','id'),names(x))[1]
    if(is.na(label)) label <- ''
    if(nzchar(label) && !label %in% names(x)) mq_stop('Campo de rótulo inexistente: ',label)
    if(role %in% c('PA_priorit','PA_altern')) {
      if(!nzchar(label) || anyNA(x[[label]]) || any(!nzchar(as.character(x[[label]]))) || anyDuplicated(x[[label]])) mq_stop('PAs fornecidos exigem rótulos únicos, não vazios.')
    }
    ano <- if(length(ii) && 'ano' %in% names(m)) m$ano[ii] else NA_character_
    if(!is.na(ano) && nzchar(ano) && !grepl('^[0-9]{4}$',ano)) mq_stop('Ano inválido no manifesto.')
    nm <- if(role=='areas_elegiveis') 'AE' else if(role=='limites_uc') 'UC_forn' else if(role=='adicional') substr(monitora_qfield_slug(la),1,22) else role
    if(role%in%c('estradas_pavimentadas','estradas_terra','trilhas_preexistentes'))nm<-switch(role,estradas_pavimentadas='vias_pav',estradas_terra='vias_terra',trilhas_preexistentes='trilhas')
    if(role %in% c('verg_ini','verg_fin','transectos') && !is.na(ano) && nzchar(ano)) nm <- paste0(role,'_',ano)
    if(length(ii) && 'nome' %in% names(m) && !is.na(m$nome[ii]) && nzchar(m$nome[ii]) && !role %in% c('PA_priorit','PA_altern','grade_amostral')) nm <- m$nome[ii]
    if(nchar(nm)>26) mq_stop('Nome exibido longo (>26 caracteres): ',nm)
    if(nm %in% vapply(out,`[[`,character(1),'nome')) mq_stop('Nome de camada repetido; consolide fontes ou indique nomes distintos: ',nm)
    out[[length(out)+1L]] <- list(x=x,papel=role,nome=nm,label=label,ano=ano,fonte=paste(ar,la,sep=' | '))
  }
  if(nrow(m) && !setequal(used,seq_len(nrow(m)))) mq_stop('Fonte/camada do manifesto não localizada.')
  ae <- Filter(function(l) l$papel=='areas_elegiveis',out)
  if(!length(ae)) mq_stop('Forneça areas_elegiveis.<formato> ou declare o papel areas_elegiveis no manifesto.')
  union <- sf::st_union(do.call(base::c,lapply(ae,function(l)sf::st_geometry(l$x))))
  list(camadas=out,ae=sf::st_sf(geometry=union))
}

# Consulta obrigatória, com paginação e conferência do total. Nunca interpreta erro como ausência de UC.
mq_uc <- function(ae,c) {
  cap <- httr::GET(c$uc_url,query=list(service='WFS',version='2.0.0',request='GetCapabilities'),httr::timeout(c$timeout_s))
  httr::stop_for_status(cap)
  doc <- xml2::read_xml(httr::content(cap,as='raw'),options='NONET')
  f <- xml2::xml_find_all(doc,"//*[local-name()='FeatureType']")
  ids <- vapply(f,function(n)xml2::xml_text(xml2::xml_find_first(n,"./*[local-name()='Name']")),character(1))
  if(!c$uc_camada %in% ids) mq_stop('Camada federal ausente no serviço oficial.')
  titulo <- xml2::xml_text(xml2::xml_find_first(f[[match(c$uc_camada,ids)]],"./*[local-name()='Title']"))
  b <- sf::st_bbox(sf::st_transform(ae,4326))
  filtro <- sprintf("BBOX(the_geom,%.10f,%.10f,%.10f,%.10f,'EPSG:4326')",b[1],b[2],b[3],b[4])
  feats <- list(); offset <- 0L; total <- NA_integer_; page_ids <- character()
  repeat {
    res <- httr::GET(c$uc_url,query=list(service='WFS',version='2.0.0',request='GetFeature',typeNames=c$uc_camada,outputFormat='application/json',srsName='EPSG:4326',count=100,startIndex=offset,CQL_FILTER=filtro),httr::timeout(c$timeout_s))
    httr::stop_for_status(res); d <- jsonlite::fromJSON(httr::content(res,as='text',encoding='UTF-8'),simplifyVector=FALSE)
    n <- suppressWarnings(as.integer(d$numberMatched)); nr <- length(d$features)
    if(length(n)!=1 || is.na(n) || n<0 || n>10000 || (!is.na(total) && n!=total) || !identical(d$type,'FeatureCollection')) mq_stop('Resposta federal incompleta/inconsistente.')
    total <- n
    if(nr==0 && offset<total) mq_stop('Paginação federal interrompida.')
    if(nr) {
      fi <- vapply(d$features,function(z)as.character(z$id),character(1))
      if(anyDuplicated(c(page_ids,fi))) mq_stop('Paginação federal repetiu feições.')
      page_ids <- c(page_ids,fi); feats <- c(feats,d$features)
    }
    offset <- offset+nr
    if(offset>=total) break
  }
  if(length(feats)!=total) mq_stop('Total federal divergente.')
  u <- if(total) sf::st_read(jsonlite::toJSON(list(type='FeatureCollection',features=feats),auto_unbox=TRUE,digits=16),quiet=TRUE) else mq_empty(4326)
  if(nrow(u)) {
    u <- monitora_qfield_geometria(u,'limites oficiais federais')
    req <- c('cnuc','nomeuc','esferaadm');if(!all(req %in% names(u)) || anyNA(u$cnuc) || any(!nzchar(u$cnuc)) || any(tolower(u$esferaadm)!='federal')) mq_stop('Identificação federal incompleta.')
    # Consulta espacial no CRS métrico local evita degenerações S2 de vértices repetidos
    # em limites oficiais válidos em GEOS, sem alterar as coordenadas de origem.
    local_crs<-mq_crs(ae,c$epsg);up<-sf::st_transform(u,local_crs);ap<-sf::st_transform(ae,local_crs)
    if(any(!sf::st_is_valid(up)))mq_stop('Limite oficial inválido no CRS de análise.')
    u <- u[lengths(sf::st_intersects(up,ap))>0,]
  }
  list(x=u,fonte=c$uc_url,camada=c$uc_camada,titulo=titulo,consulta=format(Sys.time(),tz='UTC',usetz=TRUE),total_bbox=total,status='consulta_completa')
}

mq_contexts <- function(ae,uc,crs,grade,previous=NULL) {
  if(!is.null(previous)) {
    if(!identical(as.numeric(previous$grade_m),as.numeric(grade)) || sf::st_crs(previous$epsg)!=crs) mq_stop('Expansão exige mesma projeção e dimensão da malha.')
    out <- previous$contextos
    known <- vapply(out,function(z)as.character(z$uc),character(1))
    fresh <- if(nrow(uc)) uc[!uc$cnuc %in% known,] else uc
    if(nrow(fresh)) {
      fresh <- sf::st_transform(fresh,crs)
      additions <- mq_contexts(ae,fresh,crs,grade,NULL)
      additions <- Filter(function(z)z$chave!='EXTERNO',additions)
      if(is.null(previous$dominio_processado_wkt))mq_stop('Referência antiga sem domínio processado; expansão para nova UC exige migração explícita.')
      hist <- sf::st_as_sfc(previous$dominio_processado_wkt,crs=crs)
      for(i in seq_along(additions)) additions[[i]]$wkt <- sf::st_as_text(suppressWarnings(sf::st_difference(sf::st_as_sfc(additions[[i]]$wkt,crs=crs),hist)),digits=16)
      # Validar também UC nova contra domínios de UCs anteriores fora da área histórica.
      for(new in additions)for(old in Filter(function(z)z$chave!='EXTERNO',out)) {
        ov<-suppressWarnings(sf::st_intersection(sf::st_intersection(sf::st_as_sfc(new$wkt,crs=crs),sf::st_as_sfc(old$wkt,crs=crs)),sf::st_union(sf::st_transform(ae,crs))))
        if(length(ov) && sum(as.numeric(sf::st_area(ov)))>0.01)mq_stop('Expansão com UCs federais sobrepostas; associação territorial explícita necessária.')
      }
      out <- c(Filter(function(z)z$chave!='EXTERNO',out),additions,Filter(function(z)z$chave=='EXTERNO',out))
    }
    return(out)
  }
  a <- sf::st_transform(ae,crs); u <- sf::st_transform(uc,crs)
  if(nrow(u)>1) for(i in seq_len(nrow(u)-1)) for(j in (i+1):nrow(u)) {
    ov <- suppressWarnings(sf::st_intersection(sf::st_intersection(sf::st_geometry(u[i,]),sf::st_geometry(u[j,])),sf::st_geometry(a)))
    if(length(ov) && sum(as.numeric(sf::st_area(ov)))>0.01) mq_stop('AEs em UCs federais sobrepostas: associação territorial explícita necessária.')
  }
  out <- list()
  if(nrow(u)) for(i in order(u$cnuc)) {
    b <- sf::st_bbox(u[i,]); geom <- sf::st_as_text(sf::st_geometry(u[i,]),digits=16)
    out[[length(out)+1]] <- list(chave=paste0('UC_',u$cnuc[i]),origem=c(unname(b[1]),unname(b[2])),wkt=geom,uc=as.character(u$cnuc[i]))
  }
  # Origem externa congelada desde a primeira execução, inclusive se expansão futura sair da UC.
  b <- sf::st_bbox(a);out[[length(out)+1]] <- list(chave='EXTERNO',origem=c(unname(b[1]),unname(b[2])),wkt=NULL,uc='')
  out
}
mq_grid <- function(ae,contextos,crs,c,old=NULL) {
  contextos<-mq_id_contexts(contextos,ae,crs,c,old)
  a <- sf::st_union(sf::st_transform(ae,crs)); masks <- list(); rest <- a
  for(ctx in contextos) {
    if(ctx$chave=='EXTERNO') mask <- rest else {
      boundary <- sf::st_as_sfc(ctx$wkt,crs=crs)
      mask <- suppressWarnings(sf::st_intersection(a,boundary));rest <- suppressWarnings(sf::st_difference(rest,boundary))
    }
    masks[[ctx$chave]] <- mask
  }
  parts <- list(); usedxy <- character()
  for(ctx in contextos) {
    mask <- masks[[ctx$chave]]
    if(!length(mask) || all(sf::st_is_empty(mask))) next
    b<-sf::st_bbox(mask);o<-as.numeric(ctx$origem);d<-c$grade_m
    ir<-c(ceiling((b[1]-o[1])/d[1]-1e-9),floor((b[3]-o[1])/d[1]+1e-9));jr<-c(ceiling((b[2]-o[2])/d[2]-1e-9),floor((b[4]-o[2])/d[2]+1e-9))
    if(ir[1]>ir[2] || jr[1]>jr[2]) next
    if((diff(ir)+1)*(diff(jr)+1)>c$max_pontos*20) mq_stop('Extensão exige grade muito grande; dividir o lote.')
    for(j in seq(jr[1],jr[2])) {
      ii<-seq(ir[1],ir[2]);x<-o[1]+ii*d[1];y<-rep(o[2]+j*d[2],length(ii))
      z<-sf::st_as_sf(data.frame(malha=ctx$chave,coluna=ii,linha=j,x_m=x,y_m=y,uc_cnuc=ctx$uc),coords=c('x_m','y_m'),remove=FALSE,crs=crs)
      z<-z[lengths(sf::st_intersects(z,mask))>0,]
      if(!nrow(z)) next
      k<-paste(sprintf('%.6f',z$x_m),sprintf('%.6f',z$y_m),sep=':');z<-z[!k %in% usedxy,];usedxy<-c(usedxy,k)
      if(nrow(z)) parts[[length(parts)+1]]<-z
      if(length(usedxy)>c$max_pontos) mq_stop('Limite configurado de pontos excedido.')
    }
  }
  if(!length(parts)) mq_stop('Nenhum vértice da grade nas Áreas Elegíveis.')
  g<-do.call(rbind,parts);g$chave_grade<-paste(g$malha,g$coluna,g$linha,sep=':')
  g<-g[order(g$malha,g$linha,g$coluna),]
  mq_grid_identify(g,contextos,old)
}

mq_quantity <- function(q,N,default=NULL) {
  if(!is.null(q$n) && !is.null(q$percentual)) mq_stop('Informe n OU percentual, nunca ambos.')
  if(is.null(q$n) && is.null(q$percentual)) return(default)
  v<-if(!is.null(q$n))q$n else q$percentual
  if(length(v)!=1 || !is.numeric(v) || !is.finite(v) || v<0) mq_stop('Quantidade inválida.')
  if(!is.null(q$n)) {if(v!=floor(v))mq_stop('n precisa ser inteiro.');return(as.integer(v))}
  if(v>100)mq_stop('Percentual deve estar entre 0 e 100.')
  as.integer(ceiling(N*v/100))
}
mq_select <- function(g,c) {
  N<-nrow(g);np<-mq_quantity(c$prioritarios,N,ceiling(.2*N));na<-mq_quantity(c$alternativos,N,2L*np)
  viable<-mq_road_available(g)
  if(any(g$categoria!='grade'&!viable))mq_stop('PA histórico conflita com novas restrições viárias; revisão explícita necessária.')
  if(np+na>sum(viable))mq_stop('Quantidade solicitada excede candidatos viáveis após restrições viárias; nenhuma cota reduzida.')
  if(np+na>N)mq_stop(sprintf('Cotas inviáveis: %d prioritários + %d alternativos para %d pontos. Não houve redução automática.',np,na,N))
  oldp<-sum(g$categoria=='prioritario');olda<-sum(g$categoria=='alternativo')
  if(oldp>np || olda>na)mq_stop('Cotas inferiores à seleção anterior preservada; revisão explícita necessária.')
  # Ordenação por hash evita alterar o gerador aleatório global e independe da ordem das entradas.
  rank<-vapply(g$chave_grade,function(k)digest::digest(paste(c$semente,k,sep=':'),algo='sha256',serialize=FALSE),character(1))
  available<-which(g$categoria=='grade'&viable);available<-available[order(rank[available],g$id_grade[available])]
  if(np>oldp){take<-head(available,np-oldp);g$categoria[take]<-'prioritario';available<-setdiff(available,take)}
  if(na>olda)g$categoria[head(available,na-olda)]<-'alternativo'
  g
}

# Desenho cumulativo: classificação -> margens -> solução inteira -> sorteio nas células.
mq_design_active <- function(c) isTRUE(c$estratificar_vegetacao)||isTRUE(c$estratificar_por_atributos)
mq_design_contract <- function(c) {
  m<-c$formacao_mapa;if(!is.null(m))m<-m[order(m$classe,method='radix'),,drop=FALSE]
  structure<-list(vegetacao=c$estratificar_vegetacao,florestal=c$incluir_formacao_florestal,perfil=c$perfil,
    campo_formacao=c$formacao_campo,mapa=m,fitofisionomia=c$fitofisionomia_campo,
    atributos=if(c$estratificar_por_atributos)sort(names(c$cotas_atributos),method='radix')else character(),
    arquivo=c$estratos_arquivo,camada=c$estratos_camada,
    mapbiomas=if(c$estratificar_vegetacao&&is.null(c$formacao_campo))list(produto=c$mb_produto,colecao=c$mb_colecao,ano=c$mb_ano)else NULL,
    regra_classificacao='conservadora_v1',regra_cotas='margens_inteiras_conjuntas_v1')
  list(estrutura=structure,sha256=digest::digest(jsonlite::toJSON(structure,auto_unbox=TRUE,null='null',digits=16),algo='sha256',serialize=FALSE))
}
mq_design_validate <- function(c) {
  for(n in c('estratificar_vegetacao','incluir_formacao_florestal','estratificar_por_atributos'))
    if(length(c[[n]])!=1 || !is.logical(c[[n]]) || is.na(c[[n]]))mq_stop('Opção lógica inválida: ',n)
  if(length(c$perfil)!=1 || !c$perfil%in%c('campestre_savanico','ilha','personalizado'))mq_stop('Perfil inválido.')
  if(c$incluir_formacao_florestal && c$perfil=='campestre_savanico')mq_stop('Florestal exige perfil ilha ou personalizado, com parâmetros explícitos.')
  for(n in c('solver_timeout_s','max_estratos'))if(length(c[[n]])!=1||!is.numeric(c[[n]])||!is.finite(c[[n]])||c[[n]]<1||c[[n]]!=floor(c[[n]]))mq_stop('Limite inválido: ',n)
  for(n in c('formacao_campo','fitofisionomia_campo','estratos_arquivo','estratos_camada','cotas_atributos_arquivo'))if(!is.null(c[[n]])&&(length(c[[n]])!=1||!is.character(c[[n]])||is.na(c[[n]])||!nzchar(c[[n]])))mq_stop('Campo/caminho inválido: ',n)
  if(xor(is.null(c$estratos_arquivo),is.null(c$estratos_camada)))mq_stop('Informe estratos_arquivo e estratos_camada juntos.')
  if(!is.list(c$cotas_atributos)|| (length(c$cotas_atributos)&&(is.null(names(c$cotas_atributos))||any(!nzchar(names(c$cotas_atributos)))||anyDuplicated(names(c$cotas_atributos)))))mq_stop('cotas_atributos exige lista nomeada por campo, sem duplicações.')
  if(length(c$cotas_atributos)&&!is.null(c$cotas_atributos_arquivo))mq_stop('Use cotas no bloco OU arquivo CSV, nunca ambos.')
  if(!c$estratificar_por_atributos && (length(c$cotas_atributos)||!is.null(c$cotas_atributos_arquivo)))message('Cotas de atributos configuradas, mas desabilitadas; não serão aplicadas.')
  if(!c$estratificar_vegetacao && (!is.null(c$cotas_formacao)||!is.null(c$fitofisionomia_campo)))mq_stop('Cotas de vegetação/fitofisionomia exigem estratificar_vegetacao=TRUE.')
  invisible(c)
}
mq_design_load <- function(c,input,report) {
  if(isTRUE(c$estratificar_por_atributos)&&!is.null(c$cotas_atributos_arquivo)) {
    path<-monitora_qfield_caminho_local(c$cotas_atributos_arquivo,input)
    d<-as.data.frame(data.table::fread(path,colClasses='character',encoding='UTF-8'))
    if(!all(c('atributo','classe')%in%names(d))||!nrow(d)||anyNA(d$atributo)||any(!nzchar(d$atributo)))mq_stop('CSV de cotas exige atributo, classe e n OU percentual.')
    measures<-intersect(c('n','percentual'),names(d));if(length(measures)!=1)mq_stop('CSV de cotas exige exatamente uma coluna n OU percentual.')
    d[[measures]]<-suppressWarnings(as.numeric(d[[measures]]));c$cotas_atributos<-split(d[,setdiff(names(d),'atributo'),drop=FALSE],d$atributo)
    mq_json(list(arquivo=path,sha256=mq_hash(path)),file.path(report,'fonte_cotas.json'))
  }
  if(c$estratificar_por_atributos&&!length(c$cotas_atributos))mq_stop('Estratificação por atributos habilitada sem cotas.')
  c
}
mq_design_fields <- function(c) unique(c(if(c$estratificar_vegetacao)c(c$formacao_campo,c$fitofisionomia_campo),if(c$estratificar_por_atributos)names(c$cotas_atributos)))
mq_design_attributes <- function(g,layers,c,report) {
  fields<-mq_design_fields(c);if(!length(fields))return(g)
  src<-if(is.null(c$estratos_arquivo))Filter(function(l)l$papel=='areas_elegiveis',layers)else Filter(function(l)identical(l$fonte,paste(c$estratos_arquivo,c$estratos_camada,sep=' | ')),layers)
  if(!length(src))mq_stop('Camada de estratificação não localizada; confira arquivo e camada exatos.')
  hits<-vector('list',nrow(g));sources<-list()
  for(l in src) {
    x<-l$x
    if(any(!as.character(sf::st_geometry_type(x))%in%c('POLYGON','MULTIPOLYGON')))mq_stop('Estratificação exige polígonos.')
    absent<-setdiff(fields,names(x));if(length(absent))mq_stop('Campos ausentes: ',paste(absent,collapse=', '),'. Disponíveis: ',paste(names(sf::st_drop_geometry(x)),collapse=', '))
    vals<-as.data.frame(lapply(sf::st_drop_geometry(x)[,fields,drop=FALSE],as.character),stringsAsFactors=FALSE)
    if(any(vapply(x[,fields,drop=FALSE],is.list,logical(1))[fields]))mq_stop('Campos de estrato devem ser escalares.')
    ix<-sf::st_intersects(g,sf::st_transform(x,sf::st_crs(g)))
    for(i in which(lengths(ix)>0))hits[[i]]<-rbind(hits[[i]],vals[ix[[i]],,drop=FALSE])
    sources[[length(sources)+1]]<-list(fonte=l$fonte,campos=fields,sha256=digest::digest(list(sf::st_as_binary(sf::st_geometry(x)),vals),algo='sha256'))
  }
  bad<-character(nrow(g));out<-as.data.frame(setNames(rep(list(rep(NA_character_,nrow(g))),length(fields)),fields))
  for(i in seq_len(nrow(g))) {
    h<-unique(hits[[i]])
    if(is.null(h)||!nrow(h))bad[i]<-'sem_poligono' else if(nrow(h)>1)bad[i]<-'atributos_conflitantes' else if(anyNA(h)||any(!nzchar(trimws(unlist(h)))))bad[i]<-'atributo_ausente' else out[i,]<-h[1,]
  }
  mq_json(sources,file.path(report,'fontes_estratos.json'))
  inv<-do.call(rbind,lapply(fields,function(f){z<-as.data.frame(table(out[[f]],useNA='always'));data.frame(atributo=f,classe=as.character(z[[1]]),n_grade=z[[2]])}))
  mq_csv(inv,file.path(report,'inventario_atributos.csv'))
  if(any(nzchar(bad))) {
    idfield<-intersect(c('PA','UA','Name','nome','id'),names(g))[1]
    mq_csv(data.frame(linha_entrada=seq_len(nrow(g)),campo_identificador=if(is.na(idfield))'linha_entrada'else idfield,
      identificador=if(is.na(idfield))as.character(seq_len(nrow(g)))else as.character(g[[idfield]]),motivo=bad)[nzchar(bad),],file.path(report,'conflitos_atributos.csv'))
    mq_stop('Atribuição espacial ambígua/ausente em ',sum(nzchar(bad)),' pontos; consulte conflitos_atributos.csv. Nenhuma classe foi presumida.')
  }
  for(f in fields) {
    dest<-paste0('atr_',f)
    if(dest%in%names(g) && !identical(as.character(g[[dest]]),out[[f]]))mq_stop('Campo derivado fornecido conflita com vetor: ',dest)
    g[[dest]]<-out[[f]]
  }
  g
}
mq_design_classify <- function(g,layers,c,report) {
  original<-sf::st_drop_geometry(g)
  preserve<-function(x) {
    for(f in intersect(c('mq_formacao','mq_formacao_fonte','mq_apto','mq_fitofisionomia'),names(original)))
      if(f%in%names(x)&&!identical(as.character(original[[f]]),as.character(x[[f]])))mq_stop('Classificação fornecida diverge da atual: ',f,'. Histórico preservado; revisar explicitamente.')
    x
  }
  g<-mq_design_attributes(g,layers,c,report)
  if(!c$estratificar_vegetacao){g$mq_apto<-TRUE;return(preserve(g))}
  if(!is.null(c$formacao_campo)) {
    raw<-g[[paste0('atr_',c$formacao_campo)]];f<-raw
    if(!is.null(c$formacao_mapa)) {
      m<-c$formacao_mapa
      if(!is.data.frame(m)||!all(c('classe','formacao')%in%names(m))||anyNA(m)||anyDuplicated(m$classe))mq_stop('formacao_mapa exige classe única e formacao, sem ausentes.')
      f<-as.character(m$formacao[match(raw,as.character(m$classe))])
    }
    g$mq_formacao_fonte<-paste0('vetor:',c$formacao_campo)
  } else {
    if(!c$mapbiomas)mq_stop('Vegetação exige formacao_campo ou MapBiomas habilitado.')
    if(!'mb_codigo'%in%names(g))g<-mq_mb(g,c,report)
    k<-g$mb_codigo;f<-rep(NA_character_,nrow(g))
    f[k%in%12]<-'campestre';f[k%in%c(4,7)]<-'savanica';f[k%in%c(3,5,6,49)]<-'florestal'
    excluded<-c(15,39,20,40,62,41,46,47,35,48,9,21,23,24,30,75,91,25,33,31)
    f[k%in%excluded]<-'fora_alvo';g$mq_formacao_fonte<-paste0('MapBiomas_',c$mb_produto,'_C',c$mb_colecao,'_',c$mb_ano)
    # 11, 29, 32, 50, 77, 84: composição/estrutura heterogênea; não inferir formação.
  }
  bad<-is.na(f)|!f%in%c('campestre','savanica','florestal','fora_alvo')
  if(any(bad)) {
    mq_csv(sf::st_drop_geometry(g[bad,]),file.path(report,'vegetacao_pendente.csv'))
    mq_stop('Formação não resolvida em ',sum(bad),' pontos. Forneça polígonos com formacao_campo e mapeamento explícito; campos rupestres não equivalem a toda classe 29. Veja vegetacao_pendente.csv.')
  }
  g$mq_formacao<-f;g$mq_apto<-f%in%c('campestre','savanica',if(c$incluir_formacao_florestal)'florestal')
  if(!is.null(c$fitofisionomia_campo))g$mq_fitofisionomia<-g[[paste0('atr_',c$fitofisionomia_campo)]]
  mq_csv(as.data.frame(table(formacao=f,apto=g$mq_apto)),file.path(report,'classificacao_vegetacao.csv'))
  preserve(g)
}
mq_quota <- function(q,classes,N,label) {
  if(!is.data.frame(q)||!all('classe'%in%names(q))||!nrow(q))mq_stop('Cota inválida: ',label)
  measure<-intersect(c('n','percentual'),names(q));if(length(measure)!=1)mq_stop('Use n OU percentual para ',label)
  q$classe<-as.character(q$classe);v<-q[[measure]]
  if(anyNA(q$classe)||any(!nzchar(trimws(q$classe)))||anyDuplicated(q$classe)||!is.numeric(v)||any(!is.finite(v))||any(v<0))mq_stop('Classes/valores inválidos: ',label)
  if(length(setdiff(classes,q$classe)))mq_stop('Classes não contempladas em ',label,': ',paste(setdiff(classes,q$classe),collapse=', '),'. Declare cota zero se a exclusão for intencional.')
  if(measure=='n') {
    if(any(v!=floor(v))||sum(v)!=N)mq_stop('Cotas n de ',label,' devem ser inteiras e somar ',N)
    target<-v
  } else {
    if(any(v>100)||abs(sum(v)-100)>1e-7)mq_stop('Percentuais de ',label,' devem somar 100.')
    target<-N*v/100
  }
  q$alvo<-target;q$medida<-measure;q$valor<-v
  q[order(q$classe,method='radix'),c('classe','alvo','medida','valor')]
}
mq_design_criteria <- function(g,c,N) {
  ans<-list();a<-g[g$mq_apto,]
  if(c$estratificar_vegetacao) {
    forms<-sort(unique(a$mq_formacao),method='radix');q<-c$cotas_formacao
    if(!length(forms)&&N>0)mq_stop('Nenhum ponto classificado nas formações habilitadas.')
    if(is.null(q)) {
      if(c$incluir_formacao_florestal&&'florestal'%in%forms)mq_stop('Alvo florestal exige cotas_formacao explícitas; não se presume divisão em terços.')
      if(!is.null(c$fitofisionomia_campo)) {
        pairs<-unique(sf::st_drop_geometry(a)[,c('mq_formacao','mq_fitofisionomia')]);if(anyDuplicated(pairs$mq_fitofisionomia))mq_stop('Uma fitofisionomia está associada a mais de uma formação.')
        w<-table(factor(pairs$mq_formacao,levels=forms));q<-data.frame(classe=forms,percentual=100*as.numeric(w)/sum(w))
      } else if(length(forms))q<-data.frame(classe=forms,percentual=rep(100/length(forms),length(forms)))
    }
    if(!is.null(q))ans$mq_formacao<-mq_quota(q,forms,N,'formação')
    if(!is.null(c$fitofisionomia_campo)) {
      phy<-sort(unique(a$mq_fitofisionomia),method='radix')
      if(length(phy))ans$mq_fitofisionomia<-mq_quota(data.frame(classe=phy,percentual=rep(100/length(phy),length(phy))),phy,N,'fitofisionomia')
    }
  }
  if(c$estratificar_por_atributos)for(f in sort(names(c$cotas_atributos),method='radix'))ans[[paste0('atr_',f)]]<-mq_quota(c$cotas_atributos[[f]],sort(unique(a[[paste0('atr_',f)]])),N,f)
  ans
}
mq_design_select <- function(g,c,report,old=NULL) {
  if(!requireNamespace('lpSolve',quietly=TRUE))mq_stop('Instale o pacote lpSolve para resolver cotas cumulativas.')
  N<-nrow(g);np<-mq_quantity(c$prioritarios,N,ceiling(.2*N));na<-mq_quantity(c$alternativos,N,2L*np)
  doubled<-is.null(c$alternativos$n)&&is.null(c$alternativos$percentual)
  if(np==0&&na>0)mq_stop('No desenho estratificado, alternativos positivos exigem prioritários positivos.')
  criteria<-mq_design_criteria(g,c,np);fields<-names(criteria)
  if(!length(fields))mq_stop('Nenhum critério de estratificação disponível.')
  tuples<-do.call(paste,c(lapply(sf::st_drop_geometry(g)[,fields,drop=FALSE],function(v){v<-as.character(v);paste0(nchar(v,type='bytes'),':',v)}),sep='|'))
  keys<-vapply(tuples,digest::digest,character(1),algo='sha256',serialize=FALSE)
  g$mq_estrato<-keys;g$mq_estrato[!g$mq_apto]<-'fora_alvo'
  if(!is.null(old)) {
    ix<-match(g$chave_grade,old$chave_grade);sel<-which(!is.na(ix)&g$categoria!='grade')
    if(length(sel)&&(!'mq_estrato'%in%names(old)||anyNA(old$mq_estrato[ix[sel]])||any(old$mq_estrato[ix[sel]]!=g$mq_estrato[sel])))mq_stop('Expansão mudaria estrato de PA preservado ou não há classificação histórica; revisão explícita necessária.')
  }
  if(any(g$categoria!='grade'&!g$mq_apto))mq_stop('PA preservado está fora das formações habilitadas.')
  viable<-g$mq_apto&mq_road_available(g)
  if(any(g$categoria!='grade'&!viable))mq_stop('PA histórico conflita com restrições viárias; histórico preservado.')
  cells<-sort(unique(keys[viable]),method='radix');H<-length(cells)
  if(!H||H>c$max_estratos)mq_stop('Quantidade de combinações vazia ou acima de max_estratos: ',H)
  cell<-match(keys,cells);cap<-tabulate(cell[viable],H)
  op<-tabulate(cell[g$categoria=='prioritario'],H);oa<-tabulate(cell[g$categoria=='alternativo'],H)
  combos<-sf::st_drop_geometry(g[match(cells,keys),fields,drop=FALSE]);combos$estrato<-cells;combos$disponiveis<-cap;combos$preservados_p<-op;combos$preservados_a<-oa
  mq_csv(combos,file.path(report,'combinacoes_disponiveis.csv'))
  bands<-list()
  for(f in fields)for(j in seq_len(nrow(criteria[[f]]))) {
    q<-criteria[[f]][j,];ix<-which(combos[[f]]==q$classe)
    bands[[length(bands)+1]]<-list(campo=f,classe=q$classe,grupo='prioritario',ix=ix,alvo=q$alvo,medida=q$medida,valor=q$valor)
    if(!doubled)bands[[length(bands)+1]]<-list(campo=f,classe=q$classe,grupo='alternativo',ix=H+ix,alvo=if(np)na*q$alvo/np else 0,medida='proporcao_dos_prioritarios',valor=if(np)100*q$alvo/np else 0)
  }
  B<-length(bands);V<-6*H+2*B;rows<-list();dirs<-character();rhs<-numeric()
  add<-function(ix,val,dir,b) {r<-length(rhs)+1L;rows[[r]]<<-if(length(ix))cbind(r,ix,val)else matrix(numeric(),0,3);dirs[r]<<-dir;rhs[r]<<-b}
  add(seq_len(H),rep(1,H),'=',np);add(H+seq_len(H),rep(1,H),'=',na)
  for(h in seq_len(H)) {
    add(c(h,H+h),c(1,1),'<=',cap[h]);add(h,1,'>=',op[h]);add(H+h,1,'>=',oa[h])
    if(doubled)add(c(h,H+h),c(-2,1),'=',0)
    for(k in 0:1) {
      dv<-2*H+2*B+2*(k*H+h)-1:0
      add(c(k*H+h,dv),c(1,-1,1),'=',c(np,na)[k+1]*cap[h]/sum(cap))
    }
  }
  for(j in seq_along(bands)) {
    b<-bands[[j]];lo<-floor(b$alvo+1e-8);hi<-ceiling(b$alvo-1e-8)
    add(b$ix,rep(1,length(b$ix)),'>=',lo);add(b$ix,rep(1,length(b$ix)),'<=',hi)
    add(c(b$ix,2*H+2*j-1:0),c(rep(1,length(b$ix)),-1,1),'=',b$alvo)
  }
  marginal<-do.call(rbind,lapply(bands,function(b)data.frame(atributo=b$campo,classe=b$classe,grupo=b$grupo,medida=b$medida,valor=b$valor,alvo_real=b$alvo,minimo=floor(b$alvo+1e-8),maximo=ceiling(b$alvo-1e-8),disponiveis=sum(cap[if(b$grupo=='prioritario')b$ix else b$ix-H]))))
  marginal$minimo_com_reservas<-if(doubled)3*marginal$minimo else marginal$minimo
  marginal$deficit_minimo<-pmax(0,marginal$minimo_com_reservas-marginal$disponiveis)
  mq_csv(marginal,file.path(report,'cotas_solicitadas.csv'))
  solve<-function(obj)lpSolve::lp('min',obj,const.dir=dirs,const.rhs=rhs,dense.const=do.call(rbind,rows),int.vec=seq_len(2*H),timeout=as.integer(c$solver_timeout_s),scale=0)
  margvars<-2*H+seq_len(2*B);obj<-numeric(V);obj[margvars]<-1
  progress<-mq_progress('Compatibilizar todas as cotas',2);on.exit(mq_progress_done(progress),add=TRUE)
  s<-solve(obj);mq_progress_update(progress,1,'Margens inteiras')
  fail<-function(status){mq_json(list(status_solver=status,interpretacao=if(status==2)'inviabilidade_comprovada'else 'sem_solucao_otima_comprovada',denominador=N,prioritarios=np,alternativos=na),file.path(report,'solucao_cotas.json'));def<-marginal[marginal$deficit_minimo>0,];detail<-if(nrow(def))paste(sprintf('%s=%s: mínimo com reservas %d, disponíveis %d',def$atributo,def$classe,def$minimo_com_reservas,def$disponiveis),collapse='; ')else 'Verificar combinações simultâneas, preservados e limites inteiros; margens isoladas não comprovam viabilidade.';writeLines(detail,file.path(report,'diagnostico_cotas.txt'));mq_stop(if(status==2)'Cotas cumulativas inviáveis com capacidades/histórico e arredondamento inteiro permitido. 'else 'Solver interrompido/sem ótimo comprovado; não interpretar como inviabilidade. ',detail,' Consulte cotas_solicitadas.csv e combinacoes_disponiveis.csv; nenhuma cota foi relaxada.')}
  if(s$status!=0)fail(s$status)
  add(margvars,rep(1,length(margvars)),'<=',s$objval+1e-7)
  obj[]<-0;obj[(2*H+2*B+1):V]<-1
  s<-solve(obj);mq_progress_update(progress,2,'Combinações compatíveis');if(s$status!=0)fail(s$status)
  counts<-round(s$solution[seq_len(2*H)]);p<-counts[seq_len(H)];a<-counts[H+seq_len(H)]
  valid<-all(abs(counts-s$solution[seq_len(2*H)])<1e-5)&&sum(p)==np&&sum(a)==na&&all(p>=op)&all(a>=oa)&all(p+a<=cap)&all(counts>=0)
  if(doubled)valid<-valid&&all(a==2*p)
  actual<-vapply(bands,function(b)sum(counts[b$ix]),numeric(1));valid<-valid&&all(actual>=marginal$minimo&actual<=marginal$maximo)
  if(!valid)mq_stop('Solução inteira falhou na verificação independente; nenhum pacote será promovido.')
  marginal$realizado<-actual;marginal$desvio<-actual-marginal$alvo_real
  if(doubled){ar<-marginal;ar$grupo<-'alternativo';for(f in c('alvo_real','minimo','maximo','realizado','desvio'))ar[[f]]<-2*ar[[f]];ar$medida<-'dobro_dos_prioritarios';marginal<-rbind(marginal,ar)}
  mq_csv(marginal,file.path(report,'cotas_realizadas.csv'))
  combos$prioritarios<-p;combos$alternativos<-a;mq_csv(combos,file.path(report,'alocacao_combinacoes.csv'))
  rank<-vapply(g$chave_grade,function(k)digest::digest(paste(c$semente,k,sep=':'),algo='sha256',serialize=FALSE),character(1))
  for(h in seq_len(H)) {
    avail<-which(viable&!is.na(cell)&cell==h&g$categoria=='grade');avail<-avail[order(rank[avail],g$id_grade[avail])]
    pp<-head(avail,p[h]-op[h]);if(length(pp))g$categoria[pp]<-'prioritario'
    aa<-head(setdiff(avail,pp),a[h]-oa[h]);if(length(aa))g$categoria[aa]<-'alternativo'
  }
  if(c$estratificar_vegetacao&&c$perfil=='campestre_savanico') {
    field<-if('mq_fitofisionomia'%in%names(g))'mq_fitofisionomia'else 'mq_formacao'
    effort<-data.frame(estrato=sort(unique(g[[field]][g$mq_apto]),method='radix'),stringsAsFactors=FALSE)
    effort$PA_prioritarios<-vapply(effort$estrato,function(v)sum(g$categoria=='prioritario'&g[[field]]==v),integer(1))
    effort$observacao<-'PA candidato não comprova UA instalada; referência do protocolo: 12 UAs por fitofisionomia, ao menos duas fitofisionomias.'
    mq_csv(effort,file.path(report,'esforco_planejado.csv'))
    if(any(effort$PA_prioritarios<12)||nrow(effort)<2)message('ATENÇÃO: esforço/estratos de PAs não comprova consolidação do protocolo (12 UAs por fitofisionomia, ao menos duas). Veja esforco_planejado.csv.')
  }
  mq_json(list(status='verificado',solver='lpSolve',versao_solver=as.character(utils::packageVersion('lpSolve')),denominador_grade_AE=N,candidatos_habilitados=sum(cap),prioritarios=np,alternativos=na,alternativos_regra=if(doubled)'dobro_por_combinacao'else 'margens_proporcionais; combinacoes_resolvidas_conjuntamente',arredondamento='piso/teto de cada margem, escolha conjunta minimiza desvio total; não há arredondamento isolado',objetivo_secundario='menor desvio absoluto da composição disponível nas combinações; não pressupõe independência',campos=fields,semente=c$semente,limite_inferencia='áreas elegíveis e desenho executado; cotas não garantem inclusão positiva nem representatividade de toda UC'),file.path(report,'solucao_cotas.json'))
  g
}

mq_coordinates <- function(x) {
  if(!nrow(x)) return(x)
  typ<-unique(as.character(sf::st_geometry_type(x)))
  if(all(typ=='POINT')) {
    ll<-sf::st_coordinates(sf::st_transform(x,4326));xy<-sf::st_coordinates(x)
    expected<-list(qf_lon=ll[,1],qf_lat=ll[,2],qf_x_m=xy[,1],qf_y_m=xy[,2]);for(n in intersect(names(expected),names(x)))if(anyNA(x[[n]]) || !is.numeric(x[[n]]) || any(abs(x[[n]]-expected[[n]])>1e-7))mq_stop('Coordenada derivada fornecida diverge da geometria/CRS: ',n)
    x$qf_lon<-ll[,1];x$qf_lat<-ll[,2];x$qf_x_m<-xy[,1];x$qf_y_m<-xy[,2]
  }
  x
}
# Cache persistente por coordenada/produto. Lotes imutáveis evitam apagar entradas antigas.
mq_mb <- function(x,c,report) {
  if(!nrow(x)||any(grepl('^mb_',names(x))))return(mq_mb_fresh(x,c,report))
  xy<-sf::st_coordinates(sf::st_transform(x,4326));keys<-paste(sprintf('%a',xy[,1]),sprintf('%a',xy[,2]),sep='|')
  folder<-file.path(c$cache_dir,'mapbiomas',paste(c$mb_produto,c$mb_colecao,c$mb_ano,sep='_'));dir.create(folder,recursive=TRUE,showWarnings=FALSE)
  files<-list.files(folder,pattern='\\.rds$',full.names=TRUE);batches<-lapply(files,function(p)if(mq_verified(p))tryCatch(readRDS(p),error=function(e)NULL)else NULL);batches<-Filter(Negate(is.null),batches)
  cache<-if(length(batches))do.call(rbind,lapply(batches,`[[`,'dados'))else NULL
  ix<-if(is.null(cache))rep(NA_integer_,length(keys))else match(keys,cache$.chave)
  missing<-which(is.na(ix));fresh<-NULL
  if(length(missing)) {
    fresh<-mq_mb_fresh(x[missing,],c,report);attrs<-sf::st_drop_geometry(fresh)[,grep('^mb_',names(fresh),value=TRUE),drop=FALSE];attrs$.chave<-keys[missing]
    good<-attrs$mb_status=='obtido'
    if(any(good)) {
      lf<-file.path(report,paste0('legenda_mb_',c$mb_produto,'.csv'));mf<-file.path(report,paste0('fonte_mb_',c$mb_produto,'.json'))
      batch<-list(dados=attrs[good,,drop=FALSE],legenda=if(file.exists(lf))readBin(lf,'raw',file.info(lf)$size)else raw(),fonte=if(file.exists(mf))jsonlite::read_json(mf)else NULL)
      dest<-file.path(folder,paste0(digest::digest(batch),'.rds'));if(!file.exists(dest)){tmp<-tempfile(tmpdir=folder);saveRDS(batch,tmp);if(!file.rename(tmp,dest))mq_stop('Falha ao materializar cache MapBiomas.');mq_seal(dest)}
    }
  }
  if(!is.null(cache))for(n in setdiff(names(cache),'.chave'))x[[n]]<-cache[[n]][ix]
  if(!is.null(fresh))for(n in names(fresh)[grepl('^mb_',names(fresh))]) {if(!n%in%names(x))x[[n]]<-fresh[[n]][rep(NA_integer_,nrow(x))];x[[n]][missing]<-fresh[[n]]}
  if(!length(missing)&&length(batches)) {
    batch<-batches[[1]];writeBin(batch$legenda,file.path(report,paste0('legenda_mb_',c$mb_produto,'.csv')));mq_json(batch$fonte,file.path(report,paste0('fonte_mb_',c$mb_produto,'.json')))
  }
  mq_json(list(reutilizados=sum(!is.na(ix)),consultados=length(missing),pasta_cache=folder),file.path(report,paste0('cache_mb_',c$mb_produto,'_',substr(digest::digest(keys),1,8),'.json')))
  x
}

mq_mb_fresh <- function(x,c,report) {
  if(!nrow(x)) return(x)
  if(any(grepl('^mb_',names(x)))){if(all(c('mb_codigo','mb_classe','mb_formacao','mb_produto','mb_colecao','mb_ano','mb_res_m','mb_status','mb_fonte')%in%names(x)))return(x);mq_stop('Campos mb_* parciais/conflitantes na entrada; preserve a origem e use mapbiomas=FALSE.')}
  x$mb_produto<-c$mb_produto;x$mb_colecao<-c$mb_colecao;x$mb_ano<-c$mb_ano;x$mb_res_m<-as.integer(sub('m','',c$mb_produto))
  x$mb_codigo<-NA_integer_;x$mb_classe<-NA_character_;x$mb_formacao<-NA_character_;x$mb_status<-'nao_obtido'
  url<-if(c$mb_produto=='30m')sprintf('https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection11/lulc/coverage/brazil_coverage/brazil_coverage-col11_%d.tif',c$mb_ano) else sprintf('https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/lulc_10m/collection4/coverage/brazil_coverage/brazil_coverage-col4_10m_%d.tif',c$mb_ano)
  legend<-if(c$mb_produto=='30m') 'https://brasil.mapbiomas.org/wp-content/uploads/sites/3/2026/08/legend_code_mapbiomas_brazil_collection_11.csv' else 'https://brasil.mapbiomas.org/wp-content/uploads/sites/3/2026/08/legend_code_mapbiomas10m_brazil_collection_4.csv'
  x$mb_fonte<-url;err<-NULL
  tryCatch({
    lf<-file.path(report,paste0('legenda_mb_',c$mb_produto,'.csv'))
    re<-httr::GET(legend,httr::timeout(c$timeout_s));httr::stop_for_status(re)
    writeBin(httr::content(re,as='raw'),lf);leg<-data.table::fread(lf,encoding='UTF-8')
    if(!all(c('class_id','class_name_pt_br')%in%names(leg)) || anyDuplicated(leg$class_id))mq_stop('Legenda MapBiomas incompatível.')
    envnames<-c('GDAL_HTTP_TIMEOUT','GDAL_DISABLE_READDIR_ON_OPEN','CPL_VSIL_CURL_ALLOWED_EXTENSIONS');prev<-Sys.getenv(envnames,unset=NA_character_)
    on.exit({for(i in seq_along(prev))if(is.na(prev[i]))Sys.unsetenv(envnames[i])else do.call(Sys.setenv,setNames(list(prev[i]),envnames[i]))},add=TRUE)
    Sys.setenv(GDAL_HTTP_TIMEOUT=as.character(c$timeout_s),GDAL_DISABLE_READDIR_ON_OPEN='EMPTY_DIR',CPL_VSIL_CURL_ALLOWED_EXTENSIONS='.tif')
    r<-terra::rast(paste0('/vsicurl/',url));p<-terra::project(terra::vect(x),terra::crs(r))
    code<-as.integer(terra::extract(r,p,method='simple')[,2]);label<-leg$class_name_pt_br[match(code,leg$class_id)]
    x$mb_codigo<-code;x$mb_classe<-label
    x$mb_formacao<-ifelse(!is.na(label)&grepl('^Formação ',label),label,NA_character_)
    x$mb_status<-ifelse(is.na(code),'sem_dado',ifelse(is.na(label),'codigo_sem_legenda','obtido'))
    if(any(x$mb_status=='codigo_sem_legenda'))mq_stop('Classe sem correspondência na legenda oficial.')
    head<-httr::HEAD(url,httr::timeout(c$timeout_s));httr::stop_for_status(head)
    mq_json(list(url=url,legenda=legend,legenda_sha256=mq_hash(lf),etag=httr::headers(head)$etag,consulta=format(Sys.time(),tz='UTC',usetz=TRUE),metodo='pixel que contém o ponto; sem interpolação; não substitui formação de campo'),file.path(report,paste0('fonte_mb_',c$mb_produto,'.json')))
  },error=function(e)err<<-conditionMessage(e))
  if(!is.null(err)) {
    x$mb_codigo<-NA_integer_;x$mb_classe<-NA_character_;x$mb_formacao<-NA_character_;x$mb_status<-paste0('falha: ',err)
    if(isTRUE(c$mb_obrigatorio))mq_stop('MapBiomas obrigatório: ',err)
    warning('MapBiomas não concluído: ',err,call.=FALSE)
  }
  x
}

mq_diagnostics <- function(points,camadas,ae,c,report) {
  if(!nrow(points))return(invisible(NULL))
  if(c$perfil!='campestre_savanico'){writeLines(paste('Procedimento campestre não aplicado ao perfil',c$perfil),file.path(report,'procedimento_campo_nao_aplicado.txt'));return(invisible(NULL))}
  # Posição original imutável; direção a partir do norte geográfico local, em metros no CRS escolhido.
  if(nrow(points)>20000) {writeLines('Simulações não executadas: mais de 20.000 PAs. Nenhuma seleção alterada.',file.path(report,'simulacoes_nao_executadas.txt'));return(invisible(NULL))}
  xy<-sf::st_coordinates(points);ll<-sf::st_coordinates(sf::st_transform(points,4326));ll[,2]<-ll[,2]+1e-5
  north<-sf::st_coordinates(sf::st_transform(sf::st_as_sf(data.frame(lon=ll[,1],lat=ll[,2]),coords=c('lon','lat'),crs=4326),sf::st_crs(points)))-xy[,1:2,drop=FALSE]
  north<-north/sqrt(rowSums(north^2));rows<-list();directions<-c('N','L','S','O')
  known<-list(transectos=c$distancia_min_m,formacao_florestal=100)
  for(k in seq_along(directions)) {
    vec<-switch(k,north,cbind(north[,2],-north[,1]),-north,cbind(-north[,2],north[,1]))
    geom<-sf::st_sfc(lapply(seq_len(nrow(points)),function(i)sf::st_linestring(rbind(xy[i,1:2],xy[i,1:2]+c$transecto_m*vec[i,]))),crs=sf::st_crs(points))
    d<-data.frame(PA=points$PA,direcao=directions[k],comprimento_m=c$transecto_m,deslocamento_m=0,inteiro_na_AE=lengths(sf::st_covered_by(geom,sf::st_union(ae)))>0)
    for(role in names(known)) {
      src<-Filter(function(l)l$papel==role && nrow(l$x)>0,camadas)
      nm<-paste0('conflito_',role)
      if(!length(src))d[[nm]]<-NA else {
        target<-do.call(base::c,lapply(src,function(l)sf::st_geometry(l$x)))
        # Estritamente inferior: o limiar exato é aceito.
        nearby<-sf::st_is_within_distance(geom,target,dist=known[[role]])
        d[[nm]]<-vapply(seq_along(nearby),function(i)length(nearby[[i]])>0 && any(as.numeric(sf::st_distance(geom[i],target[nearby[[i]]])) < known[[role]]-1e-7),logical(1))
        if(role=='formacao_florestal' && isTRUE(c$incluir_formacao_florestal) && 'mq_formacao'%in%names(points))d[[nm]][points$mq_formacao=='florestal']<-FALSE
      }
    }
    d$status<-'simulacao; confirmar obstaculos, formacao, relevo e distancias em campo'
    rows[[k]]<-d
  }
  mq_csv(do.call(rbind,rows),file.path(report,'simulacoes_direcoes.csv'))
  # Apenas sugestão de proximidade; nunca promove PA alternativo a UA.
  pri<-points[points$categoria=='prioritario',];alt<-points[points$categoria=='alternativo',]
  if(nrow(pri)&&nrow(alt)) {
    alt<-alt[order(alt$PA),];i<-sf::st_nearest_feature(pri,alt)
    mq_csv(data.frame(PA=pri$PA,alternativo=alt$PA[i],distancia_m=as.numeric(sf::st_distance(pri,alt[i,],by_element=TRUE)),criterio='roteiro: menor distância plana PA a PA; não comprova instalação'),file.path(report,'alternativos_proximos.csv'))
    if('mq_estrato'%in%names(pri)) {
      same<-lapply(seq_len(nrow(pri)),function(j){pool<-alt[alt$mq_estrato==pri$mq_estrato[j],];if(!nrow(pool))return(data.frame(PA=pri$PA[j],alternativo=NA_character_,distancia_m=NA_real_));k<-sf::st_nearest_feature(pri[j,],pool);data.frame(PA=pri$PA[j],alternativo=pool$PA[k],distancia_m=as.numeric(sf::st_distance(pri[j,],pool[k,])))})
      mq_csv(do.call(rbind,same),file.path(report,'alternativos_mesmo_estrato.csv'))
    }
  }
}

# Adaptado de baixar_imagens_uas_xyz.R v1.0; sem CONFIG/execução externa.
qfi_erro <- function(...) stop(..., call. = FALSE)
qfi_deps <- function() {
  p <- c("sf", "data.table", "curl", "DBI", "RSQLite", "digest", "png", "jpeg")
  falta <- p[!vapply(p, requireNamespace, logical(1), quietly = TRUE)]
  if (length(falta)) qfi_erro("Instale os pacotes ausentes: ", paste(falta, collapse = ", "))
}
qfi_hash <- function(x) digest::digest(x, algo = "sha256")
qfi_filehash <- function(x) digest::digest(file = x, algo = "sha256")
qfi_slug <- function(x) {
  y <- iconv(x, to = "ASCII//TRANSLIT"); if (is.na(y)) y <- "UC"
  y <- substr(gsub("[^A-Za-z0-9_-]+", "_", y), 1, 65)
  paste0("UC_", y, "_", substr(qfi_hash(x), 1, 10))
}
qfi_validar <- function(c) {
  for (n in c("gpkg", "camada", "campo_uc", "saida", "url_xyz", "fonte", "licenca",
              "atribuicao", "data_imagem", "resolucao_nativa_m", "versao_acervo"))
    if (!is.character(c[[n]]) || length(c[[n]]) != 1L || is.na(c[[n]]) || !nzchar(trimws(c[[n]])))
      qfi_erro("Configuracao ausente/invalida: ", n)
  if (!file.exists(c$gpkg) || tolower(tools::file_ext(c$gpkg)) != "gpkg") qfi_erro("GPKG nao encontrado.")
  if (!grepl("^https?://[^/]+/", c$url_xyz) ||
      !all(vapply(c("{z}", "{x}", "{y}"), function(p) grepl(p, c$url_xyz, fixed = TRUE), logical(1))))
    qfi_erro("URL deve ser HTTP(S) XYZ com {z}, {x}, {y}. A URL nao foi registrada no log.")
  u <- c$url_xyz
  for (p in c("{z}", "{x}", "{y}")) u <- gsub(p, "0", u, fixed = TRUE)
  if (grepl("[{}]", u)) qfi_erro("URL contem marcadores nao suportados; informe um endpoint XYZ concreto.")
  for (n in c("raio_m", "intervalo_s", "tentativas", "timeout_s", "max_tiles",
              "max_tiles_por_feicao", "estimativa_kb_tile", "max_mib_mbtiles"))
    if (!is.numeric(c[[n]]) || length(c[[n]]) != 1L || !is.finite(c[[n]]) || c[[n]] <= 0)
      qfi_erro("Configuracao numerica invalida: ", n)
  if (c$raio_m > 10000 || c$intervalo_s < 0.1 || c$tentativas > 5 || c$timeout_s > 60 ||
      c$max_mib_mbtiles > 950) qfi_erro("Configuracao excede os limites de seguranca documentados.")
  for (n in c("tentativas","max_tiles","max_tiles_por_feicao"))
    if (c[[n]] != floor(c[[n]])) qfi_erro("Configuracao deve ser inteira: ",n)
  if (!is.numeric(c$zooms) || !length(c$zooms) || anyNA(c$zooms) ||
      any(c$zooms != as.integer(c$zooms)) || any(!c$zooms %in% 0:22)) qfi_erro("Zoom invalido (0 a22).")
  c$zooms <- sort(unique(as.integer(c$zooms)))
  if (!identical(c$zooms, seq.int(min(c$zooms), max(c$zooms)))) qfi_erro("Use niveis de zoom consecutivos.")
  if (!is.logical(c$executar_download) || length(c$executar_download) != 1L || is.na(c$executar_download))
    qfi_erro("executar_download deve ser TRUE ou FALSE.")
  if (length(c$cabecalhos) && (!is.character(c$cabecalhos) || is.null(names(c$cabecalhos)) ||
      anyNA(c$cabecalhos) || any(!nzchar(names(c$cabecalhos))))) qfi_erro("Cabecalhos invalidos.")
  if (c$executar_download && (grepl("SEU_PROVEDOR", c$url_xyz, fixed = TRUE) ||
      any(grepl("^PREENCHER", c(c$fonte, c$licenca, c$atribuicao)))))
    qfi_erro("Preencha fonte, licenca, atribuicao e URL antes de baixar.")
  c
}
qfi_poligonos_tiles <- function(d) {
  h <- 20037508.342789244
  g <- lapply(seq_len(nrow(d)), function(i) {
    a <- 2*h/2^d$z[i]; esquerda <- -h+d$x[i]*a; topo <- h-d$y[i]*a
    sf::st_polygon(list(matrix(c(esquerda,topo, esquerda+a,topo, esquerda+a,topo-a,
                                 esquerda,topo-a, esquerda,topo), ncol=2, byrow=TRUE)))
  })
  sf::st_sfc(g, crs=3857)
}
qfi_planejar <- function(c) {
  camadas <- sf::st_layers(c$gpkg)$name
  if (!c$camada %in% camadas) qfi_erro("Camada nao encontrada. Disponiveis: ", paste(camadas, collapse=", "))
  a <- sf::st_read(c$gpkg, layer=c$camada, quiet=TRUE, stringsAsFactors=FALSE)
  if (!nrow(a) || is.na(sf::st_crs(a))) qfi_erro("Camada vazia ou sem CRS definido.")
  if (!c$campo_uc %in% names(a)) qfi_erro("Coluna UC ausente. Disponiveis: ", paste(names(a), collapse=", "))
  uc <- trimws(as.character(a[[c$campo_uc]]))
  if (anyNA(uc) || any(!nzchar(uc))) qfi_erro("Ha feicoes sem identificacao de UC.")
  tipo <- as.character(sf::st_geometry_type(a))
  if (any(!tipo %in% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING")) ||
      any(sf::st_is_empty(a)) || any(!sf::st_is_valid(a)))
    qfi_erro("Use pontos/linhas validos das UAs; nao serao reparados ou descartados automaticamente.")
  g <- sf::st_transform(sf::st_zm(sf::st_geometry(a), drop=TRUE, what="ZM"), 4326)
  h <- 20037508.342789244; partes <- list(); acumulado <- 0L
  for (i in seq_along(g)) {
    bb <- sf::st_bbox(g[i])
    if (any(!is.finite(bb)) || bb["xmin"] < -180 || bb["xmax"] > 180 ||
        bb["ymin"] < -79 || bb["ymax"] > 83 || bb["xmax"]-bb["xmin"] > 6 ||
        bb["ymax"]-bb["ymin"] > 2) qfi_erro("Feicao ", i, ": extensao inadequada para UA/UTM.")
    lon <- mean(bb[c("xmin","xmax")]); lat <- mean(bb[c("ymin","ymax")])
    zona <- min(60L, max(1L, floor((lon+180)/6)+1L)); epsg <- (if (lat >= 0) 32600 else 32700)+zona
    # Raio em metros UTM, nao graus ou metros distorcidos do WebMercator.
    b <- sf::st_transform(sf::st_buffer(sf::st_transform(g[i], epsg), c$raio_m, nQuadSegs=32), 3857)
    e <- sf::st_bbox(b)
    if (e["xmin"] <= -h || e["xmax"] >= h) qfi_erro("Buffer cruza antimeridiano; separar antes de executar.")
    for (z in c$zooms) {
      n <- 2^z; passo <- 2*h/n
      xr <- pmax(0, pmin(n-1, floor((e[c("xmin","xmax")]+h)/passo)))
      yr <- pmax(0, pmin(n-1, floor((h-e[c("ymax","ymin")])/passo)))
      qtd <- (diff(xr)+1)*(diff(yr)+1)
      if (qtd > c$max_tiles_por_feicao) qfi_erro("Feicao ", i, ": tiles demais. Confira raio/geometria/zoom.")
      d <- data.table::CJ(x=seq.int(xr[1],xr[2]), y=seq.int(yr[1],yr[2]))
      d[, z := as.integer(z)]
      manter <- lengths(sf::st_intersects(qfi_poligonos_tiles(d), b)) > 0L
      d <- d[manter]; d[, UC := uc[i]]
      partes[[length(partes)+1L]] <- d[, c("UC","z","x","y"), with=FALSE]
      acumulado <- acumulado+nrow(d)
      if (acumulado > c$max_tiles*10) qfi_erro("Plano intermediario excessivo; divida a entrada por grupos de UCs.")
    }
  }
  p <- unique(data.table::rbindlist(partes)); data.table::setorderv(p,c("UC","z","x","y"))
  chaves <- unique(p[, c("z","x","y"), with=FALSE])
  if (nrow(chaves) > c$max_tiles) qfi_erro("Plano excede max_tiles: ", nrow(chaves), ". Nenhum download realizado.")
  list(plano=p, feicoes=data.table::data.table(UC=uc)[, list(feicoes=.N), by=UC])
}
qfi_abrir_cache <- function(caminho) {
  db <- DBI::dbConnect(RSQLite::SQLite(), caminho)
  DBI::dbExecute(db,"PRAGMA busy_timeout=5000")
  DBI::dbExecute(db,paste("CREATE TABLE IF NOT EXISTS tiles(z INTEGER,x INTEGER,y INTEGER,",
    "formato TEXT,largura INTEGER,altura INTEGER,sha256 TEXT,data BLOB,obtido_utc TEXT,PRIMARY KEY(z,x,y))"))
  db
}
qfi_imagem <- function(raw) {
  if (!is.raw(raw) || length(raw) < 8L || length(raw) > 4*1024^2) qfi_erro("Imagem vazia/excessiva.")
  if (identical(raw[1:8], as.raw(c(137,80,78,71,13,10,26,10)))) {
    formato <- "png"; img <- tryCatch(png::readPNG(raw), error=function(e) NULL)
  } else if (identical(raw[1:2], as.raw(c(255,216)))) {
    formato <- "jpg"; img <- tryCatch(jpeg::readJPEG(raw), error=function(e) NULL)
  } else qfi_erro("Resposta nao e uma imagem PNG/JPEG.")
  d <- dim(img)
  if (length(d) != 3L || !d[1] %in% c(256L,512L) || d[1] != d[2] || !d[3] %in% 3:4)
    qfi_erro("Tile deve ser RGB/RGBA quadrado de256 ou512 pixels.")
  list(formato=formato, largura=d[2], altura=d[1])
}
qfi_url <- function(c,z,x,y) {
  u <- c$url_xyz
  for (p in c("z","x","y")) u <- gsub(paste0("{",p,"}"), as.character(get(p)), u, fixed=TRUE)
  u
}
qfi_buscar <- function(c,z,x,y) {
  for (i in seq_len(c$tentativas)) {
    Sys.sleep(c$intervalo_s)
    h <- curl::new_handle(timeout=c$timeout_s,connecttimeout=min(15,c$timeout_s),
      followlocation=FALSE,maxfilesize=4*1024^2,useragent="QField-UA-offline/1.0")
    if (length(c$cabecalhos)) curl::handle_setheaders(h,.list=as.list(c$cabecalhos))
    # Nao propagar mensagens curl que podem revelar URL/chave de acesso.
    r <- tryCatch(curl::curl_fetch_memory(qfi_url(c,z,x,y),handle=h),error=function(e) NULL)
    st <- if (is.null(r)) 0L else r$status_code
    if (st == 200L) {
      info <- tryCatch(qfi_imagem(r$content),error=function(e) NULL)
      if (is.null(info)) return(list(ok=FALSE,motivo="HTTP200_sem_imagem_RGB_valida"))
      return(c(list(ok=TRUE,raw=r$content),info))
    }
    if (st %in% c(401L,403L)) qfi_erro("HTTP", st, ": acesso negado. Interrompido; cache preservado.")
    if (st == 429L) {
      # Nao insistir nem contornar limites da fonte.
      qfi_erro("HTTP429: limite do provedor. Interrompido; aguarde a janela indicada pelo provedor e retome.")
    }
    if (st >= 300L && st < 400L) qfi_erro("Redirecionamento HTTP recusado; configure o endpoint XYZ final.")
    if (st == 503L && grepl("(?im)^retry-after:", rawToChar(r$headers), perl=TRUE))
      qfi_erro("HTTP503 com Retry-After. Aguarde o prazo do provedor antes de retomar; cache preservado.")
    transitorio <- st == 0L || st %in% c(408L,500L,502L,503L,504L)
    if (!transitorio || i == c$tentativas) return(list(ok=FALSE,motivo=paste0("HTTP_",st)))
    Sys.sleep(min(8,2^i))
  }
}
qfi_cache_put <- function(db,z,x,y,r) {
  DBI::dbExecute(db,"INSERT OR REPLACE INTO tiles VALUES(?,?,?,?,?,?,?,?,?)",
    params=list(as.integer(z),as.integer(x),as.integer(y),r$formato,as.integer(r$largura),
      as.integer(r$altura),digest::digest(r$raw,"sha256",serialize=FALSE),list(r$raw),
      format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")))
}
qfi_cache_get <- function(db,z,x,y) {
  r <- DBI::dbGetQuery(db,"SELECT * FROM tiles WHERE z=? AND x=? AND y=?",params=list(z,x,y))
  if (!nrow(r)) return(NULL)
  if (!identical(digest::digest(r$data[[1]],"sha256",serialize=FALSE),r$sha256[1]))
    qfi_erro("Cache com checksum divergente em ",z,"/",x,"/",y,". Nenhum arquivo foi removido.")
  r
}
qfi_exportar <- function(db,p,c,raiz,id) {
  ucs <- unique(p$UC); status <- list()
  indices <- data.table::as.data.table(DBI::dbGetQuery(db,"SELECT z,x,y,formato,largura,altura,length(data) AS bytes FROM tiles"))
  for (uc in ucs) {
    a <- merge(p[UC == uc],indices,by=c("z","x","y"),all.x=TRUE,sort=TRUE)
    faltam <- sum(is.na(a$bytes)); motivo <- "completo"; arquivo <- ""
    if (faltam) motivo <- paste0("incompleto: ",faltam," tiles ausentes")
    else if (sum(a$bytes) > c$max_mib_mbtiles*1024^2*0.90) motivo <- "excede_limite: divida a UC em subconjuntos espaciais"
    else if (data.table::uniqueN(a[,c("formato","largura","altura"),with=FALSE]) != 1L) motivo <- "formatos_ou_dimensoes_mistos"
    if (motivo != "completo") {
      status[[length(status)+1L]] <- data.table::data.table(UC=uc,status=motivo,arquivo=arquivo); next
    }
    pasta <- file.path(raiz,"pacotes",id,qfi_slug(uc),"imagens")
    dir.create(pasta,recursive=TRUE,showWarnings=FALSE)
    arquivo <- file.path(pasta,paste0("detalhe_z",max(c$zooms),".mbtiles"))
    prova <- paste0(arquivo,".sha256")
    if (file.exists(arquivo)) {
      if (!file.exists(prova) || !identical(qfi_filehash(arquivo),readLines(prova,warn=FALSE)))
        qfi_erro("Saida existente sem checksum valido. Preservada: ",arquivo)
    } else {
      tmp <- tempfile("montagem_",tmpdir=pasta,fileext=".parcial")
      con <- DBI::dbConnect(RSQLite::SQLite(),tmp)
      tryCatch({
        DBI::dbExecute(con,"CREATE TABLE metadata(name TEXT PRIMARY KEY,value TEXT)")
        DBI::dbExecute(con,"CREATE TABLE tiles(zoom_level INTEGER,tile_column INTEGER,tile_row INTEGER,tile_data BLOB,PRIMARY KEY(zoom_level,tile_column,tile_row))")
        limites <- sf::st_bbox(sf::st_transform(qfi_poligonos_tiles(a),4326))
        meta <- c(name=paste(uc,"— detalhe"),format=a$formato[1],type="baselayer",version="1",
          minzoom=as.character(min(a$z)),maxzoom=as.character(max(a$z)),
          bounds=paste(format(as.numeric(limites),digits=12,scientific=FALSE,trim=TRUE,decimal.mark="."),collapse=","),
          attribution=c$atribuicao,description=paste(c$fonte,";",c$licenca),
          fonte=c$fonte,data_imagem=c$data_imagem,resolucao_nativa_m=c$resolucao_nativa_m)
        DBI::dbWriteTable(con,"metadata",data.frame(name=names(meta),value=unname(meta)),append=TRUE)
        DBI::dbWithTransaction(con,{
          for (i in seq_len(nrow(a))) {
            r <- qfi_cache_get(db,a$z[i],a$x[i],a$y[i])
            DBI::dbExecute(con,"INSERT INTO tiles VALUES(?,?,?,?)",params=list(as.integer(a$z[i]),
              as.integer(a$x[i]),as.integer(2^a$z[i]-1-a$y[i]),list(r$data[[1]])))
          }
        })
        if (DBI::dbGetQuery(con,"PRAGMA integrity_check")[[1]][1] != "ok" ||
            DBI::dbGetQuery(con,"SELECT count(*) AS n FROM tiles")$n != nrow(a)) qfi_erro("Falha na verificacao MBTiles.")
      },finally=DBI::dbDisconnect(con))
      if (file.info(tmp)$size > c$max_mib_mbtiles*1024^2) qfi_erro("MBTiles excede limite; parcial preservado: ",tmp)
      if (!file.rename(tmp,arquivo)) qfi_erro("Falha ao finalizar MBTiles; parcial preservado: ",tmp)
      writeLines(qfi_filehash(arquivo),prova)
    }
    m <- data.table::data.table(arquivo=basename(arquivo),papel="detalhe",fonte=c$fonte,
      licenca=c$licenca,resolucao_nativa_m=c$resolucao_nativa_m,data_imagem=c$data_imagem,ativo="S")
    data.table::fwrite(m,file.path(pasta,"fontes_imagens.csv"))
    status[[length(status)+1L]] <- data.table::data.table(UC=uc,status=motivo,arquivo=arquivo)
  }
  data.table::rbindlist(status)
}
qfi_executar <- function(config) {
  inicio <- Sys.time(); qfi_deps(); c <- qfi_validar(config)
  original <- qfi_filehash(c$gpkg)
  c$saida <- normalizePath(c$saida,winslash="/",mustWork=FALSE)
  dir.create(c$saida,recursive=TRUE,showWarnings=FALSE)
  trava <- file.path(c$saida,"EM_EXECUCAO")
  if (!dir.create(trava,showWarnings=FALSE)) qfi_erro("Outra execucao/trava existe: ",trava,
    ". Se houve fechamento abrupto, remova SOMENTE essa pasta vazia apos confirmar que o script parou.")
  # Remove apenas a pasta vazia de trava criada por esta execucao.
  on.exit(if (dir.exists(trava) && !length(list.files(trava,all.files=TRUE,no..=TRUE)))
    unlink(trava,recursive=TRUE),add=TRUE)
  message("Planejando entornos de ",c$raio_m," m; nenhum download nesta etapa...")
  pl <- qfi_planejar(c); p <- pl$plano
  fonte_id <- substr(qfi_hash(list(c$url_xyz,c$cabecalhos,c$fonte,c$versao_acervo)),1,24)
  # Cache e logs nao armazenam URL, headers ou credenciais em texto.
  pasta_cache <- file.path(c$saida,"cache",fonte_id); dir.create(pasta_cache,recursive=TRUE,showWarnings=FALSE)
  db <- qfi_abrir_cache(file.path(pasta_cache,"tiles.sqlite")); on.exit(DBI::dbDisconnect(db),add=TRUE)
  unicos <- unique(p[,c("z","x","y"),with=FALSE]); data.table::setorderv(unicos,c("z","x","y"))
  tem <- data.table::as.data.table(DBI::dbGetQuery(db,"SELECT z,x,y FROM tiles"))
  faltam <- unicos[!tem,on=c("z","x","y")]
  id <- substr(qfi_hash(list(fonte_id,p,c$raio_m,c$licenca,c$atribuicao,c$data_imagem,c$resolucao_nativa_m)),1,20)
  auditoria <- file.path(c$saida,"planos",id); dir.create(auditoria,recursive=TRUE,showWarnings=FALSE)
  data.table::fwrite(p,file.path(auditoria,"tiles_por_uc.csv"))
  resumo <- merge(pl$feicoes,p[,list(tiles=.N),by=UC],by="UC")
  resumo[, estimativa_MiB := round(tiles*c$estimativa_kb_tile/1024,1)]
  data.table::fwrite(resumo,file.path(auditoria,"resumo_por_uc.csv"))
  message("UCs: ",nrow(resumo)," | tiles unicos: ",nrow(unicos)," | em cache: ",nrow(unicos)-nrow(faltam),
    " | faltantes: ",nrow(faltam)," | estimativa de download: ",round(nrow(faltam)*c$estimativa_kb_tile/1024,1)," MiB.")
  message("Estimativa minima de tempo (somente intervalo): ",round(nrow(faltam)*c$intervalo_s/60,1)," min; rede/gravacao acrescem tempo.")
  writeLines(c(paste("Fonte:",c$fonte),paste("Licenca:",c$licenca),paste("Raio_m:",c$raio_m),
    paste("Zooms:",paste(c$zooms,collapse=",")),paste("GPKG_SHA256:",original),paste("Fonte_id:",fonte_id),
    "Volume e estimativa, nao garantia. Cache e exportacao ocupam espaco adicional.",
    "Tiles cobrem o buffer por intersecao; suas bordas podem ultrapassar o raio em ate um tile.",
    "Cobertura de tiles nao comprova ausencia de nuvens, atualidade ou qualidade do terreno.",
    "O script usa a geometria fornecida: pontos nao inventam o outro extremo de uma transeccao."),
    file.path(auditoria,"LEIA_ME.txt"))
  if (!c$executar_download) {
    message("PLANEJAMENTO CONCLUIDO. Confira ",auditoria,"; altere executar_download=TRUE para baixar.")
    return(invisible(list(plano=p,resumo=resumo,auditoria=auditoria)))
  }
  falhas <- list(); consecutivas <- 0L
  data.table::fwrite(data.table::data.table(z=integer(),x=integer(),y=integer(),motivo=character()),
    file.path(auditoria,"falhas.csv"))
  tamanho_fonte <- DBI::dbGetQuery(db,"SELECT DISTINCT formato,largura,altura FROM tiles")
  for (i in seq_len(nrow(faltam))) {
    t <- faltam[i]; r <- qfi_buscar(c,t$z,t$x,t$y)
    if (r$ok) {
      a <- data.frame(formato=r$formato,largura=as.integer(r$largura),altura=as.integer(r$altura))
      if (nrow(tamanho_fonte) && !identical(a,tamanho_fonte)) qfi_erro("Fonte mudou formato/dimensoes. Cache preservado; nao misturar acervos.")
      tamanho_fonte <- a; qfi_cache_put(db,t$z,t$x,t$y,r); consecutivas <- 0L
    } else {
      falhas[[length(falhas)+1L]] <- data.table::data.table(z=t$z,x=t$x,y=t$y,motivo=r$motivo)
      data.table::fwrite(data.table::rbindlist(falhas),file.path(auditoria,"falhas.csv"))
      consecutivas <- consecutivas+1L
      if (consecutivas >= 10L) qfi_erro("Dez falhas consecutivas; confira fonte/cobertura. Cache e falhas.csv preservados.")
    }
    if (i == 1L || i %% 25L == 0L || i == nrow(faltam))
      message("Download ",i,"/",nrow(faltam)," (",round(100*i/nrow(faltam),1),"%); falhas: ",length(falhas),
        "; decorrido: ",round(as.numeric(difftime(Sys.time(),inicio,units="mins")),1)," min.")
  }
  resultado <- qfi_exportar(db,p,c,c$saida,id)
  data.table::fwrite(resultado,file.path(auditoria,"resultado_por_uc.csv"))
  if (!identical(original,qfi_filehash(c$gpkg))) qfi_erro("GPKG foi modificado durante a execucao por processo externo; revise a entrada.")
  message("Fim: ",sum(resultado$status == "completo"),"/",nrow(resultado)," UCs completas. Resultado: ",auditoria)
  if (any(resultado$status != "completo")) warning("Existem UCs incompletas; nao foram entregues MBTiles parciais. Veja resultado_por_uc.csv.",call.=FALSE)
  invisible(list(plano=p,resultado=resultado,auditoria=auditoria))
}


monitora_qfield_info_raster <- function(path) {
  if (!identical(readBin(path, what = "raw", n = 16L), c(charToRaw("SQLite format 3"), as.raw(0)))) stop("QField: assinatura MBTiles/SQLite inválida; nenhum raster foi aberto.", call. = FALSE)
  jsonlite::fromJSON(sf::gdal_utils("info", path, options = c("-json", "-nomd", "-if", "MBTiles"), quiet = TRUE), simplifyVector = FALSE)
}
monitora_qfield_mbtiles <- function(rgb, destino, descricao, zoom_max = 14L) {
  r <- terra::rast(rgb)
  if (terra::nlyr(r) < 3L || !nzchar(terra::crs(r))) stop("QField: raster deve ser RGB georreferenciado.", call. = FALSE)
  bb <- terra::ext(terra::project(terra::as.polygons(terra::ext(r), crs = terra::crs(r)), "EPSG:3857"))
  resolucao <- 156543.03392804097 / 2^zoom_max
  estimativa <- ceiling((bb$xmax - bb$xmin) / resolucao) * ceiling((bb$ymax - bb$ymin) / resolucao)
  if (!is.finite(estimativa) || estimativa > 120000000) stop("QField: orçamento de 120 milhões de pixels excedido; reduzir extensão, não a qualidade silenciosamente.", call. = FALSE)
  sf::gdal_utils("warp", rgb, destino, options = c("-of", "MBTiles", "-t_srs", "EPSG:3857", "-tr", format(resolucao, digits = 16), format(resolucao, digits = 16), "-r", "bilinear", "-dstalpha", "-co", "TILE_FORMAT=PNG", "-co", paste0("DESCRIPTION=", descricao), "-wm", "64"), quiet = TRUE, config_options = c(GDAL_NUM_THREADS = "1", GDAL_HTTP_TIMEOUT = "60", GDAL_HTTP_CONNECTTIMEOUT = "15"))
  sf::gdal_addo(destino, overviews = as.integer(2^seq_len(max(1L, zoom_max - 8L))), method = "AVERAGE", read_only = FALSE)
  if (!file.exists(destino) || file.info(destino)$size <= 0) stop("QField: MBTiles não materializado.", call. = FALSE)
  invisible(monitora_qfield_info_raster(destino))
}
monitora_qfield_sentinel <- function(pontos, scratch, limite = NULL) {
  pt <- sf::st_transform(pontos, 4326)
  b <- sf::st_bbox(pt); lon <- mean(b[c(1, 3)]); lat <- mean(b[c(2, 4)])
  crs_local <- (if (lat < 0) 32700L else 32600L) + floor((lon + 180) / 6) + 1L
  alvo <- sf::st_union(sf::st_geometry(sf::st_transform(if (is.null(limite)) pt else limite, crs_local)))
  # O limite recebido já contém a margem de contexto configurada.
  b <- sf::st_bbox(sf::st_transform(alvo, 4326))
  bbm <- sf::st_bbox(sf::st_transform(alvo, 3857))
  resolucao <- 156543.03392804097 / 2^14
  npix <- prod(ceiling(c(bbm[[3]] - bbm[[1]], bbm[[4]] - bbm[[2]]) / resolucao))
  if (!is.finite(npix) || npix > 120000000) stop("QField: área Sentinel excede orçamento de 120 milhões de pixels antes da aquisição.", call. = FALSE)
  bbox <- paste(format(as.numeric(b), scientific = FALSE, digits = 12), collapse = ",")
  url <- "https://earth-search.aws.element84.com/v1/search"
  res <- httr::GET(url, query = list(collections = "sentinel-2-l2a", bbox = bbox, datetime = paste0(Sys.Date() - 365, "T00:00:00Z/", Sys.Date(), "T23:59:59Z"), limit = 100), httr::timeout(45))
  httr::stop_for_status(res)
  features <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)$features
  if (!length(features)) stop("QField: Sentinel sem cenas na janela de 365 dias.", call. = FALSE)
  clouds <- vapply(features, function(f) if (is.null(f$properties[["eo:cloud_cover"]])) Inf else as.numeric(f$properties[["eo:cloud_cover"]]), numeric(1))
  dates <- vapply(features, function(f) as.character(f$properties$datetime), character(1))
  ordem <- order(clouds, -as.numeric(as.POSIXct(dates, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")))
  features <- features[ordem]
  arquivos <- character(); metas <- list(); tiles <- character()
  for (f in features) {
    tile <- paste(f$properties[["mgrs:utm_zone"]], f$properties[["mgrs:latitude_band"]], f$properties[["mgrs:grid_square"]])
    if (!length(tile) || !nzchar(tile)) tile <- f$id
    if (is.null(f$assets$visual$href)) next
    href <- f$assets$visual$href
    if (!grepl("^https://sentinel-cogs\\.s3\\.[a-z0-9-]+\\.amazonaws\\.com/", href)) stop("QField: host COG fora da fonte Sentinel permitida.", call. = FALSE)
    if (length(arquivos) >= 16L) stop('Contexto Sentinel excede 16 cenas; divida o projeto.',call.=FALSE)
    if (tile %in% tiles) next
    dst <- file.path(scratch, paste0("sentinel_", length(arquivos) + 1L, ".tif"))
    message('Sentinel: obtendo cena ',length(arquivos)+1L,' / tile ',tile,'; ',f$properties$datetime)
    sf::gdal_utils("warp", paste0("/vsicurl/", href), dst, options = c("-t_srs", "EPSG:3857", "-te", as.character(bbm), "-tr", as.character(resolucao), as.character(resolucao), "-r", "bilinear", "-dstalpha", "-co", "COMPRESS=DEFLATE", "-wm", "64"), quiet = TRUE, config_options = c(GDAL_NUM_THREADS = "1", GDAL_HTTP_TIMEOUT = "60", GDAL_HTTP_CONNECTTIMEOUT = "15", GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR", CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".tif"))
    arquivos <- c(arquivos, dst); tiles <- c(tiles, tile)
    metas[[length(metas) + 1L]] <- data.table::data.table(item = f$id, data = f$properties$datetime, nuvens_cena_pct = f$properties[["eo:cloud_cover"]], fonte = href)
    r <- terra::rast(dst)
    v <- terra::extract(r, terra::vect(pt), ID = FALSE)
    # A cobertura é validada para todo o contexto, não somente para os pontos.
  }
  if (!length(arquivos)) stop("QField: nenhum COG RGB disponível.", call. = FALSE)
  vrt <- file.path(scratch, "sentinel_rgb.vrt")
  sf::gdal_utils("buildvrt", rev(arquivos), vrt, options = c("-resolution", "highest"), quiet = TRUE)
  list(rgb = vrt, metadados = data.table::rbindlist(metas), nota = "Sentinel-2 L2A RGB nativo 10 m; seleção por nuvens da cena, não classificação local. Conferir nuvens e época da imagem; não equivale a alta resolução.")
}

# Progresso por etapa; não confundir conclusão do download com projeto concluído.
mq_progress <- function(name,n) {
  e<-new.env(parent=emptyenv());e$start<-proc.time()[3];e$last<-e$start;e$current<-0
  dynamic<-isTRUE(cli::is_dynamic_tty())
  id<-if(dynamic)cli::cli_progress_bar(name,total=n,clear=FALSE,format='{name} {cli::pb_bar} {cli::pb_percent} ({cli::pb_current}/{cli::pb_total}) | decorrido {cli::pb_elapsed} | restante {cli::pb_eta}')else NULL
  if(!dynamic)message('[',name,'] iniciado',if(is.na(n))'; duração ainda não estimada'else paste0('; ',n,' unidades'))
  list(id=id,dynamic=dynamic,n=n,name=name,state=e)
}
mq_progress_update <- function(id,set,status='') {
  id$state$current<-set
  if(id$dynamic)return(cli::cli_progress_update(id=id$id,set=set,status=status))
  now<-proc.time()[3];elapsed<-now-id$state$start
  if(set==1||set==id$n||now-id$state$last>=5) {
    pct<-100*set/id$n;eta<-if(set>0)elapsed*(id$n-set)/set else NA
    message(sprintf('[%s] [%s%s] %.1f%% | restante %.1f%% | %d/%d | %.0f s decorridos | ~%.0f s restantes %s',id$name,strrep('=',floor(pct/5)),strrep(' ',20-floor(pct/5)),pct,100-pct,set,id$n,elapsed,eta,status));id$state$last<-now
  }
}
mq_progress_done <- function(id) {
  if(id$dynamic)cli::cli_progress_done(id=id$id)else message('[',id$name,'] etapa encerrada; ',round(proc.time()[3]-id$state$start,1),' s',if(!is.na(id$n))paste0('; unidades ',id$state$current,'/',id$n)else '')
}
mq_centers <- function(layers,c,cr) {
  out<-list();pick<-function(role)Filter(function(l)l$papel%in%role && nrow(l$x)>0,layers)
  hasua<-length(pick(c('verg_ini','verg_fin','UAs')))>0
  scope<-if(c$centros_detalhe=='auto')if(hasua)'UAs'else 'PAs'else c$centros_detalhe
  add<-function(x,id,origin){out[[length(out)+1L]]<<-sf::st_sf(id_centro=id,origem=origin,geometry=sf::st_geometry(x))}
  if(scope%in%c('PAs','UAs_e_PAs'))for(l in pick(c('PA_priorit','PA_altern')))add(l$x,as.character(l$x[[l$label]]),l$nome)
  if(scope%in%c('UAs','UAs_e_PAs')) {
    ini<-pick('verg_ini');fin<-pick('verg_fin')
    if(!length(ini)||!length(fin))mq_stop('Centros UAs exigem verg_ini e verg_fin pareados; não é possível inferir ponto médio de uma camada UAs isolada.')
    key<-function(l)if(!is.null(l$ano)&&!is.na(l$ano)&&nzchar(l$ano))as.character(l$ano)else ''
    ki<-vapply(ini,key,character(1));kf<-vapply(fin,key,character(1))
    if(anyDuplicated(ki)||anyDuplicated(kf)||!setequal(ki,kf))mq_stop('Pares anuais de UAs ambíguos; declare ano e campo_rotulo no manifesto.')
    for(i in seq_along(ini)) {
      a<-ini[[i]];b<-fin[[match(ki[i],kf)]]
      if(!nzchar(a$label)||!nzchar(b$label))mq_stop('UAs exigem campo_rotulo identificador comum aos extremos.')
      ka<-as.character(a$x[[a$label]]);kb<-as.character(b$x[[b$label]])
      if(anyNA(ka)||anyNA(kb)||any(!nzchar(ka))||any(!nzchar(kb))||anyDuplicated(ka)||anyDuplicated(kb)||!setequal(ka,kb))mq_stop('Identificação dos extremos de UA ausente, duplicada ou sem par.')
      xy<-(sf::st_coordinates(sf::st_zm(a$x))[,1:2,drop=FALSE]+sf::st_coordinates(sf::st_zm(b$x[match(ka,kb),]))[,1:2,drop=FALSE])/2
      x<-sf::st_as_sf(data.frame(x=xy[,1],y=xy[,2]),coords=c('x','y'),crs=cr)
      add(x,paste(ka,ki[i],sep=' / '),'ponto_medio_UA_observada')
    }
  }
  if(!length(out))return(mq_empty(cr))
  x<-do.call(rbind,out);x<-sf::st_zm(x);attr(x,'abrangencia')<-scope;x
}
mq_mask <- function(centers,c) {
  if(!nrow(centers))return(sf::st_sfc(crs=sf::st_crs(centers)))
  xy<-sf::st_coordinates(centers);g<-sf::st_geometry(centers[order(xy[,1],xy[,2]),]);g<-unique(g)
  sf::st_union(sf::st_buffer(g,c$raio_detalhe_m,nQuadSegs=90))
}
mq_verified <- function(p) file.exists(p)&&file.exists(paste0(p,'.sha256'))&&identical(mq_hash(p),readLines(paste0(p,'.sha256'),warn=FALSE))
mq_seal <- function(p){writeLines(mq_hash(p),paste0(p,'.sha256'));p}
mq_rgba <- function(raw) {
  if(identical(raw[1:8],as.raw(c(137,80,78,71,13,10,26,10))))a<-png::readPNG(raw)
  else if(identical(raw[1:2],as.raw(c(255,216))))a<-jpeg::readJPEG(raw)
  else {p<-tempfile(fileext='.webp');on.exit(unlink(p));writeBin(raw,p);a<-as.array(terra::rast(p))/255}
  if(length(dim(a))!=3 || !dim(a)[3]%in%c(3,4))mq_stop('Tile sem RGB/RGBA.')
  if(dim(a)[3]==3){b<-array(1,c(dim(a)[1:2],4));b[,,1:3]<-a;a<-b};a
}
mq_clip_mb <- function(src,mask,c,report) {
  key<-digest::digest(list(mq_hash(src),sf::st_as_binary(mask),sf::st_crs(mask)$wkt,'rgba_png_v1'))
  dest<-file.path(c$cache_dir,'recortes',paste0(key,'.mbtiles'));dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE)
  if(mq_verified(dest))return(dest)
  if(file.exists(dest))mq_stop('Recorte em cache sem integridade: ',dest)
  con<-DBI::dbConnect(RSQLite::SQLite(),src,flags=RSQLite::SQLITE_RO);on.exit(DBI::dbDisconnect(con),add=TRUE)
  tiles<-DBI::dbGetQuery(con,'SELECT zoom_level AS z,tile_column AS x,tile_row AS t FROM tiles ORDER BY zoom_level,tile_column,tile_row');tiles$y<-2^tiles$z-1-tiles$t
  geom<-qfi_poligonos_tiles(tiles);buffer<-sf::st_transform(mask,3857);ids<-which(lengths(sf::st_intersects(geom,buffer))>0)
  if(!length(ids))return(NULL)
  tmp<-paste0(dest,'.parcial');if(file.exists(tmp))unlink(tmp)
  db<-DBI::dbConnect(RSQLite::SQLite(),tmp);on.exit(if(DBI::dbIsValid(db))DBI::dbDisconnect(db),add=TRUE)
  DBI::dbExecute(db,'CREATE TABLE metadata(name TEXT PRIMARY KEY,value TEXT)');DBI::dbExecute(db,'CREATE TABLE tiles(zoom_level INTEGER,tile_column INTEGER,tile_row INTEGER,tile_data BLOB,PRIMARY KEY(zoom_level,tile_column,tile_row))')
  meta<-DBI::dbGetQuery(con,'SELECT name,value FROM metadata');meta<-meta[!meta$name%in%c('format','recorte_circular_m','recorte_fonte_sha256','recorte_buffers_sha256','bounds'),]
  bb<-sf::st_bbox(sf::st_transform(mask,4326));meta<-rbind(meta,data.frame(name=c('format','recorte_circular_m','recorte_fonte_sha256','recorte_buffers_sha256','bounds'),value=c('png',as.character(c$raio_detalhe_m),mq_hash(src),key,paste(as.numeric(bb),collapse=','))))
  DBI::dbWriteTable(db,'metadata',meta,append=TRUE);bar<-mq_progress('Recorte 500 m',length(ids));on.exit(mq_progress_done(id=bar),add=TRUE);n<-0L
  DBI::dbBegin(db)
  for(j in seq_along(ids)) {
    i<-ids[j];t<-tiles[i,];raw<-DBI::dbGetQuery(con,'SELECT tile_data FROM tiles WHERE zoom_level=? AND tile_column=? AND tile_row=?',params=list(t$z,t$x,t$t))$tile_data[[1]]
    a<-mq_rgba(raw);bb<-sf::st_bbox(geom[i]);r<-terra::rast(nrows=dim(a)[1],ncols=dim(a)[2],xmin=bb[1],xmax=bb[3],ymin=bb[2],ymax=bb[4],crs='EPSG:3857')
    m<-as.matrix(terra::rasterize(terra::vect(buffer),r,field=1,background=0),wide=TRUE)
    a[,,4]<-a[,,4]*m
    if(any(a[,,4]>0)){for(k in 1:3)a[,,k][a[,,4]==0]<-0;blob<-png::writePNG(a,target=raw());DBI::dbExecute(db,'INSERT INTO tiles VALUES(?,?,?,?)',params=list(t$z,t$x,t$t,list(blob)));n<-n+1L}
    mq_progress_update(id=bar,set=j)
  }
  DBI::dbCommit(db);if(DBI::dbGetQuery(db,'PRAGMA integrity_check')[[1]]!='ok')mq_stop('Recorte MBTiles inconsistente.')
  DBI::dbDisconnect(db)
  if(!n){unlink(tmp);return(NULL)}
  if(file.info(tmp)$size>950*1024^2)message('Recorte >950 MiB; consolidação por tiles, sem divisão da camada. Confira espaço no aparelho.')
  if(!file.rename(tmp,dest))mq_stop('Falha ao finalizar recorte.');mq_seal(dest)
}
# Tiles locais completamente opacos substituem download, sem misturar sua origem ao cache remoto.
mq_available_tiles <- function(paths) {
  result<-list()
  for(p in paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),p,flags=RSQLite::SQLITE_RO)
    tryCatch({
      meta<-DBI::dbGetQuery(con,'SELECT name,value FROM metadata');d<-DBI::dbGetQuery(con,'SELECT zoom_level AS z,tile_column AS x,tile_row AS t FROM tiles');d$y<-2^d$z-1-d$t
      fmt<-meta$value[match('format',meta$name)]
      if(!fmt%in%c('jpg','jpeg')) {
        good<-vapply(seq_len(nrow(d)),function(i){t<-d[i,];raw<-DBI::dbGetQuery(con,'SELECT tile_data FROM tiles WHERE zoom_level=? AND tile_column=? AND tile_row=?',params=list(t$z,t$x,t$t))$tile_data[[1]];all(mq_rgba(raw)[,,4]>0)},logical(1));d<-d[good,]
      }
      result[[length(result)+1]]<-d[,c('z','x','y')]
    },finally=DBI::dbDisconnect(con))
  }
  if(length(result))unique(data.table::as.data.table(do.call(rbind,result)))else data.table::data.table(z=integer(),x=integer(),y=integer())
}
mq_download <- function(centers,provided,c,scratch,report) {
  if(!nrow(centers))return(list(paths=character(),status='sem_centros'))
  if(!isTRUE(c$baixar_imagem_detalhe))c$confirmar_download<-FALSE
  gp<-file.path(scratch,'centros_download.gpkg');x<-centers;x$UC<-'Projeto';sf::st_write(x,gp,layer='centros',quiet=TRUE)
  cfg<-list(gpkg=gp,camada='centros',campo_uc='UC',url_xyz=c$url_xyz,cabecalhos=c$cabecalhos_xyz,fonte=c$fonte_xyz,licenca=c$licenca_xyz,atribuicao=c$atribuicao_xyz,data_imagem='não informada',resolucao_nativa_m='não informada',versao_acervo=c$versao_acervo,saida=c$cache_dir,raio_m=c$raio_detalhe_m,zooms=c$zooms_detalhe,executar_download=TRUE,intervalo_s=c$intervalo_download_s,tentativas=c$tentativas_download,timeout_s=30,max_tiles=c$max_tiles,max_tiles_por_feicao=10000,estimativa_kb_tile=100,max_mib_mbtiles=950)
  qfi_validar(cfg);message('Planejando tiles e conferindo cache; nenhum download de detalhe iniciado.')
  plan<-qfi_planejar(cfg)$plano;local<-mq_available_tiles(provided);required<-plan[!local,on=c('z','x','y')]
  sourceid<-substr(qfi_hash(list(cfg$url_xyz,cfg$cabecalhos,cfg$fonte,cfg$versao_acervo)),1,24)
  cache<-file.path(c$cache_dir,'cache',sourceid);dir.create(cache,recursive=TRUE,showWarnings=FALSE)
  db<-qfi_abrir_cache(file.path(cache,'tiles.sqlite'));on.exit(DBI::dbDisconnect(db),add=TRUE)
  # Importar apenas caches com fingerprint da mesma fonte/acervo. Headers nunca são gravados.
  legacy_ids<-sourceid
  if(grepl('^https://mt1.google.com/vt/',cfg$url_xyz))legacy_ids<-unique(c(sourceid,substr(qfi_hash(list(cfg$url_xyz,cfg$cabecalhos,'wms',cfg$versao_acervo)),1,24)))
  for(root in c$caches_adicionais) {
    others<-list.files(root,pattern='tiles.sqlite$',recursive=TRUE,full.names=TRUE)
    for(p in others[basename(dirname(others))%in%legacy_ids])if(normalizePath(p,winslash='/',mustWork=FALSE)!=normalizePath(file.path(cache,'tiles.sqlite'),winslash='/',mustWork=FALSE)) {
      old<-DBI::dbConnect(RSQLite::SQLite(),p,flags=RSQLite::SQLITE_RO)
      tryCatch({for(i in seq_len(nrow(required))){t<-required[i];if(is.null(qfi_cache_get(db,t$z,t$x,t$y))){a<-qfi_cache_get(old,t$z,t$x,t$y);if(!is.null(a))qfi_cache_put(db,t$z,t$x,t$y,list(raw=a$data[[1]],formato=a$formato,largura=a$largura,altura=a$altura))}}},finally=DBI::dbDisconnect(old))
    }
  }
  present<-vapply(seq_len(nrow(required)),function(i){t<-required[i];!is.null(qfi_cache_get(db,t$z,t$x,t$y))},logical(1));missing<-required[!present]
  estimate<-list(centros=nrow(centers),abrangencia=attr(centers,'abrangencia'),tiles=nrow(plan),locais=nrow(plan)-nrow(required),cache=sum(present),faltantes=nrow(missing),MiB_estimados=round(nrow(missing)*100/1024,1),minutos_minimos_intervalo=round(nrow(missing)*cfg$intervalo_s/60,1))
  mq_json(estimate,file.path(report,'plano_download.json'));mq_csv(plan,file.path(report,'tiles_planejados.csv'))
  message(sprintf('DETALHE: %d centros | %d tiles locais | %d em cache | %d a baixar | ~%.1f MiB | mínimo %.1f min só de intervalos; rede, gravação e recorte acrescentam tempo.',estimate$centros,estimate$locais,estimate$cache,estimate$faltantes,estimate$MiB_estimados,estimate$minutos_minimos_intervalo))
  accepted<-c$confirmar_download
  if(nrow(missing)&&is.null(accepted)) {
    if(!interactive())mq_stop('Download requer confirmação: use confirmar_download=TRUE para autorizar ou FALSE para seguir com fontes locais. Consulte plano_download.json.')
    answer<-toupper(trimws(readline('Baixar detalhe? [S] sim / [N] continuar sem novo download / [C] cancelar: ')))
    if(!answer%in%c('S','N'))mq_stop('Execução cancelada; nenhum novo download de detalhe autorizado.');accepted<-answer=='S'
  }
  failures<-list();st<-'cache_completo'
  if(nrow(missing)&&isTRUE(accepted)) {
    bar<-mq_progress('Download detalhe',nrow(missing));on.exit(mq_progress_done(id=bar),add=TRUE)
    for(i in seq_len(nrow(missing))) {
      t<-missing[i];r<-tryCatch(qfi_buscar(cfg,t$z,t$x,t$y),error=function(e)list(ok=FALSE,motivo=conditionMessage(e),interromper=TRUE))
      if(isTRUE(r$ok))qfi_cache_put(db,t$z,t$x,t$y,r)else failures[[length(failures)+1]]<-data.frame(z=t$z,x=t$x,y=t$y,motivo=r$motivo)
      mq_progress_update(id=bar,set=i,status=paste(length(failures),'falhas'))
      if(isTRUE(r$interromper)||length(failures)>=10)break
    }
    st<-if(length(failures))'download_incompleto'else 'download_completo'
  }else if(nrow(missing))st<-'download_nao_autorizado'
  if(length(failures))mq_csv(do.call(rbind,failures),file.path(report,'falhas_download.csv'))
  good<-vapply(seq_len(nrow(required)),function(i){t<-required[i];!is.null(qfi_cache_get(db,t$z,t$x,t$y))},logical(1))
  ready<-required[good];paths<-character()
  if(nrow(ready)){id<-substr(qfi_hash(list(sourceid,ready)),1,20);ex<-qfi_exportar(db,ready,cfg,c$cache_dir,id);paths<-ex$arquivo[ex$status=='completo'];if(any(ex$status!='completo'))st<-'exportacao_detalhe_incompleta'}
  mq_json(list(status=st,faltantes_apos=sum(!good),novos_confirmados=isTRUE(accepted)),file.path(report,'resultado_download.json'))
  list(paths=paths,status=st)
}
mq_sentinel_covers <- function(path,context) {
  tryCatch({
    r<-terra::rast(path);if(terra::nlyr(r)<3)return(FALSE)
    if(inherits(context,'sfc'))context<-sf::st_sf(geometry=context)
    a<-terra::project(terra::vect(context),terra::crs(r));b<-terra::ext(a);e<-terra::ext(r)
    # Tolerância submicrométrica apenas para arredondamento de CRS; pixels continuam todos validados.
    eps<-1e-6
    if(b$xmin<e$xmin-eps||b$xmax>e$xmax+eps||b$ymin<e$ymin-eps||b$ymax>e$ymax+eps)return(FALSE)
    # Avaliar todos os pixels do contexto, não apenas os centros.
    z<-terra::crop(r,b);inside<-terra::rasterize(a,z[[1]],field=1,background=NA,touches=FALSE)
    valid<-if(terra::nlyr(z)>=4)!is.na(z[[4]])&z[[4]]>0 else !is.na(z[[1]])
    bad<-terra::global(terra::ifel(!is.na(inside)&!valid,1,0),'sum',na.rm=TRUE)[1,1]
    is.finite(bad)&&bad==0
  },error=function(e)FALSE)
}
mq_sentinel_evidence <- function(p) {
  side<-paste0(p,'.fonte.json')
  if(file.exists(side)) {
    d<-jsonlite::read_json(side,simplifyVector=TRUE)
    if(identical(d$produto,'Sentinel-2 L2A')&&identical(d$sha256,mq_hash(p))&&identical(as.numeric(d$resolucao_nativa_m),10))return(d)
    return(NULL)
  }
  # Compatibilidade com pacotes públicos Monitora: cenas registradas junto ao projeto.
  scenes<-file.path(dirname(dirname(p)),'cenas_sentinel.csv')
  if(!file.exists(scenes)||tolower(tools::file_ext(p))!='mbtiles')return(NULL)
  con<-DBI::dbConnect(RSQLite::SQLite(),p,flags=RSQLite::SQLITE_RO)
  meta<-tryCatch(DBI::dbGetQuery(con,'SELECT name,value FROM metadata'),finally=DBI::dbDisconnect(con))
  if(!any(grepl('Sentinel-2',meta$value[meta$name%in%c('description','name')],fixed=TRUE)))return(NULL)
  d<-data.table::fread(scenes);if(!all(c('item','data','fonte')%in%names(d))||!nrow(d)||any(!grepl('^https://sentinel-cogs\\.s3\\.',d$fonte)))return(NULL)
  list(produto='Sentinel-2 L2A',resolucao_nativa_m=10,sha256=mq_hash(p),versao_acervo='legado',cenas=as.data.frame(d),origem='cenas do pacote Monitora fornecido')
}
mq_sentinel <- function(context,points,c,scratch,report) {
  key<-digest::digest(list(sf::st_as_binary(context),sf::st_crs(context)$wkt,c$versao_acervo,'sentinel_z14_v2'))
  cache<-file.path(c$cache_dir,'sentinel');dir.create(cache,recursive=TRUE,showWarnings=FALSE)
  target<-file.path(cache,paste0(key,'.mbtiles'))
  candidates<-unique(c(c$sentinel_arquivo,target,list.files(cache,pattern='\\.mbtiles$',full.names=TRUE),unlist(lapply(c$caches_adicionais,function(d)list.files(d,pattern='\\.mbtiles$',recursive=TRUE,full.names=TRUE)))))
  for(p in candidates[file.exists(candidates)]) {
    ev<-tryCatch(mq_sentinel_evidence(p),error=function(e)NULL)
    explicit<-!is.null(c$sentinel_arquivo)&&identical(p,c$sentinel_arquivo)
    compatible<-!is.null(ev)&&(identical(ev$versao_acervo,c$versao_acervo)||(explicit&&!isTRUE(c$renovar_imagens)))
    if(compatible&&!isTRUE(c$renovar_imagens)&&mq_sentinel_covers(p,context)) {
      ev$acao<-'reutilizado';ev$arquivo<-p;mq_json(ev,file.path(report,'sentinel_fonte.json'));return(p)
    }
  }
  bb<-sf::st_bbox(sf::st_transform(context,3857));mib<-prod(c(bb[3]-bb[1],bb[4]-bb[2])/9.55462853565)*3/1024^2
  estimate<-list(RGB_bruto_MiB=round(mib,1),observacao='Tamanho RGB sem compressão; tráfego depende das cenas e blocos COG. Tempo depende da rede, pode levar minutos.')
  mq_json(estimate,file.path(report,'plano_sentinel.json'))
  message(sprintf('Sentinel offline: contexto ~%.1f MiB de RGB bruto. Tráfego pode ser maior; obtenção pode levar minutos. Cache compatível insuficiente.',mib))
  allowed<-c$confirmar_sentinel
  if(is.null(allowed)&&interactive())allowed<-identical(toupper(trimws(readline('Autorizar aquisição do Sentinel obrigatório? [S/N]: '))),'S')
  if(!isTRUE(allowed))mq_stop('Sentinel não autorizado e sem cache suficiente. Entrega mínima bloqueada; use confirmar_sentinel=TRUE após conferir plano_sentinel.json.')
  bar<-mq_progress('Sentinel contexto',NA);on.exit(mq_progress_done(id=bar),add=TRUE)
  a<-sf::st_sf(geometry=context)
  if(is.null(points)||!nrow(points))points<-suppressWarnings(sf::st_point_on_surface(a))
  s<-monitora_qfield_sentinel(points,scratch,limite=a)
  partial<-file.path(scratch,'sentinel_contexto.mbtiles');monitora_qfield_mbtiles(s$rgb,partial,s$nota)
  if(!mq_sentinel_covers(partial,context))mq_stop('Sentinel não cobre integralmente o contexto. Entrega mínima bloqueada; cache anterior preservado.')
  if(file.exists(target)&&!mq_verified(target))mq_stop('Sentinel em cache sem integridade: ',target)
  if(!file.copy(partial,target,overwrite=FALSE))mq_stop('Falha ao armazenar Sentinel no cache.');mq_seal(target)
  ev<-list(produto='Sentinel-2 L2A',resolucao_nativa_m=10,versao_acervo=c$versao_acervo,sha256=mq_hash(target),cenas=as.data.frame(s$metadados),nota=s$nota)
  mq_json(ev,paste0(target,'.fonte.json'));mq_csv(s$metadados,file.path(report,'cenas_sentinel.csv'));ev$acao<-'adquirido';ev$arquivo<-target;mq_json(ev,file.path(report,'sentinel_fonte.json'));target
}
mq_imagery <- function(input,root,layers,ae,coverage,c,scratch,report) {
  c$cache_dir<-normalizePath(c$cache_dir,winslash='/',mustWork=FALSE)
  if(c$renovar_imagens)c$versao_acervo<-paste(c$versao_acervo,format(Sys.time(),'%Y%m%d%H%M%S'),sep='_')
  # Uma trava protege downloads/recortes contra concorrência. Nunca limpa o cache.
  dir.create(c$cache_dir,recursive=TRUE,showWarnings=FALSE);lock<-file.path(c$cache_dir,'EM_EXECUCAO_QFIELD')
  if(!dir.create(lock,showWarnings=FALSE))mq_stop('Cache em uso ou trava remanescente: ',lock)
  on.exit(unlink(lock,recursive=TRUE),add=TRUE)
  centers<-mq_centers(layers,c,sf::st_crs(ae));mask<-mq_mask(centers,c)
  if(nrow(centers)) {
    sf::st_write(centers,file.path(report,'centros_recorte.gpkg'),layer='centros',quiet=TRUE)
    sf::st_write(sf::st_sf(raio_m=c$raio_detalhe_m,geometry=mask),file.path(report,'centros_recorte.gpkg'),layer='uniao_raios',quiet=TRUE,append=FALSE)
  }else message('Sem centros válidos para detalhe; grade completa não foi usada automaticamente.')
  local<-mq_files(input,'\\.mbtiles$')
  detail<-character();regional<-character()
  for(p in local) {
    con<-DBI::dbConnect(RSQLite::SQLite(),p,flags=RSQLite::SQLITE_RO)
    z<-tryCatch(DBI::dbGetQuery(con,'SELECT max(zoom_level) AS z FROM tiles')$z,finally=DBI::dbDisconnect(con))
    if(is.finite(z)&&z>=16)detail<-c(detail,p)else regional<-c(regional,p)
  }
  download<-mq_download(centers,detail,c,scratch,report)
  detail<-unique(c(detail,download$paths));cuts<-character();audit<-list()
  for(p in detail)if(length(mask)&&!all(sf::st_is_empty(mask))) {
    cut<-mq_clip_mb(p,mask,c,report)
    audit[[length(audit)+1]]<-data.frame(fonte=p,sha256_fonte=mq_hash(p),recorte=if(is.null(cut))''else cut,raio_m=c$raio_detalhe_m,centros=nrow(centers),bytes_antes=file.info(p)$size,bytes_depois=if(is.null(cut))0 else file.info(cut)$size)
    if(!is.null(cut))cuts<-c(cuts,cut)
  }
  if(length(audit))mq_csv(do.call(rbind,audit),file.path(report,'auditoria_recorte.csv'))
  ucs<-Filter(function(l)l$papel=='limites_uc',layers)
  context<-sf::st_geometry(ae);for(l in ucs)context<-c(context,sf::st_geometry(l$x))
  if(nrow(centers))context<-c(context,mask)
  context<-sf::st_buffer(sf::st_union(context),c$margem_contexto_m)
  sentinel<-mq_sentinel(context,coverage,c,scratch,report)
  additional<-mq_files(input,'\\.(tif|tiff|img|asc)$')
  cuts<-if(length(cuts))mq_mosaic_mb(cuts,c,report,mask)else character()
  paths<-c(cuts,additional,regional,sentinel)
  ras<-mq_rasters(input,file.path(root,'01_qfield/mapas'),coverage,report,paths=paths)
  for(i in seq_along(ras)) {
    if(i<=length(cuts))ras[[i]]$nome<-'sat_escala_local'
    if(i==length(ras))ras[[i]]$nome<-'sat_escala_regional'
    target<-if(ras[[i]]$nome=='sat_escala_local')'detalhe.mbtiles'else if(ras[[i]]$nome=='sat_escala_regional')'regional.mbtiles'else ras[[i]]$arquivo
    if(target!=ras[[i]]$arquivo){if(!file.rename(file.path(root,'01_qfield/mapas',ras[[i]]$arquivo),file.path(root,'01_qfield/mapas',target)))mq_stop('Falha ao padronizar imagem.');ras[[i]]$arquivo<-target}
    ras[[i]]$descricao<-paste('Fonte e datas: relatório de execução.',if(ras[[i]]$nome=='sat_escala_local')'União dos raios de 500 m; fontes em mosaico_fontes.json. Resolução nativa não inferida.'else if(ras[[i]]$nome=='sat_escala_regional')'Sentinel-2 L2A RGB nativo 10 m; contexto regional completo. Cenas/procedência em sentinel_fonte.json.'else '',ras[[i]]$descricao)
  }
  if(file.exists(file.path(report,'imagens.csv'))){im<-data.table::fread(file.path(report,'imagens.csv'));im$destino<-vapply(ras,`[[`,character(1),'arquivo');im$camada<-vapply(ras,`[[`,character(1),'nome');mq_csv(im,file.path(report,'imagens.csv'))}
  attr(ras,'download_status')<-download$status
  attr(ras,'contexto')<-'Sentinel cobre integralmente o contexto de UC/AEs/raios; Google Satellite online incluído desligado.'
  ras
}

# Ler apenas os tiles dos pontos: evita materializar a extensão inteira de MBTiles esparsos.
mq_mb_visible <- function(path,points) {
  con<-DBI::dbConnect(RSQLite::SQLite(),path,flags=RSQLite::SQLITE_RO);on.exit(DBI::dbDisconnect(con),add=TRUE)
  z<-DBI::dbGetQuery(con,'SELECT max(zoom_level) AS z FROM tiles')$z;h<-20037508.342789244
  xy<-sf::st_coordinates(sf::st_transform(points,3857));fx<-(xy[,1]+h)/(2*h)*2^z;fy<-(h-xy[,2])/(2*h)*2^z
  tx<-floor(fx);ty<-floor(fy);visible<-rep(FALSE,nrow(points));valid<-is.finite(tx)&is.finite(ty)&tx>=0&ty>=0&tx<2^z&ty<2^z
  groups<-split(which(valid),paste(tx[valid],ty[valid],sep='/'))
  for(ix in groups) {
    d<-DBI::dbGetQuery(con,'SELECT tile_data FROM tiles WHERE zoom_level=? AND tile_column=? AND tile_row=?',params=list(z,tx[ix[1]],2^z-1-ty[ix[1]]))
    if(!nrow(d))next
    a<-mq_rgba(d$tile_data[[1]]);nr<-dim(a)[1];nc<-dim(a)[2]
    row<-pmin(nr,floor((fy[ix]-ty[ix])*nr)+1L);col<-pmin(nc,floor((fx[ix]-tx[ix])*nc)+1L)
    visible[ix]<-a[cbind(row,col,rep(4L,length(ix)))]>0
  }
  visible
}

mq_rasters <- function(entrada,destino,points,report,paths=NULL) {
  files<-mq_files(entrada,'\\.(mbtiles|tif|tiff|img|asc)$')
  if(!is.null(paths))files<-paths
  out<-list();aud<-list();coverage_all<-if(is.null(points))logical()else rep(FALSE,nrow(points))
  for(f in files) {
    monitora_qfield_caminho_local(basename(f),dirname(f))
    ext<-tolower(tools::file_ext(f));r<-terra::rast(f)
    if(!nzchar(terra::crs(r)))mq_stop('Raster sem CRS: ',basename(f))
    meta<-list();zmin<-zmax<-NA_integer_
    if(ext=='mbtiles') {
      con<-DBI::dbConnect(RSQLite::SQLite(),f,flags=RSQLite::SQLITE_RO)
      info<-tryCatch({
        if(!all(c('metadata','tiles')%in%DBI::dbListTables(con)))mq_stop('MBTiles sem tabelas obrigatórias.')
        if(DBI::dbGetQuery(con,'PRAGMA quick_check')[[1]][1]!='ok')mq_stop('MBTiles corrompido.')
        m<-DBI::dbGetQuery(con,'SELECT name,value FROM metadata');z<-DBI::dbGetQuery(con,'SELECT min(zoom_level) as zmin,max(zoom_level) as zmax,count(*) as n FROM tiles')
        if(z$n<1 || !any(m$name=='format' & m$value%in%c('jpg','jpeg','png','webp')))mq_stop('MBTiles raster vazio/formato inválido.')
        list(m=m,z=z)
      },finally=DBI::dbDisconnect(con))
      meta<-info$m;zmin<-info$z$zmin;zmax<-info$z$zmax
    }
    nm<-sprintf('%02d_%s.%s',length(out)+1,substr(monitora_qfield_slug(tools::file_path_sans_ext(basename(f))),1,32),ext)
    dst<-file.path(destino,nm)
    if(!file.copy(f,dst,overwrite=FALSE) || mq_hash(f)!=mq_hash(dst))mq_stop('Falha ao copiar raster sem alterações.')
    # Rasters auxiliares com sidecars obrigatórios são materializados em GeoTIFF autossuficiente.
    if(ext%in%c('asc','img') || (ext%in%c('tif','tiff') && any(file.exists(c(paste0(f,'.aux.xml'),paste0(tools::file_path_sans_ext(f),'.tfw')))))) {
      gt<-paste0(tools::file_path_sans_ext(dst),'_autossuficiente.tif');terra::writeRaster(r,gt,overwrite=FALSE);unlink(dst);dst<-gt;nm<-basename(gt);r<-terra::rast(gt)
    }
    covered<-NA_integer_
    if(!is.null(points)&&nrow(points)) {
      if(ext=='mbtiles')visible<-mq_mb_visible(f,points)else {p<-terra::project(terra::vect(points),terra::crs(r));val<-terra::extract(r,p,method='simple');visible<-rowSums(!is.na(val[,-1,drop=FALSE]))>0 & if(terra::nlyr(r)>=4)!is.na(val[[5]]) & val[[5]]>0 else TRUE};covered<-sum(visible);coverage_all<-coverage_all|visible
    }
    pixel<-terra::res(r);sr<-sf::st_crs(terra::crs(r));lat<-if(!is.null(points)&&nrow(points))mean(sf::st_bbox(sf::st_transform(points,4326))[c('ymin','ymax')])else NA_real_;solo<-if(identical(sr$epsg,3857L)&&is.finite(lat))pixel[1]*cos(lat*pi/180)else if(sr$units_gdal%in%c('metre','meter','m'))pixel[1]else NA_real_
    description<-paste('Pixel no CRS:',paste(signif(pixel,6),collapse=' x '),sr$units_gdal,'; pixel aproximado no terreno:',signif(solo,4),'m; resolução nativa não deduzida da grade.')
    out[[length(out)+1]]<-list(descricao=description,arquivo=nm,nome=if(ext=='mbtiles')paste0('Imagem_',length(out)+1,'_z',zmax)else paste0('Raster_',length(out)+1),epsg=sf::st_crs(terra::crs(r))$epsg,wkt=terra::crs(r),bandas=terra::nlyr(r),ativo=TRUE)
    aud[[length(aud)+1]]<-data.frame(arquivo=basename(f),destino=nm,sha256=mq_hash(dst),zoom_min=zmin,zoom_max=zmax,pixel_x_crs=pixel[1],pixel_y_crs=pixel[2],pixel_solo_m=solo,pontos_com_valor=covered,pontos_avaliados=if(is.null(points))0 else nrow(points),observacao='Valor no ponto não garante cobertura da transecção, acessos ou ausência de nuvens. Resolução nativa não inferida pelo zoom.')
  }
  if(length(aud))mq_csv(do.call(rbind,aud),file.path(report,'imagens.csv'))
  attr(out,'cobertura')<-list(avaliados=length(coverage_all),cobertos=sum(coverage_all));out
}

mq_style <- function(role,type) {
  e<-monitora_qfield_xml
  opt<-function(k,v)paste0('<Option name="',k,'" value="',e(v),'" type="QString"/>')
  color<-switch(role,PA_priorit='227,26,28,255',PA_altern='253,191,111,255',grade_amostral='255,255,255,255',verg_fin='255,255,255,255',pontos_interesse='141,90,255,255','31,120,180,255')
  border<-if(role=='grade_amostral')'35,35,35,255'else if(role=='verg_fin')'31,120,180,255'else '255,255,255,255'
  if(type=='Point'){cls<-'SimpleMarker';sym<-'marker';props<-c(opt('name','circle'),opt('color',color),opt('outline_color',border),opt('outline_width','0.25'),opt('size','2'),opt('size_unit','MM'))}
  else if(type=='Line'){cls<-'SimpleLine';sym<-'line';props<-c(opt('line_color',if(role=='trajeto')'141,90,255,255'else if(role=='transectos')'227,26,28,255'else '0,135,104,255'),opt('line_width','0.3'),opt('line_width_unit','MM'))}
  else {cls<-'SimpleFill';sym<-'fill';props<-c(opt('style','no'),opt('outline_style','solid'),opt('outline_color',if(role=='limites_uc')'255,255,0,255'else '0,255,0,255'),opt('outline_width',if(role=='limites_uc')'0.60'else '0.30'),opt('outline_width_unit','MM'))}
  paste0('<renderer-v2 type="singleSymbol"><symbols><symbol name="0" type="',sym,'" alpha="1"><layer class="',cls,'" enabled="1"><Option type="Map">',paste(props,collapse=''),'</Option></layer></symbol></symbols></renderer-v2>')
}
mq_srs <- function(crs) {
  s<-sf::st_crs(crs);e<-monitora_qfield_xml
  paste0('<spatialrefsys nativeFormat="Wkt"><wkt>',e(s$wkt),'</wkt><proj4>',e(s$proj4string),'</proj4><srsid>0</srsid><srid>',if(!is.na(s$epsg))s$epsg else 0,'</srid><authid>',if(!is.na(s$epsg))paste0('EPSG:',s$epsg)else '', '</authid><description>',e(s$Name),'</description><projectionacronym>',if(isTRUE(s$IsGeographic))'longlat'else 'utm','</projectionacronym><ellipsoidacronym>',if(identical(s$epsg,4326L))'WGS84'else 'GRS80','</ellipsoidacronym><geographicflag>',tolower(as.character(isTRUE(s$IsGeographic))),'</geographicflag></spatialrefsys>')
}
mq_qgs <- function(layers,rasters,ae,c,path) {
  e<-monitora_qfield_xml;tree<-xml<-character();b<-sf::st_bbox(ae);dx<-max(b[3]-b[1],100)*.04;dy<-max(b[4]-b[2],100)*.04;b<-b+c(-dx,-dy,dx,dy)
  extent<-paste0('<extent><xmin>',b[1],'</xmin><ymin>',b[2],'</ymin><xmax>',b[3],'</xmax><ymax>',b[4],'</ymax></extent>')
  # Apoio e pontos acima dos polígonos e fundos; grade sem rótulos para legibilidade.
  rank<-c('pontos_interesse','trajeto','verg_ini','verg_fin','UAs','PA_priorit','PA_altern','grade_amostral','transectos','acessos','trilhas','estradas','rodovias','areas_elegiveis','limites_uc')
  layers<-layers[order(match(vapply(layers,`[[`,character(1),'papel'),rank),na.last=TRUE)]
  for(i in seq_along(layers)) {
    l<-layers[[i]];x<-l$x;id<-paste0('monitora_qfield_vector_',i);g<-if(nrow(x))as.character(sf::st_geometry_type(x)[1])else sub('sfc_','',class(sf::st_geometry(x))[1])
    typ<-if(grepl('POINT',g))'Point'else if(grepl('LINE',g))'Line'else 'Polygon'
    edit<-l$papel%in%c('pontos_interesse','trajeto');label<-if(l$papel=='grade_amostral')''else l$label
    source<-paste0('./dados/',l$arquivo,'|layername=',l$camada)
    tree<-c(tree,paste0('<layer-tree-layer id="',id,'" name="',e(l$nome),'" source="',e(source),'" providerKey="ogr" checked="Qt::Checked" expanded="0"/>'))
    labels<-if(nzchar(label))paste0('<labeling type="simple"><settings><text-style fieldName="',e(label),'" isExpression="0" fontFamily="Open Sans" fontSize="10" fontSizeUnit="Point" textColor="255,255,255,255"><text-buffer bufferDraw="1" bufferSize="0.6" bufferColor="20,20,20,255"/></text-style><placement placement="0" priority="5"/><rendering drawLabels="1"/></settings></labeling>')else ''
    xml<-c(xml,paste0('<maplayer type="vector" geometry="',typ,'" labelsEnabled="',as.integer(nzchar(label)),'" readOnly="',as.integer(!edit),'"><id>',id,'</id><datasource>',e(source),'</datasource><layername>',e(l$nome),'</layername><srs>',mq_srs(sf::st_crs(x)),'</srs><provider encoding="UTF-8">ogr</provider><displayfield>',e(label),'</displayfield><previewExpression>',if(nzchar(l$label))paste0('&quot;',e(l$label),'&quot;')else '', '</previewExpression>',mq_style(l$papel,typ),labels,'<editforminit/><editforminitcodesource>0</editforminitcodesource><editforminitcode/><customproperties><Option type="Map"><Option name="QFieldSync/action" value="no_action" type="QString"/></Option></customproperties></maplayer>'))
  }
  for(i in seq_along(rasters)) {
    l<-rasters[[i]];id<-paste0('monitora_qfield_raster_',i);source<-paste0('./mapas/',l$arquivo)
    tree<-c(tree,paste0('<layer-tree-layer id="',id,'" name="',e(l$nome),'" source="',e(source),'" providerKey="gdal" checked="Qt::Checked" expanded="0"/>'))
    renderer<-if(l$bandas>=3)paste0('<rasterrenderer type="multibandcolor" redBand="1" greenBand="2" blueBand="3" alphaBand="',if(l$bandas>=4)4 else -1,'" opacity="1"/>')else '<rasterrenderer type="singlebandgray" grayBand="1" alphaBand="-1" opacity="1"><contrastEnhancement><minValue>0</minValue><maxValue>255</maxValue><algorithm>StretchToMinimumMaximum</algorithm></contrastEnhancement></rasterrenderer>'
    xml<-c(xml,paste0('<maplayer type="raster"><id>',id,'</id><datasource>',e(source),'</datasource><layername>',e(l$nome),'</layername><srs>',mq_srs(l$wkt),'</srs><resourceMetadata><title>',e(l$nome),'</title><abstract>',e(if(is.null(l$descricao))''else l$descricao),'</abstract></resourceMetadata><provider>gdal</provider><pipe>',renderer,'</pipe></maplayer>'))
  }
  google_id <- 'monitora_google_satellite_online'
  google_nome <- 'Google Satellite'
  google_fonte <- 'crs=EPSG:3857&format&type=xyz&url=https://mt1.google.com/vt/lyrs%3Ds%26x%3D%7Bx%7D%26y%3D%7By%7D%26z%3D%7Bz%7D&zmax=20&zmin=0'
  tree <- c(tree, paste0('<layer-tree-layer id="', google_id, '" name="', e(google_nome), '" source="', e(google_fonte), '" providerKey="wms" checked="Qt::Unchecked" expanded="0"/>'))
  mundo_3857 <- '<extent><xmin>-20037508.342789244</xmin><ymin>-20037508.342789248</ymin><xmax>20037508.342789244</xmax><ymax>20037508.342789248</ymax></extent><wgs84extent><xmin>-180</xmin><ymin>-85.0511287798066</ymin><xmax>180</xmax><ymax>85.0511287798066</ymax></wgs84extent>'
  xml <- c(xml, paste0(
    '<maplayer type="raster" hasScaleBasedVisibilityFlag="0" minScale="100000000" maxScale="0">',
    mundo_3857, '<id>', google_id, '</id><datasource>', e(google_fonte),
    '</datasource><layername>', e(google_nome), '</layername><srs>', mq_srs(3857),
    '</srs><attribution href="https://www.google.com/permissions/geoguidelines/">Google</attribution>',
    '<provider>wms</provider><customproperties><Option type="Map">',
    '<Option name="QFieldSync/action" type="QString" value="no_action"/>',
    '<Option name="QFieldSync/cloud_action" type="QString" value="no_action"/>',
    '<Option name="QFieldSync/remoteLayerId" type="QString" value="', google_id, '"/>',
    '</Option></customproperties><pipe><provider><resampling enabled="false" zoomedInResamplingMethod="nearestNeighbour" maxOversampling="2" zoomedOutResamplingMethod="nearestNeighbour"/></provider>',
    '<rasterrenderer band="1" opacity="1" type="singlebandcolordata" alphaBand="-1"/></pipe></maplayer>'
  ))
  cr<-mq_srs(sf::st_crs(ae))
  coords<-paste0('<ProjectDisplaySettings CoordinateAxisOrder="Default" CoordinateType="MapGeographic"><GeographicCoordinateFormat id="geographiccoordinate"><Option type="Map"><Option name="angle_format" type="QString" value="DecimalDegrees"/><Option name="decimals" type="int" value="6"/><Option name="show_suffix" type="bool" value="false"/><Option name="show_thousand_separator" type="bool" value="false"/></Option></GeographicCoordinateFormat><CoordinateCustomCrs>',mq_srs(4326),'</CoordinateCustomCrs></ProjectDisplaySettings>')
  document<-paste0('<?xml version="1.0" encoding="UTF-8"?><qgis version="3.44.9" projectname="Monitora QField"><title>',e(c$projeto),'</title><homePath path=""/><projectCrs>',cr,'</projectCrs><mapcanvas><units>meters</units>',extent,'<destinationsrs>',cr,'</destinationsrs></mapcanvas><layer-tree-group name="" checked="Qt::Checked">',paste(tree,collapse=''),'</layer-tree-group><projectlayers>',paste(xml,collapse=''),'</projectlayers><properties><SpatialRefSys><ProjectionsEnabled type="int">1</ProjectionsEnabled><ProjectCrs type="QString">EPSG:',sf::st_crs(ae)$epsg,'</ProjectCrs></SpatialRefSys><Paths><Absolute type="bool">false</Absolute></Paths><PositionPrecision><Automatic type="bool">false</Automatic><DecimalPlaces type="int">6</DecimalPlaces></PositionPrecision></properties><ProjectViewSettings><DefaultViewExtent xmin="',b[1],'" ymin="',b[2],'" xmax="',b[3],'" ymax="',b[4],'">',cr,'</DefaultViewExtent></ProjectViewSettings>',coords,'</qgis>')
  xml2::read_xml(document,options='NONET');writeLines(enc2utf8(document),path,useBytes=TRUE)
}

mq_kml <- function(x,label,path) {
  e<-monitora_qfield_xml;y<-sf::st_transform(x,4326);a<-sf::st_drop_geometry(y);xy<-sf::st_coordinates(y)
  lines<-c('<?xml version="1.0" encoding="UTF-8"?>','<kml xmlns="http://www.opengis.net/kml/2.2"><Document>')
  if(nrow(y))for(i in seq_len(nrow(y))) {
    val<-vapply(a,function(v)if(is.na(v[i]))''else as.character(v[i]),character(1))
    data<-paste0('<Data name="',e(names(a)),'"><value>',e(val),'</value></Data>',collapse='')
    co<-paste(format(xy[i,seq_len(min(3,ncol(xy)))],digits=16,scientific=FALSE,trim=TRUE),collapse=',')
    lines<-c(lines,paste0('<Placemark><name>',e(if(nzchar(label))a[[label]][i]else as.character(i)),'</name><ExtendedData>',data,'</ExtendedData><Point><coordinates>',co,'</coordinates></Point></Placemark>'))
  }
  lines<-c(lines,'</Document></kml>');writeLines(enc2utf8(lines),path,useBytes=TRUE)
}
mq_write_gpkg <- function(x,path,layer) {
  sf::st_write(x,path,layer=layer,quiet=TRUE,append=FALSE,layer_options='SPATIAL_INDEX=YES')
  if(!nrow(x)) {
    typ<-attr(x,'qfield_tipo');if(is.null(typ))typ<-sub('sfc_','',class(sf::st_geometry(x))[1])
    if(!typ%in%c('POINT','MULTIPOINT','LINESTRING','MULTILINESTRING','POLYGON','MULTIPOLYGON'))mq_stop('Tipo da camada vazia não determinado: ',layer)
    con<-DBI::dbConnect(RSQLite::SQLite(),path);on.exit(DBI::dbDisconnect(con),add=TRUE)
    DBI::dbExecute(con,'UPDATE gpkg_geometry_columns SET geometry_type_name=? WHERE table_name=?',params=list(typ,layer))
  }
}
mq_export <- function(layers,root) {
  dirq<-file.path(root,'01_qfield');rep<-file.path(root,'02_relatorio');audit<-list();dict<-list()
  safe<-vapply(layers,function(l)tolower(monitora_qfield_slug(l$nome)),character(1));if(anyDuplicated(safe))mq_stop('Nomes de arquivos GPKG colidem após normalização; ajuste nomes das camadas.')
  for(i in seq_along(layers)) {
    l<-layers[[i]];x<-l$x;nm<-l$nome
    l$arquivo<-paste0(monitora_qfield_slug(nm),'.gpkg')
    if(l$papel%in%c('PA_priorit','PA_altern','grade_amostral'))l$arquivo<-paste0(l$papel,'.gpkg')
    if(nm%in%c('AE','UC'))l$arquivo<-paste0(nm,'.gpkg')
    l$camada<-nm
    target<-file.path(dirq,'dados',l$arquivo)
    mq_write_gpkg(x,target,nm)
    check<-sf::st_read(target,layer=nm,quiet=TRUE)
    if(nrow(check)!=nrow(x))mq_stop('Contagem GeoPackage divergente: ',nm)
    if(nrow(x) && !all(lengths(sf::st_equals_exact(sf::st_geometry(x),sf::st_geometry(check),par=1e-8))>0))mq_stop('Geometria GeoPackage divergente: ',nm)
    ispoint<-if(nrow(x))all(as.character(sf::st_geometry_type(x))=='POINT')else identical(class(sf::st_geometry(x))[1],'sfc_POINT')
    if(ispoint) {
      filebase<-paste0(sprintf('%02d_',i),monitora_qfield_slug(nm))
      gp<-file.path(root,'03_vetores','gpkg',paste0(filebase,'.gpkg'));mq_write_gpkg(x,gp,nm)
      cs<-file.path(root,'04_csv',paste0(filebase,'.csv'));mq_csv(x,cs)
      z<-data.table::fread(cs,encoding='UTF-8',colClasses='character')
      if(nrow(z)!=nrow(x) || !setequal(names(z),names(sf::st_drop_geometry(x))))mq_stop('CSV não preservou contagens/campos: ',nm)
      for(k in intersect(c('PA','id_grade'),names(x)))if(!identical(as.character(z[[k]]),as.character(x[[k]])))mq_stop('CSV alterou identificadores: ',nm)
      km<-file.path(root,'03_vetores','kml',paste0(filebase,'.kml'));mq_kml(x,l$label,km)
      doc<-xml2::read_xml(km);if(length(xml2::xml_find_all(doc,'//*[local-name()="Placemark"]'))!=nrow(x))mq_stop('Contagem KML divergente.')
      kz<-file.path(root,'03_vetores','kmz',paste0(filebase,'.kmz'));zip::zipr(kz,basename(km),root=dirname(km))
      if(!basename(km)%in%zip::zip_list(kz)$filename)mq_stop('KMZ incompleto.')
      attrs<-sf::st_drop_geometry(x);dict[[length(dict)+1]]<-data.frame(camada=nm,campo=names(attrs),tipo=vapply(attrs,function(v)class(v)[1],character(1)),observacao='CSV UTF-8 BOM; separador ; e decimal ponto; campos vazios representam NA; KML atributos textuais')
    }
    audit[[i]]<-data.frame(camada=nm,papel=l$papel,n=nrow(x),fonte=l$fonte,editavel=l$papel%in%c('pontos_interesse','trajeto'))
    layers[[i]]<-l
  }
  mq_csv(do.call(rbind,audit),file.path(rep,'camadas.csv'))
  if(length(dict))mq_csv(do.call(rbind,dict),file.path(root,'04_csv','dicionario_campos.csv'))
  layers
}

mq_overview <- function(layers,ae,path) {
  grDevices::png(path,width=1400,height=1000,res=130)
  on.exit(grDevices::dev.off(),add=TRUE)
  graphics::par(mar=c(4,4,3,1),bg='#27382f',fg='white',col.axis='white',col.lab='white')
  plot(sf::st_geometry(ae),border='#00ff00',col=NA,axes=TRUE,main='Monitora — planejamento / referências')
  for(role in c('grade_amostral','PA_altern','PA_priorit','verg_ini','verg_fin'))for(l in Filter(function(l)l$papel==role && nrow(l$x)>0,layers)) {
    color<-switch(role,grade_amostral='white',PA_altern='#FDBF6F',PA_priorit='#E31A1C',verg_ini='#1F78B4',verg_fin='white')
    plot(sf::st_geometry(l$x),add=TRUE,pch=21,bg=color,col=if(role=='verg_fin')'#1F78B4'else '#eeeeee',cex=.65)
  }
  graphics::legend('bottomleft',legend=c('AE','Grade','Prioritário','Alternativo','UA início'),col=c('#00ff00','white','#E31A1C','#FDBF6F','#1F78B4'),pch=c(NA,19,19,19,19),lty=c(1,NA,NA,NA,NA),text.col='white',bg='#27382f',cex=.8)
}

mq_report <- function(root,c,stages,status,notes,inventory) {
  r<-file.path(root,'02_relatorio');e<-monitora_qfield_xml
  mq_csv(stages,file.path(r,'etapas.csv'));mq_json(utils::modifyList(c,list(url_xyz='configurada; omitida do relatório',cabecalhos_xyz=as.list(names(c$cabecalhos_xyz)))),file.path(r,'configuracao.json'));mq_csv(inventory,file.path(r,'fontes_e_checksums.csv'))
  trs<-apply(stages,1,function(v)paste0('<tr>',paste0('<td>',e(v),'</td>',collapse=''),'</tr>'))
  txt<-c('<!doctype html><html lang="pt-BR"><meta charset="utf-8"><title>Relatório de execução — Monitora QField</title><style>body{font:16px sans-serif;max-width:1100px;margin:40px auto;line-height:1.5;padding:0 20px}table{border-collapse:collapse;width:100%}td,th{border:1px solid #bbb;padding:8px;text-align:left}h1{color:#165c46}</style>',paste0('<h1>',e(c$projeto),'</h1><p>Versão 0.4.2 · ',e(status),'</p>'),'<p>Produto de planejamento e navegação. Homologação automática não substitui verificação em QField no aparelho, em modo avião, nem aplicação do roteiro em campo.</p>',paste0('<ul>',paste0('<li>',e(notes),'</li>',collapse=''),'</ul>'),paste0('<table><thead><tr>',paste0('<th>',e(names(stages)),'</th>',collapse=''),'</tr></thead><tbody>',paste(trs,collapse=''),'</tbody></table>'),if(file.exists(file.path(r,'mapa_planejamento.png'))) '<p><img src="mapa_planejamento.png" alt="Visão geral das áreas e pontos" style="width:100%"></p>' else '', '<p>Detalhes: cotas_solicitadas.csv, cotas_realizadas.csv, alocacao_combinacoes.csv, solucao_cotas.json, fontes_estratos.json (quando habilitadas); configuracao.json; consulta_uc.json; referencia_grade.json e cadastro_grade.gpkg (planejamento/expansão); camadas.csv; imagens.csv; legenda/fonte MapBiomas; diagnosticos/; fontes_e_checksums.csv; manifesto.csv.</p></html>')
  writeLines(enc2utf8(txt),file.path(r,'relatorio_execucao.html'),useBytes=TRUE)
}

monitora_criar_qfield <- function(config=MQ_CONFIG) {
  mq_deps();c<-mq_protocol(utils::modifyList(MQ_CONFIG,config,keep.null=TRUE));mq_validate_config(c)
  if(mq_design_active(c)&&!requireNamespace('lpSolve',quietly=TRUE))mq_stop('Instale lpSolve antes de iniciar o desenho com cotas cumulativas.')
  c$script_sha256<-if(!is.na(MQ_SCRIPT_ARQUIVO))mq_hash(MQ_SCRIPT_ARQUIVO)else NA_character_
  c<-mq_resolve_paths(c)
  input<-normalizePath(c$entrada,winslash='/',mustWork=TRUE);out<-normalizePath(c$saida,winslash='/',mustWork=FALSE)
  if(tolower(input)==tolower(out) || startsWith(tolower(out),paste0(tolower(input),'/')) || startsWith(tolower(input),paste0(tolower(out),'/')))mq_stop('Entrada e saída precisam ser pastas separadas e não aninhadas.')
  cache_abs<-normalizePath(c$cache_dir,winslash='/',mustWork=FALSE)
  if(tolower(cache_abs)==tolower(input)||startsWith(tolower(cache_abs),paste0(tolower(input),'/')))mq_stop('Cache não pode ficar dentro da entrada.')
  slug<-monitora_qfield_slug(c$projeto);dir.create(out,recursive=TRUE,showWarnings=FALSE)
  final<-file.path(out,paste0(slug,'_',format(Sys.time(),'%Y%m%d_%H%M%S'),'_',substr(digest::digest(tempfile()),1,6)))
  root<-paste0(final,'.construcao');dir.create(root)
  for(d in c('01_qfield/dados','01_qfield/mapas','02_relatorio/diagnosticos','03_vetores/gpkg','03_vetores/kml','03_vetores/kmz','04_csv','05_qgis/dados','mapas_pdf','mapas_png'))dir.create(file.path(root,d),recursive=TRUE)
  report<-file.path(root,'02_relatorio');scratch<-file.path(root,'.temporarios');dir.create(scratch)
  files<-mq_files(input)
  inv<-data.frame(arquivo=substring(files,nchar(input)+2),bytes=file.info(files)$size,sha256=vapply(files,mq_hash,character(1)))
  stages<-data.frame(etapa=character(),segundos=numeric(),descricao=character());t0<-proc.time()[3];last<-t0
  note<-if(c$perfil=='campestre_savanico')c('Roteiro 29/04/2026: PA = início previsto; deslocamento em campo até 10 m; tentar N, L, S, O; depois alternativo mais próximo. Distâncias se aplicam ao segmento inteiro.', 'Grade 156,25 m não garante 100 m entre transectos instalados. A formação MapBiomas não substitui a observada. Restrições viárias ativas são aplicadas antes da seleção; demais simulações são diagnósticas. Quando habilitada, a classificação do desenho e suas exclusões são registradas no relatório.', 'As linhas entre extremos observados representam ligações; não são trajetos de acesso levantados. Apoio de campo é editável e precisa ser preservado antes de atualizar o projeto.') else c(paste('Perfil',c$perfil,': sem aplicação automática do procedimento campestre. Parâmetros explícitos registrados.'),'PAs/UAs fornecidos são preservados; vias podem servir somente à referência visual.')
  step<-function(name,description){now<-proc.time()[3];stages<<-rbind(stages,data.frame(etapa=name,segundos=round(now-last,2),descricao=description));last<<-now;message(sprintf('[%s] %s — %.1f s acumulados',name,description,now-t0))}
  result<-tryCatch({
    if(c$gerar_cartografia)c$qgis_runtime<-mq_qgis_prepare(c,root,scratch)
    c<-mq_design_load(c,input,report)
    dat<-mq_read(input,scratch);step('01_entrada',paste(length(dat$camadas),'camadas; geometria/CRS/papéis conferidos; fontes preservadas'))
    supplied<-any(vapply(dat$camadas,function(l)l$papel%in%c('PA_priorit','PA_altern','grade_amostral','verg_ini','verg_fin','UAs','transectos'),logical(1)))
    mode<-c$modo;if(mode=='auto')mode<-if(supplied)'montar'else 'planejar'
    if(mode%in%c('planejar','expandir') && supplied)mq_stop('Camadas de pontos/UAs/grade fornecidas: use montar. Expansão recebe o cadastro anterior em referencia_anterior.')
    if(mode=='montar' && !supplied)mq_stop('Montagem exige pontos/grade/UAs fornecidos; não cria pontos automaticamente.')
    cr<-if(mode=='expandir' && is.null(c$epsg) && !is.null(c$referencia_anterior)) mq_crs(dat$ae,jsonlite::read_json(file.path(c$referencia_anterior,'referencia_grade.json'))$epsg) else mq_crs(dat$ae,c$epsg);ae<-sf::st_transform(dat$ae,cr)
    for(i in seq_along(dat$camadas))dat$camadas[[i]]$x<-mq_transform(dat$camadas[[i]]$x,cr)
    official<-mq_uc(dat$ae,c);mq_json(official[setdiff(names(official),'x')],file.path(report,'consulta_uc.json'))
    if(nrow(official$x))sf::st_write(official$x,file.path(report,'limites_federais_consultados.gpkg'),quiet=TRUE)
    step('02_UC',paste('Consulta oficial completa;',nrow(official$x),'UCs intersectam AEs;',official$titulo))
    roads<-mq_road_sources(dat$camadas,c,report)
    old<-prev<-NULL;g<-NULL
    if(mode=='expandir') {
      if(is.null(c$referencia_anterior))mq_stop('Expansão exige referencia_anterior.')
      rd<-normalizePath(c$referencia_anterior,mustWork=TRUE)
      prev<-jsonlite::read_json(file.path(rd,'referencia_grade.json'),simplifyVector=FALSE)
      old<-sf::st_read(file.path(rd,'cadastro_grade.gpkg'),quiet=TRUE)
      old<-sf::st_sf(sf::st_drop_geometry(old),geometry=sf::st_geometry(old))
      if(mq_hash(file.path(rd,'cadastro_grade.gpkg'))!=prev$cadastro_sha256)mq_stop('Cadastro anterior difere da assinatura registrada.')
      prev$grade_m<-unlist(prev$grade_m);for(i in seq_along(prev$contextos))prev$contextos[[i]]$origem<-unlist(prev$contextos[[i]]$origem)
      # A origem/projeção anteriores prevalecem sobre seleção automática baseada em AE expandida.
      if(is.null(c$epsg)){cr<-sf::st_crs(prev$epsg);ae<-sf::st_transform(dat$ae,cr);for(i in seq_along(dat$camadas))dat$camadas[[i]]$x<-mq_transform(dat$camadas[[i]]$x,cr)}
      note<-c(note,'Expansão: consulta federal atual registrada; origem e associação territorial da malha anterior preservadas. Alteração do limite não deslocou a grade.')
    }
    if(mode!='montar') {
      contexts<-mq_contexts(dat$ae,official$x,cr,c$grade_m,prev)
      g<-mq_grid(dat$ae,contexts,cr,c,old);contexts<-attr(g,'contextos')
      g<-mq_road_screen(g,roads,ae,c,report)
      if(mq_design_active(c)) {
        contract<-mq_design_contract(c)
        if(!is.null(prev$contrato_estratos)&&!identical(prev$contrato_estratos$sha256,contract$sha256))mq_stop('Contrato de classificação mudou (fonte, campos, mapeamento, coleção/ano ou perfil). Expansão exige revisão/migração explícita; histórico não alterado.')
        g<-mq_design_classify(g,dat$camadas,c,report)
        if(!is.null(prev$campos_estratos) && !identical(sort(unlist(prev$campos_estratos)),sort(names(mq_design_criteria(g,c,mq_quantity(c$prioritarios,nrow(g),ceiling(.2*nrow(g))))))))mq_stop('Campos de estratificação mudaram em relação ao cadastro anterior.')
        g<-mq_design_select(g,c,report,old)
        note<-c(note,'Cotas cumulativas resolvidas conjuntamente; veja cotas_solicitadas.csv, cotas_realizadas.csv e alocacao_combinacoes.csv. Zero por combinação não permite inferência para ela. PA não é UA instalada; viabilidade do segmento depende de campo.')
      } else {
        if(!is.null(old)&&'mq_estrato'%in%names(old))mq_stop('Expansão não pode desativar o desenho estratificado anterior.')
        g<-mq_select(g,c)
      }
      # Cadastro inclui pontos históricos fora da AE atual para nunca reciclar seus IDs.
      ledger<-g
      if(!is.null(old)){keep<-old[!old$chave_grade%in%g$chave_grade,];if(nrow(keep)){
        for(n in setdiff(names(g),names(keep)))keep[[n]]<-g[[n]][rep(NA_integer_,nrow(keep))]
        for(n in setdiff(names(keep),names(g)))ledger[[n]]<-keep[[n]][rep(NA_integer_,nrow(ledger))]
        ledger<-rbind(ledger,keep[,names(ledger)])
      }}
      sf::st_write(ledger,file.path(report,'cadastro_grade.gpkg'),layer='cadastro',quiet=TRUE)
      domain<-sf::st_union(ae);if(!is.null(prev$dominio_processado_wkt))domain<-sf::st_union(c(sf::st_geometry(domain),sf::st_as_sfc(prev$dominio_processado_wkt,crs=cr)))
      ref<-list(dominio_processado_wkt=sf::st_as_text(domain,digits=16),versao=2,epsg=cr$epsg,grade_m=c$grade_m,contextos=contexts,cadastro_sha256=mq_hash(file.path(report,'cadastro_grade.gpkg')),regra='vértices únicos; fronteiras incluídas; origem fixa; ID nunca renumerado',semente=c$semente)
      if(mq_design_active(c)){
        ref$campos_estratos<-names(mq_design_criteria(g,c,sum(g$categoria=='prioritario')))
        ref$contrato_estratos<-contract
      }
      mq_json(ref,file.path(report,'referencia_grade.json'))
      step('03_grade',sprintf('%d vértices na união das AEs; %d prioritários; %d alternativos; denominador global=%d',nrow(g),sum(g$categoria=='prioritario'),sum(g$categoria=='alternativo'),nrow(g)))
      if(isTRUE(c$mapbiomas)&&!'mb_codigo'%in%names(g))g<-mq_mb(g,c,report)
      g<-mq_coordinates(g)
      for(role in c('grade_amostral','PA_priorit','PA_altern')) {
        z<-if(role=='grade_amostral')g else g[g$categoria==if(role=='PA_priorit')'prioritario'else 'alternativo',]
        dat$camadas[[length(dat$camadas)+1]]<-list(x=z,papel=role,nome=role,label='codigo_pa',fonte=paste('gerado',mode))
      }
      pts<-g[g$categoria!='grade',]
      mq_diagnostics(pts,dat$camadas,ae,c,file.path(report,'diagnosticos'))
    } else {
      labels<-list();assembled<-list();audit_complete<-TRUE
      for(i in seq_along(dat$camadas)) {
        l<-dat$camadas[[i]];x<-l$x
        if(l$papel%in%c('PA_priorit','PA_altern')){x<-mq_road_screen(x,roads,ae,c,report,l$nome);dat$camadas[[i]]$x<-x}
        if(nrow(x)&&all(as.character(sf::st_geometry_type(x))=='POINT')) {
          if(isTRUE(c$mapbiomas))x<-mq_mb(x,c,report)
          if(mq_design_active(c)) {
            audit_dir<-file.path(report,'diagnosticos',paste0('estratos_',l$nome));dir.create(audit_dir,recursive=TRUE)
            x<-tryCatch(mq_design_classify(x,dat$camadas,c,audit_dir),error=function(e){note<<-c(note,paste('Auditoria de estratos pendente em',l$nome,':',conditionMessage(e)));if(l$papel%in%c('PA_priorit','PA_altern'))audit_complete<<-FALSE;x})
          }
          x<-mq_coordinates(x);dat$camadas[[i]]$x<-x
          if(l$papel%in%c('PA_priorit','PA_altern')){
            labels[[l$papel]]<-as.character(x[[l$label]])
            if('mq_apto'%in%names(x)){x$categoria<-if(l$papel=='PA_priorit')'prioritario'else 'alternativo';assembled[[l$papel]]<-x}
          }
        }
      }
      if(length(intersect(labels[['PA_priorit']],labels[['PA_altern']]))>0)mq_stop('Mesmo rótulo PA em prioritários e alternativos.')
      if(length(assembled)&&mq_design_active(c))tryCatch({
        if(!audit_complete)mq_stop('Classificação incompleta nas camadas de PAs; margens não calculadas com subconjunto.')
        # Somente conferência das margens fornecidas; jamais substituir os pontos.
        z<-do.call(rbind,lapply(assembled,function(x)x[,unique(c('categoria','mq_apto',if(c$estratificar_vegetacao)c('mq_formacao',if(!is.null(c$fitofisionomia_campo))'mq_fitofisionomia'),paste0('atr_',if(c$estratificar_por_atributos)names(c$cotas_atributos)))),drop=FALSE]))
        np<-sum(z$categoria=='prioritario');crit<-mq_design_criteria(z,c,np);audit<-list()
        for(f in names(crit))for(j in seq_len(nrow(crit[[f]]))){q<-crit[[f]][j,];audit[[length(audit)+1]]<-data.frame(atributo=f,classe=q$classe,alvo=q$alvo,prioritarios=sum(z$categoria=='prioritario'&z[[f]]==q$classe),alternativos=sum(z$categoria=='alternativo'&z[[f]]==q$classe))}
        mq_csv(do.call(rbind,audit),file.path(report,'cotas_fornecidas_auditoria.csv'))
        note<-c(note,'Montagem: cotas auditadas contra o total de prioritários fornecidos; nenhuma complementação ou alteração dos PAs/UAs.')
      },error=function(e)note<<-c(note,paste('Auditoria de cotas fornecidas pendente:',conditionMessage(e))))
      imp<-lapply(dat$camadas,function(l){x<-l$x;if(!nrow(x)||!all(as.character(sf::st_geometry_type(x))=='POINT'))return(NULL);data.frame(camada=l$nome,n=nrow(x),fora_AE=sum(lengths(sf::st_intersects(x,ae))==0),id_grade_ausente=if('id_grade'%in%names(x))sum(is.na(x$id_grade))else nrow(x),observacao='Identidade e posição fornecidas preservadas; ausência de id_grade não foi preenchida por inferência.')});imp<-Filter(Negate(is.null),imp);if(length(imp))mq_csv(do.call(rbind,imp),file.path(report,'diagnosticos','pontos_fornecidos.csv'))
      step('03_montagem','Pontos fornecidos preservados; nenhuma grade/PA novo nem complementação de cotas.')
    }
    dat$camadas<-mq_pa_aliases(dat$camadas,report)
    step('04_atributos',if(isTRUE(c$mapbiomas))'MapBiomas e coordenadas decimais; consulte mb_status e fontes.'else 'MapBiomas desativado; coordenadas decimais adicionadas.')
    if(nrow(official$x))dat$camadas[[length(dat$camadas)+1]]<-list(x=sf::st_transform(official$x,cr),papel='limites_uc',nome='UC',label='',fonte=official$titulo)
    existing<-vapply(dat$camadas,`[[`,character(1),'papel')
    support<-monitora_qfield_apoio_vazio()
    for(n in names(support))if(!n%in%existing){x<-mq_transform(support[[n]],cr);attr(x,'qfield_tipo')<-if(n=='trajeto')'MULTILINESTRING'else 'POINT';dat$camadas[[length(dat$camadas)+1]]<-list(x=x,papel=n,nome=n,label=if(n=='trajeto')'trajeto'else 'ponto_interesse',fonte='camada vazia de apoio editável')}
    # Cobertura raster avaliada sobre todas as camadas pontuais, incluindo a grade quando presente.
    pg<-lapply(Filter(function(l)nrow(l$x)&&all(as.character(sf::st_geometry_type(l$x))=='POINT'),dat$camadas),function(l)sf::st_geometry(l$x))
    coverage<-if(length(pg))sf::st_sf(geometry=unique(do.call(base::c,pg)))else NULL
    ras<-mq_imagery(input,root,dat$camadas,ae,coverage,c,scratch,report)
    note<-c(note,attr(ras,'contexto'),paste('Detalhe:',attr(ras,'download_status')))
    cover<-attr(ras,'cobertura');if(cover$avaliados>0)note<-c(note,sprintf('Fundos locais: %d de %d pontos têm pixel válido. %d pontos sem imagem; a cobertura não comprova acesso ou transecção inteira.',cover$cobertos,cover$avaliados,cover$avaliados-cover$cobertos))
    step('05_imagens',paste(length(ras),'rasters; Sentinel offline validado, detalhe recortado conforme centros, fontes/cache auditados.'))
    nms<-vapply(dat$camadas,`[[`,character(1),'nome');if(anyDuplicated(tolower(nms)))mq_stop('Nomes finais de camadas repetidos (incluindo UC/apoio); ajuste o manifesto.')
    dat$camadas<-mq_export(dat$camadas,root)
    mq_qgs(dat$camadas,ras,ae,c,file.path(root,'01_qfield','projeto.qgs'))
    if(c$gerar_cartografia)mq_cartography(dat$camadas,ras,ae,c,root,scratch)
    step('06_exportacao','QGS, GeoPackages, CSV, KML e KMZ gerados e reabertos para conferência.')
    after<-vapply(files,mq_hash,character(1));if(!identical(unname(after),unname(inv$sha256)))mq_stop('Arquivo de entrada alterado durante a execução.')
    guide<-c('MONITORA QFIELD — v0.4.2',paste('Projeto:',c$projeto),paste('Modo:',mode),note,'Coordenadas exibidas em latitude/longitude WGS84, graus decimais, seis casas. Cálculos em CRS métrico. Confirme posicionamento, identificação, labels, zoom e apoio editável no QField em modo avião.','Importe o ZIP em pasta nova. Preserve pontos_interesse.gpkg e trajeto.gpkg preenchidos (ou apoio_campo.gpkg de versões antigas).','Camadas PA são referências planejadas, não UAs instaladas. Não há envio automático ao QFieldCloud.','Relatório completo está na pasta 02_relatorio da entrega. Falhas opcionais constam nos atributos e no relatório.')
    writeLines(enc2utf8(guide),file.path(root,'01_qfield','LEIA_ME.txt'),useBytes=TRUE)
    zipfile<-file.path(root,'01_qfield','pacote_qfield.zip');zip::zipr(zipfile,c('projeto.qgs','dados','mapas','LEIA_ME.txt'),root=file.path(root,'01_qfield'))
    z<-zip::zip_list(zipfile);if(!all(c('projeto.qgs','LEIA_ME.txt')%in%z$filename))mq_stop('Pacote QField incompleto.')
    step('07_pacote','ZIP independente com caminhos relativos; integridade das entradas preservada.')
    status<-'GERADO E VALIDADO AUTOMATICAMENTE; teste QField móvel pendente'
    if(length(ras)>0 && cover$cobertos<cover$avaliados)status<-paste(status,'; cobertura de imagens parcial')
    if(attr(ras,'download_status')%in%c('download_incompleto','exportacao_detalhe_incompleta','download_nao_autorizado'))status<-paste(status,'; detalhe sem aquisição completa')
    mb_states<-unlist(lapply(dat$camadas,function(l)if('mb_status'%in%names(l$x))unique(l$x$mb_status)else NULL))
    if(any(grepl('^falha|sem_dado|codigo_sem_legenda',mb_states))){status<-paste(status,'; MapBiomas com pendências');note<-c(note,unique(mb_states[mb_states!='obtido']))}
    if(any(grepl('Auditoria.*pendente',note)))status<-paste(status,'; estratificação fornecida com pendências')
    mq_overview(dat$camadas,ae,file.path(report,'mapa_planejamento.png'))
    inv<-mq_inventory_status(inv,dat$camadas,c)
    if(!c$gerar_cartografia){status<-paste(status,'; cartografia desativada explicitamente');note<-c(note,'Projeto QGIS editável e mapas não solicitados nesta execução.')}
    mq_report(root,c,stages,status,note,inv)
    mq_entry(root,c,mode,status)
    unlink(scratch,recursive=TRUE)
    fs<-list.files(root,recursive=TRUE,full.names=TRUE);fs<-fs[!file.info(fs)$isdir]
    mq_csv(data.frame(arquivo=substring(fs,nchar(root)+2),bytes=file.info(fs)$size,sha256=vapply(fs,mq_hash,character(1))),file.path(report,'manifesto.csv'))
    mq_json(list(status=status,modo=mode,segundos=round(proc.time()[3]-t0,2),versao='0.4.2'),file.path(report,'resultado.json'))
    if(file.exists(final)||!file.rename(root,final))mq_stop('Falha ao promover pacote; construção preservada.')
    message('Concluído: ',final);list(pasta=final,status=status,modo=mode)
  },error=function(e){
    step('FALHA',conditionMessage(e));mq_report(root,c,stages,'BLOQUEADO',c(note,conditionMessage(e)),inv)
    mq_json(list(status='BLOQUEADO',motivo=conditionMessage(e)),file.path(report,'resultado.json'))
    mq_stop(conditionMessage(e),'\nDiagnóstico preservado em: ',root)
  })
  invisible(result)
}

# HELPERS_REVISAO_INICIO
# Helpers mantidos em fonte legível e incorporados ao script autossuficiente.
mq_files <- function(root, pattern=NULL, relative=FALSE) {
  all<-sort(list.files(root,recursive=TRUE,full.names=FALSE,all.files=FALSE))
  all<-all[!file.info(file.path(root,all))$isdir]
  if(length(all))invisible(vapply(all,monitora_qfield_caminho_local,character(1),raiz=root))
  if(!is.null(pattern))all<-all[grepl(pattern,all,ignore.case=TRUE)]
  if(relative)all else file.path(root,all)
}
mq_resolve_paths <- function(c) {
  base<-c$pasta_base
  if(is.null(base))base<-if(!is.na(MQ_SCRIPT_ARQUIVO))dirname(MQ_SCRIPT_ARQUIVO)else NULL
  absolute<-function(p)grepl('^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)',p)
  if(is.null(base)&&any(!vapply(c(c$entrada,c$saida),absolute,logical(1))))mq_stop('Não foi possível localizar o script. Informe pasta_base ou caminhos absolutos.')
  if(!is.null(base))base<-normalizePath(base,winslash='/',mustWork=TRUE)
  for(n in c('entrada','saida','cache_dir','referencia_anterior','sentinel_arquivo','qgis_python'))if(!is.null(c[[n]])&&!absolute(c[[n]]))c[[n]]<-file.path(base,c[[n]])
  c$caches_adicionais<-vapply(c$caches_adicionais,function(p)if(absolute(p))p else file.path(base,p),character(1));c$pasta_base<-base;c
}
mq_inventory_status <- function(inv,layers,c) {
  used<-unique(vapply(layers,function(l)strsplit(l$fonte,' | ',fixed=TRUE)[[1]][1],character(1)))
  ext<-tolower(tools::file_ext(inv$arquivo));raster<-ext%in%c('mbtiles','tif','tiff','img','asc')
  config<-inv$arquivo%in%c('camadas_qfield.csv',c$cotas_atributos_arquivo,c$estratos_arquivo)
  aux<-ext%in%c('shx','dbf','prj','cpg','qix','sbn','sbx','tfw','wld','xml','ovr','ige','rrd')
  inv$estado<-ifelse(inv$arquivo%in%used|raster,'processado',ifelse(config,'configuracao',ifelse(aux,'auxiliar','ignorado')))
  inv$motivo<-ifelse(inv$estado=='ignorado','Arquivo sem papel de entrada reconhecido',ifelse(aux,'Auxiliar disponível ao driver da fonte','Leitura ou configuração registrada'))
  inv
}
mq_id_contexts <- function(contexts,ae,cr,c,old=NULL) {
  top<-if(!is.null(old)&&'id_malha'%in%names(old))max(old$id_grade,na.rm=TRUE)else 0
  for(z in contexts)if(!is.null(z$base_id))top<-max(top,z$base_id+z$ncol_ref*z$nlin_ref)
  for(i in seq_along(contexts)) {
    z<-contexts[[i]]
    if(is.null(z$ncol_ref)) {
      b<-sf::st_bbox(if(is.null(z$wkt))sf::st_transform(ae,cr)else sf::st_as_sfc(z$wkt,crs=cr))
      z$ncol_ref<-max(1,floor((b[3]-z$origem[1])/c$grade_m[1]+1e-9)+1)
      z$nlin_ref<-max(1,floor((b[4]-z$origem[2])/c$grade_m[2]+1e-9)+1)
      z$base_id<-top;top<-top+z$ncol_ref*z$nlin_ref
      contexts[[i]]<-z
    }
  }
  if(top>2^52)mq_stop('Domínio numérico da malha excedido.')
  contexts
}
mq_grid_identify <- function(g,contexts,old=NULL) {
  g$id_malha<-NA_real_;candidate<-rep(NA_real_,nrow(g));reserved<-0
  for(z in contexts) {
    k<-which(g$malha==z$chave & g$coluna>=0 & g$coluna<z$ncol_ref & g$linha>=0 & g$linha<z$nlin_ref)
    g$id_malha[k]<-g$linha[k]*z$ncol_ref+g$coluna[k]+1
    candidate[k]<-z$base_id+g$id_malha[k];reserved<-max(reserved,z$base_id+z$ncol_ref*z$nlin_ref)
  }
  g$id_grade<-candidate;g$categoria<-'grade';ix<-if(is.null(old))rep(NA_integer_,nrow(g))else match(g$chave_grade,old$chave_grade)
  known<-which(!is.na(ix));fresh<-which(is.na(ix))
  if(length(known)){g$id_grade[known]<-old$id_grade[ix[known]];g$categoria[known]<-old$categoria[ix[known]]}
  used<-if(is.null(old))numeric()else old$id_grade
  nextid<-max(c(reserved,used,candidate),na.rm=TRUE)
  for(k in fresh)if(is.na(g$id_grade[k]) || g$id_grade[k]%in%used){nextid<-nextid+1;g$id_grade[k]<-nextid;used<-c(used,nextid)}else used<-c(used,g$id_grade[k])
  if(anyDuplicated(g$id_grade))mq_stop('Colisão no cadastro da grade.')
  base<-ceiling(max(reserved,10000)/10000)*10000
  alias_id<-ifelse(is.na(candidate),g$id_grade,candidate)
  codes<-ifelse(alias_id<10000,base+alias_id,alias_id)
  previous_codes<-if(!is.null(old)&&'codigo_pa'%in%names(old))old$codigo_pa else character()
  g$codigo_pa<-paste0('PA',format(codes,scientific=FALSE,trim=TRUE))
  if(length(previous_codes)&&length(known)){valid<-known[!is.na(previous_codes[ix[known]])&grepl('^PA[0-9]{5,}$',previous_codes[ix[known]])];g$codigo_pa[valid]<-previous_codes[ix[valid]]}
  allcodes<-c(stats::na.omit(previous_codes),g$codigo_pa[known]);nextcode<-max(c(10000,suppressWarnings(as.numeric(sub('^PA','',c(g$codigo_pa,allcodes))))),na.rm=TRUE)
  for(k in fresh){if(g$codigo_pa[k]%in%allcodes){nextcode<-nextcode+1;g$codigo_pa[k]<-paste0('PA',format(nextcode,scientific=FALSE,trim=TRUE))};allcodes<-c(allcodes,g$codigo_pa[k])}
  # Migração v0.3: mantém PA e id_grade, acrescentando código de exibição.
  g$PA<-g$codigo_pa
  if(length(known))g$PA[known]<-old$PA[ix[known]]
  if(anyDuplicated(g$codigo_pa)||any(!grepl('^PA[0-9]{5,}$',g$codigo_pa)))mq_stop('Código PA inválido ou repetido.')
  attr(g,'contextos')<-contexts;g
}
mq_pa_aliases <- function(layers,report) {
  ids<-which(vapply(layers,function(l)l$papel%in%c('PA_priorit','PA_altern','grade_amostral'),logical(1)))
  if(!length(ids))return(layers)
  # Mesma chave espacial identifica o mesmo ponto na grade e na camada selecionada.
  rows<-list();cmp_crs<-sf::st_crs(layers[[ids[1]]]$x);if(isTRUE(cmp_crs$IsGeographic))cmp_crs<-sf::st_crs(3857)
  for(i in ids){l<-layers[[i]];x<-l$x;if(!nrow(x))next
    geom<-sf::st_as_binary(sf::st_geometry(x));original<-if(l$label=='codigo_pa'&&'PA'%in%names(x))as.character(x$PA)else if(nzchar(l$label))as.character(x[[l$label]])else rep('',nrow(x))
    key<-if('chave_grade'%in%names(x))as.character(x$chave_grade)else paste(original,vapply(geom,digest::digest,character(1),algo='sha256'),sep=':')
    code<-if('codigo_pa'%in%names(x))as.character(x$codigo_pa)else ifelse(grepl('^PA[0-9]{5,}$',original),original,NA_character_)
    previous<-code;code[is.na(code)|!grepl('^PA[0-9]{5,}$',code)]<-NA_character_;xy<-sf::st_coordinates(sf::st_transform(x,cmp_crs))
    rows[[length(rows)+1]]<-data.frame(i=i,row=seq_len(nrow(x)),chave=key,original=original,codigo_anterior=previous,codigo=code,x=xy[,1],y=xy[,2])
  }
  if(!length(rows))return(layers)
  d<-do.call(rbind,rows);keys<-sort(unique(d$chave));assigned<-character();used<-unique(stats::na.omit(d$codigo));counter<-max(c(10000,suppressWarnings(as.numeric(sub('^PA','',used)))),na.rm=TRUE)
  for(k in keys){dk<-d[d$chave==k,];if(diff(range(dk$x))>.01||diff(range(dk$y))>.01)mq_stop('Chave PA com posições conflitantes (>1 cm): ',k);v<-unique(stats::na.omit(d$codigo[d$chave==k]));if(length(v)>1)mq_stop('Códigos conflitantes para a mesma chave PA.')
    code<-if(length(v))v else NA_character_;if(is.na(code)||code%in%assigned){counter<-counter+1;while(paste0('PA',counter)%in%used)counter<-counter+1;code<-paste0('PA',counter)}
    assigned<-c(assigned,code);used<-c(used,code);d$codigo[d$chave==k]<-code
  }
  for(i in ids){r<-d[d$i==i,];if(!nrow(r))next;if(any(!is.na(r$codigo_anterior)&r$codigo_anterior!=r$codigo)&&!'codigo_pa_anterior'%in%names(layers[[i]]$x))layers[[i]]$x$codigo_pa_anterior<-r$codigo_anterior;layers[[i]]$x$codigo_pa<-r$codigo;layers[[i]]$label<-'codigo_pa'}
  d$camada<-vapply(d$i,function(i)layers[[i]]$nome,character(1));mq_csv(d[,c('camada','chave','original','codigo_anterior','codigo')],file.path(report,'correspondencia_codigos_PA.csv'))
  layers
}
mq_entry <- function(root,c,mode,status) {
  e<-monitora_qfield_xml
  links<-c('01_qfield/pacote_qfield.zip'='Projeto para QField','01_qfield/projeto.qgs'='Projeto de navegação no QGIS','02_relatorio/relatorio_execucao.html'='Relatório de execução','03_vetores'='Vetores GPKG, KML e KMZ','04_csv'='Tabelas CSV')
  if(c$gerar_cartografia)links<-c(links,'05_qgis/projeto_edicao.qgz'='Projeto QGIS editável e quatro layouts','mapas_pdf'='Quatro mapas PDF georreferenciados','mapas_png'='Quatro mapas PNG')
  writeLines(c('<!doctype html><html lang="pt-BR"><meta charset="utf-8"><title>Monitora — entrega</title><style>body{font:18px/1.6 sans-serif;max-width:900px;margin:40px auto;padding:20px;color:#174b3b}</style>',paste0('<h1>',e(c$projeto),'</h1><p>v0.4.2 · ',e(mode),' · ',e(status),'</p><ul>'),paste0('<li><a href="',names(links),'">',links,'</a></li>'),'</ul><p>Edite os vetores em 05_qgis. Essas alterações não modificam o pacote QField, os CSV/KML ou mapas já exportados. Reexporte os layouts após editar. Preserve a pasta completa para manter as imagens compartilhadas e os caminhos relativos.</p></html>'),file.path(root,'ABRA_AQUI.html'))
}
# Composição source-over: a primeira fonte local prevalece; transparências são preenchidas.
mq_alpha_over <- function(front,back) {
  af<-front[,,4];ab<-back[,,4];alpha<-af+ab*(1-af);out<-front
  for(k in 1:3)out[,,k]<-ifelse(alpha>0,(front[,,k]*af+back[,,k]*ab*(1-af))/pmax(alpha,1e-20),0)
  out[,,4]<-alpha;out
}
mq_mosaic_mb <- function(paths,c,report,mask=NULL) {
  paths<-unique(paths);hashes<-vapply(paths,mq_hash,character(1));key<-digest::digest(list(hashes,if(!is.null(mask))list(sf::st_as_binary(mask),sf::st_crs(mask)$wkt), 'alpha_local_first_normalize256_v3'))
  target<-file.path(c$cache_dir,'mosaicos',paste0(key,'.mbtiles'));dir.create(dirname(target),recursive=TRUE,showWarnings=FALSE)
  sources<-lapply(paths,function(p){db<-DBI::dbConnect(RSQLite::SQLite(),p,flags=RSQLite::SQLITE_RO);d<-DBI::dbGetQuery(db,'SELECT zoom_level z,tile_column x,tile_row t FROM tiles');m<-DBI::dbGetQuery(db,'SELECT name,value FROM metadata');list(db=db,d=d,meta=m)})
  on.exit(for(v in sources)if(DBI::dbIsValid(v$db))DBI::dbDisconnect(v$db),add=TRUE)
  provenance<-lapply(seq_along(paths),function(i)list(prioridade=i,arquivo=paths[i],sha256=hashes[i],metadados=sources[[i]]$meta))
  mq_json(list(regra='Fonte local primeiro; complemento somente via alpha; fontes ordenadas por caminho relativo. Pixel exportado não comprova resolução nativa.',fontes=provenance,cache=target),file.path(report,'mosaico_fontes.json'))
  if(mq_verified(target))return(target)
  if(file.exists(target))mq_stop('Mosaico em cache sem integridade: ',target)
  sizes<-vapply(sources,function(v){b<-DBI::dbGetQuery(v$db,'SELECT tile_data FROM tiles LIMIT 1')$tile_data[[1]];a<-mq_rgba(b);if(dim(a)[1]!=dim(a)[2]||!dim(a)[1]%in%c(256,512))mq_stop('Tile deve ser quadrado de 256 ou 512 pixels.');dim(a)[1]},integer(1))
  levels<-lapply(sources,function(v)sort(unique(v$d$z)));same<-all(sizes==256)&&length(levels[[1]])==1&&all(vapply(levels,identical,logical(1),levels[[1]]))
  tmp<-paste0(target,'.parcial');if(file.exists(tmp))unlink(tmp)
  db<-DBI::dbConnect(RSQLite::SQLite(),tmp);on.exit(if(DBI::dbIsValid(db))DBI::dbDisconnect(db),add=TRUE)
  DBI::dbExecute(db,'CREATE TABLE tiles(zoom_level INTEGER,tile_column INTEGER,tile_row INTEGER,tile_data BLOB,PRIMARY KEY(zoom_level,tile_column,tile_row))');DBI::dbExecute(db,'CREATE TABLE metadata(name TEXT PRIMARY KEY,value TEXT)')
  tile<-function(v,z,x,t)DBI::dbGetQuery(v$db,'SELECT tile_data FROM tiles WHERE zoom_level=? AND tile_column=? AND tile_row=?',params=list(z,x,t))$tile_data
  mask3857<-if(is.null(mask))NULL else terra::vect(sf::st_transform(mask,3857))
  put<-function(z,x,t,a) {
    if(!is.null(mask3857)){h<-20037508.342789244;w<-2*h/2^z;r<-terra::rast(nrows=256,ncols=256,xmin=-h+x*w,xmax=-h+(x+1)*w,ymin=-h+t*w,ymax=-h+(t+1)*w,crs='EPSG:3857');a[,,4]<-a[,,4]*as.matrix(terra::rasterize(mask3857,r,field=1,background=0),wide=TRUE)}
    if(any(a[,,4]>0))DBI::dbExecute(db,'INSERT INTO tiles VALUES(?,?,?,?)',params=list(z,x,t,list(png::writePNG(a,target=raw()))))}
  DBI::dbBegin(db)
  if(same) {
    keys<-unique(do.call(rbind,lapply(sources,`[[`,'d')));keys<-keys[order(keys$z,keys$x,keys$t),]
    bar<-mq_progress('Mosaico único',nrow(keys));on.exit(mq_progress_done(bar),add=TRUE)
    for(i in seq_len(nrow(keys))) {
      k<-keys[i,];a<-array(0,c(256,256,4))
      for(v in sources){raw<-tile(v,k$z,k$x,k$t);if(length(raw)){b<-mq_rgba(raw[[1]]);if(!identical(dim(b),c(256L,256L,4L)))mq_stop('Dimensões de tiles variam dentro da fonte.');a<-mq_alpha_over(a,b)}}
      put(k$z,k$x,k$t,a);mq_progress_update(bar,i)
    }
  } else {
    # Normalização esparsa: usa pixels nativos de 512 e expande apenas tiles existentes.
    zmax<-max(vapply(levels,max,numeric(1))+log2(sizes/256));zmin<-min(unlist(levels))
    keys<-list()
    for(j in seq_along(sources)) {
      d<-sources[[j]]$d
      if(sum(4^(zmax-d$z))>c$max_tiles*20)mq_stop('Normalização excede volume seguro; escolha fontes de resolução compatível.')
      for(i in seq_len(nrow(d))){k<-d[i,];factor<-2^(zmax-k$z);if(factor^2>c$max_tiles)mq_stop('Normalização excede max_tiles.');keys[[length(keys)+1]]<-expand.grid(x=k$x*factor+0:(factor-1),t=k$t*factor+0:(factor-1))}
    }
    keys<-unique(do.call(rbind,keys));if(nrow(keys)>c$max_tiles)mq_stop('Mosaico excede max_tiles.');bar<-mq_progress('Mosaico/resoluções',nrow(keys));on.exit(mq_progress_done(bar),add=TRUE)
    read_at<-function(v,z,x,t) {
      out<-array(0,c(256,256,4))
      for(zz in sort(unique(v$d$z[v$d$z<=z]),decreasing=TRUE)) {
        f<-2^(z-zz);xx<-floor(x/f);tt<-floor(t/f);raw<-tile(v,zz,xx,tt)
        if(length(raw)) {
          a<-mq_rgba(raw[[1]]);n<-dim(a)[1];if(dim(a)[2]!=n||!n%in%c(256,512))mq_stop('Tile inconsistente.')
          # Amostragem nearest preserva cores/alpha: tamanho de saída explicitamente reamostrado.
          cols<-pmin(n,floor(((x%%f)+(seq_len(256)-.5)/256)*n/f)+1)
          rows<-pmin(n,floor(((f-1-t%%f)+(seq_len(256)-.5)/256)*n/f)+1)
          out<-mq_alpha_over(out,a[rows,cols,,drop=FALSE]);if(all(out[,,4]>=1))return(out)
        }
      };out
    }
    for(i in seq_len(nrow(keys))){a<-array(0,c(256,256,4));for(v in sources)a<-mq_alpha_over(a,read_at(v,zmax,keys$x[i],keys$t[i]));put(zmax,keys$x[i],keys$t[i],a);mq_progress_update(bar,i)}
    if(zmax>zmin)for(z in seq(zmax-1,zmin)) {
      d<-DBI::dbGetQuery(db,'SELECT DISTINCT cast(tile_column/2 as integer) x,cast(tile_row/2 as integer) t FROM tiles WHERE zoom_level=?',params=list(z+1))
      for(i in seq_len(nrow(d))) {
        a<-array(0,c(512,512,4))
        for(dx in 0:1)for(dt in 0:1){raw<-DBI::dbGetQuery(db,'SELECT tile_data FROM tiles WHERE zoom_level=? AND tile_column=? AND tile_row=?',params=list(z+1,2*d$x[i]+dx,2*d$t[i]+dt))$tile_data;if(length(raw))a[(1:256)+(1-dt)*256,(1:256)+dx*256,]<-mq_rgba(raw[[1]])}
        # Média premultiplicada evita franjas pretas nas bordas transparentes.
        prem<-a;for(k in 1:3)prem[,,k]<-a[,,k]*a[,,4]
        b<-(prem[seq(1,512,2),seq(1,512,2),]+prem[seq(2,512,2),seq(1,512,2),]+prem[seq(1,512,2),seq(2,512,2),]+prem[seq(2,512,2),seq(2,512,2),])/4
        for(k in 1:3)b[,,k]<-ifelse(b[,,4]>0,b[,,k]/pmax(b[,,4],1e-20),0)
        put(z,d$x[i],d$t[i],b)
      }
    }
  }
  d<-DBI::dbGetQuery(db,'SELECT min(zoom_level) zmin,max(zoom_level) zmax,count(*) n FROM tiles');if(d$n==0)mq_stop('Mosaico vazio.')
  # Bounds são a união declarada dos recortes, sem reutilizar descrição de uma fonte isolada.
  bounds<-lapply(sources,function(v)as.numeric(strsplit(v$meta$value[match('bounds',v$meta$name)],',',fixed=TRUE)[[1]]));b<-do.call(rbind,bounds)
  attribution<-unique(unlist(lapply(sources,function(v)v$meta$value[v$meta$name=='attribution'])))
  meta<-data.frame(name=c('name','format','type','version','minzoom','maxzoom','bounds','description','attribution','recorte_circular_m'),value=c('sat_escala_local','png','overlay','1.1',d$zmin,d$zmax,paste(c(min(b[,1]),min(b[,2]),max(b[,3]),max(b[,4])),collapse=','),'Mosaico de fontes locais e complementares; raio 500 m; detalhes em mosaico_fontes.json',paste(attribution,collapse='; '),'500'))
  DBI::dbWriteTable(db,'metadata',meta,append=TRUE);DBI::dbCommit(db);if(DBI::dbGetQuery(db,'PRAGMA integrity_check')[[1]]!='ok')mq_stop('Mosaico inválido.');DBI::dbDisconnect(db)
  if(!file.rename(tmp,target))mq_stop('Falha ao finalizar mosaico.');mq_seal(target)
}
mq_qgis_call <- function(runtime,script,mode,arg,log) {
  if(runtime$windows) {
    conv<-function(p)if(.Platform$OS.type=='windows')normalizePath(p,winslash='\\',mustWork=FALSE)else system2('wslpath',c('-w',shQuote(p)),stdout=TRUE)
    bat<-file.path(dirname(script),paste0('qgis_',mode,'.cmd'))
    q<-function(p)paste0('"',p,'"')
    # Nenhum segredo é inserido no comando; caminhos com metacaracteres de cmd são rejeitados.
    vals<-vapply(c(runtime$python,script,arg,log),conv,character(1));if(any(grepl('[%\r\n!]',vals)))mq_stop('Caminho com caractere incompatível com o launcher QGIS.')
    writeLines(c('@echo off','set QT_QPA_PLATFORM=windows',paste('call',q(vals[1]),q(vals[2]),mode,q(vals[3]),'>',q(vals[4]),'2>&1'),'exit /b %errorlevel%'),bat,useBytes=TRUE)
    launcher<-if(.Platform$OS.type=='windows')Sys.getenv('COMSPEC','cmd.exe')else '/mnt/c/Windows/System32/cmd.exe'
    rc<-system2(launcher,c('/d','/c',shQuote(conv(bat))),stdout=FALSE,stderr=FALSE)
  }else rc<-system2(runtime$python,c(shQuote(script),mode,shQuote(arg)),stdout=log,stderr=log,env='QT_QPA_PLATFORM=offscreen')
  if(file.exists(log)){lines<-readLines(log,warn=FALSE);for(line in lines[grepl('^Cartografia',lines)])message(line)}
  if(rc!=0)mq_stop('QGIS falhou; consulte ',log,' (código ',rc,').')
}
mq_qgis_prepare <- function(c,root,scratch) {
  for(n in names(MQ_RECURSOS))writeBin(jsonlite::base64_dec(MQ_RECURSOS[[n]]),file.path(scratch,n))
  python<-c$qgis_python
  windows<-.Platform$OS.type=='windows'
  if(is.null(python)) {
    search<-if(windows)'C:/Program Files/QGIS*/bin/python-qgis*.bat'else '/mnt/c/Program Files/QGIS*/bin/python-qgis*.bat'
    candidates<-sort(Sys.glob(search),decreasing=TRUE)
    if(length(candidates)){python<-candidates[1];windows<-TRUE}else python<-Sys.which('python3')
  }else windows<-grepl('\\.bat$',python,ignore.case=TRUE)
  if(!length(python)||!nzchar(python))mq_stop('QGIS/PyQGIS não localizado. Instale QGIS ou informe qgis_python antes de gerar a cartografia.')
  runtime<-list(python=python,windows=windows)
  mq_qgis_call(runtime,file.path(scratch,'cartografia_qgis.py'),'probe',file.path(root,'02_relatorio','qgis_capacidades.json'),file.path(root,'02_relatorio','qgis_probe.log'))
  runtime
}
mq_locator_base <- function(c,root) {
  # Bases oficiais de contexto; primeira obtenção ~27 MiB, reutilizadas com SHA256.
  cache<-file.path(c$cache_dir,'cartografia_ibge_2025');dir.create(cache,recursive=TRUE,showWarnings=FALSE)
  sources<-c(estados='https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2025/Brasil/BR_UF_2025.zip',biomas='https://geoftp.ibge.gov.br/informacoes_ambientais/estudos_ambientais/biomas/vetores/2025_Biomas-e-Sistema-Costeiro-Marinho-do-Brasil-1-250000_shp.zip')
  gp<-file.path(cache,'contexto_simplificado_1000m_v2.gpkg');meta<-file.path(cache,'fontes.json')
  if(!mq_verified(gp)||!file.exists(meta)) {
    audit<-list();tmpgp<-file.path(cache,'contexto_ibge.parcial.gpkg');if(file.exists(tmpgp))unlink(tmpgp);if(file.exists(gp))mq_stop('Base de contexto sem integridade; confira o cache: ',gp)
    for(n in names(sources)) {
      z<-file.path(cache,paste0(n,'.zip'))
      if(!mq_verified(z)) {
        message('Localizador: obtendo base oficial IBGE de ',n,' (primeira execução; cache persistente).')
        tmp<-paste0(z,'.parcial');res<-httr::GET(sources[[n]],httr::write_disk(tmp,overwrite=TRUE),httr::timeout(max(120,c$timeout_s)));httr::stop_for_status(res)
        if(!file.rename(tmp,z))mq_stop('Falha ao materializar base IBGE.');mq_seal(z)
      }
      folder<-tempfile('ibge_');dir.create(folder);entries<-utils::unzip(z,list=TRUE)$Name
      if(any(grepl('(^/|(^|/)\\.\\.(/|$)|:|\\\\)',entries)))mq_stop('Caminho inseguro no ZIP IBGE.')
      utils::unzip(z,exdir=folder);shp<-list.files(folder,'\\.shp$',recursive=TRUE,full.names=TRUE,ignore.case=TRUE)
      if(length(shp)!=1)mq_stop('Base IBGE com seleção de camada ambígua: ',n)
      x<-sf::st_read(shp,quiet=TRUE);if(is.na(sf::st_crs(x)))mq_stop('Base IBGE sem CRS.')
      field<-intersect(if(n=='estados')c('SIGLA_UF','SIGLA')else c('Bioma','BIOMA','NOM_BIOMA','NM_BIOMA'),names(x))[1]
      if(is.na(field))mq_stop('Campo de identificação ausente na base IBGE: ',n,'; campos: ',paste(names(x),collapse=', '))
      x<-sf::st_sf(nome=as.character(x[[field]]),geometry=sf::st_geometry(x));if(n=='estados')sf::st_write(sf::st_transform(sf::st_make_valid(x),4674),tmpgp,layer='estados_identificacao',quiet=TRUE,append=FALSE);x<-sf::st_transform(sf::st_simplify(sf::st_make_valid(sf::st_transform(x,5880)),dTolerance=1000,preserveTopology=TRUE),4674)
      if(n=='estados'&&nrow(x)!=27)mq_stop('Malha estadual incompleta.')
      sf::st_write(x,tmpgp,layer=n,quiet=TRUE,append=FALSE)
      audit[[n]]<-list(autoridade='IBGE',edicao='2025',fonte=sources[[n]],sha256_zip=mq_hash(z),simplificacao_m=1000,uso='Somente contexto cartográfico; não classifica pontos nem delimita áreas elegíveis.')
      unlink(folder,recursive=TRUE)
    }
    if(!file.rename(tmpgp,gp))mq_stop('Falha ao finalizar base do localizador.');mq_seal(gp);mq_json(audit,meta)
  }
  dir.create(file.path(root,'05_qgis/contexto'),recursive=TRUE,showWarnings=FALSE)
  if(!file.copy(gp,file.path(root,'05_qgis/contexto/contexto_ibge.gpkg'),overwrite=TRUE))mq_stop('Falha ao copiar base do localizador.')
  file.copy(meta,file.path(root,'02_relatorio/fontes_localizador.json'),overwrite=TRUE)
  invisible(gp)
}
mq_cartography <- function(layers,ras,ae,c,root,scratch) {
  mq_locator_base(c,root)
  message('Cartografia QGIS: quatro layouts, PDFs georreferenciados e PNGs. Aguarde a renderização.')
  project_root<-if(c$qgis_runtime$windows&&.Platform$OS.type!='windows')system2('wslpath',c('-w',shQuote(root)),stdout=TRUE)else root
  config<-file.path(scratch,'cartografia.json');mq_json(list(root=project_root,projeto=c$projeto,bioma_min_mm2=c$localizador_bioma_min_mm2,dpi=c$mapas_dpi,papel=c$mapas_papel,elaboracao=c$elaboracao,camadas=lapply(layers,function(l)list(nome=l$nome,papel=l$papel))),config)
  mq_qgis_call(c$qgis_runtime,file.path(scratch,'cartografia_qgis.py'),'build',config,file.path(root,'02_relatorio','qgis_cartografia.log'))
  evidence<-jsonlite::read_json(file.path(root,'02_relatorio/cartografia.json'));if(evidence$status!='PASS'||length(evidence$mapas)!=4)mq_stop('Cartografia não validada.')
}

# Regras geométricas explícitas; nenhuma interpretação automática de imagem.
mq_protocol <- function(c) {
  if(!c$perfil%in%c('campestre_savanico','ilha','personalizado'))mq_stop('Perfil inválido.')
  if(c$perfil=='campestre_savanico') {
    c$direcoes_campo<-c('N','L','S','O')
    c$distancias_viarias_m<-c(estradas_pavimentadas=100,estradas_terra=50,trilhas_preexistentes=5)
  } else {
    p<-c$parametros_protocolo
    if(!is.list(p)||!all(c('transecto_m','grade_m')%in%names(p)))mq_stop('Perfil ',c$perfil,': informe parametros_protocolo com transecto_m e grade_m; padrões campestres não são herdados.')
    c$transecto_m<-p$transecto_m;c$grade_m<-p$grade_m
    c$distancia_min_m<-NA_real_;c$deslocamento_max_m<-NA_real_
    c$direcoes_campo<-character();c$distancias_viarias_m<-numeric()
    if(c$perfil=='personalizado') {
      if(!all(c('direcoes','distancias_viarias_m')%in%names(p))||!length(p$direcoes)||any(!p$direcoes%in%c('N','L','S','O'))||anyDuplicated(p$direcoes))mq_stop('Personalizado: declare direcoes e distancias_viarias_m.')
      d<-p$distancias_viarias_m
      if(!is.numeric(d)||!length(d)||is.null(names(d))||anyDuplicated(names(d))||any(!names(d)%in%c('estradas_pavimentadas','estradas_terra','trilhas_preexistentes'))||any(!is.finite(d)|d<0))mq_stop('Distâncias viárias personalizadas inválidas.')
      c$direcoes_campo<-p$direcoes;c$distancias_viarias_m<-d
    }
  }
  for(n in c('usar_estradas_pavimentadas','usar_estradas_terra','usar_trilhas_preexistentes'))
    if(!is.null(c[[n]])&&(length(c[[n]])!=1||!is.logical(c[[n]])||is.na(c[[n]])))mq_stop('Use NULL, TRUE ou FALSE em ',n)
  c
}
mq_segments <- function(points,c) {
  if(!nrow(points))return(list())
  xy<-sf::st_coordinates(points)[,1:2,drop=FALSE];ll<-sf::st_coordinates(sf::st_transform(points,4326));ll[,2]<-ll[,2]+1e-5
  north<-sf::st_coordinates(sf::st_transform(sf::st_as_sf(data.frame(lon=ll[,1],lat=ll[,2]),coords=c('lon','lat'),crs=4326),sf::st_crs(points)))-xy
  north<-north/sqrt(rowSums(north^2))
  ans<-lapply(c$direcoes_campo,function(k){v<-switch(k,N=north,L=cbind(north[,2],-north[,1]),S=-north,O=cbind(-north[,2],north[,1]));sf::st_sfc(lapply(seq_len(nrow(points)),function(i)sf::st_linestring(rbind(xy[i,],xy[i,]+c$transecto_m*v[i,]))),crs=sf::st_crs(points))})
  stats::setNames(ans,c$direcoes_campo)
}
mq_road_sources <- function(layers,c,report) {
  audit<-list();targets<-list()
  for(role in c('estradas_pavimentadas','estradas_terra','trilhas_preexistentes')) {
    flag<-c[[paste0('usar_',role)]];src<-Filter(function(l)l$papel==role,layers);count<-sum(vapply(src,function(l)nrow(l$x),integer(1)))
    if(isTRUE(flag)&&!count)mq_stop('Camada exigida ausente ou vazia: ',role)
    active<-count>0&&!identical(flag,FALSE)&&role%in%names(c$distancias_viarias_m)
    if(active) {
      items<-lapply(src,function(l){x<-l$x;line<-as.character(sf::st_geometry_type(x))%in%c('LINESTRING','MULTILINESTRING');width<-rep(0,nrow(x));
        if('largura_m'%in%names(x)){w<-suppressWarnings(as.numeric(x$largura_m));if(any(line&(!is.finite(w)|w<0)))mq_stop('largura_m inválida em ',role);width[line]<-w[line]}
        # Limiar por feição: distância ao eixo acrescida de metade da largura, quando declarada.
        sf::st_sf(limite=c$distancias_viarias_m[[role]]+width/2,geometry=sf::st_geometry(x))})
      targets[[role]]<-do.call(rbind,items)
    }
    audit[[role]]<-list(configuracao=if(is.null(flag))'auto'else flag,feicoes=count,aplicada=active,distancia_m=if(role%in%names(c$distancias_viarias_m))c$distancias_viarias_m[[role]]else NULL,fontes=vapply(src, function(l)if(is.null(l$fonte))l$papel else l$fonte,character(1)),nota=if(!count)'Não fornecida/vazia: ausência de conflito não comprovada.'else if(c$perfil=='ilha')'Referência visual: procedimento campestre não se aplica ao perfil Ilha.'else if(identical(flag,FALSE))'Desabilitada explicitamente; somente referência visual.'else 'Polígonos: borda; linhas: eixo, com largura_m/2 quando informada. Sem largura, limitação de distância ao eixo. Cobertura fora das AEs deve ser fornecida.')
    message('Vias — ',role,': ',count,' feições; ',if(active)'restrição aplicada'else 'restrição não aplicada')
  }
  legacy<-Filter(function(l)l$papel%in%c('rodovias','estradas','trilhas'),layers)
  if(length(legacy))message('Vias com nomes legados: somente referência. Declare papel explícito em camadas_qfield.csv para aplicar restrições.')
  mq_json(list(perfil=c$perfil,camadas=audit,legadas_sem_classificacao=vapply(legacy,`[[`,character(1),'nome')),file.path(report,'restricoes_viarias.json'))
  targets
}
mq_road_screen <- function(g,targets,ae,c,report,tag='grade') {
  g$mq_viavel_vias<-rep(TRUE,nrow(g));g$mq_direcoes_vias<-rep(NA_character_,nrow(g));g$mq_status_vias<-rep('não avaliado: sem restrições viárias ativas',nrow(g))
  if(!length(targets)||!nrow(g))return(g)
  segments<-mq_segments(g,c);domain<-sf::st_union(ae);rows<-list();viable<-matrix(FALSE,nrow(g),length(segments),dimnames=list(NULL,names(segments)))
  progress<-mq_progress('Triagem de restrições viárias',length(segments));on.exit(mq_progress_done(progress),add=TRUE)
  for(k in seq_along(segments)) {
    line<-segments[[k]];inside<-lengths(sf::st_covered_by(line,domain))>0
    d<-data.frame(id=if('chave_grade'%in%names(g))g$chave_grade else seq_len(nrow(g)),direcao=names(segments)[k],inteiro_na_AE=inside)
    conflict<-rep(FALSE,nrow(g))
    for(role in names(targets)) {
      t<-targets[[role]];near<-sf::st_is_within_distance(line,t,dist=max(t$limite))
      v<-vapply(seq_along(near),function(i)length(near[[i]])>0&&any(as.numeric(sf::st_distance(line[i],t[near[[i]],]))<t$limite[near[[i]]]-1e-7),logical(1))
      d[[paste0('conflito_',role)]]<-v;conflict<-conflict|v
    }
    viable[,k]<-!conflict;d$viavel<-viable[,k];rows[[k]]<-d;mq_progress_update(progress,k,names(segments)[k])
  }
  g$mq_viavel_vias<-rowSums(viable)>0;g$mq_direcoes_vias<-apply(viable,1,function(v)paste(colnames(viable)[v],collapse=','))
  g$mq_status_vias<-ifelse(g$mq_viavel_vias,'sem conflito detectado nas fontes ativas','indisponível no PA original; eventual deslocamento exige avaliação de campo')
  mq_csv(do.call(rbind,rows),file.path(report,paste0('restricoes_direcoes_',tag,'.csv')))
  mq_csv(sf::st_drop_geometry(g),file.path(report,paste0('restricoes_pontos_',tag,'.csv')))
  g
}
mq_road_available <- function(g) if('mq_viavel_vias'%in%names(g))g$mq_viavel_vias else rep(TRUE,nrow(g))

MQ_RECURSOS <- list("logo_cbc.png"="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","logo_icmbio.png"="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","logo_monitora.png"="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","cartografia_qgis.py"="IiIiUmVuZGVyaXphZG9yIFFHSVMgaW5jb3Jwb3JhZG8gYW8gUi4gU2VtIGRlcGVuZMOqbmNpYSBkZSByZWRlIG5hIGNhcnRvZ3JhZmlhLiIiIgppbXBvcnQgb3Msc3lzLGpzb24sbWF0aCxzaHV0aWwsaGFzaGxpYixkYXRldGltZSxyZSx1bmljb2RlZGF0YSx0ZXh0d3JhcApmcm9tIHhtbC5zYXguc2F4dXRpbHMgaW1wb3J0IGVzY2FwZQpmcm9tIHBhdGhsaWIgaW1wb3J0IFBhdGgKb3MuZW52aXJvblsnUVRfUVBBX1BMQVRGT1JNJ109J3dpbmRvd3MnIGlmIG9zLm5hbWU9PSdudCcgZWxzZSAnb2Zmc2NyZWVuJwpmcm9tIHFnaXMuY29yZSBpbXBvcnQgKgpmcm9tIHFnaXMuUHlRdC5RdENvcmUgaW1wb3J0IFFTaXplLFF0CmZyb20gcWdpcy5QeVF0LlF0R3VpIGltcG9ydCBRQ29sb3IsUUZvbnQsUUltYWdlCmZyb20gb3NnZW8gaW1wb3J0IGdkYWwsb3NyCmFwcD1RZ3NBcHBsaWNhdGlvbihbXSxGYWxzZSk7YXBwLmluaXRRZ2lzKCk7Z2RhbC5Vc2VFeGNlcHRpb25zKCkKCmRlZiBkdW1wKGRhdGEscGF0aCk6IFBhdGgocGF0aCkud3JpdGVfdGV4dChqc29uLmR1bXBzKGRhdGEsZW5zdXJlX2FzY2lpPUZhbHNlLGluZGVudD0yKSxlbmNvZGluZz0ndXRmLTgnKQpkZWYgc2hhKHBhdGgpOgogaD1oYXNobGliLnNoYTI1NigpCiB3aXRoIG9wZW4ocGF0aCwncmInKSBhcyBmOgogIGZvciBiIGluIGl0ZXIobGFtYmRhOmYucmVhZCgxMDI0KjEwMjQpLGInJyk6aC51cGRhdGUoYikKIHJldHVybiBoLmhleGRpZ2VzdCgpCmRlZiBjaGVja19wZGYocGF0aCxjcnMpOgogZHM9Z2RhbC5PcGVuRXgoc3RyKHBhdGgpLGdkYWwuT0ZfUkFTVEVSLG9wZW5fb3B0aW9ucz1bJ0RQST03MiddKTthc3NlcnQgZHMgaXMgbm90IE5vbmUsJ1BERiBzZW0gZHJpdmVyIHJhc3RlcicKIHdrdD1kcy5HZXRQcm9qZWN0aW9uKCk7YXNzZXJ0IHdrdCwnUERGIHNlbSBDUlMnO3NyPW9zci5TcGF0aWFsUmVmZXJlbmNlKCk7c3IuSW1wb3J0RnJvbVdrdCh3a3QpO2V4cGVjdGVkPW9zci5TcGF0aWFsUmVmZXJlbmNlKCk7ZXhwZWN0ZWQuSW1wb3J0RnJvbVdrdChjcnMudG9Xa3QoKSk7YXNzZXJ0IHNyLklzU2FtZShleHBlY3RlZCksJ0NSUyBkaXZlcmdlbnRlIG5vIFBERicKIGd0PWRzLkdldEdlb1RyYW5zZm9ybShjYW5fcmV0dXJuX251bGw9VHJ1ZSk7YXNzZXJ0IGd0IGFuZCBndCE9KDAsMSwwLDAsMCwxKSwnUERGIHNlbSB0cmFuc2Zvcm1hw6fDo28nCiByZXN1bHQ9eydjcnMnOnNyLkdldEF1dGhvcml0eUNvZGUoTm9uZSksJ2dlb3RyYW5zZm9ybSc6Z3QsJ2xhcmd1cmFfcHhfNzJkcGknOmRzLlJhc3RlclhTaXplLCdhbHR1cmFfcHhfNzJkcGknOmRzLlJhc3RlcllTaXplfTtkcz1Ob25lO3JldHVybiByZXN1bHQKCmRlZiBwcm9iZShvdXRwdXQpOgogYXNzZXJ0IGdkYWwuR2V0RHJpdmVyQnlOYW1lKCdQREYnKSwgJ0dEQUwgc2VtIFBERicKIHByb2plY3Q9UWdzUHJvamVjdCgpO3Byb2plY3Quc2V0Q3JzKFFnc0Nvb3JkaW5hdGVSZWZlcmVuY2VTeXN0ZW0oMzE5ODMpKTtsYXlvdXQ9UWdzUHJpbnRMYXlvdXQocHJvamVjdCk7bGF5b3V0LmluaXRpYWxpemVEZWZhdWx0cygpCiBtPVFnc0xheW91dEl0ZW1NYXAobGF5b3V0KTtsYXlvdXQuYWRkTGF5b3V0SXRlbShtKTttLmF0dGVtcHRNb3ZlKFFnc0xheW91dFBvaW50KDIwLDIwKSk7bS5hdHRlbXB0UmVzaXplKFFnc0xheW91dFNpemUoMTAwLDEwMCkpO20uc2V0Q3JzKHByb2plY3QuY3JzKCkpO20uem9vbVRvRXh0ZW50KFFnc1JlY3RhbmdsZSg2MDAwMDAsODAwMDAwMCw2MDEwMDAsODAwMTAwMCkpO2xheW91dC5zZXRSZWZlcmVuY2VNYXAobSkKIGY9UGF0aChvdXRwdXQpLndpdGhfc3VmZml4KCcucGRmJyk7c2V0dGluZ3M9UWdzTGF5b3V0RXhwb3J0ZXIuUGRmRXhwb3J0U2V0dGluZ3MoKTtzZXR0aW5ncy5hcHBlbmRHZW9yZWZlcmVuY2U9VHJ1ZTtzZXR0aW5ncy5kcGk9OTYKIGFzc2VydCBRZ3NMYXlvdXRFeHBvcnRlcihsYXlvdXQpLmV4cG9ydFRvUGRmKHN0cihmKSxzZXR0aW5ncyk9PVFnc0xheW91dEV4cG9ydGVyLlN1Y2Nlc3MsJ0V4cG9ydGHDp8OjbyBQREYgaW5kaXNwb27DrXZlbCcKIGV2aWRlbmNlPWNoZWNrX3BkZihmLHByb2plY3QuY3JzKCkpO2YudW5saW5rKCk7ZHVtcCh7J3N0YXR1cyc6J1BBU1MnLCdRR0lTJzpRZ2lzLlFHSVNfVkVSU0lPTiwnR0RBTCc6Z2RhbC5WZXJzaW9uSW5mbygpLCdQREYnOmV2aWRlbmNlfSxvdXRwdXQpCgpkZWYgbGFiZWwobGF5b3V0LHRleHQseCx5LHcsaCxzaXplPTksYm9sZD1GYWxzZSxjb2xvcj0nIzE3NGIzYicpOgogaXRlbT1RZ3NMYXlvdXRJdGVtTGFiZWwobGF5b3V0KTtpdGVtLnNldFRleHQodGV4dCk7Zm9udD1RRm9udCgnQXJpYWwnKTtmb250LnNldFBvaW50U2l6ZUYoc2l6ZSk7Zm9udC5zZXRCb2xkKGJvbGQpO2l0ZW0uc2V0Rm9udChmb250KTtpdGVtLnNldEZvbnRDb2xvcihRQ29sb3IoY29sb3IpKTtsYXlvdXQuYWRkTGF5b3V0SXRlbShpdGVtKTtpdGVtLmF0dGVtcHRNb3ZlKFFnc0xheW91dFBvaW50KHgseSkpO2l0ZW0uYXR0ZW1wdFJlc2l6ZShRZ3NMYXlvdXRTaXplKHcsaCkpO3JldHVybiBpdGVtCgpkZWYgYm94KGxheW91dCx4LHksdyxoKToKIGl0ZW09UWdzTGF5b3V0SXRlbVNoYXBlKGxheW91dCk7aXRlbS5zZXRTaGFwZVR5cGUoUWdzTGF5b3V0SXRlbVNoYXBlLlJlY3RhbmdsZSk7aXRlbS5zZXRTeW1ib2woUWdzRmlsbFN5bWJvbC5jcmVhdGVTaW1wbGUoeydjb2xvcic6JzI0OCwyNTAsMjQ4LDI1NScsJ291dGxpbmVfY29sb3InOicxMjAsMTQwLDEzMCwyNTUnLCdvdXRsaW5lX3dpZHRoJzonMC4xNSd9KSk7bGF5b3V0LmFkZExheW91dEl0ZW0oaXRlbSk7aXRlbS5hdHRlbXB0TW92ZShRZ3NMYXlvdXRQb2ludCh4LHkpKTtpdGVtLmF0dGVtcHRSZXNpemUoUWdzTGF5b3V0U2l6ZSh3LGgpKTtyZXR1cm4gaXRlbQoKZGVmIGV4dGVudF9vZihsYXllciwgZmFsbGJhY2spOgogaWYgbGF5ZXIgYW5kIGxheWVyLmZlYXR1cmVDb3VudCgpPjA6CiAgZT1RZ3NSZWN0YW5nbGUobGF5ZXIuZXh0ZW50KCkpCiBlbHNlOmU9UWdzUmVjdGFuZ2xlKGZhbGxiYWNrKQogaWYgZS53aWR0aCgpPDEwMDplLnNldFhNaW5pbXVtKGUuY2VudGVyKCkueCgpLTUwKTtlLnNldFhNYXhpbXVtKGUueE1pbmltdW0oKSsxMDApCiBpZiBlLmhlaWdodCgpPDEwMDplLnNldFlNaW5pbXVtKGUuY2VudGVyKCkueSgpLTUwKTtlLnNldFlNYXhpbXVtKGUueU1pbmltdW0oKSsxMDApCiBlLnNjYWxlKDEuMDYpO3JldHVybiBlCgpkZWYgc3R5bGVfbGFiZWxzKGxheWVyLHNpemUpOgogcD1sYXllci5sYWJlbGluZygpLnNldHRpbmdzKCk7Zm10PXAuZm9ybWF0KCk7Zm10LnNldFNpemUoc2l6ZSk7Zm10LnNldEZvbnQoUUZvbnQoJ0FyaWFsJyxzaXplKSk7Zm10LnNldENvbG9yKFFDb2xvcignd2hpdGUnKSk7YnVmPVFnc1RleHRCdWZmZXJTZXR0aW5ncygpO2J1Zi5zZXRFbmFibGVkKFRydWUpO2J1Zi5zZXRTaXplKC40NSk7YnVmLnNldENvbG9yKFFDb2xvcignIzIwMjgyMCcpKTtmbXQuc2V0QnVmZmVyKGJ1Zik7cC5zZXRGb3JtYXQoZm10KQogcC5kaXNwbGF5QWxsPUZhbHNlO3AucHJpb3JpdHk9ODtwLmRpc3Q9MS41CiBjYWxsPVFnc1NpbXBsZUxpbmVDYWxsb3V0KCk7Y2FsbC5zZXRFbmFibGVkKFRydWUpO3Auc2V0Q2FsbG91dChjYWxsKQogbGF5ZXIuc2V0TGFiZWxpbmcoUWdzVmVjdG9yTGF5ZXJTaW1wbGVMYWJlbGluZyhwKSk7bGF5ZXIuc2V0TGFiZWxzRW5hYmxlZChUcnVlKQoKZGVmIGNob29zZV9sYWJlbHMobGF5ZXIsZXh0ZW50LHcsaCxwcm9qZWN0KToKIGlmIG5vdCBsYXllciBvciBsYXllci5mZWF0dXJlQ291bnQoKT09MDpyZXR1cm4geydmb250ZV9wdCc6Tm9uZSwncm90dWxvc19leGliaWRvcyc6MCwncm90dWxvc19zdXByaW1pZG9zJzowLCd0b3RhbCc6MH0KIGJlc3Q9KC0xLDApO3Zpc2libGU9MDt0b3RhbD1sYXllci5mZWF0dXJlQ291bnQoKQogZm9yIHNpemUgaW4gKDksOCw3KToKICBzdHlsZV9sYWJlbHMobGF5ZXIsc2l6ZSk7bXM9UWdzTWFwU2V0dGluZ3MoKTttcy5zZXRMYXllcnMoW2xheWVyXSk7bXMuc2V0RGVzdGluYXRpb25DcnMocHJvamVjdC5jcnMoKSk7bXMuc2V0RXh0ZW50KGV4dGVudCk7bXMuc2V0T3V0cHV0U2l6ZShRU2l6ZShyb3VuZCh3KjE1MC8yNS40KSxyb3VuZChoKjE1MC8yNS40KSkpO21zLnNldE91dHB1dERwaSgxNTApCiAgam9iPVFnc01hcFJlbmRlcmVyUGFyYWxsZWxKb2IobXMpO2pvYi5zdGFydCgpO2pvYi53YWl0Rm9yRmluaXNoZWQoKTthc3NlcnQgbm90IGpvYi5lcnJvcnMoKSxqb2IuZXJyb3JzKCk7cmVzdWx0cz1qb2IudGFrZUxhYmVsaW5nUmVzdWx0cygpO3Zpc2libGU9bGVuKHtwLmZlYXR1cmVJZCBmb3IgcCBpbiByZXN1bHRzLmxhYmVsc1dpdGhpblJlY3QobXMuZXh0ZW50KCkpIGlmIHAubGF5ZXJJRD09bGF5ZXIuaWQoKX0pCiAgaWYgKHZpc2libGUsc2l6ZSk+YmVzdDpiZXN0PSh2aXNpYmxlLHNpemUpCiAgaWYgdmlzaWJsZT09dG90YWw6YnJlYWsKIHN0eWxlX2xhYmVscyhsYXllcixiZXN0WzFdKTtyZXR1cm4geydmb250ZV9wdCc6YmVzdFsxXSwncm90dWxvc19wcmV2aWEnOmJlc3RbMF0sJ3RvdGFsJzp0b3RhbH0KCmRlZiB1Y190aXRsZShmZWF0dXJlKToKICMgU2lnbGEgZSBjYXRlZ29yaWEgdsOqbSBkYSBtZXNtYSBiYXNlIGZlZGVyYWw7IG7Do28gaW5mZXJpciBzaWdsYSBwZWxvIG5vbWUuCiBmaWVsZHM9ZmVhdHVyZS5maWVsZHMoKS5uYW1lcygpCiBhc3NlcnQgYWxsKG4gaW4gZmllbGRzIGZvciBuIGluIFsnbm9tZXVjJywnc2lnbGFfY2F0ZScsJ2NhdGVnb3JpYV8nXSksJ0Jhc2UgZmVkZXJhbCBzZW0gY2FtcG9zIGRlIG5vbWUvY2F0ZWdvcmlhL3NpZ2xhJwogbmFtZT1zdHIoZmVhdHVyZVsnbm9tZXVjJ10pLnN0cmlwKCk7c2lnbGE9c3RyKGZlYXR1cmVbJ3NpZ2xhX2NhdGUnXSkuc3RyaXAoKTtjYXRlZ29yeT1zdHIoZmVhdHVyZVsnY2F0ZWdvcmlhXyddKS5zdHJpcCgpCiBhc3NlcnQgbmFtZSBhbmQgc2lnbGEgYW5kIHNpZ2xhIT0nTlVMTCcsJ1VDIGZlZGVyYWwgc2VtIGlkZW50aWZpY2HDp8OjbycKIHN1ZmZpeD1yZS5zdWIocideJytyZS5lc2NhcGUoY2F0ZWdvcnkpK3InXHMqJywnJyxuYW1lLGZsYWdzPXJlLkkpCiBzdWZmaXg9cmUuc3ViKHInXicrcmUuZXNjYXBlKHNpZ2xhKStyJ1xzKicsJycsc3VmZml4LGZsYWdzPXJlLkkpLnRpdGxlKCkKIHN1ZmZpeD1yZS5zdWIocidcYihEYXxEYXN8RGV8RG98RG9zfEUpXGInLGxhbWJkYSBtOm1bMF0ubG93ZXIoKSxzdWZmaXgpCiByZXR1cm4gc2lnbGEudXBwZXIoKSsnICcrc3VmZml4CgpkZWYgbm9ybWFsaXplX25hbWUodmFsdWUpOgogcmV0dXJuICcnLmpvaW4oYyBmb3IgYyBpbiB1bmljb2RlZGF0YS5ub3JtYWxpemUoJ05GRCcsdmFsdWUudXBwZXIoKSkgaWYgbm90IHVuaWNvZGVkYXRhLmNvbWJpbmluZyhjKSkKCmRlZiBsb2NhdG9yX2xheWVycyhwcm9qZWN0LG91dCxhZXMsY3JzKToKIHBhdGg9b3V0Lydjb250ZXh0by9jb250ZXh0b19pYmdlLmdwa2cnO2Fzc2VydCBwYXRoLmlzX2ZpbGUoKSwnQmFzZSBJQkdFIGRvIGxvY2FsaXphZG9yIGF1c2VudGUnCiBzdGF0ZXM9UWdzVmVjdG9yTGF5ZXIoc3RyKHBhdGgpKyd8bGF5ZXJuYW1lPWVzdGFkb3MnLCdFc3RhZG9zIOKAlCBJQkdFIDIwMjUnLCdvZ3InKTtiaW9tZXM9UWdzVmVjdG9yTGF5ZXIoc3RyKHBhdGgpKyd8bGF5ZXJuYW1lPWJpb21hcycsJ0Jpb21hcyDigJQgSUJHRSAyMDI1Jywnb2dyJykKIGFzc2VydCBzdGF0ZXMuaXNWYWxpZCgpIGFuZCBiaW9tZXMuaXNWYWxpZCgpCiBwYWxldHRlPXsnQU1BWk9OSUEnOicjQzZERkM1JywnQ0FBVElOR0EnOicjRUFENkIxJywnQ0VSUkFETyc6JyNEREUzQjInLCdNQVRBIEFUTEFOVElDQSc6JyNCRkQ4QzgnLCdQQU1QQSc6JyNENURGQkInLCdQQU5UQU5BTCc6JyNDN0RDRTEnfQogY2F0cz1bXQogZm9yIG5hbWUgaW4gc29ydGVkKHtzdHIoZlsnbm9tZSddKSBmb3IgZiBpbiBiaW9tZXMuZ2V0RmVhdHVyZXMoKX0pOgogIGNvbG9yPXBhbGV0dGUuZ2V0KG5vcm1hbGl6ZV9uYW1lKG5hbWUpLCcjRUNFOERDJyk7Y2F0cy5hcHBlbmQoUWdzUmVuZGVyZXJDYXRlZ29yeShuYW1lLFFnc0ZpbGxTeW1ib2wuY3JlYXRlU2ltcGxlKHsnY29sb3InOmNvbG9yLCdvdXRsaW5lX2NvbG9yJzonIzhBODA2RCcsJ291dGxpbmVfd2lkdGgnOicwLjEyJ30pLG5hbWUpKQogYmlvbWVzLnNldFJlbmRlcmVyKFFnc0NhdGVnb3JpemVkU3ltYm9sUmVuZGVyZXIoJ25vbWUnLGNhdHMpKQogc3RhdGVzLnNldFJlbmRlcmVyKFFnc1NpbmdsZVN5bWJvbFJlbmRlcmVyKFFnc0ZpbGxTeW1ib2wuY3JlYXRlU2ltcGxlKHsnY29sb3InOicyNTUsMjU1LDI1NSwwJywnb3V0bGluZV9jb2xvcic6JyM0NTVhNjQnLCdvdXRsaW5lX3dpZHRoJzonMC4xNSd9KSkpCiBsYWJlbHM9UWdzUGFsTGF5ZXJTZXR0aW5ncygpO2xhYmVscy5maWVsZE5hbWU9J25vbWUnO2ZtdD1RZ3NUZXh0Rm9ybWF0KCk7Zm10LnNldEZvbnQoUUZvbnQoJ0FyaWFsJyw2KSk7Zm10LnNldFNpemUoNik7Zm10LnNldENvbG9yKFFDb2xvcignIzQ1NWE2NCcpKTtidWY9UWdzVGV4dEJ1ZmZlclNldHRpbmdzKCk7YnVmLnNldEVuYWJsZWQoVHJ1ZSk7YnVmLnNldENvbG9yKFFDb2xvcignd2hpdGUnKSk7YnVmLnNldFNpemUoLjM1KTtmbXQuc2V0QnVmZmVyKGJ1Zik7bGFiZWxzLnNldEZvcm1hdChmbXQpO3N0YXRlcy5zZXRMYWJlbGluZyhRZ3NWZWN0b3JMYXllclNpbXBsZUxhYmVsaW5nKGxhYmVscykpO3N0YXRlcy5zZXRMYWJlbHNFbmFibGVkKFRydWUpCiBnZW9tPVFnc0dlb21ldHJ5LnVuYXJ5VW5pb24oW2YuZ2VvbWV0cnkoKSBmb3IgYSBpbiBhZXMgZm9yIGYgaW4gYS5nZXRGZWF0dXJlcygpXSk7Z2VvbS50cmFuc2Zvcm0oUWdzQ29vcmRpbmF0ZVRyYW5zZm9ybShjcnMsc3RhdGVzLmNycygpLHByb2plY3QpKQogb3JpZ2luYWw9UWdzVmVjdG9yTGF5ZXIoc3RyKHBhdGgpKyd8bGF5ZXJuYW1lPWVzdGFkb3NfaWRlbnRpZmljYWNhbycsJ0lkZW50aWZpY2HDp8OjbyBkZSBlc3RhZG9zJywnb2dyJyk7YXNzZXJ0IG9yaWdpbmFsLmlzVmFsaWQoKQogc2VsZWN0ZWQ9W2YgZm9yIGYgaW4gb3JpZ2luYWwuZ2V0RmVhdHVyZXMoKSBpZiBmLmdlb21ldHJ5KCkuaW50ZXJzZWN0cyhnZW9tKV0KIGV4dGVudD1RZ3NSZWN0YW5nbGUoc2VsZWN0ZWRbMF0uZ2VvbWV0cnkoKS5ib3VuZGluZ0JveCgpKSBpZiBzZWxlY3RlZCBlbHNlIFFnc1JlY3RhbmdsZShnZW9tLmJvdW5kaW5nQm94KCkpCiBmb3IgZiBpbiBzZWxlY3RlZFsxOl06ZXh0ZW50LmNvbWJpbmVFeHRlbnRXaXRoKGYuZ2VvbWV0cnkoKS5ib3VuZGluZ0JveCgpKQogaWYgbm90IHNlbGVjdGVkOmV4dGVudD1RZ3NSZWN0YW5nbGUoZXh0ZW50LmNlbnRlcigpLngoKS0yLGV4dGVudC5jZW50ZXIoKS55KCktMixleHRlbnQuY2VudGVyKCkueCgpKzIsZXh0ZW50LmNlbnRlcigpLnkoKSsyKQogc3RhdGVzLnNldEN1c3RvbVByb3BlcnR5KCdtb25pdG9yYV91Zl9jb250ZXh0bycsJywnLmpvaW4oc3RyKGZbJ25vbWUnXSkgZm9yIGYgaW4gc2VsZWN0ZWQpIG9yICdBRXMgc2VtIGludGVyc2XDp8OjbyBjb20gYSBtYWxoYSBlc3RhZHVhbCBvcmlnaW5hbCcpCiBleHRlbnQuc2NhbGUoMS4xNSkKIG1hcmtlcj1RZ3NWZWN0b3JMYXllcignTXVsdGlQb2x5Z29uP2Nycz0nK2Nycy5hdXRoaWQoKSwnw4FyZWFzIEVsZWfDrXZlaXMg4oCUIGxvY2FsaXphw6fDo28nLCdtZW1vcnknKTsKIGZvciBhIGluIGFlczoKICBmb3IgZiBpbiBhLmdldEZlYXR1cmVzKCk6CiAgIGl0ZW09UWdzRmVhdHVyZSgpO2l0ZW0uc2V0R2VvbWV0cnkoZi5nZW9tZXRyeSgpKTthc3NlcnQgbWFya2VyLmRhdGFQcm92aWRlcigpLmFkZEZlYXR1cmVzKFtpdGVtXSlbMF0KIG1hcmtlci51cGRhdGVFeHRlbnRzKCkKICMgUGVyc2lzdGlyIHJlZmVyw6puY2lhIHJlZHV6aWRhIG5vIHByb2pldG8gZGUgZWRpw6fDo287IG7Do28gbW9kaWZpY2Egb3MgcG9sw61nb25vcyBkZSBjYW1wby4KIG9wdHM9UWdzVmVjdG9yRmlsZVdyaXRlci5TYXZlVmVjdG9yT3B0aW9ucygpO29wdHMuZHJpdmVyTmFtZT0nR1BLRyc7b3B0cy5sYXllck5hbWU9J2FyZWFzX2VsZWdpdmVpcyc7ZGVzdD1vdXQvJ2NvbnRleHRvL2xvY2FsaXphY2FvX2FlLmdwa2cnCiBhc3NlcnQgUWdzVmVjdG9yRmlsZVdyaXRlci53cml0ZUFzVmVjdG9yRm9ybWF0VjMobWFya2VyLHN0cihkZXN0KSxwcm9qZWN0LnRyYW5zZm9ybUNvbnRleHQoKSxvcHRzKVswXT09UWdzVmVjdG9yRmlsZVdyaXRlci5Ob0Vycm9yCiBtYXJrZXI9UWdzVmVjdG9yTGF5ZXIoc3RyKGRlc3QpKyd8bGF5ZXJuYW1lPWFyZWFzX2VsZWdpdmVpcycsJ8OBcmVhcyBFbGVnw612ZWlzIOKAlCBsb2NhbGl6YcOnw6NvJywnb2dyJyk7bWFya2VyLnNldFJlbmRlcmVyKFFnc1NpbmdsZVN5bWJvbFJlbmRlcmVyKFFnc0ZpbGxTeW1ib2wuY3JlYXRlU2ltcGxlKHsnY29sb3InOicjZTMxYTFjJywnb3V0bGluZV9jb2xvcic6JyNhNDAwMDAnLCdvdXRsaW5lX3dpZHRoJzonMC42J30pKSkKIGdyb3VwPXByb2plY3QubGF5ZXJUcmVlUm9vdCgpLmFkZEdyb3VwKCdDb250ZXh0byBkb3MgbG9jYWxpemFkb3JlcycpCiBmb3IgbGF5ZXIgaW4gW21hcmtlcixzdGF0ZXMsYmlvbWVzXToKICBsYXllci5zZXRDdXN0b21Qcm9wZXJ0eSgnbW9uaXRvcmFfY29udGV4dG8nLFRydWUpO3Byb2plY3QuYWRkTWFwTGF5ZXIobGF5ZXIsRmFsc2UpO2dyb3VwLmFkZExheWVyKGxheWVyKQogZ3JvdXAuc2V0RXhwYW5kZWQoRmFsc2UpCiByZXR1cm4gc3RhdGVzLGJpb21lcyxtYXJrZXIsZXh0ZW50LHBhbGV0dGUKCmRlZiB2aXNpYmxlX2Jpb21lcyhwcm9qZWN0LGJpb21lcyxtLG91dCxuYW1lLHRocmVzaG9sZCk6CiAjIE1lc21vIHJlY29ydGUvZ2VuZXJhbGl6YcOnw6NvIG5vIGRlc2VuaG8gZSBuYSBsZWdlbmRhOyBiYXNlIElCR0Ugb3JpZ2luYWwgcHJlc2VydmFkYS4KIGZyYW1lPVFnc0dlb21ldHJ5LmZyb21Qb2x5Z29uWFkoW1tRZ3NQb2ludFhZKHYueCgpLHYueSgpKSBmb3IgdiBpbiBtLnZpc2libGVFeHRlbnRQb2x5Z29uKCldXSkKIG1lbT1RZ3NWZWN0b3JMYXllcignTXVsdGlQb2x5Z29uP2Nycz0nK2Jpb21lcy5jcnMoKS5hdXRoaWQoKSwnQmlvbWFzIOKAlCAnK25hbWUsJ21lbW9yeScpO21lbS5kYXRhUHJvdmlkZXIoKS5hZGRBdHRyaWJ1dGVzKGJpb21lcy5maWVsZHMoKSk7bWVtLnVwZGF0ZUZpZWxkcygpO2F1ZGl0PVtdCiBmb3IgZiBpbiBiaW9tZXMuZ2V0RmVhdHVyZXMoKToKICBjdXQ9Zi5nZW9tZXRyeSgpLmludGVyc2VjdGlvbihmcmFtZSk7YXJlYT1jdXQuYXJlYSgpL2ZyYW1lLmFyZWEoKSptLnNpemVXaXRoVW5pdHMoKS53aWR0aCgpKm0uc2l6ZVdpdGhVbml0cygpLmhlaWdodCgpO3Nob3c9YXJlYT4wIGFuZCBhcmVhPj10aHJlc2hvbGQKICBhdWRpdC5hcHBlbmQoeydiaW9tYSc6c3RyKGZbJ25vbWUnXSksJ2FyZWFfcGFwZWxfbW0yJzphcmVhLCdyZXByZXNlbnRhZG8nOnNob3d9KQogIGlmIHNob3c6CiAgIGN1dC5jb252ZXJ0VG9NdWx0aVR5cGUoKTtpdGVtPVFnc0ZlYXR1cmUobWVtLmZpZWxkcygpKTtpdGVtLnNldEF0dHJpYnV0ZXMoZi5hdHRyaWJ1dGVzKCkpO2l0ZW0uc2V0R2VvbWV0cnkoY3V0KTthc3NlcnQgbWVtLmRhdGFQcm92aWRlcigpLmFkZEZlYXR1cmVzKFtpdGVtXSlbMF0KIG1lbS51cGRhdGVFeHRlbnRzKCk7ZGVzdD1vdXQvJ2NvbnRleHRvJy8oJ2Jpb21hc18nK25hbWUrJy5ncGtnJyk7b3B0cz1RZ3NWZWN0b3JGaWxlV3JpdGVyLlNhdmVWZWN0b3JPcHRpb25zKCk7b3B0cy5kcml2ZXJOYW1lPSdHUEtHJztvcHRzLmxheWVyTmFtZT0nYmlvbWFzJwogYXNzZXJ0IFFnc1ZlY3RvckZpbGVXcml0ZXIud3JpdGVBc1ZlY3RvckZvcm1hdFYzKG1lbSxzdHIoZGVzdCkscHJvamVjdC50cmFuc2Zvcm1Db250ZXh0KCksb3B0cylbMF09PVFnc1ZlY3RvckZpbGVXcml0ZXIuTm9FcnJvcgogbGF5ZXI9UWdzVmVjdG9yTGF5ZXIoc3RyKGRlc3QpKyd8bGF5ZXJuYW1lPWJpb21hcycsbWVtLm5hbWUoKSwnb2dyJyk7YXNzZXJ0IGxheWVyLmlzVmFsaWQoKTtsYXllci5zZXRSZW5kZXJlcihiaW9tZXMucmVuZGVyZXIoKS5jbG9uZSgpKTtsYXllci5zZXRDdXN0b21Qcm9wZXJ0eSgnbW9uaXRvcmFfY29udGV4dG8nLFRydWUpCiBwcm9qZWN0LmFkZE1hcExheWVyKGxheWVyLEZhbHNlKTtwcm9qZWN0LmxheWVyVHJlZVJvb3QoKS5maW5kR3JvdXAoJ0NvbnRleHRvIGRvcyBsb2NhbGl6YWRvcmVzJykuYWRkTGF5ZXIobGF5ZXIpCiByZXR1cm4gbGF5ZXIsW3ZbJ2Jpb21hJ10gZm9yIHYgaW4gYXVkaXQgaWYgdlsncmVwcmVzZW50YWRvJ11dLGF1ZGl0CgpkZWYgYnVpbGQoY29uZmlnX2ZpbGUpOgogY2ZnPWpzb24ubG9hZHMoUGF0aChjb25maWdfZmlsZSkucmVhZF90ZXh0KGVuY29kaW5nPSd1dGYtOCcpKTtyb290PVBhdGgoY2ZnWydyb290J10pO291dD1yb290LycwNV9xZ2lzJztvdXQubWtkaXIoZXhpc3Rfb2s9VHJ1ZSk7YXNzZXRzPW91dC8ncmVjdXJzb3MnO2Fzc2V0cy5ta2RpcihleGlzdF9vaz1UcnVlKQogcHJvamVjdD1RZ3NQcm9qZWN0Lmluc3RhbmNlKCk7YXNzZXJ0IHByb2plY3QucmVhZChzdHIocm9vdC8nMDFfcWZpZWxkL3Byb2pldG8ucWdzJykpCiBjcnM9cHJvamVjdC5jcnMoKTtwcm9qZWN0LnNldEZpbGVOYW1lKHN0cihvdXQvJ3Byb2pldG9fZWRpY2FvLnFneicpKTtwcm9qZWN0LnNldEZpbGVQYXRoU3RvcmFnZShRZ2lzLkZpbGVQYXRoVHlwZS5SZWxhdGl2ZSkKIHNvdXJjZV9oYXNoPXt9O2J5X25hbWU9e307YWxsX2xheWVycz1bXQogZm9yIG5vZGUgaW4gcHJvamVjdC5sYXllclRyZWVSb290KCkuZmluZExheWVycygpOgogIGxheWVyPW5vZGUubGF5ZXIoKTthc3NlcnQgbGF5ZXIgYW5kIGxheWVyLmlzVmFsaWQoKSxub2RlLm5hbWUoKTthbGxfbGF5ZXJzLmFwcGVuZChsYXllcik7YnlfbmFtZVtsYXllci5uYW1lKCldPWxheWVyCiAgaWYgaXNpbnN0YW5jZShsYXllcixRZ3NWZWN0b3JMYXllcik6CiAgIHNyYz1QYXRoKGxheWVyLnNvdXJjZSgpLnNwbGl0KCd8JylbMF0pO3NvdXJjZV9oYXNoW3N0cihzcmMpXT1zaGEoc3JjKTtkZXN0PW91dC8nZGFkb3MnL3NyYy5uYW1lO2Rlc3QucGFyZW50Lm1rZGlyKGV4aXN0X29rPVRydWUpO3NodXRpbC5jb3B5MihzcmMsZGVzdCkKICAgbGF5ZXIuc2V0RGF0YVNvdXJjZShzdHIoZGVzdCkrJ3wnK2xheWVyLnNvdXJjZSgpLnNwbGl0KCd8JywxKVsxXSxsYXllci5uYW1lKCksJ29ncicpO2xheWVyLnNldFJlYWRPbmx5KEZhbHNlKQogICBmb3JtPWxheWVyLmVkaXRGb3JtQ29uZmlnKCkKICAgZm9yIG5hbWUgaW4gWydjaGF2ZV9ncmFkZScsJ2lkX2dyYWRlJywnaWRfbWFsaGEnLCdjb2RpZ29fcGEnXToKICAgIGlkeD1sYXllci5maWVsZHMoKS5pbmRleEZyb21OYW1lKG5hbWUpCiAgICBpZiBpZHg+PTA6Zm9ybS5zZXRSZWFkT25seShpZHgsVHJ1ZSkKICAgbGF5ZXIuc2V0RWRpdEZvcm1Db25maWcoZm9ybSkKICAgaWYgbGF5ZXIuZ2VvbWV0cnlUeXBlKCk9PVFnaXMuR2VvbWV0cnlUeXBlLlBvaW50OgogICAgZm9yIG5hbWUsZXhwciBpbiB7J3FmX2xvbic6IngodHJhbnNmb3JtKCRnZW9tZXRyeSxAbGF5ZXJfY3JzLCdFUFNHOjQzMjYnKSkiLCdxZl9sYXQnOiJ5KHRyYW5zZm9ybSgkZ2VvbWV0cnksQGxheWVyX2NycywnRVBTRzo0MzI2JykpIiwncWZfeF9tJzoneCgkZ2VvbWV0cnkpJywncWZfeV9tJzoneSgkZ2VvbWV0cnkpJ30uaXRlbXMoKToKICAgICBpZHg9bGF5ZXIuZmllbGRzKCkuaW5kZXhGcm9tTmFtZShuYW1lKQogICAgIGlmIGlkeD49MDpsYXllci5zZXREZWZhdWx0VmFsdWVEZWZpbml0aW9uKGlkeCxRZ3NEZWZhdWx0VmFsdWUoZXhwcixUcnVlKSkKICAgYXNzZXJ0IGxheWVyLmlzVmFsaWQoKSxsYXllci5uYW1lKCkKICBlbHNlOgogICAjIFJlc29sdmUgY2FtaW5obyBhYnNvbHV0byBhbnRlcyBkZSBzYWx2YXIgbyBwcm9qZXRvIGVtIG91dHJhIHN1YnBhc3RhLgogICBpZiBsYXllci5wcm92aWRlclR5cGUoKT09J2dkYWwnOmxheWVyLnNldERhdGFTb3VyY2Uoc3RyKFBhdGgobGF5ZXIuc291cmNlKCkpLnJlc29sdmUoKSksbGF5ZXIubmFtZSgpLCdnZGFsJykKIGZvciBwIGluIChyb290LycudGVtcG9yYXJpb3MnKS5nbG9iKCdsb2dvXyoucG5nJyk6c2h1dGlsLmNvcHkyKHAsYXNzZXRzL3AubmFtZSkKIG5vcnRoPWFzc2V0cy8nbm9ydGUuc3ZnJztub3J0aC53cml0ZV90ZXh0KCc8c3ZnIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgd2lkdGg9IjUwIiBoZWlnaHQ9Ijk1IiB2aWV3Qm94PSIwIDAgNTAgOTUiPjx0ZXh0IHg9IjI1IiB5PSIxNiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1mYW1pbHk9IkFyaWFsIiBmb250LXNpemU9IjE4IiBmb250LXdlaWdodD0iYm9sZCI+TjwvdGV4dD48cGF0aCBkPSJNMjUgMjIgTDggODggTDI1IDcwIFoiIGZpbGw9IndoaXRlIiBzdHJva2U9IiMxMTEiIHN0cm9rZS13aWR0aD0iMiIvPjxwYXRoIGQ9Ik0yNSAyMiBMNDIgODggTDI1IDcwIFoiIGZpbGw9IiMxMTEiIHN0cm9rZT0id2hpdGUiIHN0cm9rZS13aWR0aD0iMSIvPjwvc3ZnPicpCiBhZXM9W2J5X25hbWVbdlsnbm9tZSddXSBmb3IgdiBpbiBjZmcuZ2V0KCdjYW1hZGFzJyxbeydub21lJzonQUUnLCdwYXBlbCc6J2FyZWFzX2VsZWdpdmVpcyd9XSkgaWYgdlsncGFwZWwnXT09J2FyZWFzX2VsZWdpdmVpcyddO2Fzc2VydCBhZXMsJ8OBcmVhcyBFbGVnw612ZWlzIGF1c2VudGVzJzthZT1hZXNbMF07YWVfZXh0ZW50PVFnc1JlY3RhbmdsZShhZS5leHRlbnQoKSkKIGZvciB2IGluIGFlc1sxOl06YWVfZXh0ZW50LmNvbWJpbmVFeHRlbnRXaXRoKHYuZXh0ZW50KCkpCiB1Yz1ieV9uYW1lLmdldCgnVUMnKTtncmlkPWJ5X25hbWUuZ2V0KCdncmFkZV9hbW9zdHJhbCcpO3JlZ2lvbmFsPWJ5X25hbWUuZ2V0KCdzYXRfZXNjYWxhX3JlZ2lvbmFsJykKIGFzc2VydCByZWdpb25hbCBpcyBub3QgTm9uZSwnQmFzZSByZWdpb25hbCBhdXNlbnRlJwogdWNfbmFtZXM9W10KIGlmIHVjOgogIGFlX3VuaW9uPVFnc0dlb21ldHJ5LnVuYXJ5VW5pb24oW2YuZ2VvbWV0cnkoKSBmb3IgYSBpbiBhZXMgZm9yIGYgaW4gYS5nZXRGZWF0dXJlcygpXSk7dWNfZmVhdHVyZXM9W2YgZm9yIGYgaW4gdWMuZ2V0RmVhdHVyZXMoKSBpZiBzdHIoZlsnZXNmZXJhYWRtJ10pLmNhc2Vmb2xkKCk9PSdmZWRlcmFsJyBhbmQgZi5nZW9tZXRyeSgpLmludGVyc2VjdGlvbihhZV91bmlvbikuYXJlYSgpPjBdCiAgaWYgbm90IHVjX2ZlYXR1cmVzOnByb2plY3QucmVtb3ZlTWFwTGF5ZXIodWMuaWQoKSk7dWM9Tm9uZQogIGVsc2U6CiAgIHVjX25hbWVzPVt1Y190aXRsZShmKSBmb3IgZiBpbiB1Y19mZWF0dXJlc107aWRzPScsJy5qb2luKCInIitzdHIoZlsnY251YyddKS5yZXBsYWNlKCInIiwiJyciKSsiJyIgZm9yIGYgaW4gdWNfZmVhdHVyZXMpCiAgIGlmIGxlbih1Y19mZWF0dXJlcykhPXVjLmZlYXR1cmVDb3VudCgpOgogICAgYXNzZXJ0IHVjLnNldFN1YnNldFN0cmluZygnImNudWMiIElOICgnK2lkcysnKScpO3VjLnJlbG9hZCgpCiAgIHN5bWJvbD11Yy5yZW5kZXJlcigpLnN5bWJvbCgpLmNsb25lKCkKICAgaWYgbGVuKHVjX25hbWVzKT4xOnVjLnNldFJlbmRlcmVyKFFnc0NhdGVnb3JpemVkU3ltYm9sUmVuZGVyZXIoJ2NudWMnLFtRZ3NSZW5kZXJlckNhdGVnb3J5KHN0cihmWydjbnVjJ10pLHN5bWJvbC5jbG9uZSgpLHVjX3RpdGxlKGYpKSBmb3IgZiBpbiB1Y19mZWF0dXJlc10pKQogICB1Yy5zZXROYW1lKHVjX25hbWVzWzBdIGlmIGxlbih1Y19uYW1lcyk9PTEgZWxzZSAnVW5pZGFkZXMgZGUgQ29uc2VydmHDp8OjbyBmZWRlcmFpcycpCiBmb3Igb3JpZ2luYWwsZnJpZW5kbHkgaW4gWygnZ3JhZGVfYW1vc3RyYWwnLCdHcmFkZSBBbW9zdHJhbCcpLCgnUEFfcHJpb3JpdCcsJ1BvbnRvcyBBbW9zdHJhaXMgcHJpb3JpdMOhcmlvcycpLCgnUEFfYWx0ZXJuJywnUG9udG9zIEFtb3N0cmFpcyBhbHRlcm5hdGl2b3MnKV06CiAgaWYgb3JpZ2luYWwgaW4gYnlfbmFtZTpieV9uYW1lW29yaWdpbmFsXS5zZXROYW1lKGZyaWVuZGx5KQogZm9yIGxheWVyIGluIGFlczpsYXllci5zZXROYW1lKCfDgXJlYXMgRWxlZ8OtdmVpcycrKCcg4oCUICcrbGF5ZXIubmFtZSgpIGlmIGxlbihhZXMpPjEgZWxzZSAnJykpCiBzdGF0ZXMsYmlvbWVzLGFlX21hcmtlcixzdGF0ZV9leHRlbnQscGFsZXR0ZT1sb2NhdG9yX2xheWVycyhwcm9qZWN0LG91dCxhZXMsY3JzKQogdWNfbG9jYXRvcj1Ob25lCiBpZiB1YzoKICB1Y19sb2NhdG9yPXVjLmNsb25lKCk7dWNfbG9jYXRvci5zZXROYW1lKHVjLm5hbWUoKSsnIOKAlCBsb2NhbGl6YWRvcicpO3VjX2xvY2F0b3Iuc2V0Q3VzdG9tUHJvcGVydHkoJ21vbml0b3JhX2NvbnRleHRvJyxUcnVlKTtwcm9qZWN0LmFkZE1hcExheWVyKHVjX2xvY2F0b3IsRmFsc2UpO3Byb2plY3QubGF5ZXJUcmVlUm9vdCgpLmZpbmRHcm91cCgnQ29udGV4dG8gZG9zIGxvY2FsaXphZG9yZXMnKS5hZGRMYXllcih1Y19sb2NhdG9yKQoKIGZvciB2IGluIGFlcytbdWMsZ3JpZF06CiAgaWYgdjp2LnNldExhYmVsc0VuYWJsZWQoRmFsc2UpCiBpZiBncmlkOgogIHN5bWJvbD1ncmlkLnJlbmRlcmVyKCkuc3ltYm9sKCk7c3ltYm9sLnNldFNpemUoLjcpO3N5bWJvbC5zeW1ib2xMYXllcigwKS5zZXRTdHJva2VXaWR0aCguMTIpCiBzY2VuZV9maWxlPXJvb3QvJzAyX3JlbGF0b3Jpby9zZW50aW5lbF9mb250ZS5qc29uJztzY2VuZV9pbmZvPWpzb24ubG9hZHMoc2NlbmVfZmlsZS5yZWFkX3RleHQoZW5jb2Rpbmc9J3V0Zi04JykpIGlmIHNjZW5lX2ZpbGUuZXhpc3RzKCkgZWxzZSB7fTtkYXRlcz1zb3J0ZWQoe2NbJ2RhdGEnXVs6MTBdIGZvciBjIGluIHNjZW5lX2luZm8uZ2V0KCdjZW5hcycsW10pIGlmIGMuZ2V0KCdkYXRhJyl9KTtkYXRlX2xhYmVsPScsICcuam9pbihkYXRlcykgaWYgbGVuKGRhdGVzKTw9MiBlbHNlIGRhdGVzWzBdKycgYSAnK2RhdGVzWy0xXQogc3BlY3M9WygnMDFfYXJlYXNfZWxlZ2l2ZWlzJywnw4FyZWFzIEVsZWfDrXZlaXMnLGFlLGFlcytbdWNdKSwoJzAyX2dyYWRlX2Ftb3N0cmFsJywnR3JhZGUgQW1vc3RyYWwnLGdyaWQsW2dyaWRdK2FlcytbdWNdKSwoJzAzX3BhX3ByaW9yaXRhcmlvcycsJ1BvbnRvcyBhbW9zdHJhaXMgcHJpb3JpdMOhcmlvcycsYnlfbmFtZS5nZXQoJ1BBX3ByaW9yaXQnKSxbYnlfbmFtZS5nZXQoJ1BBX3ByaW9yaXQnKSxncmlkXSthZXMrW3VjXSksKCcwNF9wYV9hbHRlcm5hdGl2b3MnLCdQb250b3MgYW1vc3RyYWlzIGFsdGVybmF0aXZvcycsYnlfbmFtZS5nZXQoJ1BBX2FsdGVybicpLFtieV9uYW1lLmdldCgnUEFfYWx0ZXJuJyksZ3JpZF0rYWVzK1t1Y10pXQogYXVkaXRzPVtdO2RwaT1pbnQoY2ZnWydkcGknXSk7ZmFjdG9yPW1hdGguc3FydCgyKSBpZiBjZmdbJ3BhcGVsJ109PSdBMycgZWxzZSAxCiBmb3IgaWR4LChuYW1lLHRpdGxlLHRhcmdldCx2ZWN0b3JzKSBpbiBlbnVtZXJhdGUoc3BlY3MpOgogIHByaW50KGYnQ2FydG9ncmFmaWEge2lkeCsxfS80OiB7dGl0bGV9JyxmbHVzaD1UcnVlKQogIGxheW91dD1RZ3NQcmludExheW91dChwcm9qZWN0KTtsYXlvdXQuaW5pdGlhbGl6ZURlZmF1bHRzKCk7bGF5b3V0LnNldE5hbWUobmFtZSk7cGFnZT1sYXlvdXQucGFnZUNvbGxlY3Rpb24oKS5wYWdlKDApO3BhZ2Uuc2V0UGFnZVNpemUoUWdzTGF5b3V0U2l6ZSgyMTAqZmFjdG9yLDI5NypmYWN0b3IpKTtsYXlvdXQucmVuZGVyQ29udGV4dCgpLnNldERwaShkcGkpCiAgZT1leHRlbnRfb2YoTm9uZSxhZV9leHRlbnQpIGlmIGlkeD09MCBlbHNlIGV4dGVudF9vZih0YXJnZXQsYWVfZXh0ZW50KTtsYW5kc2NhcGU9ZS53aWR0aCgpPmUuaGVpZ2h0KCkqMS4xNQogIHdpZHRoLGhlaWdodD0oMjk3KmZhY3RvciwyMTAqZmFjdG9yKSBpZiBsYW5kc2NhcGUgZWxzZSAoMjEwKmZhY3RvciwyOTcqZmFjdG9yKTtwYWdlLnNldFBhZ2VTaXplKFFnc0xheW91dFNpemUod2lkdGgsaGVpZ2h0KSk7Zng9d2lkdGgvMjEwO214LG15LG13LG1oPTEzKmZhY3RvciwyOSpmYWN0b3Isd2lkdGgtMjYqZmFjdG9yLGhlaWdodC0xMDUqZmFjdG9yCiAgbGFiZWwobGF5b3V0LHRpdGxlLDEzKmZhY3Rvciw1KmZhY3Rvcixtdyw5KmZhY3RvciwxNSxUcnVlKTtsYWJlbChsYXlvdXQsY2ZnWydwcm9qZXRvJ10sMTMqZmFjdG9yLDE1KmZhY3Rvcixtdyw5KmZhY3RvciwxMCkKICBtPVFnc0xheW91dEl0ZW1NYXAobGF5b3V0KTtsYXlvdXQuYWRkTGF5b3V0SXRlbShtKTttLnNldElkKG5hbWUrJ19wcmluY2lwYWwnKTttLmF0dGVtcHRNb3ZlKFFnc0xheW91dFBvaW50KG14LG15KSk7bS5hdHRlbXB0UmVzaXplKFFnc0xheW91dFNpemUobXcsbWgpKTttLnNldENycyhjcnMpO20uc2V0RnJhbWVFbmFibGVkKFRydWUpO20uem9vbVRvRXh0ZW50KGUpO2xheW91dC5zZXRSZWZlcmVuY2VNYXAobSkKICB2ZWN0b3JzPVt2IGZvciB2IGluIHZlY3RvcnMgaWYgdiBpcyBub3QgTm9uZV07bWFwbGF5ZXJzPXZlY3RvcnMrW3JlZ2lvbmFsXQogIG0uc2V0TGF5ZXJzKG1hcGxheWVycyk7bS5zZXRLZWVwTGF5ZXJTZXQoVHJ1ZSkKICBsYWJlbF9pbmZvPWNob29zZV9sYWJlbHModGFyZ2V0LG0uZXh0ZW50KCksbXcsbWgscHJvamVjdCkgaWYgbmFtZS5zdGFydHN3aXRoKCgnMDMnLCcwNCcpKSBlbHNlIHsndG90YWwnOjAsJ3JvdHVsb3NfZXhpYmlkb3MnOjAsJ3JvdHVsb3Nfc3VwcmltaWRvcyc6MH0KICBpZiBncmlkOmdyaWQuc2V0TGFiZWxzRW5hYmxlZChGYWxzZSkKICBzdHlsZXM9e30KICBmb3IgdiBpbiBtYXBsYXllcnM6CiAgIHN0PVFnc01hcExheWVyU3R5bGUoKTtzdC5yZWFkRnJvbUxheWVyKHYpO3N0eWxlc1t2LmlkKCldPXN0LnhtbERhdGEoKQogIG0uc2V0TGF5ZXJTdHlsZU92ZXJyaWRlcyhzdHlsZXMpO20uc2V0S2VlcExheWVyU3R5bGVzKFRydWUpCiAgIyBUZW1hIGUgbWFyY2Fkb3IgZGUgZXh0ZW5zw6NvIG1hbnTDqm0gb3MgcXVhdHJvIGVzdGFkb3MgcmVhYnLDrXZlaXMgbm8gUUdJUy4KICBmb3Igbm9kZSBpbiBwcm9qZWN0LmxheWVyVHJlZVJvb3QoKS5maW5kTGF5ZXJzKCk6bm9kZS5zZXRJdGVtVmlzaWJpbGl0eUNoZWNrZWQobm9kZS5sYXllcigpIGluIG1hcGxheWVycykKICB0aGVtZT1RZ3NNYXBUaGVtZUNvbGxlY3Rpb24uY3JlYXRlVGhlbWVGcm9tQ3VycmVudFN0YXRlKHByb2plY3QubGF5ZXJUcmVlUm9vdCgpLHByb2plY3QubGF5ZXJUcmVlTW9kZWwoKSBpZiBoYXNhdHRyKHByb2plY3QsJ2xheWVyVHJlZU1vZGVsJykgZWxzZSBRZ3NMYXllclRyZWVNb2RlbChwcm9qZWN0LmxheWVyVHJlZVJvb3QoKSkpCiAgcHJvamVjdC5tYXBUaGVtZUNvbGxlY3Rpb24oKS5pbnNlcnQodGl0bGUsdGhlbWUpCiAgYm9va21hcms9UWdzQm9va21hcmsoKTtib29rbWFyay5zZXROYW1lKHRpdGxlKTtib29rbWFyay5zZXRHcm91cCgnTW9uaXRvcmEnKTtib29rbWFyay5zZXRFeHRlbnQoUWdzUmVmZXJlbmNlZFJlY3RhbmdsZShtLmV4dGVudCgpLGNycykpO3Byb2plY3QuYm9va21hcmtNYW5hZ2VyKCkuYWRkQm9va21hcmsoYm9va21hcmspCiAgZ3I9bS5ncmlkKCk7Z3Iuc2V0RW5hYmxlZChUcnVlKTtnci5zZXRDcnMoUWdzQ29vcmRpbmF0ZVJlZmVyZW5jZVN5c3RlbSg0MzI2KSk7Z2U9UWdzQ29vcmRpbmF0ZVRyYW5zZm9ybShjcnMsUWdzQ29vcmRpbmF0ZVJlZmVyZW5jZVN5c3RlbSg0MzI2KSxwcm9qZWN0KS50cmFuc2Zvcm1Cb3VuZGluZ0JveChtLmV4dGVudCgpKTtpbnRlcnZhbD1tYXgoZ2Uud2lkdGgoKSxnZS5oZWlnaHQoKSkvNAogIHBvd2VyPTEwKiptYXRoLmZsb29yKG1hdGgubG9nMTAoaW50ZXJ2YWwpKTtpbnRlcnZhbD1taW4oW3YqcG93ZXIgZm9yIHYgaW4gWzEsMiw1LDEwXV0sa2V5PWxhbWJkYSB4OmFicyh4LWludGVydmFsKSk7Z3Iuc2V0SW50ZXJ2YWxYKGludGVydmFsKTtnci5zZXRJbnRlcnZhbFkoaW50ZXJ2YWwpO2dyLnNldFN0eWxlKFFnc0xheW91dEl0ZW1NYXBHcmlkLkZyYW1lQW5ub3RhdGlvbnNPbmx5KTtnci5zZXRBbm5vdGF0aW9uRW5hYmxlZChUcnVlKTtnci5zZXRBbm5vdGF0aW9uRm9ybWF0KFFnc0xheW91dEl0ZW1NYXBHcmlkLkRlY2ltYWxXaXRoU3VmZml4KTtnci5zZXRBbm5vdGF0aW9uUHJlY2lzaW9uKG1heCgyLC1pbnQobWF0aC5mbG9vcihtYXRoLmxvZzEwKGludGVydmFsKSkpKzEpKTtnci5zZXRBbm5vdGF0aW9uRm9udChRRm9udCgnQXJpYWwnLDcpKTtnci5zZXRGcmFtZVN0eWxlKFFnc0xheW91dEl0ZW1NYXBHcmlkLkV4dGVyaW9yVGlja3MpO2dyLnNldEFubm90YXRpb25EaXJlY3Rpb24oUWdzTGF5b3V0SXRlbU1hcEdyaWQuVmVydGljYWxEZXNjZW5kaW5nLFFnc0xheW91dEl0ZW1NYXBHcmlkLkxlZnQpO2dyLnNldEFubm90YXRpb25EaXJlY3Rpb24oUWdzTGF5b3V0SXRlbU1hcEdyaWQuVmVydGljYWxEZXNjZW5kaW5nLFFnc0xheW91dEl0ZW1NYXBHcmlkLlJpZ2h0KQogIHNjYWxlPVFnc0xheW91dEl0ZW1TY2FsZUJhcihsYXlvdXQpO3NjYWxlLnNldFN0eWxlKCdTaW5nbGUgQm94Jyk7c2NhbGUuc2V0TGlua2VkTWFwKG0pO3NjYWxlLmFwcGx5RGVmYXVsdFNpemUoKTtzY2FsZS5zZXRVbml0cyhRZ2lzLkRpc3RhbmNlVW5pdC5LaWxvbWV0ZXJzKTtzY2FsZS5zZXRVbml0c1BlclNlZ21lbnQobWF4KC4xLDEwKiptYXRoLmZsb29yKG1hdGgubG9nMTAobS5leHRlbnQoKS53aWR0aCgpLzYwMDApKSkpO3NjYWxlLnNldE51bWJlck9mU2VnbWVudHMoMik7c2NhbGUuc2V0TnVtYmVyT2ZTZWdtZW50c0xlZnQoMCk7c2NhbGUuc2V0VW5pdExhYmVsKCdrbScpO3NjYWxlLnNldEZvbnQoUUZvbnQoJ0FyaWFsJyw4KSk7c2NhbGUuc2V0QmFja2dyb3VuZEVuYWJsZWQoVHJ1ZSk7c2NhbGUuc2V0QmFja2dyb3VuZENvbG9yKFFDb2xvcigyNTUsMjU1LDI1NSwyMTApKTtsYXlvdXQuYWRkTGF5b3V0SXRlbShzY2FsZSk7c2NhbGUuYXR0ZW1wdE1vdmUoUWdzTGF5b3V0UG9pbnQobXgrMixteSttaC0xMSkpCiAgcGljPVFnc0xheW91dEl0ZW1QaWN0dXJlKGxheW91dCk7bGF5b3V0LmFkZExheW91dEl0ZW0ocGljKTtwaWMuYXR0ZW1wdE1vdmUoUWdzTGF5b3V0UG9pbnQobXgrbXctMTIsbXkrMikpO3BpYy5hdHRlbXB0UmVzaXplKFFnc0xheW91dFNpemUoOSwxOCkpO3BpYy5zZXRQaWN0dXJlUGF0aChzdHIobm9ydGgpKTtwaWMuc2V0TGlua2VkTWFwKG0pO3BpYy5zZXROb3J0aE1vZGUoUWdzTGF5b3V0SXRlbVBpY3R1cmUuVHJ1ZU5vcnRoKQogIGZ5PWhlaWdodC02NCpmYWN0b3I7Zmg9NTYqZmFjdG9yCiAgIyBGYWl4YSBpbmZlcmlvcjogZG9pcyBsb2NhbGl6YWRvcmVzIHF1YW5kbyBleGlzdGUgVUM7IGNhaXhhcyBhZGFwdGFkYXMgw6AgcMOhZ2luYS4KICBtYXJnaW49OCpmYWN0b3I7Z2FwPTIqZmFjdG9yO2F2YWlsYWJsZT13aWR0aC0yKm1hcmdpbgogIHdlaWdodHM9KFs0MiwzMiw0NSw1NSwxOF0gaWYgdWMgZWxzZSBbNjIsNTIsNjAsMThdKTt1bml0PShhdmFpbGFibGUtZ2FwKihsZW4od2VpZ2h0cyktMSkpL3N1bSh3ZWlnaHRzKTtzbG90cz1bXTt4eD1tYXJnaW4KICBmb3Igd3cgaW4gd2VpZ2h0czpzbG90cy5hcHBlbmQoKHh4LHd3KnVuaXQpKTt4eCs9d3cqdW5pdCtnYXAKICBmb3IgeHgsd3cgaW4gc2xvdHM6Ym94KGxheW91dCx4eCxmeSx3dyxmaCkKICByeCxydz1zbG90c1swXTtsYWJlbChsYXlvdXQsJ0VzdGFkb3MgZSBiaW9tYXMnLHJ4KzEsZnkrMSxydy0yLDUsNyxUcnVlKQogIGNvbnRleHRfbWFwPVFnc0xheW91dEl0ZW1NYXAobGF5b3V0KTtsYXlvdXQuYWRkTGF5b3V0SXRlbShjb250ZXh0X21hcCk7Y29udGV4dF9tYXAuc2V0SWQobmFtZSsnX2VzdGFkb3NfYmlvbWFzJyk7Y29udGV4dF9tYXAuYXR0ZW1wdE1vdmUoUWdzTGF5b3V0UG9pbnQocngrMixmeSs3KmZhY3RvcikpO2NvbnRleHRfbWFwLmF0dGVtcHRSZXNpemUoUWdzTGF5b3V0U2l6ZShydy00LDM1KmZhY3RvcikpO2NvbnRleHRfbWFwLnNldENycyhzdGF0ZXMuY3JzKCkpO2NvbnRleHRfbWFwLnNldExheWVycyhbYWVfbWFya2VyLHN0YXRlcyxiaW9tZXNdKTtjb250ZXh0X21hcC5zZXRLZWVwTGF5ZXJTZXQoVHJ1ZSk7Y29udGV4dF9tYXAuem9vbVRvRXh0ZW50KHN0YXRlX2V4dGVudCk7Y29udGV4dF9tYXAuc2V0RnJhbWVFbmFibGVkKFRydWUpCiAgIyBEZXN0YXF1ZSBkYSBleHRlbnPDo28gZGFzIEFFcyBnYXJhbnRlIGxvY2FsaXphw6fDo28gbGVnw612ZWwgbWVzbW8gZW0gw6FyZWFzIHBlcXVlbmFzLgogIG92ZXJ2aWV3PWNvbnRleHRfbWFwLm92ZXJ2aWV3KCk7b3ZlcnZpZXcuc2V0TGlua2VkTWFwKG0pO292ZXJ2aWV3LnNldEVuYWJsZWQoVHJ1ZSk7b3ZlcnZpZXcuc2V0RnJhbWVTeW1ib2woUWdzRmlsbFN5bWJvbC5jcmVhdGVTaW1wbGUoeydjb2xvcic6JzIyNywyNiwyOCw0NScsJ291dGxpbmVfY29sb3InOicjZTMxYTFjJywnb3V0bGluZV93aWR0aCc6JzAuNSd9KSkKICBsb2NhbF9iaW9tZXMsbmFtZXNfYmlvbWVzLGJpb21lX2F1ZGl0PXZpc2libGVfYmlvbWVzKHByb2plY3QsYmlvbWVzLGNvbnRleHRfbWFwLG91dCxuYW1lLGZsb2F0KGNmZy5nZXQoJ2Jpb21hX21pbl9tbTInLC41KSkpO25hbWVzX2Jpb21lcy5zb3J0KCk7Y29udGV4dF9tYXAuc2V0TGF5ZXJzKFthZV9tYXJrZXIsc3RhdGVzLGxvY2FsX2Jpb21lc10pCiAgZm9yIGosYm5hbWUgaW4gZW51bWVyYXRlKG5hbWVzX2Jpb21lcyk6CiAgIGJ4PXJ4KzIrKGolMikqKHJ3LTQpLzI7Ynk9ZnkrNDMqZmFjdG9yKyhqLy8yKSozLjUqZmFjdG9yCiAgIGNoaXA9UWdzTGF5b3V0SXRlbVNoYXBlKGxheW91dCk7Y2hpcC5zZXRTaGFwZVR5cGUoUWdzTGF5b3V0SXRlbVNoYXBlLlJlY3RhbmdsZSk7Y2hpcC5zZXRTeW1ib2woUWdzRmlsbFN5bWJvbC5jcmVhdGVTaW1wbGUoeydjb2xvcic6cGFsZXR0ZS5nZXQobm9ybWFsaXplX25hbWUoYm5hbWUpLCcjRUNFOERDJyksJ291dGxpbmVfY29sb3InOicjOEE4MDZEJywnb3V0bGluZV93aWR0aCc6JzAuMSd9KSk7bGF5b3V0LmFkZExheW91dEl0ZW0oY2hpcCk7Y2hpcC5hdHRlbXB0TW92ZShRZ3NMYXlvdXRQb2ludChieCxieSsuNikpO2NoaXAuYXR0ZW1wdFJlc2l6ZShRZ3NMYXlvdXRTaXplKDIsMikpO2xhYmVsKGxheW91dCxibmFtZSxieCsyLjUsYnksKHJ3LTQpLzItMi41LDMuNSw1LjcpCiAgbWFya2VyX2xhYmVsPWxhYmVsKGxheW91dCwnw4FyZWFzIEVsZWfDrXZlaXMgZW0gdmVybWVsaG8nLHJ4KzIsZnkrMzgqZmFjdG9yLHJ3LTQsNCw1LjcsY29sb3I9JyNhNDAwMDAnKTttYXJrZXJfbGFiZWwuc2V0QmFja2dyb3VuZEVuYWJsZWQoVHJ1ZSk7bWFya2VyX2xhYmVsLnNldEJhY2tncm91bmRDb2xvcihRQ29sb3IoMjU1LDI1NSwyNTUsMjIwKSkKICBpZiB1YzoKICAgdXgsdXc9c2xvdHNbMV07bGFiZWwobGF5b3V0LCdMb2NhbGl6YcOnw6NvIG5hIFVDJyx1eCsxLGZ5KzEsdXctMiw1LDcsVHJ1ZSkKICAgbG9jPVFnc0xheW91dEl0ZW1NYXAobGF5b3V0KTtsYXlvdXQuYWRkTGF5b3V0SXRlbShsb2MpO2xvYy5zZXRJZChuYW1lKydfbG9jYWxpemFkb3JfdWMnKTtsb2MuYXR0ZW1wdE1vdmUoUWdzTGF5b3V0UG9pbnQodXgrMixmeSs3KmZhY3RvcikpO2xvYy5hdHRlbXB0UmVzaXplKFFnc0xheW91dFNpemUodXctNCw0NipmYWN0b3IpKTtsb2Muc2V0Q3JzKGNycyk7bG9jLnNldExheWVycyhhZXMrW3VjX2xvY2F0b3IscmVnaW9uYWxdKTtsb2Muc2V0S2VlcExheWVyU2V0KFRydWUpO2xvY2FsX3N0eWxlcz17fQogICBmb3IgdiBpbiBhZXMrW3VjX2xvY2F0b3IscmVnaW9uYWxdOgogICAgc3R5bGU9UWdzTWFwTGF5ZXJTdHlsZSgpO3N0eWxlLnJlYWRGcm9tTGF5ZXIodik7bG9jYWxfc3R5bGVzW3YuaWQoKV09c3R5bGUueG1sRGF0YSgpCiAgIGxvYy5zZXRMYXllclN0eWxlT3ZlcnJpZGVzKGxvY2FsX3N0eWxlcyk7bG9jLnNldEtlZXBMYXllclN0eWxlcyhUcnVlKTtsb2Muem9vbVRvRXh0ZW50KGV4dGVudF9vZih1YyxhZV9leHRlbnQpKTtsb2Mub3ZlcnZpZXcoKS5zZXRMaW5rZWRNYXAobSk7bG9jLm92ZXJ2aWV3KCkuc2V0RW5hYmxlZChUcnVlKTtsb2Muc2V0RnJhbWVFbmFibGVkKFRydWUpCiAgbHgsbHc9c2xvdHNbLTNdO2l4LGl3PXNsb3RzWy0yXTtneCxndz1zbG90c1stMV0KICBsZWdlbmQ9UWdzTGF5b3V0SXRlbUxlZ2VuZChsYXlvdXQpO2xlZ2VuZC5zZXRJZChuYW1lKydfbGVnZW5kYScpO2xlZ2VuZC5zZXRUaXRsZSgnTGVnZW5kYScpO2xlZ2VuZC5zZXRMaW5rZWRNYXAobSk7bGVnZW5kLnNldEF1dG9VcGRhdGVNb2RlbChGYWxzZSk7bGVnZW5kLm1vZGVsKCkucm9vdEdyb3VwKCkuY2xlYXIoKTtsZWdlbmQuc2V0V3JhcFN0cmluZygnXG4nKQogIGxlZ2VuZF9uYW1lcz1bXTthZV9hZGRlZD1GYWxzZQogIGZvciB2IGluIHZlY3RvcnM6CiAgIGlmIHYgaW4gYWVzOgogICAgaWYgYWVfYWRkZWQ6Y29udGludWUKICAgIGFlX2FkZGVkPVRydWU7dGl0bGVfbGVnZW5kPSfDgXJlYXMgRWxlZ8OtdmVpcycKICAgZWxzZTp0aXRsZV9sZWdlbmQ9di5uYW1lKCkKICAgbm9kZT1sZWdlbmQubW9kZWwoKS5yb290R3JvdXAoKS5hZGRMYXllcih2KTtub2RlLnNldE5hbWUodGV4dHdyYXAuZmlsbCh0aXRsZV9sZWdlbmQsd2lkdGg9bWF4KDIyLGludCgobHctOSkvMS4wNSkpKSk7bGVnZW5kX25hbWVzLmFwcGVuZCh0aXRsZV9sZWdlbmQpCiAgIGlmIHY9PXVjIGFuZCBsZW4odWNfbmFtZXMpPjE6UWdzTGVnZW5kUmVuZGVyZXIuc2V0Tm9kZUxlZ2VuZFN0eWxlKG5vZGUsUWdzTGVnZW5kU3R5bGUuSGlkZGVuKQogIGxlZ2VuZC5zZXRTdHlsZUZvbnQoUWdzTGVnZW5kU3R5bGUuVGl0bGUsUUZvbnQoJ0FyaWFsJyw4LFFGb250LkJvbGQpKTtsZWdlbmQuc2V0U3R5bGVGb250KFFnc0xlZ2VuZFN0eWxlLlN5bWJvbExhYmVsLFFGb250KCdBcmlhbCcsNykpO2xheW91dC5hZGRMYXlvdXRJdGVtKGxlZ2VuZCk7bGVnZW5kLmF0dGVtcHRNb3ZlKFFnc0xheW91dFBvaW50KGx4KzEsZnkrMSkpO2xlZ2VuZC5hdHRlbXB0UmVzaXplKFFnc0xheW91dFNpemUobHctMixmaC0yKSk7bGVnZW5kLnNldFJlc2l6ZVRvQ29udGVudHMoRmFsc2UpCiAgbm90ZT1mIntjcnMuYXV0aGlkKCl9IMK3IGVzY2FsYSAxOntyb3VuZChtLnNjYWxlKCkpOix9Ii5yZXBsYWNlKCcsJywnLicpCiAgaW5mbz1ub3RlKydcbkFFcyBlIHBvbnRvczogZGFkb3MgZm9ybmVjaWRvcy5cbicrKCdVQ3MgZmVkZXJhaXM6IElDTUJpby5cbicgaWYgdWMgZWxzZSAnJykrJ0VzdGFkb3MgZSBiaW9tYXM6IElCR0UsIDIwMjUuXG5TZW50aW5lbC0yIEwyQSDCtyBSR0IgbmF0aXZvIDEwIG0uXG5EYXRhczogJysoZGF0ZV9sYWJlbCBvciAnbsOjbyBpbmZvcm1hZGFzJykrJy5cbkNvcGVybmljdXMgU2VudGluZWwgLyBBV1MgRWFydGggU2VhcmNoLlxuUEE6IGxvY2FsIHBsYW5lamFkbzsgbsOjbyDDqSBVQSBpbnN0YWxhZGEuXG5FbGFib3Jhw6fDo286ICcrY2ZnWydlbGFib3JhY2FvJ10rJ1xuJytkYXRldGltZS5kYXRlLnRvZGF5KCkuaXNvZm9ybWF0KCkrJyDCtyBNb25pdG9yYSBRRmllbGQgdjAuNC4yXG5Gb250ZXMgZSBtw6l0b2RvczogMDJfcmVsYXRvcmlvLicKICBsYWJlbChsYXlvdXQsJ0luZm9ybWHDp8O1ZXMgZG8gbWFwYScsaXgrMSxmeSsxLGl3LTIsNSw3LFRydWUpO2xhYmVsKGxheW91dCxpbmZvLGl4KzEuNSxmeSs3LGl3LTMsZmgtOCw2LjUpCiAgZm9yIGosbiBpbiBlbnVtZXJhdGUoWydtb25pdG9yYScsJ2NiYycsJ2ljbWJpbyddKToKICAgaW09UWdzTGF5b3V0SXRlbVBpY3R1cmUobGF5b3V0KTtpbS5zZXRJZChuYW1lKydfbG9nb18nK24pO2xheW91dC5hZGRMYXlvdXRJdGVtKGltKTtpbS5hdHRlbXB0TW92ZShRZ3NMYXlvdXRQb2ludChneCsxLjUsZnkrKDIraioxOCkqZmFjdG9yKSk7aW0uYXR0ZW1wdFJlc2l6ZShRZ3NMYXlvdXRTaXplKGd3LTMsMTUqZmFjdG9yKSk7aW0uc2V0UGljdHVyZVBhdGgoc3RyKGFzc2V0cy9mJ2xvZ29fe259LnBuZycpKTtpbS5zZXRQaWN0dXJlQW5jaG9yKFFnc0xheW91dEl0ZW1QaWN0dXJlLk1pZGRsZSkKICBtaXNzaW5nPXRhcmdldCBpcyBOb25lIG9yIHRhcmdldC5mZWF0dXJlQ291bnQoKT09MAogIGlmIG1pc3Npbmc6bGFiZWwobGF5b3V0LCdTZW0gZmVpw6fDtWVzIGZvcm5lY2lkYXMgcGFyYSBlc3RlIHRlbWE7IHJlZmVyw6puY2lhOiDDgXJlYXMgRWxlZ8OtdmVpcy4nLG14KzIsbXkrMixtdy00LDgsOSxUcnVlKQogIHByb2plY3QubGF5b3V0TWFuYWdlcigpLmFkZExheW91dChsYXlvdXQpCiAgcGRmPXJvb3QvJ21hcGFzX3BkZicvZid7bmFtZX0ucGRmJztwbmc9cm9vdC8nbWFwYXNfcG5nJy9mJ3tuYW1lfS5wbmcnCiAgZXhwb3J0ZXI9UWdzTGF5b3V0RXhwb3J0ZXIobGF5b3V0KTtwcz1RZ3NMYXlvdXRFeHBvcnRlci5QZGZFeHBvcnRTZXR0aW5ncygpO3BzLmRwaT1kcGk7cHMuYXBwZW5kR2VvcmVmZXJlbmNlPVRydWU7cHMuZXhwb3J0TWV0YWRhdGE9VHJ1ZTtwcy50ZXh0UmVuZGVyRm9ybWF0PVFnaXMuVGV4dFJlbmRlckZvcm1hdC5BbHdheXNUZXh0CiAgYXNzZXJ0IGV4cG9ydGVyLmV4cG9ydFRvUGRmKHN0cihwZGYpLHBzKT09UWdzTGF5b3V0RXhwb3J0ZXIuU3VjY2VzcyxleHBvcnRlci5lcnJvck1lc3NhZ2UoKQogIGV2aWRlbmNlPWNoZWNrX3BkZihwZGYsY3JzKQogIGd0PWV2aWRlbmNlWydnZW90cmFuc2Zvcm0nXTtweD0obXgrbXcvMikvd2lkdGgqZXZpZGVuY2VbJ2xhcmd1cmFfcHhfNzJkcGknXTtweT0obXkrbWgvMikvaGVpZ2h0KmV2aWRlbmNlWydhbHR1cmFfcHhfNzJkcGknXTtjeD1ndFswXStweCpndFsxXStweSpndFsyXTtjeT1ndFszXStweCpndFs0XStweSpndFs1XQogIGVycm9yPW1hdGguaHlwb3QoY3gtbS5leHRlbnQoKS5jZW50ZXIoKS54KCksY3ktbS5leHRlbnQoKS5jZW50ZXIoKS55KCkpO2Fzc2VydCBlcnJvcjwyKm1heChhYnMoZ3RbMV0pLGFicyhndFs1XSkpLCgnR2VvcnJlZmVyw6puY2lhIGRvIG1hcGEgcHJpbmNpcGFsIGRpdmVyZ2VudGUnLGVycm9yKTtldmlkZW5jZVsnZXJyb19jZW50cm9fbSddPWVycm9yCiAgY29udHJvbHM9WyhteCxteSxtLmV4dGVudCgpLnhNaW5pbXVtKCksbS5leHRlbnQoKS55TWF4aW11bSgpKSwobXgrbXcsbXksbS5leHRlbnQoKS54TWF4aW11bSgpLG0uZXh0ZW50KCkueU1heGltdW0oKSksKG14LG15K21oLG0uZXh0ZW50KCkueE1pbmltdW0oKSxtLmV4dGVudCgpLnlNaW5pbXVtKCkpLChteCttdyxteSttaCxtLmV4dGVudCgpLnhNYXhpbXVtKCksbS5leHRlbnQoKS55TWluaW11bSgpKV07ZXJyb3JzPVtdCiAgZm9yIHh4LHl5LGd4LGd5IGluIGNvbnRyb2xzOgogICBweD14eC93aWR0aCpldmlkZW5jZVsnbGFyZ3VyYV9weF83MmRwaSddO3B5PXl5L2hlaWdodCpldmlkZW5jZVsnYWx0dXJhX3B4XzcyZHBpJ107ZXJyb3JzLmFwcGVuZChtYXRoLmh5cG90KGd0WzBdK3B4Kmd0WzFdK3B5Kmd0WzJdLWd4LGd0WzNdK3B4Kmd0WzRdK3B5Kmd0WzVdLWd5KSkKICBhc3NlcnQgbWF4KGVycm9ycyk8MiptYXgoYWJzKGd0WzFdKSxhYnMoZ3RbNV0pKSxlcnJvcnM7ZXZpZGVuY2VbJ2Vycm9zX2NhbnRvc19tJ109ZXJyb3JzCiAgaW1zPVFnc0xheW91dEV4cG9ydGVyLkltYWdlRXhwb3J0U2V0dGluZ3MoKTtpbXMuZHBpPWRwaTtpbXMuZ2VuZXJhdGVXb3JsZEZpbGU9RmFsc2U7aW1zLmV4cG9ydE1ldGFkYXRhPUZhbHNlCiAgYXNzZXJ0IGV4cG9ydGVyLmV4cG9ydFRvSW1hZ2Uoc3RyKHBuZyksaW1zKT09UWdzTGF5b3V0RXhwb3J0ZXIuU3VjY2VzcyxleHBvcnRlci5lcnJvck1lc3NhZ2UoKQogIGEsYixjLGQsZSxmPWV4cG9ydGVyLmNvbXB1dGVXb3JsZEZpbGVQYXJhbWV0ZXJzKGRwaSkKICBwbmcud2l0aF9zdWZmaXgoJy5wZ3cnKS53cml0ZV90ZXh0KCdcbicuam9pbihmb3JtYXQodiwnLjE2ZycpIGZvciB2IGluIFthLGQsYixlLGMsZl0pKydcbicpCiAgIyBSb3R1bGFnZW0gZWZldGl2YW1lbnRlIGV4cG9ydGFkYSwgbsOjbyBhcGVuYXMgdW1hIGVzdGltYXRpdmEgcG9yIG7Dum1lcm8gZGUgcG9udG9zLgogIHJlc3VsdHM9ZXhwb3J0ZXIubGFiZWxpbmdSZXN1bHRzKCk7dmlzaWJsZT0wCiAgaWYgbmFtZS5zdGFydHN3aXRoKCgnMDMnLCcwNCcpKSBhbmQgdGFyZ2V0OgogICBmb3Iga2V5LHJlc3VsdCBpbiByZXN1bHRzLml0ZW1zKCk6CiAgICBpZiBzdHIoa2V5KT09bS51dWlkKCk6dmlzaWJsZT1sZW4oe3AuZmVhdHVyZUlkIGZvciBwIGluIHJlc3VsdC5sYWJlbHNXaXRoaW5SZWN0KG0uZXh0ZW50KCkpIGlmIHAubGF5ZXJJRD09dGFyZ2V0LmlkKCl9KQogICBsYWJlbF9pbmZvLnVwZGF0ZShyb3R1bG9zX2V4aWJpZG9zPXZpc2libGUscm90dWxvc19zdXByaW1pZG9zPXRhcmdldC5mZWF0dXJlQ291bnQoKS12aXNpYmxlKQogIChwbmcud2l0aF9zdWZmaXgoJy5wcmonKSkud3JpdGVfdGV4dChjcnMudG9Xa3QoKSxlbmNvZGluZz0ndXRmLTgnKQogIGd0X3BuZz1bYy1hLzItYi8yLGEsYixmLWQvMi1lLzIsZCxlXTtQYXRoKHN0cihwbmcpKycuYXV4LnhtbCcpLndyaXRlX3RleHQoJzxQQU1EYXRhc2V0PjxTUlM+Jytlc2NhcGUoY3JzLnRvV2t0KCkpKyc8L1NSUz48R2VvVHJhbnNmb3JtPicrJywnLmpvaW4oZm9ybWF0KHYsJy4xNmcnKSBmb3IgdiBpbiBndF9wbmcpKyc8L0dlb1RyYW5zZm9ybT48L1BBTURhdGFzZXQ+JyxlbmNvZGluZz0ndXRmLTgnKQogIGRzPWdkYWwuT3BlbihzdHIocG5nKSk7YXNzZXJ0IGRzLkdldFByb2plY3Rpb24oKSBhbmQgZHMuR2V0R2VvVHJhbnNmb3JtKCk7ZHM9Tm9uZQogIGF1ZGl0cy5hcHBlbmQoeydtYXBhJzpuYW1lLCd0ZW1hJzp0aXRsZSwnc2VtX2ZlaWNvZXMnOm1pc3NpbmcsJ2VzY2FsYSc6bS5zY2FsZSgpLCdleHRlbnQnOlttLmV4dGVudCgpLnhNaW5pbXVtKCksbS5leHRlbnQoKS55TWluaW11bSgpLG0uZXh0ZW50KCkueE1heGltdW0oKSxtLmV4dGVudCgpLnlNYXhpbXVtKCldLCdtYXBhX3ByaW5jaXBhbF91dWlkJzptLnV1aWQoKSwncm90dWxvcyc6bGFiZWxfaW5mbywncGRmJzpldmlkZW5jZSwnY2FtYWRhcyc6W3YubmFtZSgpIGZvciB2IGluIG1hcGxheWVyc10sJ2xlZ2VuZGEnOmxlZ2VuZF9uYW1lcywndWNzJzp1Y19uYW1lcywnbG9jYWxpemFkb3Jlcyc6KFsnZXN0YWRvc19iaW9tYXMnLCd1YyddIGlmIHVjIGVsc2UgWydlc3RhZG9zX2Jpb21hcyddKSwnY29udGV4dG9fdWYnOnN0YXRlcy5jdXN0b21Qcm9wZXJ0eSgnbW9uaXRvcmFfdWZfY29udGV4dG8nKSwnYmlvbWFzX2xvY2FsaXphZG9yJzpiaW9tZV9hdWRpdCwnYmlvbWFzX2xlZ2VuZGEnOm5hbWVzX2Jpb21lcywnYmlvbWFfbWluX21tMic6ZmxvYXQoY2ZnLmdldCgnYmlvbWFfbWluX21tMicsLjUpKX0pCiAjIEV4aWJpw6fDo28gaW5pY2lhbCBkZSBlZGnDp8OjbyBpbmNsdWkgdG9kYXMgYXMgcmVmZXLDqm5jaWFzIGUgZnVuZG9zIG9mZmxpbmUuCiBmb3Igbm9kZSBpbiBwcm9qZWN0LmxheWVyVHJlZVJvb3QoKS5maW5kTGF5ZXJzKCk6bm9kZS5zZXRJdGVtVmlzaWJpbGl0eUNoZWNrZWQobm9kZS5sYXllcigpLm5hbWUoKSE9J0dvb2dsZSBTYXRlbGxpdGUnIGFuZCBub3Qgbm9kZS5sYXllcigpLmN1c3RvbVByb3BlcnR5KCdtb25pdG9yYV9jb250ZXh0bycsRmFsc2UpKQogcHJvamVjdC52aWV3U2V0dGluZ3MoKS5zZXREZWZhdWx0Vmlld0V4dGVudChRZ3NSZWZlcmVuY2VkUmVjdGFuZ2xlKGV4dGVudF9vZihOb25lLGFlX2V4dGVudCksY3JzKSkKIGFzc2VydCBwcm9qZWN0LndyaXRlKCksJ0ZhbGhhIGFvIHNhbHZhciBRR1onCiBmb3IgcCxoIGluIHNvdXJjZV9oYXNoLml0ZW1zKCk6YXNzZXJ0IHNoYShwKT09aCwnRWRpw6fDo28gYWx0ZXJvdSBmb250ZSBRRmllbGQnCiBkdW1wKHsnc3RhdHVzJzonUEFTUycsJ1FHSVMnOlFnaXMuUUdJU19WRVJTSU9OLCdtYXBhcyc6YXVkaXRzLCd2ZXRvcmVzX2lzb2xhZG9zJzpUcnVlLCdwcm9qZXRvJzonMDVfcWdpcy9wcm9qZXRvX2VkaWNhby5xZ3onfSxyb290LycwMl9yZWxhdG9yaW8vY2FydG9ncmFmaWEuanNvbicpCiBwcmludCgnQ2FydG9ncmFmaWE6IHF1YXRybyBQREYgZ2VvcnJlZmVyZW5jaWFkb3MgZSBxdWF0cm8gUE5HIHZlcmlmaWNhZG9zLicsZmx1c2g9VHJ1ZSkKCmlmIF9fbmFtZV9fPT0nX19tYWluX18nOgogaWYgc3lzLmFyZ3ZbMV09PSdwcm9iZSc6cHJvYmUoc3lzLmFyZ3ZbMl0pCiBlbGlmIHN5cy5hcmd2WzFdPT0nYnVpbGQnOmJ1aWxkKHN5cy5hcmd2WzJdKQogZWxzZTpyYWlzZSBWYWx1ZUVycm9yKCdNb2RvIGludsOhbGlkbycpCiAjIFFHSVMvUXQgcG9kZSBlbmNlcnJhciBjb20gcmVmZXLDqm5jaWFzIGdyw6FmaWNhcyBwZW5kZW50ZXM7IFNPIGxpYmVyYSBvIHByb2Nlc3NvLgogc3lzLnN0ZG91dC5mbHVzaCgpO3N5cy5zdGRlcnIuZmx1c2goKTtvcy5fZXhpdCgwKQo=")
# HELPERS_REVISAO_FIM

# Rscript: execução direta. RStudio: botão Source executa, salvo opção de somente carregar.
if ((sys.nframe()==0L || interactive()) && !isTRUE(getOption('monitora.qfield.somente_funcoes',FALSE))) {
  monitora_criar_qfield(MQ_CONFIG)
}
