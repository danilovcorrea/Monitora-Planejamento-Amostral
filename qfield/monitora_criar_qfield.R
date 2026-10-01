# Monitora — criação independente de projetos QField
# Versão 0.3.0 — cotas cumulativas, 01/10/2026.
# Leitores adaptados da versão pública v3.0.6; SHA256 da fonte:
# 454cb8f1d6f74ec8df2695236add1206080f869a3209c8a09aa92af9b6186d45
# Arquivo autossuficiente: não carrega o script biológico do Monitora.


# CONFIGURAÇÃO — edite este bloco antes de executar no RStudio.
# Para fornecer número, use list(n=40, percentual=NULL).
MQ_CONFIG <- list(
  entrada = 'qfield_input', saida = 'qfield_output', projeto = 'Monitora',
  modo = 'auto', # auto: montar se houver PAs/UAs/grade fornecidos; senão planejar.
  referencia_anterior = NULL, # pasta 02_relatorio de uma execução; obrigatória em expandir
  transecto_m = 50, distancia_min_m = 100, deslocamento_max_m = 10,
  grade_m = c(156.25, 156.25), epsg = NULL, semente = 20261001L,
  prioritarios = list(n=NULL, percentual=20),
  alternativos = list(n=NULL, percentual=NULL), # ambos NULL: 2 x prioritários
  # Cotas são cumulativas: todas devem ser satisfeitas pelo MESMO conjunto de PAs.
  estratificar_vegetacao = TRUE, incluir_formacao_florestal = FALSE,
  perfil = 'campestre_savanico', # ilha ou personalizado: parâmetros explicitamente definidos
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
  fontes <- setdiff(list.files(entrada, pattern = "\\.(kml|kmz|gpkg|zip|shp)$", ignore.case = TRUE, full.names = FALSE), "recorte_imagens_500m.gpkg")
  if (length(fontes) > 100L) stop("QField: mais de 100 fontes adicionais.", call. = FALSE)
  saida <- list()
  for (nome in fontes) {
    f <- monitora_qfield_caminho_local(nome, entrada)
    if (file.info(f)$size > 512 * 1024^2) stop("QField: fonte vetorial maior que 512 MiB.", call. = FALSE)
    ext <- tolower(tools::file_ext(f)); arquivos <- f
    if (ext == "shp") {
    nomes <- list.files(entrada,full.names=FALSE)
    req <- paste0(tolower(tools::file_path_sans_ext(nome)),c(".shp",".shx",".dbf",".prj"))
    if (any(vapply(req,function(z)sum(tolower(nomes)==z),integer(1))!=1L)) stop("QField: shapefile direto incompleto/ambíguo; SHP, SHX, DBF e PRJ obrigatórios.",call.=FALSE)
    for (z in nomes[tolower(nomes) %in% req]) monitora_qfield_caminho_local(z,entrada)
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
    arquivo_apoio <- identical(nome, "apoio_campo.gpkg")
    if (arquivo_apoio && !setequal(ls$name, c("pontos_interesse", "trajeto"))) stop("QField: apoio_campo.gpkg deve conter somente pontos_interesse e trajeto.", call. = FALSE)
    for (camada in ls$name) {
      x <- if (arquivo_apoio) sf::st_read(arq, layer = camada, quiet = TRUE, drivers = driver, stringsAsFactors = FALSE, fid_column_name = "fid") else sf::st_read(arq, layer = camada, quiet = TRUE, drivers = driver, stringsAsFactors = FALSE)
      if (ext_arq=="kml" && !arquivo_apoio && !nrow(x)) next
      if (!is.null(esperado_kml)) importadas_kml <- importadas_kml + nrow(x)
      if (arquivo_apoio) {
        esperado <- if (camada == "pontos_interesse") "POINT" else "MULTILINESTRING"
        campo <- if (camada == "pontos_interesse") "ponto_interesse" else "trajeto"
        campos <- names(sf::st_drop_geometry(x))
        tipo_declarado <- toupper(gsub("[ _]", "", ls$geomtype[[match(camada, ls$name)]]))
        if (!setequal(campos, c("fid", campo, "obs", "data_hora")) || !is.character(x[[campo]]) || !is.character(x$obs) || !inherits(x$data_hora, "POSIXt") || is.na(sf::st_crs(x)) || !identical(tipo_declarado, esperado) || (nrow(x) && !identical(class(sf::st_geometry(x))[1], paste0("sfc_", esperado)))) stop("QField: estrutura de apoio_campo incompatível; nenhum campo será descartado.", call. = FALSE)
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
  for(n in c('transecto_m','distancia_min_m','deslocamento_max_m','max_pontos','timeout_s'))
    if(length(c[[n]])!=1L || !is.numeric(c[[n]]) || !is.finite(c[[n]]) || c[[n]]<0 || (n!='deslocamento_max_m' && c[[n]]==0)) mq_stop('Parâmetro inválido: ',n)
  if(length(c$grade_m)!=2 || any(!is.finite(c$grade_m)) || any(c$grade_m<=0)) mq_stop('grade_m exige largura e altura positivas.')
  if(!c$modo %in% c('auto','planejar','montar','expandir')) mq_stop('Modo inválido.')
  if(length(c$semente)!=1 || !is.finite(c$semente) || c$semente!=as.integer(c$semente)) mq_stop('Semente inválida.')
  if(!c$mb_produto %in% c('30m','10m')) mq_stop('MapBiomas: produto deve ser 30m ou 10m.')
  if(isTRUE(c$mapbiomas) && !((c$mb_produto=='30m' && c$mb_colecao==11 && c$mb_ano %in% 1985:2025) || (c$mb_produto=='10m' && c$mb_colecao==4 && c$mb_ano %in% 2017:2025))) mq_stop('Produto/coleção/ano MapBiomas não homologado; use 30m/11 ou 10m/4 até 2025.')
  if(!c$centros_detalhe%in%c('auto','PAs','UAs','UAs_e_PAs'))mq_stop('centros_detalhe inválido.')
  for(n in c('baixar_imagem_detalhe','renovar_imagens'))if(length(c[[n]])!=1||!is.logical(c[[n]])||is.na(c[[n]]))mq_stop('Opção lógica inválida: ',n)
  if(!is.null(c$confirmar_download)&&(length(c$confirmar_download)!=1||!is.logical(c$confirmar_download)||is.na(c$confirmar_download)))mq_stop('confirmar_download deve ser NULL, TRUE ou FALSE.')
  if(!identical(as.numeric(c$raio_detalhe_m),500))mq_stop('Versão atual homologa raio_detalhe_m=500.')
  if(length(c$margem_contexto_m)!=1||!is.numeric(c$margem_contexto_m)||!is.finite(c$margem_contexto_m)||c$margem_contexto_m<0)mq_stop('Margem de contexto inválida.')
  for(n in c('confirmar_download','confirmar_sentinel'))if(!is.null(c[[n]])&&(length(c[[n]])!=1||!is.logical(c[[n]])||is.na(c[[n]])))mq_stop('Confirmação inválida: ',n)
  if(length(c$cache_dir)!=1||!nzchar(c$cache_dir))mq_stop('Informe cache_dir persistente.')
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
  roles <- c('areas_elegiveis','limites_uc','grade_amostral','PA_priorit','PA_altern','verg_ini','verg_fin','UAs','transectos','estradas','rodovias','acessos','trilhas','formacao_florestal','pontos_interesse','trajeto','adicional')
  out <- list(); used <- integer()
  for(x in raw) {
    ar <- attr(x,'qfield_arquivo'); la <- attr(x,'qfield_camada')
    ii <- if(nrow(m)) which(m$arquivo==ar & m$camada==la) else integer()
    hits <- roles[tolower(roles) %in% tolower(c(la,tools::file_path_sans_ext(ar)))]
    role <- if(length(ii)) m$papel[ii] else if(length(hits)==1) hits else 'adicional'
    if(length(hits)>1 && !length(ii)) mq_stop('Papel ambíguo; declare camadas_qfield.csv: ',ar,' / ',la)
    if(!role %in% roles) mq_stop('Papel não reconhecido: ',role)
    used <- c(used,ii)
    tipos <- as.character(sf::st_geometry_type(x))
    if(role %in% c('areas_elegiveis','limites_uc','formacao_florestal') && any(!tipos %in% c('POLYGON','MULTIPOLYGON'))) mq_stop('Polígonos exigidos para ',role)
    if(role %in% c('grade_amostral','PA_priorit','PA_altern','verg_ini','verg_fin','UAs') && any(tipos!='POINT')) mq_stop('Pontos simples exigidos para ',role)
    if(role %in% c('estradas','rodovias','acessos','trilhas','transectos','trajeto') && any(!tipos %in% c('LINESTRING','MULTILINESTRING'))) mq_stop('Linhas exigidas para ',role)
    label <- if(length(ii) && 'campo_rotulo' %in% names(m) && !is.na(m$campo_rotulo[ii]) && nzchar(m$campo_rotulo[ii])) m$campo_rotulo[ii] else intersect(c('PA','UA','Name','nome','ponto_interesse','trajeto','id'),names(x))[1]
    if(is.na(label)) label <- ''
    if(nzchar(label) && !label %in% names(x)) mq_stop('Campo de rótulo inexistente: ',label)
    if(role %in% c('PA_priorit','PA_altern')) {
      if(!nzchar(label) || anyNA(x[[label]]) || any(!nzchar(as.character(x[[label]]))) || anyDuplicated(x[[label]])) mq_stop('PAs fornecidos exigem rótulos únicos, não vazios.')
    }
    ano <- if(length(ii) && 'ano' %in% names(m)) m$ano[ii] else NA_character_
    if(!is.na(ano) && nzchar(ano) && !grepl('^[0-9]{4}$',ano)) mq_stop('Ano inválido no manifesto.')
    nm <- if(role=='areas_elegiveis') 'AE' else if(role=='limites_uc') 'UC_forn' else if(role=='adicional') substr(monitora_qfield_slug(la),1,22) else role
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
    u <- u[lengths(sf::st_intersects(u,ae))>0,]
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
  g$id_grade<-NA_integer_;g$categoria<-'grade'
  if(!is.null(old)) {
    ix<-match(g$chave_grade,old$chave_grade);g$id_grade<-old$id_grade[ix];g$categoria[!is.na(ix)]<-old$categoria[ix[!is.na(ix)]]
  }
  ni<-which(is.na(g$id_grade));start<-if(is.null(old))0 else max(old$id_grade)
  g$id_grade[ni]<-start+seq_along(ni);g$PA<-paste0('PA',g$id_grade)
  g
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
  if(np+na>N)mq_stop(sprintf('Cotas inviáveis: %d prioritários + %d alternativos para %d pontos. Não houve redução automática.',np,na,N))
  oldp<-sum(g$categoria=='prioritario');olda<-sum(g$categoria=='alternativo')
  if(oldp>np || olda>na)mq_stop('Cotas inferiores à seleção anterior preservada; revisão explícita necessária.')
  # Ordenação por hash evita alterar o gerador aleatório global e independe da ordem das entradas.
  rank<-vapply(g$chave_grade,function(k)digest::digest(paste(c$semente,k,sep=':'),algo='sha256',serialize=FALSE),character(1))
  available<-which(g$categoria=='grade');available<-available[order(rank[available],g$id_grade[available])]
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
  cells<-sort(unique(keys[g$mq_apto]),method='radix');H<-length(cells)
  if(!H||H>c$max_estratos)mq_stop('Quantidade de combinações vazia ou acima de max_estratos: ',H)
  cell<-match(keys,cells);cap<-tabulate(cell[g$mq_apto],H)
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
    avail<-which(g$mq_apto&!is.na(cell)&cell==h&g$categoria=='grade');avail<-avail[order(rank[avail],g$id_grade[avail])]
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
  # Posição original imutável; direção a partir do norte geográfico local, em metros no CRS escolhido.
  if(nrow(points)>20000) {writeLines('Simulações não executadas: mais de 20.000 PAs. Nenhuma seleção alterada.',file.path(report,'simulacoes_nao_executadas.txt'));return(invisible(NULL))}
  xy<-sf::st_coordinates(points);ll<-sf::st_coordinates(sf::st_transform(points,4326));ll[,2]<-ll[,2]+1e-5
  north<-sf::st_coordinates(sf::st_transform(sf::st_as_sf(data.frame(lon=ll[,1],lat=ll[,2]),coords=c('lon','lat'),crs=4326),sf::st_crs(points)))-xy[,1:2,drop=FALSE]
  north<-north/sqrt(rowSums(north^2));rows<-list();directions<-c('N','L','S','O')
  known<-list(transectos=c$distancia_min_m,formacao_florestal=100,rodovias=100,estradas=50,trilhas=5)
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
  if(file.info(tmp)$size>950*1024^2)mq_stop('Recorte excede 950 MiB; divida a área. Parcial preservado.')
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
  local<-list.files(input,pattern='\\.mbtiles$',full.names=TRUE,ignore.case=TRUE)
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
  additional<-list.files(input,pattern='\\.(tif|tiff|img|asc)$',full.names=TRUE,ignore.case=TRUE)
  paths<-c(cuts,additional,regional,sentinel)
  ras<-mq_rasters(input,file.path(root,'01_qfield/mapas'),coverage,report,paths=paths)
  for(i in seq_along(ras)) {
    if(i<=length(cuts))ras[[i]]$nome<-paste0('detalhe_raio_500m',if(length(cuts)>1)paste0('_',sprintf('%02d',i))else '')
    if(i==length(ras))ras[[i]]$nome<-'Sentinel-2 — contexto 10 m, não alta resolução'
  }
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
  files<-list.files(entrada,pattern='\\.(mbtiles|tif|tiff|img|asc)$',full.names=TRUE,ignore.case=TRUE)
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
    if(ext%in%c('asc','img')) {
      gt<-paste0(tools::file_path_sans_ext(dst),'.tif');terra::writeRaster(r,gt,overwrite=FALSE);unlink(dst);dst<-gt;nm<-basename(gt);r<-terra::rast(gt)
    }
    covered<-NA_integer_
    if(!is.null(points)&&nrow(points)) {
      if(ext=='mbtiles')visible<-mq_mb_visible(f,points)else {p<-terra::project(terra::vect(points),terra::crs(r));val<-terra::extract(r,p,method='simple');visible<-rowSums(!is.na(val[,-1,drop=FALSE]))>0 & if(terra::nlyr(r)>=4)!is.na(val[[5]]) & val[[5]]>0 else TRUE};covered<-sum(visible);coverage_all<-coverage_all|visible
    }
    out[[length(out)+1]]<-list(arquivo=nm,nome=if(ext=='mbtiles')paste0('Imagem_',length(out)+1,'_z',zmax)else paste0('Raster_',length(out)+1),epsg=sf::st_crs(terra::crs(r))$epsg,wkt=terra::crs(r),bandas=terra::nlyr(r),ativo=TRUE)
    aud[[length(aud)+1]]<-data.frame(arquivo=basename(f),destino=nm,sha256=mq_hash(dst),zoom_min=zmin,zoom_max=zmax,pontos_com_valor=covered,pontos_avaliados=if(is.null(points))0 else nrow(points),observacao='Valor no ponto não garante cobertura da transecção, acessos ou ausência de nuvens. Resolução nativa não inferida pelo zoom.')
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
    xml<-c(xml,paste0('<maplayer type="raster"><id>',id,'</id><datasource>',e(source),'</datasource><layername>',e(l$nome),'</layername><srs>',mq_srs(l$wkt),'</srs><provider>gdal</provider><pipe>',renderer,'</pipe></maplayer>'))
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
  for(i in seq_along(layers)) {
    l<-layers[[i]];x<-l$x;nm<-l$nome
    l$arquivo<-if(l$papel%in%c('pontos_interesse','trajeto'))'apoio_campo.gpkg'else 'referencias.gpkg'
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
  txt<-c('<!doctype html><html lang="pt-BR"><meta charset="utf-8"><title>Relatório de execução — Monitora QField</title><style>body{font:16px sans-serif;max-width:1100px;margin:40px auto;line-height:1.5;padding:0 20px}table{border-collapse:collapse;width:100%}td,th{border:1px solid #bbb;padding:8px;text-align:left}h1{color:#165c46}</style>',paste0('<h1>',e(c$projeto),'</h1><p>Versão 0.3.0 · ',e(status),'</p>'),'<p>Produto de planejamento e navegação. Homologação automática não substitui verificação em QField no aparelho, em modo avião, nem aplicação do roteiro em campo.</p>',paste0('<ul>',paste0('<li>',e(notes),'</li>',collapse=''),'</ul>'),paste0('<table><thead><tr>',paste0('<th>',e(names(stages)),'</th>',collapse=''),'</tr></thead><tbody>',paste(trs,collapse=''),'</tbody></table>'),if(file.exists(file.path(r,'mapa_planejamento.png'))) '<p><img src="mapa_planejamento.png" alt="Visão geral das áreas e pontos" style="width:100%"></p>' else '', '<p>Detalhes: cotas_solicitadas.csv, cotas_realizadas.csv, alocacao_combinacoes.csv, solucao_cotas.json, fontes_estratos.json (quando habilitadas); configuracao.json; consulta_uc.json; referencia_grade.json e cadastro_grade.gpkg (planejamento/expansão); camadas.csv; imagens.csv; legenda/fonte MapBiomas; diagnosticos/; fontes_e_checksums.csv; manifesto.csv.</p></html>')
  writeLines(enc2utf8(txt),file.path(r,'relatorio_execucao.html'),useBytes=TRUE)
}

monitora_criar_qfield <- function(config=MQ_CONFIG) {
  mq_deps();c<-utils::modifyList(MQ_CONFIG,config,keep.null=TRUE);mq_validate_config(c)
  if(mq_design_active(c)&&!requireNamespace('lpSolve',quietly=TRUE))mq_stop('Instale lpSolve antes de iniciar o desenho com cotas cumulativas.')
  c$script_sha256<-if(!is.na(MQ_SCRIPT_ARQUIVO))mq_hash(MQ_SCRIPT_ARQUIVO)else NA_character_
  input<-normalizePath(c$entrada,winslash='/',mustWork=TRUE);out<-normalizePath(c$saida,winslash='/',mustWork=FALSE)
  if(tolower(input)==tolower(out) || startsWith(tolower(out),paste0(tolower(input),'/')) || startsWith(tolower(input),paste0(tolower(out),'/')))mq_stop('Entrada e saída precisam ser pastas separadas e não aninhadas.')
  cache_abs<-normalizePath(c$cache_dir,winslash='/',mustWork=FALSE)
  if(tolower(cache_abs)==tolower(input)||startsWith(tolower(cache_abs),paste0(tolower(input),'/')))mq_stop('Cache não pode ficar dentro da entrada.')
  slug<-monitora_qfield_slug(c$projeto);dir.create(out,recursive=TRUE,showWarnings=FALSE)
  final<-file.path(out,paste0(slug,'_',format(Sys.time(),'%Y%m%d_%H%M%S'),'_',substr(digest::digest(tempfile()),1,6)))
  root<-paste0(final,'.construcao');dir.create(root)
  for(d in c('01_qfield/dados','01_qfield/mapas','02_relatorio/diagnosticos','03_vetores/gpkg','03_vetores/kml','03_vetores/kmz','04_csv'))dir.create(file.path(root,d),recursive=TRUE)
  report<-file.path(root,'02_relatorio');scratch<-file.path(root,'.temporarios');dir.create(scratch)
  files<-list.files(input,recursive=TRUE,full.names=TRUE);files<-files[!file.info(files)$isdir]
  inv<-data.frame(arquivo=substring(files,nchar(input)+2),bytes=file.info(files)$size,sha256=vapply(files,mq_hash,character(1)))
  stages<-data.frame(etapa=character(),segundos=numeric(),descricao=character());t0<-proc.time()[3];last<-t0
  note<-c('Roteiro 29/04/2026: PA = início previsto; deslocamento em campo até 10 m; tentar N, L, S, O; depois alternativo mais próximo. Distâncias se aplicam ao segmento inteiro.', 'Grade 156,25 m não garante 100 m entre transectos instalados. A formação MapBiomas não substitui a observada. Simulações e proximidade não excluem pontos automaticamente. Quando habilitada, a classificação do desenho e suas exclusões são registradas no relatório.', 'As linhas entre extremos observados representam ligações; não são trajetos de acesso levantados. Apoio de campo é editável e precisa ser preservado antes de atualizar o projeto.')
  step<-function(name,description){now<-proc.time()[3];stages<<-rbind(stages,data.frame(etapa=name,segundos=round(now-last,2),descricao=description));last<<-now;message(sprintf('[%s] %s — %.1f s acumulados',name,description,now-t0))}
  result<-tryCatch({
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
      g<-mq_grid(dat$ae,contexts,cr,c,old)
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
      ref<-list(dominio_processado_wkt=sf::st_as_text(domain,digits=16),versao=1,epsg=cr$epsg,grade_m=c$grade_m,contextos=contexts,cadastro_sha256=mq_hash(file.path(report,'cadastro_grade.gpkg')),regra='vértices únicos; fronteiras incluídas; origem fixa; ID nunca renumerado',semente=c$semente)
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
        dat$camadas[[length(dat$camadas)+1]]<-list(x=z,papel=role,nome=role,label='PA',fonte=paste('gerado',mode))
      }
      pts<-g[g$categoria!='grade',]
      mq_diagnostics(pts,dat$camadas,ae,c,file.path(report,'diagnosticos'))
    } else {
      labels<-list();assembled<-list();audit_complete<-TRUE
      for(i in seq_along(dat$camadas)) {
        l<-dat$camadas[[i]];x<-l$x
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
    step('06_exportacao','QGS, GeoPackages, CSV, KML e KMZ gerados e reabertos para conferência.')
    after<-vapply(files,mq_hash,character(1));if(!identical(unname(after),unname(inv$sha256)))mq_stop('Arquivo de entrada alterado durante a execução.')
    guide<-c('MONITORA QFIELD — v0.3.0',paste('Projeto:',c$projeto),paste('Modo:',mode),note,'Coordenadas exibidas em latitude/longitude WGS84, graus decimais, seis casas. Cálculos em CRS métrico. Confirme posicionamento, identificação, labels, zoom e apoio editável no QField em modo avião.','Importe o ZIP em pasta nova. Não substitua apoio_campo.gpkg já preenchido.','Camadas PA são referências planejadas, não UAs instaladas. Não há envio automático ao QFieldCloud.','Relatório completo está na pasta 02_relatorio da entrega. Falhas opcionais constam nos atributos e no relatório.')
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
    mq_report(root,c,stages,status,note,inv)
    unlink(scratch,recursive=TRUE)
    fs<-list.files(root,recursive=TRUE,full.names=TRUE);fs<-fs[!file.info(fs)$isdir]
    mq_csv(data.frame(arquivo=substring(fs,nchar(root)+2),bytes=file.info(fs)$size,sha256=vapply(fs,mq_hash,character(1))),file.path(report,'manifesto.csv'))
    mq_json(list(status=status,modo=mode,segundos=round(proc.time()[3]-t0,2),versao='0.3.0'),file.path(report,'resultado.json'))
    if(file.exists(final)||!file.rename(root,final))mq_stop('Falha ao promover pacote; construção preservada.')
    message('Concluído: ',final);list(pasta=final,status=status,modo=mode)
  },error=function(e){
    step('FALHA',conditionMessage(e));mq_report(root,c,stages,'BLOQUEADO',c(note,conditionMessage(e)),inv)
    mq_json(list(status='BLOQUEADO',motivo=conditionMessage(e)),file.path(report,'resultado.json'))
    mq_stop(conditionMessage(e),'\nDiagnóstico preservado em: ',root)
  })
  invisible(result)
}

# Rscript: execução direta. RStudio: botão Source executa, salvo opção de somente carregar.
if ((sys.nframe()==0L || interactive()) && !isTRUE(getOption('monitora.qfield.somente_funcoes',FALSE))) {
  monitora_criar_qfield(MQ_CONFIG)
}
