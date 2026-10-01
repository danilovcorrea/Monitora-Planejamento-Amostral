# Monitora — criação independente de projetos QField
# Versão 0.1.0 — candidata de homologação, 01/10/2026.
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
  mapbiomas = TRUE, mb_produto = '30m', mb_colecao = 11L, mb_ano = 2025L,
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
  p <- c('sf','terra','xml2','zip','jsonlite','digest','httr','data.table','DBI','RSQLite')
  miss <- p[!vapply(p, requireNamespace, logical(1), quietly=TRUE)]
  if(length(miss)) mq_stop('Instale os pacotes: ',paste(miss,collapse=', '))
}
mq_validate_config <- function(c) {
  for(n in c('transecto_m','distancia_min_m','deslocamento_max_m','max_pontos','timeout_s'))
    if(length(c[[n]])!=1L || !is.numeric(c[[n]]) || !is.finite(c[[n]]) || c[[n]]<0 || (n!='deslocamento_max_m' && c[[n]]==0)) mq_stop('Parâmetro inválido: ',n)
  if(length(c$grade_m)!=2 || any(!is.finite(c$grade_m)) || any(c$grade_m<=0)) mq_stop('grade_m exige largura e altura positivas.')
  if(!c$modo %in% c('auto','planejar','montar','expandir')) mq_stop('Modo inválido.')
  if(length(c$semente)!=1 || !is.finite(c$semente) || c$semente!=as.integer(c$semente)) mq_stop('Semente inválida.')
  if(!c$mb_produto %in% c('30m','10m')) mq_stop('MapBiomas: produto deve ser 30m ou 10m.')
  if(isTRUE(c$mapbiomas) && !((c$mb_produto=='30m' && c$mb_colecao==11 && c$mb_ano %in% 1985:2025) || (c$mb_produto=='10m' && c$mb_colecao==4 && c$mb_ano %in% 2017:2025))) mq_stop('Produto/coleção/ano MapBiomas não homologado; use 30m/11 ou 10m/4 até 2025.')
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
    out[[length(out)+1L]] <- list(x=x,papel=role,nome=nm,label=label,fonte=paste(ar,la,sep=' | '))
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
mq_mb <- function(x,c,report) {
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
    mq_csv(data.frame(PA=pri$PA,alternativo=alt$PA[i],distancia_m=as.numeric(sf::st_distance(pri,alt[i,],by_element=TRUE)),criterio='menor distância plana PA a PA; não comprova instalação'),file.path(report,'alternativos_proximos.csv'))
  }
}

mq_rasters <- function(entrada,destino,points,report) {
  files<-list.files(entrada,pattern='\\.(mbtiles|tif|tiff|img|asc)$',full.names=TRUE,ignore.case=TRUE)
  out<-list();aud<-list();coverage_all<-if(is.null(points))logical()else rep(FALSE,nrow(points))
  for(f in files) {
    monitora_qfield_caminho_local(basename(f),entrada)
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
      p<-terra::project(terra::vect(points),terra::crs(r));val<-terra::extract(r,p,method='simple');visible<-rowSums(!is.na(val[,-1,drop=FALSE]))>0 & if(terra::nlyr(r)>=4)!is.na(val[[5]]) & val[[5]]>0 else TRUE;covered<-sum(visible);coverage_all<-coverage_all|visible
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
  mq_csv(stages,file.path(r,'etapas.csv'));mq_json(c,file.path(r,'configuracao.json'));mq_csv(inventory,file.path(r,'fontes_e_checksums.csv'))
  trs<-apply(stages,1,function(v)paste0('<tr>',paste0('<td>',e(v),'</td>',collapse=''),'</tr>'))
  txt<-c('<!doctype html><html lang="pt-BR"><meta charset="utf-8"><title>Relatório de execução — Monitora QField</title><style>body{font:16px sans-serif;max-width:1100px;margin:40px auto;line-height:1.5;padding:0 20px}table{border-collapse:collapse;width:100%}td,th{border:1px solid #bbb;padding:8px;text-align:left}h1{color:#165c46}</style>',paste0('<h1>',e(c$projeto),'</h1><p>Versão 0.1.0 · ',e(status),'</p>'),'<p>Produto de planejamento e navegação. Homologação automática não substitui verificação em QField no aparelho, em modo avião, nem aplicação do roteiro em campo.</p>',paste0('<ul>',paste0('<li>',e(notes),'</li>',collapse=''),'</ul>'),paste0('<table><thead><tr>',paste0('<th>',e(names(stages)),'</th>',collapse=''),'</tr></thead><tbody>',paste(trs,collapse=''),'</tbody></table>'),if(file.exists(file.path(r,'mapa_planejamento.png'))) '<p><img src="mapa_planejamento.png" alt="Visão geral das áreas e pontos" style="width:100%"></p>' else '', '<p>Detalhes: configuracao.json; consulta_uc.json; referencia_grade.json e cadastro_grade.gpkg (planejamento/expansão); camadas.csv; imagens.csv; legenda/fonte MapBiomas; diagnosticos/; fontes_e_checksums.csv; manifesto.csv.</p></html>')
  writeLines(enc2utf8(txt),file.path(r,'relatorio_execucao.html'),useBytes=TRUE)
}

monitora_criar_qfield <- function(config=MQ_CONFIG) {
  mq_deps();c<-utils::modifyList(MQ_CONFIG,config,keep.null=TRUE);mq_validate_config(c)
  c$script_sha256<-if(!is.na(MQ_SCRIPT_ARQUIVO))mq_hash(MQ_SCRIPT_ARQUIVO)else NA_character_
  input<-normalizePath(c$entrada,winslash='/',mustWork=TRUE);out<-normalizePath(c$saida,winslash='/',mustWork=FALSE)
  if(tolower(input)==tolower(out) || startsWith(tolower(out),paste0(tolower(input),'/')) || startsWith(tolower(input),paste0(tolower(out),'/')))mq_stop('Entrada e saída precisam ser pastas separadas e não aninhadas.')
  slug<-monitora_qfield_slug(c$projeto);dir.create(out,recursive=TRUE,showWarnings=FALSE)
  final<-file.path(out,paste0(slug,'_',format(Sys.time(),'%Y%m%d_%H%M%S'),'_',substr(digest::digest(tempfile()),1,6)))
  root<-paste0(final,'.construcao');dir.create(root)
  for(d in c('01_qfield/dados','01_qfield/mapas','02_relatorio/diagnosticos','03_vetores/gpkg','03_vetores/kml','03_vetores/kmz','04_csv'))dir.create(file.path(root,d),recursive=TRUE)
  report<-file.path(root,'02_relatorio');scratch<-file.path(root,'.temporarios');dir.create(scratch)
  files<-list.files(input,recursive=TRUE,full.names=TRUE);files<-files[!file.info(files)$isdir]
  inv<-data.frame(arquivo=substring(files,nchar(input)+2),bytes=file.info(files)$size,sha256=vapply(files,mq_hash,character(1)))
  stages<-data.frame(etapa=character(),segundos=numeric(),descricao=character());t0<-proc.time()[3];last<-t0
  note<-c('Roteiro 29/04/2026: PA = início previsto; deslocamento em campo até 10 m; tentar N, L, S, O; depois alternativo mais próximo. Distâncias se aplicam ao segmento inteiro.', 'Grade 156,25 m não garante 100 m entre transectos instalados. A formação MapBiomas não substitui a observada. Não houve exclusão automática por simulação, proximidade ou classe cartográfica.', 'As linhas entre extremos observados representam ligações; não são trajetos de acesso levantados. Apoio de campo é editável e precisa ser preservado antes de atualizar o projeto.')
  step<-function(name,description){now<-proc.time()[3];stages<<-rbind(stages,data.frame(etapa=name,segundos=round(now-last,2),descricao=description));last<<-now;message(sprintf('[%s] %s — %.1f s acumulados',name,description,now-t0))}
  result<-tryCatch({
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
      if(mq_hash(file.path(rd,'cadastro_grade.gpkg'))!=prev$cadastro_sha256)mq_stop('Cadastro anterior difere da assinatura registrada.')
      prev$grade_m<-unlist(prev$grade_m);for(i in seq_along(prev$contextos))prev$contextos[[i]]$origem<-unlist(prev$contextos[[i]]$origem)
      # A origem/projeção anteriores prevalecem sobre seleção automática baseada em AE expandida.
      if(is.null(c$epsg)){cr<-sf::st_crs(prev$epsg);ae<-sf::st_transform(dat$ae,cr);for(i in seq_along(dat$camadas))dat$camadas[[i]]$x<-mq_transform(dat$camadas[[i]]$x,cr)}
      note<-c(note,'Expansão: consulta federal atual registrada; origem e associação territorial da malha anterior preservadas. Alteração do limite não deslocou a grade.')
    }
    if(mode!='montar') {
      contexts<-mq_contexts(dat$ae,official$x,cr,c$grade_m,prev)
      g<-mq_grid(dat$ae,contexts,cr,c,old);g<-mq_select(g,c)
      # Cadastro inclui pontos históricos fora da AE atual para nunca reciclar seus IDs.
      ledger<-g
      if(!is.null(old)){keep<-old[!old$chave_grade%in%g$chave_grade,];if(nrow(keep))ledger<-rbind(g,keep[,names(g)])}
      sf::st_write(ledger,file.path(report,'cadastro_grade.gpkg'),layer='cadastro',quiet=TRUE)
      domain<-sf::st_union(ae);if(!is.null(prev$dominio_processado_wkt))domain<-sf::st_union(c(sf::st_geometry(domain),sf::st_as_sfc(prev$dominio_processado_wkt,crs=cr)))
      ref<-list(dominio_processado_wkt=sf::st_as_text(domain,digits=16),versao=1,epsg=cr$epsg,grade_m=c$grade_m,contextos=contexts,cadastro_sha256=mq_hash(file.path(report,'cadastro_grade.gpkg')),regra='vértices únicos; fronteiras incluídas; origem fixa; ID nunca renumerado',semente=c$semente)
      mq_json(ref,file.path(report,'referencia_grade.json'))
      step('03_grade',sprintf('%d vértices na união das AEs; %d prioritários; %d alternativos; denominador global=%d',nrow(g),sum(g$categoria=='prioritario'),sum(g$categoria=='alternativo'),nrow(g)))
      if(isTRUE(c$mapbiomas))g<-mq_mb(g,c,report)
      g<-mq_coordinates(g)
      for(role in c('grade_amostral','PA_priorit','PA_altern')) {
        z<-if(role=='grade_amostral')g else g[g$categoria==if(role=='PA_priorit')'prioritario'else 'alternativo',]
        dat$camadas[[length(dat$camadas)+1]]<-list(x=z,papel=role,nome=role,label='PA',fonte=paste('gerado',mode))
      }
      pts<-g[g$categoria!='grade',]
      mq_diagnostics(pts,dat$camadas,ae,c,file.path(report,'diagnosticos'))
    } else {
      labels<-list()
      for(i in seq_along(dat$camadas)) {
        l<-dat$camadas[[i]];x<-l$x
        if(nrow(x)&&all(as.character(sf::st_geometry_type(x))=='POINT')) {
          if(isTRUE(c$mapbiomas))x<-mq_mb(x,c,report)
          x<-mq_coordinates(x);dat$camadas[[i]]$x<-x
          if(l$papel%in%c('PA_priorit','PA_altern'))labels[[l$papel]]<-as.character(x[[l$label]])
        }
      }
      if(length(intersect(labels[['PA_priorit']],labels[['PA_altern']]))>0)mq_stop('Mesmo rótulo PA em prioritários e alternativos.')
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
    ras<-mq_rasters(input,file.path(root,'01_qfield','mapas'),coverage,report)
    cover<-attr(ras,'cobertura');if(cover$avaliados>0)note<-c(note,sprintf('Fundos locais: %d de %d pontos têm pixel válido. %d pontos sem imagem; a cobertura não comprova acesso ou transecção inteira.',cover$cobertos,cover$avaliados,cover$avaliados-cover$cobertos))
    step('05_imagens',paste(length(ras),'rasters copiados; hashes e cobertura nos pontos conferidos. Nenhuma imagem adquirida automaticamente.'))
    nms<-vapply(dat$camadas,`[[`,character(1),'nome');if(anyDuplicated(tolower(nms)))mq_stop('Nomes finais de camadas repetidos (incluindo UC/apoio); ajuste o manifesto.')
    dat$camadas<-mq_export(dat$camadas,root)
    mq_qgs(dat$camadas,ras,ae,c,file.path(root,'01_qfield','projeto.qgs'))
    step('06_exportacao','QGS, GeoPackages, CSV, KML e KMZ gerados e reabertos para conferência.')
    after<-vapply(files,mq_hash,character(1));if(!identical(unname(after),unname(inv$sha256)))mq_stop('Arquivo de entrada alterado durante a execução.')
    guide<-c('MONITORA QFIELD — v0.1.0',paste('Projeto:',c$projeto),paste('Modo:',mode),note,'Coordenadas exibidas em latitude/longitude WGS84, graus decimais, seis casas. Cálculos em CRS métrico. Confirme posicionamento, identificação, labels, zoom e apoio editável no QField em modo avião.','Importe o ZIP em pasta nova. Não substitua apoio_campo.gpkg já preenchido.','Camadas PA são referências planejadas, não UAs instaladas. Não há envio automático ao QFieldCloud.','Relatório completo está na pasta 02_relatorio da entrega. Falhas opcionais constam nos atributos e no relatório.')
    writeLines(enc2utf8(guide),file.path(root,'01_qfield','LEIA_ME.txt'),useBytes=TRUE)
    zipfile<-file.path(root,'01_qfield','pacote_qfield.zip');zip::zipr(zipfile,c('projeto.qgs','dados','mapas','LEIA_ME.txt'),root=file.path(root,'01_qfield'))
    z<-zip::zip_list(zipfile);if(!all(c('projeto.qgs','LEIA_ME.txt')%in%z$filename))mq_stop('Pacote QField incompleto.')
    step('07_pacote','ZIP independente com caminhos relativos; integridade das entradas preservada.')
    status<-'GERADO E VALIDADO AUTOMATICAMENTE; teste QField móvel pendente'
    if(length(ras)>0 && cover$cobertos<cover$avaliados)status<-paste(status,'; cobertura de imagens parcial')
    mb_states<-unlist(lapply(dat$camadas,function(l)if('mb_status'%in%names(l$x))unique(l$x$mb_status)else NULL))
    if(any(grepl('^falha|sem_dado|codigo_sem_legenda',mb_states))){status<-paste(status,'; MapBiomas com pendências');note<-c(note,unique(mb_states[mb_states!='obtido']))}
    mq_overview(dat$camadas,ae,file.path(report,'mapa_planejamento.png'))
    mq_report(root,c,stages,status,note,inv)
    unlink(scratch,recursive=TRUE)
    fs<-list.files(root,recursive=TRUE,full.names=TRUE);fs<-fs[!file.info(fs)$isdir]
    mq_csv(data.frame(arquivo=substring(fs,nchar(root)+2),bytes=file.info(fs)$size,sha256=vapply(fs,mq_hash,character(1))),file.path(report,'manifesto.csv'))
    mq_json(list(status=status,modo=mode,segundos=round(proc.time()[3]-t0,2),versao='0.1.0'),file.path(report,'resultado.json'))
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
