# -------------------------------------------------------------------------
# INFRAESTRUTURA TÉCNICA DO BEIB (DOWNLOAD, CACHE, VALIDAÇÃO E ARROW ENGINE)
# -------------------------------------------------------------------------

# Tabela de equivalência de siglas oficiais do INEP para identificadores de arquivo
beib_siglas_map <- c(
  "afd"      = "adequacao_da_formacao_docente",
  "icg"      = "complexidade_de_gestao_da_escola",
  "cg"       = "complexidade_de_gestao_da_escola",
  "ied"      = "esforco_docente",
  "ed"       = "esforco_docente",
  "mat"      = "media_de_alunos_por_turma",
  "at"       = "media_de_alunos_por_turma",
  "had"      = "media_de_horas_aula_diaria",
  "dsu"      = "percentual_de_docentes_com_curso_superior",
  "ird"      = "regularidade_do_corpo_docente",
  "rem"      = "remuneracao_media_dos_docentes",
  "rmd"      = "remuneracao_media_dos_docentes",
  "tdi"      = "taxas_de_distorcao_idade_serie",
  "tnr"      = "taxas_de_nao_resposta_tnr_",
  "rend"     = "taxas_de_rendimento",
  "tx_rend"  = "taxas_de_rendimento",
  "trans"    = "taxas_de_transicao",
  "tx_trans" = "taxas_de_transicao",
  "ideb"     = "ideb",
  "inse"     = "nivel_socioeconomico_inse",
  "ife"      = "indicadores_financeiros_educacionais"
)

#' Valida se o arquivo baixado não está corrompido ou truncado
#' @noRd
beib_is_valid_file <- function(path) {
  if (!file.exists(path)) return(FALSE)
  fsize <- file.size(path)
  if (is.na(fsize) || fsize == 0) return(FALSE)
  
  ext <- tolower(tools::file_ext(path))
  
  if (ext == "zip") {
    res <- tryCatch(
      utils::unzip(path, list = TRUE),
      error = function(e) NULL,
      warning = function(w) NULL
    )
    return(!is.null(res) && nrow(res) > 0L)
  }
  
  if (ext == "pdf") {
    if (fsize < 5) return(FALSE)
    header <- tryCatch(readBin(path, "raw", n = 5L), error = function(e) raw(0))
    return(identical(header, charToRaw("%PDF-")))
  }
  
  if (ext == "parquet") {
    # Parquet válido exige no mínimo 12 bytes: 4 (PAR1) + 4 (footer len) + 4 (PAR1)
    if (fsize < 12) return(FALSE)
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    
    magic_par1 <- charToRaw("PAR1")
    header <- tryCatch(readBin(con, "raw", n = 4L), error = function(e) raw(0))
    if (!identical(header, magic_par1)) return(FALSE)
    
    # Verifica os 4 últimos bytes para garantir que o arquivo não foi truncado
    tryCatch(seek(con, where = -4, origin = "end"), error = function(e) NULL)
    footer <- tryCatch(readBin(con, "raw", n = 4L), error = function(e) raw(0))
    return(identical(footer, magic_par1))
  }
  
  TRUE
}

#' Downloader Seguro usando apenas {curl}
#' @noRd
beib_download_file <- function(url, destfile, max_tries = 3L) {
  ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
  timeout_seg <- getOption("beib.timeout", 600)
  
  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
  temp_file <- paste0(destfile, ".part")
  on.exit(if (file.exists(temp_file)) unlink(temp_file), add = TRUE)
  
  ultimo_erro <- "Falha de conexão"
  
  for (tentativa in seq_len(max_tries)) {
    res <- curl::multi_download(
      urls           = url,
      destfiles      = temp_file,
      resume         = TRUE,
      progress       = interactive(),
      useragent      = ua,
      timeout        = timeout_seg,
      connecttimeout = 30,
      ssl_verifypeer = 0L,
      ssl_verifyhost = 0L,
      http_version   = 2L, # HTTP/1.1
      ssl_options    = 2L  # CURLSSLOPT_NO_REVOKE
    )
    
    http_ok <- !is.na(res$status_code[1L]) && res$status_code[1L] %in% c(200L, 206L)
    status_ok <- isTRUE(res$success[1L]) && http_ok
    
    if (status_ok && beib_is_valid_file(temp_file)) {
      file.rename(temp_file, destfile)
      return(invisible(destfile))
    }
    
    if (status_ok || (!is.na(res$status_code[1L]) && res$status_code[1L] >= 400L)) {
      if (file.exists(temp_file)) unlink(temp_file)
    }
    
    ultimo_erro <- if (!is.na(res$error[1L]) && nzchar(res$error[1L])) {
      res$error[1L]
    } else if (!http_ok) {
      paste("HTTP Status", res$status_code[1L])
    } else {
      "Arquivo baixado corrompido ou truncado"
    }
    
    if (tentativa < max_tries) {
      cli::cli_alert_warning(
        "Oscilação no servidor ({ultimo_erro}). Tentativa {tentativa + 1L}/{max_tries}..."
      )
      Sys.sleep(2^tentativa)
    }
  }
  
  cli::cli_abort(c(
    "x" = "Não foi possível baixar o arquivo após {max_tries} tentativas.",
    "i" = "Detalhe técnico: {ultimo_erro}",
    "i" = "URL: {.url {url}}"
  ))
}

# -------------------------------------------------------------------------
# FILTROS ATÔMICOS DO ARROW (PUSHDOWN PREDICATES)
# -------------------------------------------------------------------------

#' Converte o vetor de filtro no R para o mesmo tipo da coluna no Arrow Dataset
#' @noRd
beib_cast_to_field <- function(ds, col, vals) {
  if (!inherits(ds, c("Dataset", "ArrowObject", "arrow_dplyr_query"))) {
    return(vals)
  }
  schema_obj <- if (inherits(ds, "arrow_dplyr_query")) ds$.data$schema else ds$schema
  if (is.null(schema_obj) || !col %in% names(schema_obj)) return(vals)
  
  type_str <- schema_obj[[col]]$type$ToString()
  if (grepl("^int|^uint", type_str)) {
    as.integer(vals)
  } else if (grepl("^double|^float", type_str)) {
    as.numeric(vals)
  } else {
    as.character(vals)
  }
}

#' Helper genérico para filtros por código numérico ou nome textual
#' @noRd
beib_arrow_filter_entity <- function(ds, val, col_cod, col_nome, arg_name) {
  if (is.null(val)) return(ds)
  cols_ds <- names(ds)
  
  if (!any(c(col_cod, col_nome) %in% cols_ds)) {
    cli::cli_alert_warning(
      "Filtro {.arg {arg_name}} ignorado: o nível selecionado não possui dados por {arg_name}."
    )
    return(ds)
  }
  
  is_num <- suppressWarnings(all(!is.na(as.numeric(val))))
  if (is_num && col_cod %in% cols_ds) {
    cods <- beib_cast_to_field(ds, col_cod, val)
    dplyr::filter(ds, .data[[col_cod]] %in% cods)
  } else if (col_nome %in% cols_ds) {
    nomes_lower <- tolower(trimws(as.character(val)))
    dplyr::filter(ds, tolower(.data[[col_nome]]) %in% nomes_lower)
  } else {
    ds
  }
}

#' Filtro de Ano para Dataset Arrow
#' @noRd
beib_arrow_filter_ano <- function(ds, ano = NULL) {
  if (is.null(ano)) return(ds)
  col_ano <- intersect(c("ano", "ano_"), names(ds))[1L]
  if (is.na(col_ano)) return(ds)
  
  anos_val <- beib_cast_to_field(ds, col_ano, ano)
  dplyr::filter(ds, .data[[col_ano]] %in% anos_val)
}

#' Filtro de UF para Dataset Arrow
#' @noRd
beib_arrow_filter_uf <- function(ds, uf = NULL) {
  if (is.null(uf)) return(ds)
  cols_ds <- names(ds)
  
  if ("estado" %in% cols_ds) {
    siglas <- beib_normalize_uf(uf, formato = "sigla")
    dplyr::filter(ds, .data[["estado"]] %in% siglas)
  } else if ("unidade_geografica" %in% cols_ds) {
    nomes <- beib_normalize_uf(uf, formato = "nome")
    dplyr::filter(ds, .data[["unidade_geografica"]] %in% nomes)
  } else {
    cli::cli_alert_warning(
      "Filtro {.arg uf} ignorado: o dataset não possui coluna geográfica compatível."
    )
    ds
  }
}

#' Filtro de Município para Dataset Arrow
#' @noRd
beib_arrow_filter_municipio <- function(ds, municipio = NULL) {
  beib_arrow_filter_entity(ds, municipio, "cod_municipio", "nome_municipio", "municipio")
}

#' Filtro de Escola para Dataset Arrow
#' @noRd
beib_arrow_filter_escola <- function(ds, escola = NULL) {
  beib_arrow_filter_entity(ds, escola, "cod_escola", "nome_escola", "escola")
}

#' Filtro de Rede de Ensino para Dataset Arrow
#' @noRd
beib_arrow_filter_rede <- function(ds, rede = NULL) {
  if (is.null(rede) || !"rede_ensino" %in% names(ds)) return(ds)
  redes_finais <- beib_normalize_rede(rede)
  dplyr::filter(ds, .data[["rede_ensino"]] %in% redes_finais)
}

#' Filtro de Localização para Dataset Arrow
#' @noRd
beib_arrow_filter_localizacao <- function(ds, localizacao = NULL) {
  if (is.null(localizacao) || !"localizacao" %in% names(ds)) return(ds)
  loc_finais <- beib_normalize_localizacao(localizacao)
  dplyr::filter(ds, .data[["localizacao"]] %in% loc_finais)
}

#' Filtro de Escolaridade para Dataset Arrow
#' @noRd
beib_arrow_filter_escolaridade <- function(ds, escolaridade = NULL) {
  if (is.null(escolaridade) || !"escolaridade" %in% names(ds)) return(ds)
  esc_lower <- tolower(trimws(as.character(escolaridade)))
  dplyr::filter(ds, tolower(.data[["escolaridade"]]) %in% esc_lower)
}

#' Orquestrador de Filtros Pushdown para Dataset Arrow
#' @noRd
beib_apply_arrow_filters <- function(
    ds,
    ano          = NULL,
    uf           = NULL,
    municipio    = NULL,
    escola       = NULL,
    rede         = NULL,
    localizacao  = NULL,
    escolaridade = NULL
) {
  ds |>
    beib_arrow_filter_ano(ano) |>
    beib_arrow_filter_uf(uf) |>
    beib_arrow_filter_municipio(municipio) |>
    beib_arrow_filter_escola(escola) |>
    beib_arrow_filter_rede(rede) |>
    beib_arrow_filter_localizacao(localizacao) |>
    beib_arrow_filter_escolaridade(escolaridade)
}
