# -------------------------------------------------------------------------
# DICIONÁRIO E SELEÇÃO DE MÉTRICAS VIA TABELA DECLARATIVA (TIDYVERSE)
# -------------------------------------------------------------------------

#' Identificar Colunas de Identificação / Contexto em um Dataset
#'
#' @description
#' Separa as colunas que definem as chaves e entidades (ano, localização,
#' UF, município, escola, rede de ensino) das colunas que representam métricas
#' educacionais propriamente ditas.
#' O campo `"ano_"` é incluído preventivamente porque alguns arquivos legados
#' do INEP foram consolidados com as duas colunas (`ano_` e `ano`).
#'
#' @noRd
beib_identify_id_cols <- function(cols) {
  padrao_id <- c(
    "ano", "ano_", "unidade_geografica", "nivel_geografico",
    "regiao", "estado", "cod_municipio", "nome_municipio",
    "cod_escola", "nome_escola", "localizacao", "rede_ensino",
    "TP_AGREGAÇÃO", "CD_AGREGAÇÃO", "NM_AGREGAÇÃO", "escolaridade"
  )
  intersect(cols, padrao_id)
}

#' Carregar Dicionário Declarativo de Métricas do BEIB
#' @noRd
beib_read_metric_dict <- function() {
  caminho <- system.file("extdata", "dicionario_metricas.csv", package = "BEIB")
  if (caminho == "" || !file.exists(caminho)) {
    caminho <- file.path("inst", "extdata", "dicionario_metricas.csv")
  }
  tibble::as_tibble(utils::read.csv(caminho, stringsAsFactors = FALSE, encoding = "UTF-8"))
}

#' Listar Métricas e Dimensões de um Indicador
#'
#' @description
#' Consulta o dicionário de variáveis e exibe as etapas, grupos, séries e
#' métricas disponíveis para determinado indicador educacional.
#'
#' @param indicador Nome ou sigla oficial do indicador (ex: `afd`, `rend`, `ied`).
#' @return Um [tibble::tibble()] contendo as variáveis disponíveis.
#' @export
#'
#' @examples
#' \dontrun{
#' beib_list_metrics(afd)
#' beib_list_metrics(rend)
#' }
beib_list_metrics <- function(indicador) {
  indicador_expr <- substitute(indicador)
  slug <- beib_resolve_indicator(indicador_expr, env = parent.frame())
  
  if (is.null(slug) || slug == "") {
    cli::cli_abort("Indicador {.val {as.character(indicador_expr)}} não reconhecido.")
  }
  
  dict <- beib_read_metric_dict()
  
  sub_dict <- dict |>
    dplyr::filter(indicador == slug)
  
  if (nrow(sub_dict) == 0L) {
    cli::cli_alert_warning("Nenhuma métrica catalogada para o indicador {.val {slug}}.")
    return(tibble::tibble())
  }
  
  # Remove colunas que são totalmente NA para este indicador específico
  col_valid <- vapply(sub_dict, function(col) any(!is.na(col)), logical(1))
  sub_dict[, col_valid, drop = FALSE]
}

#' Selecionar Colunas de Métricas Declarativamente via Tabela de Metadados
#' @noRd
beib_select_metric_cols <- function(
  all_cols,
  indicador_slug,
  etapa        = NULL,
  grupo        = NULL,
  metrica      = NULL,
  ano_serie    = NULL,
  regularidade = NULL,
  estatistica  = NULL
) {
  id_cols <- beib_identify_id_cols(all_cols)
  all_metric_cols <- setdiff(all_cols, id_cols)
  
  # Se nenhum filtro de métrica foi solicitado, retorna todas as métricas
  if (is.null(etapa) && is.null(grupo) && is.null(metrica) && 
      is.null(ano_serie) && is.null(regularidade) && is.null(estatistica)) {
    return(all_metric_cols)
  }
  
  # 1. Normalização de parâmetros de busca
  e_clean <- if (!is.null(etapa)) {
    ec <- beib_resolve_synonym(etapa, "etapa")
    if ("eja" %in% ec) ec <- unique(c(ec, "eja_fund", "eja_med", "eja_medio"))
    if ("medio" %in% ec) ec <- unique(c(ec, "med"))
    ec
  } else NULL
  
  g_clean <- if (!is.null(grupo)) {
    gc <- beib_norm_str(grupo)
    ifelse(grepl("^\\d+$", gc), paste0("g", gc), gc)
  } else NULL
  
  m_clean <- if (!is.null(metrica)) beib_resolve_synonym(metrica, "metrica") else NULL
  
  a_clean <- if (!is.null(ano_serie)) {
    ac <- beib_norm_str(ano_serie)
    ifelse(grepl("^\\d+$", ac), paste0("ano", ac), ac)
  } else NULL
  
  r_clean <- if (!is.null(regularidade)) beib_norm_str(regularidade) else NULL
  
  s_clean <- if (!is.null(estatistica)) {
    sc <- beib_norm_str(estatistica)
    map_stat <- c(
      "media"         = "rem_media",
      "mediana"       = "rem_mediana",
      "q1"            = "rem_quartil_1",
      "quartil_1"     = "rem_quartil_1",
      "q3"            = "rem_quartil_3",
      "quartil_3"     = "rem_quartil_3",
      "40h"           = "rem_media_40h",
      "media_40h"     = "rem_media_40h",
      "sd"            = "rem_desvio_padrao",
      "desvio_padrao" = "rem_desvio_padrao",
      "carga"         = "carga_horaria_media"
    )
    ifelse(sc %in% names(map_stat), map_stat[sc], sc)
  } else NULL
  
  # 2. Filtragem puramente declarativa no Tidyverse
  cols_sel <- beib_read_metric_dict() |>
    dplyr::filter(indicador == indicador_slug) |>
    dplyr::filter(if (!is.null(e_clean)) etapa %in% e_clean else TRUE) |>
    dplyr::filter(if (!is.null(g_clean)) grupo %in% g_clean else TRUE) |>
    dplyr::filter(if (!is.null(m_clean)) metrica %in% m_clean else TRUE) |>
    dplyr::filter(if (!is.null(a_clean)) ano_serie %in% a_clean else TRUE) |>
    dplyr::filter(if (!is.null(r_clean)) regularidade %in% r_clean else TRUE) |>
    dplyr::filter(if (!is.null(s_clean)) estatistica %in% s_clean else TRUE) |>
    dplyr::pull(coluna)
  
  cols_finais <- intersect(all_metric_cols, cols_sel)
  
  # Mantém índices de nível escola se presentes
  if ("comp_gestao_escola" %in% all_metric_cols && is.null(grupo)) {
    cols_finais <- unique(c("comp_gestao_escola", cols_finais))
  }
  if ("reg_docente_escola" %in% all_metric_cols && is.null(regularidade)) {
    cols_finais <- unique(c("reg_docente_escola", cols_finais))
  }
  
  if (length(cols_finais) == 0L) {
    cli::cli_alert_warning("Nenhuma métrica encontrada para a combinação de filtros especificada.")
  }
  
  cols_finais
}
