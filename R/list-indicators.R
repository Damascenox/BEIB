#' Obter o Manifesto de Indicadores
#'
#' @description
#' Função interna que carrega a tabela de metadados dos indicadores.
#' @noRd
beib_get_manifest <- function() {
  # URL do arquivo oficial hospedado na Release do GitHub
  url_manifesto <- "https://github.com/Damascenox/BEIB/releases/download/v1.0.0-data/manifesto.rds"
  
  con <- url(url_manifesto, "rb")
  on.exit(close(con))
  
  readRDS(con)
}

#' Listar Indicadores Disponíveis
#'
#' @description
#' Retorna a lista de indicadores educacionais da Educação Básica
#' disponíveis no pacote BEIB.
#'
#' @param as_tibble Lógico. Se `TRUE`, retorna diretamente uma [tibble::tibble()].
#'   Se `FALSE` (padrão), exibe a lista formatada no console e retorna os dados
#'   de forma invisível.
#'
#' @return Um [tibble::tibble()] com as informações dos indicadores.
#' @export
beib_list_indicators <- function(as_tibble = FALSE) {
  manifesto <- beib_get_manifest()
  
  resumo <- tibble::tibble(
    indicador          = manifesto$indicador,
    nome               = manifesto$nome,
    anos_disponiveis   = manifesto$anos_disponiveis,
    niveis_disponiveis = manifesto$niveis_disponiveis
  )
  
  if (isTRUE(as_tibble)) {
    return(resumo)
  }
  
  # Cabeçalho limpo (respeita a largura do seu console e o seu tema)
  largura <- min(getOption("width", 80), 80)
  divisor <- paste(rep("─", max(largura - 45, 10)), collapse = "")
  
  cat(sprintf("\n── Indicadores Educacionais Disponíveis (BEIB) %s\n\n", divisor))
  
  for (i in seq_len(nrow(manifesto))) {
    m <- manifesto[i, ]
    cat(sprintf(" \u2714 %s\n", m$nome))
    cat(sprintf("     \u2022 ID: \"%s\"\n", m$indicador))
    cat(sprintf("     \u2022 Anos: %s | Níveis: %s\n\n", m$anos_disponiveis, m$niveis_disponiveis))
  }
  
  cat(paste(rep("─", largura), collapse = ""), "\n")
  cat("Use beib_list_indicators(as_tibble = TRUE) para ver ou salvar como tabela.\n\n")
  
  invisible(resumo)
}

#' Listar Anos Disponíveis para um Indicador
#'
#' @description
#' Retorna os anos cobertos para um determinado indicador.
#'
#' @param indicador Nome técnico do indicador (ex: "media_de_alunos_por_turma").
#'
#' @return Um vetor numérico com a sequência de anos.
#' @export
#'
#' @examples
#' beib_list_years("media_de_alunos_por_turma")
beib_list_years <- function(indicador) {
  indicador_expr <- substitute(indicador)
  if (is.symbol(indicador_expr)) {
    indicador <- tryCatch(as.character(indicador), error = function(e) as.character(indicador_expr))
  }
  
  manifesto <- beib_get_manifest()
  linha <- manifesto[manifesto$indicador == indicador, ]
  
  if (nrow(linha) == 0) {
    cli::cli_abort(c(
      "x" = "Indicador {.val {indicador}} nao encontrado.",
      "i" = "Consulte {.fun beib_list_indicators} para ver a lista de IDs validos."
    ))
  }
  
  seq(linha$ano_min, linha$ano_max)
}