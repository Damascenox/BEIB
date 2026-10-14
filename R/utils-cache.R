#' Obter o Diretório de Cache do BEIB
#'
#' @description
#' Retorna o caminho do diretório local onde os datasets baixados são
#' armazenados. Cria o diretório se ele ainda não existir.
#'
#' @return Uma string com o caminho do diretório de cache.
#' @export
#'
#' @examples
#' beib_cache_dir()
beib_cache_dir <- function() {
  caminho <- tools::R_user_dir("BEIB", which = "cache")
  if (!dir.exists(caminho)) {
    dir.create(caminho, recursive = TRUE)
  }
  caminho
}

#' Listar o Status do Cache Local
#'
#' @description
#' Mostra quais arquivos já foram baixados e estão salvos localmente,
#' seus tamanhos em disco e a data de download.
#'
#' @return Um [data.frame] com os arquivos em cache.
#' @export
#'
#' @examples
#' beib_cache_status()
beib_cache_status <- function() {
  dir_cache <- beib_cache_dir()
  arquivos <- list.files(dir_cache, full.names = TRUE)
  
  if (length(arquivos) == 0) {
    message("O cache do BEIB est\u00e1 vazio. Nenhum dado baixado ainda.")
    return(invisible(data.frame()))
  }
  
  info <- file.info(arquivos)
  tamanho_mb <- round(info$size / (1024^2), 2)
  
  status <- data.frame(
    arquivo = basename(arquivos),
    tamanho_mb = tamanho_mb,
    data_modificacao = info$mtime,
    stringsAsFactors = FALSE
  )
  
  cat(sprintf("\n-- Cache Local do BEIB (%s) --\n", dir_cache))
  cat(sprintf("Total de arquivos: %d | Tamanho total: %.2f MB\n\n", nrow(status), sum(tamanho_mb)))
  print(status, row.names = FALSE)
  
  invisible(status)
}

#' Limpar o Cache Local do BEIB
#'
#' @description
#' Apaga todos os arquivos salvos no diretório de cache para liberar espaço
#' em disco ou forçar o re-download de dados atualizados.
#'
#' @param arquivo Nome opcional de um arquivo específico para apagar. Se for
#'   `NULL`, todos os arquivos do cache serão excluídos.
#'
#' @return Invisível.
#' @export
#'
#' @examples
#' \dontrun{
#' beib_cache_clear()
#' }
beib_cache_clear <- function(arquivo = NULL) {
  dir_cache <- beib_cache_dir()
  
  if (is.null(arquivo)) {
    arquivos <- list.files(dir_cache, full.names = TRUE)
    if (length(arquivos) > 0) {
      unlink(arquivos)
      message("Cache do BEIB limpo com sucesso!")
    } else {
      message("O cache j\u00e1 estava vazio.")
    }
  } else {
    caminho_especifico <- file.path(dir_cache, arquivo)
    if (file.exists(caminho_especifico)) {
      unlink(caminho_especifico)
      message(sprintf("Arquivo '%s' removido do cache.", arquivo))
    } else {
      warning(sprintf("Arquivo '%s' n\u00e3o encontrado no cache.", arquivo))
    }
  }
  
  invisible(NULL)
}