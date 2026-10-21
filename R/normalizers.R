# -------------------------------------------------------------------------
# NORMALIZADORES E ERGONOMIA DE ENTRADA DO USUÁRIO ("TOLERÂNCIA A ENTRADAS")
# -------------------------------------------------------------------------

# Mapa de Unidades da Federação (Siglas para Nomes por Extenso)
beib_uf_map <- c(
  "AC" = "Acre", "AL" = "Alagoas", "AP" = "Amapá", "AM" = "Amazonas",
  "BA" = "Bahia", "CE" = "Ceará", "DF" = "Distrito Federal", "ES" = "Espírito Santo",
  "GO" = "Goiás", "MA" = "Maranhão", "MT" = "Mato Grosso", "MS" = "Mato Grosso do Sul",
  "MG" = "Minas Gerais", "PA" = "Pará", "PB" = "Paraíba", "PR" = "Paraná",
  "PE" = "Pernambuco", "PI" = "Piauí", "RJ" = "Rio de Janeiro", "RN" = "Rio Grande do Norte",
  "RS" = "Rio Grande do Sul", "RO" = "Rondônia", "RR" = "Roraima", "SC" = "Santa Catarina",
  "SP" = "São Paulo", "SE" = "Sergipe", "TO" = "Tocantins"
)

#' Normalizar termo para busca insensível a acentos e maiúsculas
#' @noRd
beib_norm_str <- function(x) {
  if (is.null(x) || length(x) == 0L) return(character(0))
  texto <- tolower(trimws(enc2utf8(as.character(x))))
  texto <- iconv(texto, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  gsub("['\"^~`]", "", texto)
}

#' Helper vetorizado para mapear entradas contra uma lista canônica
#' @noRd
beib_match_canonical <- function(x, canonicos) {
  if (is.null(x) || length(x) == 0L) return(character(0))
  x_chr <- trimws(as.character(x))
  idx <- match(beib_norm_str(x_chr), beib_norm_str(canonicos))
  unique(ifelse(is.na(idx), x_chr, canonicos[idx]))
}

# Tabela de Sinônimos Comuns para Etapas e Métricas
beib_synonyms <- list(
  etapa = c(
    "fundamental"          = "fund",
    "fundamental_iniciais" = "fund_iniciais",
    "anos_iniciais"        = "fund_iniciais",
    "iniciais"             = "fund_iniciais",
    "fundamental_finais"   = "fund_finais",
    "anos_finais"          = "fund_finais",
    "finais"               = "fund_finais",
    "ensino_medio"         = "medio",
    "med"                  = "medio",
    "eja_fundamental"      = "eja_fund",
    "eja_medio"            = "eja_med",
    "pre_escola"           = "pre",
    "multietapa"           = "multi_etapa",
    "profissional"         = "prof",
    "educacao_especial"    = "especial"
  ),
  metrica = c(
    "aprov"     = "aprovacao",
    "reprov"    = "reprovacao",
    "aband"     = "abandono",
    "prom"      = "promocao",
    "repet"     = "repeticao",
    "evas"      = "evasao",
    "migr"      = "migracao"
  )
)

#' Traduzir sinônimos para os nomes canônicos do dicionário
#' @noRd
beib_resolve_synonym <- function(x, type = "etapa") {
  if (is.null(x)) return(NULL)
  x_norm <- beib_norm_str(x)
  map <- beib_synonyms[[type]]
  matched <- map[x_norm]
  ifelse(!is.na(matched), matched, x_norm)
}

# Operador interno para fallback de NA escalar
`%|NA|%` <- function(x, y) if (is.na(x)) y else x

#' Resolver Indicador a partir de Sigla, ID ou Nome
#' @noRd
beib_resolve_indicator <- function(indicador_expr, env = parent.frame()) {
  if (missing(indicador_expr) || is.null(indicador_expr)) {
    return(NULL)
  }
  
  # 1. Tenta avaliar a expressão no ambiente chamador (suporta variáveis, vetores indexados x[1], expressões)
  val <- tryCatch(eval(indicador_expr, envir = env), error = function(e) NULL)
  
  texto <- if (is.character(val) && length(val) > 0L && nzchar(trimws(val[1L]))) {
    val[1L]
  } else if (is.symbol(indicador_expr)) {
    as.character(indicador_expr)
  } else {
    deparse1(indicador_expr)
  }
  
  if (length(texto) == 0L || is.na(texto[1L]) || !nzchar(trimws(texto[1L]))) {
    return(NULL)
  }
  
  texto_limpo <- tolower(trimws(texto[1L]))
  unname(beib_siglas_map[texto_limpo]) %|NA|% texto_limpo
}

#' Normalizar Nível Geográfico de Agregação
#' @noRd
beib_normalize_nivel <- function(nivel) {
  if (missing(nivel) || is.null(nivel)) return("brasil_regioes_ufs")
  
  n_norm <- beib_norm_str(nivel[1L])
  
  mapa_niveis <- c(
    "brasil_regioes_ufs" = "brasil_regioes_ufs",
    "brasil"             = "brasil_regioes_ufs",
    "regioes"            = "brasil_regioes_ufs",
    "ufs"                = "brasil_regioes_ufs",
    "uf"                 = "brasil_regioes_ufs",
    "estado"             = "brasil_regioes_ufs",
    "estados"            = "brasil_regioes_ufs",
    "municipios"         = "municipios",
    "municipio"          = "municipios",
    "mun"                = "municipios",
    "cidade"             = "municipios",
    "cidades"            = "municipios",
    "escolas"            = "escolas",
    "escola"             = "escolas",
    "esc"                = "escolas",
    "metadataset"        = "metadataset",
    "meta"               = "metadataset",
    "metadados"          = "metadataset"
  )
  
  if (n_norm %in% names(mapa_niveis)) {
    return(unname(mapa_niveis[n_norm]))
  }
  
  cli::cli_abort(c(
    "x" = "Nível de agregação {.val {as.character(nivel)[1L]}} inválido.",
    "i" = "Níveis válidos: {.val brasil_regioes_ufs}, {.val municipios}, {.val escolas} ou {.val metadataset}."
  ))
}

#' Normalizar Siglas e Nomes de Unidades da Federação (Vetorizado)
#' @noRd
beib_normalize_uf <- function(uf, formato = c("sigla", "nome")) {
  if (is.null(uf) || length(uf) == 0L) return(character(0))
  formato <- match.arg(formato)
  
  uf_trim <- trimws(as.character(uf))
  uf_norm <- beib_norm_str(uf_trim)
  
  lookup_sigla <- c(
    stats::setNames(names(beib_uf_map), tolower(names(beib_uf_map))),
    stats::setNames(names(beib_uf_map), beib_norm_str(beib_uf_map))
  )
  
  siglas <- unname(lookup_sigla[uf_norm])
  
  res <- if (formato == "sigla") {
    ifelse(is.na(siglas), toupper(uf_trim), siglas)
  } else {
    ifelse(is.na(siglas), uf_trim, unname(beib_uf_map[siglas]))
  }
  
  unique(res)
}

#' Normalizar Rede de Ensino
#' @noRd
beib_normalize_rede <- function(rede) {
  beib_match_canonical(
    rede,
    c("Total", "Federal", "Estadual", "Municipal", "Privada", "Pública")
  )
}

#' Normalizar Localização (Total, Urbana, Rural)
#' @noRd
beib_normalize_localizacao <- function(localizacao) {
  beib_match_canonical(localizacao, c("Total", "Urbana", "Rural"))
}
