library(piggyback)
library(gh)

repo <- "Damascenox/BEIB"
tag  <- "v1.0.0-data"
manifesto <- "G:/Meu Drive/Pacote_Indicadores_Educação_Básica/dados_limpos/manifesto.rds"

arquivos <- list.files(
  path = "G:/Meu Drive/Pacote_Indicadores_Educação_Básica/dados_limpos",
  pattern = "\\.parquet$",
  recursive = TRUE,
  full.names = TRUE
)

pb_new_release(repo = repo, tag = tag, name = "Datasets Educacionais Parquet (v1.0.0)")
  
pb_upload(
  file = arquivos,
  repo = repo,
  tag = tag,
  overwrite = TRUE 
)

pb_upload(manifesto, repo = "Damascenox/BEIB", tag = "v1.0.0-data")

