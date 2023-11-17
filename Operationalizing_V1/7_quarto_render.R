library(quarto)
quarto_path <- function() {
  path_env <- Sys.getenv("QUARTO_PATH", unset = NA)
  if (is.na(path_env)) {
    path <- unname(Sys.which("quarto"))
    if (nzchar(path)) path else NULL
  } else {
    path_env
  }
}

path=quarto_path()
Sys.setenv(QUARTO_PATH=path)

quarto_render("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/end_products/README.qmd")
