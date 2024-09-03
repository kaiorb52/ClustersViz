
# zzz.R

.onLoad <- function(libname, pkgname) {

  library(shiny)
  library(shinyBS)
  library(shinythemes)

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("O pacote 'shiny' é necessário para usar o pacote ", pkgname, ".")
  }

  if (!requireNamespace("shinyBS", quietly = TRUE)) {
    stop("O pacote 'shinyBS' é necessário para usar o pacote ", pkgname, ".")
  }

    if (!requireNamespace("shinythemes", quietly = TRUE)) {
    stop("O pacote 'shinythemes' é necessário para usar o pacote ", pkgname, ".")
  }

}
