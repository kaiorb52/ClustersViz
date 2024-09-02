
# init.R
# R version 4.4.1 (2024-06-14)

# ===========================================================================
#
# Esta aplicação em shiny é uma sistematização criada para auxliar os analistas
# na produção/reprodução de dados de corpus.
#
# O Clusters Viz foi produzido principalmente no Windows 10, contudo garanto
# reprodutividade para Linux nas distros:
# . Ubuntu 24.04.1 LTS
#
# ===========================================================================

library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)

run <- function()
{

  inviewer <- dialogViewer(dialogName = "", width = 1200, height = 800)

  return(runGadget(app = ui, server = server, viewer = inviewer))
}
