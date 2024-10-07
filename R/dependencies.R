# # dependencies.R
#
# library(tidyr)
# library(dplyr)
# library(openxlsx)
# library(data.table)
# library(quanteda)
# library(stringr)
# library(rainette)
# library(kableExtra)
# library(shiny)
# library(shinyBS)
# library(shinyFiles)
# library(shinycssloaders)
# library(shinydashboard)
# library(shinyjs)
# library(shinyWidgets)
# library(shinythemes)
# library(miniUI)
#
#
# library(tidyverse) # ver. 2.0.0 - R (>= 3.3)
# library(quanteda) # ver. 4.0.2 - R (>= 3.5.0)
# library(igraph) # ver. 2.0.3 - R (>= 3.5.0)
# library(ggraph) # ver. 2.2.1 - R (>= 2.10)
# library(scales) # ver. 1.3.0 - R (>= 3.6)
# library(visNetwork)
#
# library(Matrix)
#
#
# library(wordcloud2)
#
# library(RColorBrewer)

# Função para verificar e instalar pacotes
verifica_instala_carrega <- function(pacotes) {
  pacotes_faltando <- pacotes[!pacotes %in% installed.packages()[,"Package"]]
  if(length(pacotes_faltando)) {
    install.packages(pacotes_faltando, dependencies = TRUE)
  }
  sapply(pacotes, require, character.only = TRUE)
}

# Lista de pacotes a serem verificados
pacotes <- c(
  "tidyr", "dplyr", "openxlsx", "data.table", "quanteda", "stringr", "rainette",
  "kableExtra", "shiny", "shinyBS", "shinyFiles", "shinycssloaders",
  "shinydashboard", "shinyjs", "shinyWidgets", "shinythemes", "miniUI",
  "tidyverse", "quanteda", "igraph", "ggraph", "scales", "visNetwork",
  "Matrix", "wordcloud2", "RColorBrewer"
)

# Verifica, instala e carrega os pacotes
verifica_instala_carrega(pacotes)
