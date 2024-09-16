# init.R
# R version 4.4.1 (2024)

# ===========================================================================
#
# Esta aplicação em shiny é uma sistematização criada para auxliar os analistas
# na produção/reprodução de dados de corpus.
#
# O Clusters Viz foi produzido principalmente no Windows 10, contudo garanto
# reprodutividade para Linux nas distros:
# . Ubuntu 24.04.1 LTS
# . Debian GNU/Linux 12 (bookworm)
#
# ===========================================================================

ClusterViz <- function() {
  shinyApp(ui = ui, server = server)
}
