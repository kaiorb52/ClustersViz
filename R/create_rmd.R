# create_rmd.R

library(glue)


# if !("ClusterViz_files" %in% dir()){
#
# }

create_rmd <- function(df_nrows, df_corpus_clusters, min_docfreq = 50, min_segment_size = 50, min_split_members = 100, rainette_plot, df_name, k_data, k_number) {

  print("Gerando Relatório...")

  if (!"ClusterViz_files" %in% dir()){
    dir.create("ClusterViz_files")

  }


  corpus_split_nrows   = nrow(df_corpus_clusters)
  corpus_split_percent = nrow(df_corpus_clusters |> filter(!is.na(cluster))) / nrow(df_corpus_clusters)
  corpus_split_percent = round(corpus_split_percent * 100, digits = 2)

  tab <- table(df_corpus_clusters$cluster)
  tab <- as.data.frame(tab) |>
    mutate(
      percentuais = paste0(round((Freq/sum(Freq)) * 100, digits = 2), "%")
    )


  ggsave(plot = rainette_plot, filename = "ClusterViz_files/rainette_plot.png", height = 7, width = 12)

  rmd_head <- "\n---
title: \"Relatório Automático\"
author: \"Seu Nome\"
date: \"`r Sys.Date()`\"
output: html_document
---\n"

  rmd_intro <- paste0("\n# Introdução\nForam analisados ", df_nrows, " textos que foram transformados em ", corpus_split_nrows, " segmentos de textos com o critério de . Foram identificados ", k_number, " clusters a partir do seguinte dendograma:\n")

  dendograma_png <- "\n![](rainette_plot.png)\n"

  rmd_intro_results <- paste0("\nO dendograma acima, é fruto de uma análise hierarquica descendente utilizando a metodologia de Reinert. A quantidade mínima de documentos é de ", min_docfreq,", a quantidade mínima de segmentos por cluster é de ", min_segment_size, " e a quantidade minima para a divização dos segmentos e formação de um novo cluster é ", min_split_members, ". Ao final estes clusters representam ", corpus_split_percent, "%", " segmentos dos ", corpus_split_nrows, " segmentos encontrados na seguinte distribuição.\n")

  rmd_tab <- paste0("\n|  | Frequência | Percentuais |\n", "|---------|------------|------------|\n")

  rmd_tab1 <- rmd_tab

  for (i in seq_len(k_number)){
    rmd_tab_content <- glue("| {tab[i, 1]} | {tab[i, 2]} | {tab[i, 3]} |")

    rmd_tab1 <- paste0(rmd_tab1, rmd_tab_content, "\n")
  }

  rmd_intro_f <- "\nNas próximas seções, o descritivo de cada cluster. \n"

  rmd_clusters_results <- ""

  for (i in seq_len(k_number)){

    rmd_cluster_intro <- glue("## Cluster {i}")

    clust_name <- paste0("clust_", i)

    top3 <- k_data[[clust_name]]$word_freq |>
      arrange(-freq) |>
      head(3) |>
      pull(word) |>
      paste(collapse = ", ")

    top_10 <- k_data[[clust_name]]$word_freq |>
      arrange(-freq) |>
      head(10)

    rmd_cluster_tab <- paste0("\n|  | Frequência |\n", "|---------|------------|\n")

    for (a in seq_len(nrow(top_10))){

      rmd_cluster_tab_content <- glue("| {top_10[a, 1]} | {top_10[a, 2]} |")
      rmd_cluster_tab <- paste0(rmd_cluster_tab, rmd_cluster_tab_content, "\n")

    }

    rmd_cluster_terms <- glue("O cluster a seguir, tem os seguintes principais termos: {top3}. Que pode ser representado nesta nuvem de palavras.


    ")

    rmd_cluster_network_text <- "Ao se analisar o cluster e sua rede de co-ocorrencia centralizando o termo YYYYY, encontra-se esta rede de correlações dos termos


    "

    rmd_cluster_segment_text <- "Os principais 50 segmentos de texto do cluster é:


    "

    top_50_segements <- k_data[[clust_name]]$top_50_segments |>
      pull(texto) |>
      paste0(collapse = "|###|###|")

    rmd_clusters_results <- paste0(rmd_clusters_results, "\n", rmd_cluster_intro, "\n", rmd_cluster_tab, "\n", rmd_cluster_terms, "\n", rmd_cluster_network_text,"\n", rmd_cluster_segment_text, "\n", top_50_segements, "\n")

  }

  doc_name <- glue("ClusterViz_files/{k_number}-relatorio.rmd")
  rmd_content <- paste(rmd_head, rmd_intro, "\n", dendograma_png, "\n", rmd_intro_results, "\n", rmd_tab1, "\n", rmd_intro_f, "\n", rmd_clusters_results)

  writeLines(rmd_content, doc_name)
  rmarkdown::render(doc_name, output_format = "html_document")
  rmarkdown::render(doc_name, output_format = "pdf_document")


  print("Relatório Gerado")

}

library(igraph)
library(ggraph)
library(ggplot2)

gerador_plot <- function(graphNetwork, coocTerm){

  name <- tolower(coocTerm)

  # Configurar cores e tamanhos dos nós e arestas
  V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange')
  E(graphNetwork)$color <- "DarkGray"
  E(graphNetwork)$width <- rescale(E(graphNetwork)$sig, to = c(1, 10))
  V(graphNetwork)$size <- rescale(log(degree(graphNetwork)), to = c(5, 15))

  # Converter o gráfico igraph para um formato que o visNetwork entende
  nodes <- data.frame(id = V(graphNetwork)$name,
                      label = V(graphNetwork)$name,
                      color = V(graphNetwork)$color,
                      size = V(graphNetwork)$size)

  edges <- data.frame(from = as.character(ends(graphNetwork, es = E(graphNetwork), names = TRUE)[,1]),
                      to = as.character(ends(graphNetwork, es = E(graphNetwork), names = TRUE)[,2]),
                      color = E(graphNetwork)$color,
                      width = E(graphNetwork)$width)

  # Gerar o plot interativo com visNetwork
  visNetwork(nodes, edges) %>%
    visLayout(randomSeed = 100) %>%
    visNodes(shape = "dot", font = list(size = 12)) %>%
    visEdges(smooth = FALSE) %>%
    visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
               nodesIdSelection = list(enabled = TRUE)) %>%
    visInteraction(navigationButtons = TRUE) %>%
    visPhysics(stabilization = FALSE) #%>%
  #visTitle(paste(name, "Graph"))
}
