# create_rmd.R

library(glue)


# if !("ClusterViz_files" %in% dir()){
#
# }

create_rmd <- function(rainette_plot, df_name, k_data) {

  print("Gerando Relatório...")

  k_number <- length(names(listas_k_data))

  ggsave(filename = "ClusterViz_files/plot.png")

  rmd_intro_base <- paste0("---
title: \"Relatório Automático\"
author: \"Seu Nome\"
date: \"`r Sys.Date()`\"
output: html_document
---

## Introdução

Para a execução dessa análise, são empregados os pacotes Raineette e Quanteda, utilizando do metodo de clusterização hieraquia, na analise de corpus afim de criar agrupamentos. No total, foram gerados ", k_number, " clusters, a partir do data.frame ", df_name, ".\n")

  plot_png <- "\n![Clusters Rainetteplot](plot.png)\n"

  rmd_intro_content <- ""

  rmd_results_head <- "\n## Resultados\n"
  rmd_results <- ""

  for (k_name in names(k_data)) {

    #top_word <- listas_k_data[[k_name]]$top_word
    tab_rmd  <- listas_k_data[[k_name]]$word_freq

    tab_rmd2 <- tab_rmd |>
      arrange(-freq) |>
      mutate(index = row_number()) |>
      filter(index <= 10)

    top3 <- tab_rmd2 |>
      filter(index <= 3) |>
      pull(word)

    top3 <- paste(top3, collapse = ", ")

    rmd_intro_content <- paste0(rmd_intro_content,
      glue(
      "O {k_name} os principais termos foram: {top3}. "
      )
    )
    rmd_cluster_base <- glue(
      "

      ### {k_name}\n


      "
    )

    cluster_tab_ <- paste0(
      "| Palavra | Frequência |\n",
      "|---------|------------|\n"
    )

    for (i in seq_len(10)) {

      row <- tab_rmd2 |>
        filter(index == i)

      row_content <- glue("| {row$word} | {row$freq} |")
      cluster_tab_ <- paste0(cluster_tab_, row_content, "\n")
    }

    rmd_results <- paste0(rmd_results, rmd_cluster_base, cluster_tab_)

  }

  doc_name <- paste(k_number, "relatorio.rmd")
  rmd_content <- paste0(rmd_intro_base, plot_png, rmd_intro_content, rmd_results_head, rmd_results)

  writeLines(rmd_content, doc_name)
  rmarkdown::render(doc_name, output_format = "html_document")
  rmarkdown::render(doc_name, output_format = "pdf_document")

  print("Relatório Gerado")

}

