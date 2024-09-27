# create_rmd.R

library(glue)

create_rmd <- function(k_data) {

  print("Gerando Relatório...")

  rmd_base <- "---
title: \"Relatório Automático\"
author: \"Seu Nome\"
date: \"`r Sys.Date()`\"
output: html_document
---

## Introdução

Este é um relatório gerado automaticamente.

## Resultados\n"

  rmd_results <- ""

  for (k_name in names(k_data)) {

    top_word <- listas_k_data[[k_name]]$top_word
    tab_rmd  <- listas_k_data[[k_name]]$word_freq

    tab_rmd2 <- tab_rmd |>
      arrange(-freq) |>
      mutate(index = row_number()) |>
      filter(index <= 10)

    rmd_cluster_base <- glue(
      "

      ### {k_name}\n
      O termo mais comum no cluster foi: {top_word}


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

  k_number <- length(names(listas_k_data))
  doc_name <- paste(k_number, "relatorio.rmd")

  rmd_content <- paste0(rmd_base, rmd_results)

  writeLines(rmd_content, doc_name)
  rmarkdown::render(doc_name, output_format = "html_document")
  rmarkdown::render(doc_name, output_format = "pdf_document")

  print("Relatório Gerado")

}

create_rmd(k_data = listas_k_data)

