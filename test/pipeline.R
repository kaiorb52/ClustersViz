# pipeline.R

rm(list = ls())

path <- "~/Documentos/reforma_tributaria.rds"

reforma_tributaria <- readRDS(path)


# -------------------------------------------------------------------------


source("R/dependencies.R")
source("R/zzz.R")
source("R/functions.R")

# -------------------------------------------------------------------------

# reforma_tributaria <- reforma_tributaria |>
#   lemmatização(
#     coluna_texto      = "texto",
#     coluna_id         = "V1"
#   )

corpus_list <- corpus_slipt(
  reforma_tributaria,
  texto                   = "texto_lemmatizado_limpo",
  segment_size            = 50
)

cluster <- clusterização(
  corpus_split            = corpus_list[["corpus_slipt"]],
  k                       = 16
)

cluster[["corpus_split"]]


# -------------------------------------------------------------------------


df <- criar_df_lemmatizado(
  df               = reforma_tributaria,
  texto_lemmatizado = "texto_lemmatizado_limpo",
  corpus           = corpus_list[["corpus"]],
  corpus_slipt     = corpus_list[["corpus_slipt"]],
  res1             = cluster[["res1"]],
  k_number         = cluster[["k_number"]]
)

listas_k_data <- listas_k(
  df               = df,
  k                = cluster[["k_number"]],
  texto_var        = "texto_lemmatizado_limpo",
  termos_remove    = ""
)

data_grafs <- data_plot(
  lista_data       = listas_k_data,
  texto_var        = "texto_lemmatizado_limpo",
  termo            = TRUE,
  numberOfCoocs    = 20,
  termos_remove    = c("reforma_previdencia"),
  all              = TRUE
)

gerador_plot(
  graphNetwork     = data_grafs[["clust_3"]]$graphNetwork,
  coocTerm         = data_grafs[["clust_3"]]$coocTerm
)

nuvem_plot(data = listas_k_data[["clust_3"]]$word_freq)
