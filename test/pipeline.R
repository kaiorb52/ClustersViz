# pipeline.R

rm(list = ls())

reforma_tributaria <- data.table::fread("~/Documentos/reforma_tributaria.csv")

# path <- "~/Documentos/reforma_tributaria.rds"
# reforma_tributaria <- readRDS(path)

# path <- "~/git/dados/reforma_tributaria.csv"
# reforma_tributaria <- data.table::fread(path)

# corpus <- corpus(reforma_tributaria, text_field = "texto_lemmatizado_limpo")
# corpus_slipt <- split_segments(corpus, segment_size = 100)
#
# dtm <- dfm(tokens(corpus_slipt))
# dtm <- dfm_trim(dtm, min_docfreq = 50)
# res1 <- rainette(dtm, k = 10, min_segment_size = 50, min_split_members = 100)

# -------------------------------------------------------------------------


source("R/dependencies.R")
source("R/zzz.R")
source("R/functions.R")
source("R/create_rmd.R")

# -------------------------------------------------------------------------

reforma_tributaria <- reforma_tributaria |>
  lemmatização(
    coluna_texto = "texto",
    coluna_id    = "V1"
  )

corpus_list <- corpus_slipt(
  reforma_tributaria,
  texto         = "texto_lemmatizado_limpo",
  segment_size  = 50
)

cluster <- clusterização(
  corpus_split  = corpus_list[["corpus_slipt"]],
  k             = 5
)

p <- rainette_plot(cluster$res1, cluster$dtm, k = cluster$k_number)

# -------------------------------------------------------------------------

cluster[["corpus_split"]]$clusters  <- cutree(cluster$res1, k = cluster$k_number)

df_corpus_clusters <- data.frame(
  doc_id  = names(cluster[["corpus_split"]]),
  texto   = sapply(cluster[["corpus_split"]], as.character),
  cluster = cluster[["corpus_split"]]$clusters
)

# df <- criar_df_lemmatizado(
#   df               = reforma_tributaria,
#   texto_lemmatizado = "texto_lemmatizado_limpo",
#   corpus           = corpus_list[["corpus"]],
#   corpus_slipt     = corpus_list[["corpus_slipt"]],
#   res1             = cluster[["res1"]],
#   k_number         = cluster[["k_number"]]
# )

listas_k_data <- listas_k(
  df            = df_corpus_clusters,
  k             = cluster[["k_number"]],
  texto_var     = "texto",
  termos_remove = ""
)

data_grafs <- data_plot(
  lista_data    = listas_k_data,
  texto_var     = "texto",
  termo         = TRUE,
  numberOfCoocs = 15,
  termos_remove = c("reforma_previdencia"),
  all           = TRUE
)

# gerador_plot(
#   graphNetwork     = data_grafs[["clust_1"]]$graphNetwork,
#   coocTerm         = data_grafs[["clust_1"]]$coocTerm
# )
#
# nuvem_plot(data = listas_k_data[["clust_3"]]$word_freq)
#
# gerar_plot_png(
#   graphNetwork  = data_grafs[["clust_2"]]$graphNetwork,
#   coocTerm      = data_grafs[["clust_2"]]$coocTerm
# )

create_rmd(
  df_nrows           = nrow(reforma_tributaria),
  rainette_plot      = p,
  df_corpus_clusters = df_corpus_clusters,
  df_name            = "reforma_previdencia",
  k_data             = listas_k_data,
  k_number           = cluster[["k_number"]]
)

measure <- c("chi2", "lr", "frequency", "docprop")
show_negative <- FALSE
max_k <- max(cluster[["k_number"]], na.rm = TRUE)

groups <- rainette::cutree_rainette(cluster[["res1"]], cluster[["k_number"]])
tabs   <- rainette::rainette_stats(groups, cluster[["dtm"]], measure, 100, FALSE)


