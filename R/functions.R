
# functions.R

############################################################################
# DADOS ####################################################################
############################################################################

r_environment <- function(){ls(envir = .GlobalEnv)}


############################################################################
# CLUSTERS #################################################################
############################################################################

lemmatização <- function(base, coluna_texto, coluna_id)
{
  # EXEMPLO:
  #
  # reforma_tributaria_f <- reforma_tributaria |>
  #   lemmatização(coluna_texto = "texto", coluna_id = "V1")
  #

  df <- base %>% select(all_of(coluna_texto), all_of(coluna_id))

  df_lexique <- read.xlsx("dfs/Controle _ Geração de Rede de Territórios - Anbima.xlsx", sheet = 1)
  custom_lemmatization <- setNames(df_lexique$palavra_lemmatizada, df_lexique$palavra)
  # Leitura da planilha que contém o lexique para lemmatização personalizada

  df_stopwords <- read.xlsx("dfs/Controle _ Geração de Rede de Territórios - Anbima.xlsx", sheet = 2)
  stopwords <- unique(df_stopwords$stopwords)
  # Leitura da planilha de stopwords

  limpar_texto <- function(texto) {
    texto <- str_trim(texto)
    texto <- str_replace_all(texto, "[^\\w\\s]", "")
    texto <- str_replace_all(texto, "\\b\\d{1,2}(:\\d{1,2})?(h|min|m|s|hr|hrs|hora|horas|minuto|minutos)?\\b", "")
    texto <- tolower(texto)
    texto <- iconv(texto, to = "ASCII//TRANSLIT") # Remover acentuação
    return(texto)
  }

  df[[coluna_texto]] <- sapply(df[[coluna_texto]], limpar_texto)

  filtrar_palavras_significativas <- function(texto, stopwords) {
    palavras <- unlist(str_split(texto, "\\s+"))
    palavras_filtradas <- palavras[!palavras %in% stopwords]
    return(paste(palavras_filtradas, collapse = " "))
    # removedor de stopwords
  }

  df[[coluna_texto]] <- sapply(df[[coluna_texto]], filtrar_palavras_significativas, stopwords = stopwords)

  df <- rename(df, texto_lemmatizado = texto)
  df_f <- left_join(base, df, by = coluna_id)

  return(df_f)
}


corpus_slipt <- function(df, texto, segment_size)
{

  # df <- df[!is.na(df[texto]), ]
  # df <- df %>% dplyr::filter(texto != "nan")

  corpus <- corpus(df, text_field = texto)
  corpus_slipt <- split_segments(corpus, segment_size = segment_size)

  lista <- list(
    "corpus" = corpus,
    "corpus_slipt" = corpus_slipt
  )

  return(lista)

}


clusterização <- function(corpus_split, k)
{

  dtm <- dfm(tokens(corpus_split))
  dtm <- dfm_trim(dtm, min_docfreq = 50)
  res1 <- rainette(dtm, k = k, min_segment_size = 50, min_split_members = 100)

  cluster_assignments <- cutree(res1, k = k)

  tab <- table(cluster_assignments)

  lista <- list(
    "res1" = res1,
    "dtm" = dtm,
    "corpus_split" = corpus_split,
    "k_number" = k,
    "tab" = tab
    )

  return(lista)
}


###########################################################################
# GRAFICOS DE REDE ########################################################
###########################################################################


criar_df_lemmatizado <- function(df, texto_lemmatizado, corpus, corpus_slipt, res1, k_number){

  corpus_slipt$clusters <- cutree(res1, k = k_number)

  textos <- sapply(corpus, as.character)

  doc_texto <- data.frame(doc_id = names(textos), texto = df[[texto_lemmatizado]])

  df_ids <- df |>
    dplyr::left_join(
      doc_texto,
      by = setNames("texto", texto_lemmatizado)
    )

  df_lemmatizado <- df_ids |>
    dplyr::left_join(
      clusters_by_doc_table(corpus_slipt, clust_var = "clusters"),
      by = "doc_id"
    )

  return(df_lemmatizado)
}


listas_k <- function(df, k){

  lista = list()

  df_max <- df |>
    pivot_longer(cols = starts_with("clust")) |>
    group_by(doc_id) |>
    filter(value == max(value)) |>
    ungroup()

  for (i in seq_len(k)){

    clust_nome <- paste0("clust_", i)

    lista[[clust_nome]] <- df_max |>
    filter(name == clust_nome)

  }

  return(lista)
}


data_corpus <- function(
    list_clusters,
    texto_lemmatizado_var,
    coocTerm,
    numberOfCoocs,
    termos_remove
){

  load("src/calculateCoocStatistics")
  nova_lista = list()

  for (x in names(list_clusters)){

    print("Analisando dados...")

    df <- list_clusters[[x]]
    corpus <- corpus(df[[texto_lemmatizado_var]])

    corpus_tokens <- corpus |>
      tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
      tokens_tolower()

    sotu_collocations <- quanteda.textstats::textstat_collocations(corpus_tokens, min_count = 15)
    #sotu_collocations <- sotu_collocations[1:150, ]

    corpus_tokens2 <- tokens_compound(corpus_tokens, sotu_collocations)

    dtm <- dfm(corpus_tokens2)

    binDTM <- dtm |>
      dfm_remove(termos_remove) |>
      dfm_remove(stopwords("pt")) |>
      dfm_remove(pattern = "[[:punct:]]") |>
      dfm_trim(min_docfreq = 10, max_docfreq = 1000L) %>%
      dfm_weight(scheme = "boolean")

    #term_freq <- colSums(binDTM)

    coocs <- calculateCoocStatistics(coocTerm, binDTM, measure="LOGLIK")

    resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))

    tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
    tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
    tmpGraph[, 1] <- coocTerm
    tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
    tmpGraph[, 3] <- coocs[1:numberOfCoocs]

    resultGraph <- rbind(resultGraph, tmpGraph)

    for (i in 1:numberOfCoocs){
      newCoocTerm <- names(coocs)[i]
      coocs2 <- calculateCoocStatistics(newCoocTerm, binDTM, measure="LOGLIK")

      tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
      tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
      tmpGraph[, 1] <- newCoocTerm
      tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
      tmpGraph[, 3] <- coocs2[1:numberOfCoocs]

      resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
    }

    graphNetwork <- graph_from_data_frame(resultGraph, directed = FALSE)
    graphNetwork <- simplify(graphNetwork, remove.multiple = FALSE, remove.loops = TRUE)

    verticesToRemove <- V(graphNetwork)[degree(graphNetwork) < 2]
    graphNetwork <- delete.vertices(graphNetwork, verticesToRemove)
    # pode ser interessante as vezes deixar essas duas linhas comentadas...

    nova_lista[[x]] <- graphNetwork

  }

  print("Analise pronta...")

  return(nova_lista)
}


gerador_plot <- function(graphNetwork, coocTerm, name){


  # Configurar cores e tamanhos dos nós e arestas
  V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange')
  E(graphNetwork)$color <- "DarkGray"
  E(graphNetwork)$width <- rescale(E(graphNetwork)$sig, to = c(1, 10))
  V(graphNetwork)$size <- rescale(log(degree(graphNetwork)), to = c(5, 15))

  # Converter o gráfico igraph para um formato que o visNetwork entende
  nodes <- data.frame(
    id = V(graphNetwork)$name,
    label = V(graphNetwork)$name,
    color = V(graphNetwork)$color,
    size = V(graphNetwork)$size
  )

  edges <- data.frame(
    from = as.character(ends(graphNetwork, es = E(graphNetwork), names = TRUE)[,1]),
    to = as.character(ends(graphNetwork, es = E(graphNetwork), names = TRUE)[,2]),
    color = E(graphNetwork)$color,
    width = E(graphNetwork)$width
  )

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

