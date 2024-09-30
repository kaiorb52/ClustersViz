# functions.R

source("R/calculateCoocStatistics.R")

############################################################################
# DADOS ####################################################################
############################################################################

r_environment <- function() {
  ls(envir = .GlobalEnv)
}


############################################################################
# CLUSTERS #################################################################
############################################################################

lemmatização <- function(base, coluna_texto, coluna_id) {
  # EXEMPLO:
  #
  # reforma_tributaria_f <- reforma_tributaria |>
  #   lemmatização(coluna_texto = "texto", coluna_id = "V1")
  #

  print("Rodando Lemmatização...")

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

  df <- rename(df, texto_lemmatizado_clusterviz = texto)
  df_f <- left_join(base, df, by = coluna_id)

  print("Lemmatização Pronta")

  return(df_f)
}


corpus_slipt <- function(df, texto, segment_size) {
  # EXEMPLO:
  #
  # corpus_list <- reforma_tributaria |>
  #   corpus_slipt(texto = "texto_lemmatizado_limpo", segment_size = 50)
  #

  corpus <- corpus(df, text_field = texto)
  corpus_slipt <- split_segments(corpus, segment_size = segment_size)

  lista <- list(
    "corpus" = corpus,
    "corpus_slipt" = corpus_slipt
  )

  return(lista)
}


clusterização <- function(corpus_split, k, min_docfreq = 50, min_split_members = 100) {
  # EXEMPLO:
  #
  # cluster_list <- clusterização(corpus_split = corpus_split, k = 16)

  dtm <- dfm(tokens(corpus_split))
  dtm <- dfm_trim(dtm, min_docfreq = min_docfreq)
  res1 <- rainette(dtm, k = k, min_segment_size = 50, min_split_members = min_split_members)

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


# criar_df_lemmatizado <- function(df, texto_lemmatizado, corpus, corpus_slipt, res1, k_number) {
#   corpus_slipt$clusters <- cutree(res1, k = k_number)
#
#   textos <- sapply(corpus, as.character)
#
#   doc_texto <- data.frame(doc_id = names(textos), texto = df[[texto_lemmatizado]])
#
#   df_ids <- df |>
#     dplyr::left_join(
#       doc_texto,
#       by = setNames("texto", texto_lemmatizado)
#     )
#
#   df_lemmatizado <- df_ids |>
#     dplyr::left_join(
#       clusters_by_doc_table(corpus_slipt, clust_var = "clusters"),
#       by = "doc_id"
#     )
#
#   return(df_lemmatizado)
# }


listas_k <- function(df, k, texto_var, termos_remove = "") {

  set.seed(99)

  lista <- list()

  # df_max <- df |>
  #   pivot_longer(cols = starts_with("clust")) |>
  #   group_by(doc_id) |>
  #   filter(value == max(value)) |>
  #   ungroup()

  k_ <- seq_len(k)

  k_ <- c(0, k_)

  for (i in k_) {
    clust_nome <- paste0("clust_", i)

    if (i == 0){
      df_cluster <- df
    }

    if (i != 0){
      df_cluster <- df |>
        filter(cluster == i)
    }

    corpus <- corpus(df_cluster[[texto_var]])

    dfm <- dfm(tokens(corpus))

    corpus_tokens <- corpus |>
      tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
      tokens_tolower()

    sotu_collocations <- quanteda.textstats::textstat_collocations(corpus_tokens, min_count = 5)

    corpus_tokens2 <- tokens_compound(corpus_tokens, sotu_collocations)

    dtm2 <- dfm(corpus_tokens2)

    binDTM <- dtm2 |>
      dfm_remove(termos_remove) |>
      dfm_remove(stopwords("pt")) |>
      dfm_remove(pattern = "[[:punct:]]") |>
      dfm_trim(min_docfreq = 10, max_docfreq = 100000L) %>%
      dfm_weight(scheme = "boolean")

    word_freq <- data.frame(
      word = featnames(binDTM),
      freq = colSums(binDTM)
    )

    word_freq |>
      arrange(desc(freq)) |>
      head(3)

    top_word <- word_freq |>
      arrange(desc(freq)) |>
      head(1) |>
      pull(word)

    sample <- sample_n(df_cluster, size = 50)

    lista[[clust_nome]]$text      <- df_cluster[[texto_var]]
    lista[[clust_nome]]$top_50_segments <- sample
    lista[[clust_nome]]$binDTM    <- binDTM
    lista[[clust_nome]]$word_freq <- word_freq
    lista[[clust_nome]]$top_word  <- top_word

  }

  return(lista)
}


data_plot <- function(
    lista_data,
    texto_var,
    termo,
    numberOfCoocs = 15,
    termos_remove,
    all = FALSE) {

  nova_lista <- list()

  for (x in names(lista_data)) {
    print("Analisando Dados....")

    binDTM <- lista_data[[x]]$binDTM

    if (termo == TRUE) {
      coocTerm <- lista_data[[x]]$top_word

    } else if (!is.null(termo) & termo != TRUE) {
      coocTerm <- termo
    }

    print(coocTerm)

    coocs <- calculateCoocStatistics(coocTerm, binDTM, measure = "LOGLIK")

    resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))

    tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
    tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
    tmpGraph[, 1] <- coocTerm
    tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
    tmpGraph[, 3] <- coocs[1:numberOfCoocs]

    resultGraph <- rbind(resultGraph, tmpGraph)

    for (i in 1:numberOfCoocs) {
      newCoocTerm <- names(coocs)[i]
      coocs2 <- calculateCoocStatistics(newCoocTerm, binDTM, measure = "LOGLIK")

      tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
      tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
      tmpGraph[, 1] <- newCoocTerm
      tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
      tmpGraph[, 3] <- coocs2[1:numberOfCoocs]

      resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
    }

    graphNetwork <- graph_from_data_frame(resultGraph, directed = FALSE)


    graphNetwork <- tryCatch({
      simplify(graphNetwork, remove.multiple = FALSE, remove.loops = TRUE)

    }, error = function(e) {
      message("Erro ao remover múltiplos e loops. Tentando simplificar sem argumentos adicionais.")
      simplify(graphNetwork)

    })

    if (all == FALSE) {
      verticesToRemove <- V(graphNetwork)[degree(graphNetwork) < 2]
      graphNetwork <- delete.vertices(graphNetwork, verticesToRemove)
    }

    nova_lista[[x]]$graphNetwork <- graphNetwork
    nova_lista[[x]]$coocTerm <- coocTerm
  }

  print("Analise pronta...")

  return(nova_lista)
}


gerador_plot <- function(graphNetwork, coocTerm, pallete = "default") {

  E(graphNetwork)$color <- "darkgrey"
  E(graphNetwork)$width <- rescale(E(graphNetwork)$sig, to = c(0.3, 15))
  V(graphNetwork)$size <- rescale(log(degree(graphNetwork)), to = c(5, 60))

  if (pallete == "default") {
    V(graphNetwork)$color <- ifelse(tolower(V(graphNetwork)$name) == tolower(coocTerm), "cornflowerblue", "orange")
  }


  if (pallete == "rep_dem") {
    V(graphNetwork)$color <- ifelse(tolower(V(graphNetwork)$name) == tolower(coocTerm), "steelblue", "tomato")

    vizinhos_central <- neighbors(graphNetwork, V(graphNetwork)[name == coocTerm])
    V(graphNetwork)$color[vizinhos_central] <- "skyblue"

    V(graphNetwork)$color <- ifelse(V(graphNetwork)$size <= 15, "red", V(graphNetwork)$color)
  }

  if (pallete == "YlGn") {
    V(graphNetwork)$color <- ifelse(tolower(V(graphNetwork)$name) == tolower(coocTerm), "#006837", "#addd8e")

    vizinhos_central <- neighbors(graphNetwork, V(graphNetwork)[name == coocTerm])
    V(graphNetwork)$color[vizinhos_central] <- "#41ab5d"

    V(graphNetwork)$color <- ifelse(V(graphNetwork)$size <= 15, "#f7fcb9", V(graphNetwork)$color)
  }

  if (pallete == "RdYlGn") {
    V(graphNetwork)$color <- ifelse(tolower(V(graphNetwork)$name) == tolower(coocTerm), "#1a9850", "#f46d43")

    vizinhos_central <- neighbors(graphNetwork, V(graphNetwork)[name == coocTerm])
    V(graphNetwork)$color[vizinhos_central] <- "#66bd63"

    V(graphNetwork)$color <- ifelse(V(graphNetwork)$size <= 15, "#d73027", V(graphNetwork)$color)
  }

  if (pallete == "YlGnBu") {
    V(graphNetwork)$color <- ifelse(tolower(V(graphNetwork)$name) == tolower(coocTerm), "#253494", "#7fcdbb")

    vizinhos_central <- neighbors(graphNetwork, V(graphNetwork)[name == coocTerm])
    V(graphNetwork)$color[vizinhos_central] <- "#1d91c0"

    V(graphNetwork)$color <- ifelse(V(graphNetwork)$size <= 15, "#edf8b1", V(graphNetwork)$color)
  }


  # Converter o gráfico igraph para um formato que o visNetwork entende
  nodes <- data.frame(
    id = V(graphNetwork)$name,
    label = V(graphNetwork)$name,
    color = V(graphNetwork)$color,
    size = V(graphNetwork)$size
  )

  edges <- data.frame(
    from = as.character(ends(graphNetwork, es = E(graphNetwork), names = TRUE)[, 1]),
    to = as.character(ends(graphNetwork, es = E(graphNetwork), names = TRUE)[, 2]),
    color = E(graphNetwork)$color,
    width = E(graphNetwork)$width
  )

  # Gerar o plot interativo com visNetwork
  visNetwork(nodes, edges) %>%
    visLayout(randomSeed = 100) %>%
    visNodes(
      shape = "dot",
      font = list(size = 25, face = "Arial", color = "black"), # Aumentar o tamanho da fonte
      scaling = list(label = list(enabled = TRUE))             # Escalar os rótulos proporcionalmente ao tamanho dos nós
    ) %>%
    visEdges(smooth = FALSE) %>%
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = list(enabled = TRUE)
    ) %>%
    visInteraction(navigationButtons = TRUE) %>%
    # Configurar física para espaçamento dos nós
    visPhysics(
      solver = "forceAtlas2Based",    # Algoritmo que melhora o espaçamento dos nós
      forceAtlas2Based = list(
        gravitationalConstant = -200, # Aumentar valor negativo para espaçar mais
        centralGravity = 0.01,        # Controlar a gravidade central
        springLength = 200,           # Aumentar o comprimento das "molas" entre nós
        springConstant = 0.08
      ),                              # Controlar a rigidez das "molas"
      stabilization = FALSE,
      repulsion = list(
        nodeDistance = 300,            # Distância mínima entre os nós
        centralGravity = 0.01,
        springLength = 100,
        damping = 0.09
      )
    )
}

nuvem_plot <- function(data, pallete = "default"){

  if (pallete == "rep_dem") {
    colorVec = rep(c('red', 'tomato', 'skyblue', 'steelblue'), length.out = nrow(demoFreq))
  }

  if (pallete == "YlGn") {
    colorVec = rep(c('#addd8e', '#78c679', '#238443', '#004529'), length.out = nrow(demoFreq))
  }

  if (pallete == "RdYlGn") {
    colorVec = rep(c('#d73027', '#fdae61', '#a6d96a', '#1a9850'), length.out = nrow(demoFreq))
  }

  if (pallete == "YlGnBu") {
    colorVec = rep(c('#c7e9b4', '#41b6c4', '#225ea8', '#081d58'), length.out = nrow(demoFreq))
  }

  if (pallete == "default") {
    colorVec <- "random-light"
  }

  data_desc <- data |>
    arrange(desc(freq))

  set.seed(99)
  wordcloud2(
    data_desc,
    color = colorVec,
    shuffle = FALSE,
    size = 0.5,
    rotateRatio = 0,
    shape = "circle",
  )
}

palletas <- c("default", "rep_dem", "YlGn", "RdYlGn", "YlGnBu")
