# plots.R

#######################################################################
## Graficos de rede                                                  ##
#######################################################################

# HTML =========================================================================

gerador_plot <- function(graphNetwork, coocTerm, pallete = "default") {

  set.seed(99)

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

## PNG =========================================================================

gerar_plot_png <- function(graphNetwork, coocTerm){

  set.seed(99)

  name <- coocTerm

  V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange')
  E(graphNetwork)$color <- adjustcolor("DarkGray", alpha.f = .5)
  E(graphNetwork)$width <- rescale(E(graphNetwork)$sig, to = c(0.5, 5))
  E(graphNetwork)$curved <- 0.15
  V(graphNetwork)$size <- rescale(log(degree(graphNetwork)), to = c(1, 15))
  par(mai = c(0, 0, 0, 0))

  # Calcular a distância geodésica entre o termo central e os outros nós
  dist_matrix <- distances(graphNetwork, v = V(graphNetwork)[name], to = V(graphNetwork))

  # Extrair a primeira linha da matriz de distâncias, que corresponde às distâncias do nó central para os demais
  dist_to_central <- dist_matrix[1, ]

  # Ajustar o tamanho dos rótulos com base na distância (quanto mais distante, menor o rótulo)
  V(graphNetwork)$label_cex <- rescale(1 / (1 + dist_to_central), to = c(0.25, 1))  # Tamanhos escalonados

  par(mai = c(0, 0, 0, 0))
  plot <- plot(
    graphNetwork,
    layout = layout.fruchterman.reingold, # Force Directed Layout
    #main = paste(name, ' Graph'),
    vertex.label.family = "sans",
    vertex.label.cex = V(graphNetwork)$label_cex,         # Ajuste o tamanho dos rótulos para torná-los menores
    vertex.shape = "circle",
    vertex.label.dist = 0.0,          # Labels dos nós ligeiramente movidos
    vertex.frame.color = adjustcolor("darkgray", alpha.f = .5),
    vertex.label.color = 'black',     # Cor dos nomes dos nós
    vertex.label.font = 2,            # Fonte dos nomes dos nós
    vertex.label = V(graphNetwork)$name,      # Nomes dos nós
  )

  return(plot)
}

#######################################################################
## Graficos de nuvem                                                 ##
#######################################################################

## HTML ========================================================================

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
    colorVec <- rep(c('black', 'grey5', 'grey10'), length.out = nrow(demoFreq))
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

#
# words <- listas_k_data$clust_1$word_freq$word
# frequencies <- listas_k_data$clust_1$word_freq$freq
#
# wordcloud(words = words, freq = frequencies,
#           min.freq = 5,
#           max.words = 100000,
#           random.order = FALSE,
#           colors = brewer.pal(8, "Dark2")
# )

