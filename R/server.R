# server.R

server <- function(input, output, session) {
  ###########################################################################
  # DADOS ###################################################################
  ###########################################################################

  rv <- reactiveValues(
    df               = NULL,
    file_name        = NULL,
    corpus           = NULL,
    corpus_slipt     = NULL,
    texto            = NULL,
    N                = NULL,
    data_plots       = NULL,
    listas_k_data    = NULL,
    data_plots       = NULL,
    dados_plot_nuvem = NULL
  )

  volumes <- getVolumes()()
  shinyFileChoose(input, "browser", roots = volumes, session = session)

  observeEvent(input$import_browser, {
    req(input$browser)

    file_selected <- parseFilePaths(volumes, input$browser)
    file_path <- as.character(file_selected$datapath)

    if (length(file_path) > 0) {
      print(file_selected)

      rv$file_name <- basename(file_path)

      if (grepl(".csv", file_path)) {
        rv$df <- data.table::fread(file_path, encoding = "Latin-1")
        shiny::showNotification("Dados carregados", type = "message")
      }

      if (grepl(".xlsx", file_path)) {
        rv$df <- openxlsx::read.xlsx(file_path)
        shiny::showNotification("Dados carregados", type = "message")
      }

      if (grepl(".rds", file_path)) {
        rv$df <- readRDS(file_path)
        shiny::showNotification("Dados carregados", type = "message")
      }

      if (!grepl(".(csv|xlsx|rds)", file_path)) {
        message("Tipo de arquivo não suportado...")
        shiny::showNotification("Tipo de arquivo não suportado...", type = "error")
      }
    }
  })

  observeEvent(input$import_r_envi, {
    file_selected <- input$r_envi_df

    if (!is.null(file_selected)) {
      print(file_selected)

      rv$file_name <- file_selected

      rv$df <- get(file_selected, envir = globalenv())
    }
  })

  observeEvent(input$import_googlesheets, {
    url <- input$url_googlesheets

    sheet_info <- googledrive::drive_get(url)
    rv$file_name <- sheet_info$name

    rv$df <- googlesheets4::read_sheet(url)
  })


  observeEvent(input$refresh_r_envi, {
    updateSelectInput(session, "r_envi_df", choices = r_environment())
  })

  output$df_summary <- renderText({
    req(rv$df)

    n_rows <- nrow(rv$df)
    n_cols <- ncol(rv$df)
    col_names <- colnames(rv$df)

    summary_df <- data.frame(
      "Descrição" = c("Nome do data.frame", "Número de Linhas", "Número de Colunas", "Nomes das Variáveis"),
      "_" = c(rv$file_name, n_rows, n_cols, paste(col_names, collapse = ", "))
    )

    kable(summary_df, format = "html") %>%
      kable_styling("striped", full_width = F)
  })

  output$view_table <- DT::renderDataTable({
    req(rv$df)
    rv$df
  })

  output$df_exists <- reactive({
    return(!is.null(rv$df))
  })

  outputOptions(output, "df_exists", suspendWhenHidden = FALSE)

  observeEvent(input$lemmatizar_corpus_split, {
    updateSelectInput(session, "coluna_texto", choices = names(rv$df))
    updateSelectInput(session, "coluna_id", choices = names(rv$df))

    updateSelectInput(session, "texto", choices = names(rv$df))
  })

  observeEvent(input$run_lemmatizar, {
    df <- rv$df

    # W.I.P - resvisar tudo dps

    id    <- as.character(input$coluna_id)
    texto <- as.character(input$coluna_texto)

    shiny::showNotification("Rodando Lemmatização...", type = "message")

    df <- df |>
      lemmatização(coluna_texto = texto, coluna_id = id)

    shiny::showNotification("Lemmatização Pronta", type = "message")

    rv$df <- df
  })

  observeEvent(input$run_corpus_split, {
    print("Rodando corpus split...")
    shiny::showNotification("Rodando corpus split...", type = "message")

    # Adicione mais informações de depuração
    print(paste("Texto:", input$texto))
    print(paste("Segment Size:", input$segment_size))

    corpus_slipt_result <- corpus_slipt(
      df            = rv$df,
      texto         = input$texto,
      segment_size  = input$segment_size
    )

    print("Corpus split finalizado")
    shiny::showNotification("Corpus split finalizado", type = "message")

    rv$corpus <- corpus_slipt_result[["corpus"]]
    rv$corpus_slipt <- corpus_slipt_result[["corpus_slipt"]]
    rv$texto <- input$texto
  })

  output$corpus_slipt_exist <- reactive({
    return(!is.null(rv$corpus_slipt))
  })

  outputOptions(output, "corpus_slipt_exist", suspendWhenHidden = FALSE)

  ###########################################################################
  # CLUSTERS ################################################################
  ###########################################################################

  graphInput <- eventReactive(input$run_cluster_graph, {

    tryCatch({
      shiny::showNotification("Gerando Clusters...", type = "message")
      print("Gerando Clusters...")

      corpus_slipt <- rv$corpus_slipt

      lista_cluster <- clusterização(
          corpus_slipt,
          k                 = input$k,
          min_docfreq       = input$min_docfreq,
          #min_segment_size  = input$min_segment_size,
          min_split_members = input$min_split_members
        )

      shiny::showNotification("Clusters gerados", type = "message")
      print("Clusters gerados")

      lista_cluster
    }, error = function(e) {
      message("Ocorreu um erro ao gerar o gráfico: ", e$message)

      shiny::showNotification(paste("Erro ao gerar o gráfico:", e$message), type = "error")

      NULL
    })

  })

  output$cluster_tab <- renderText({
    req(graphInput())

    tab_df <- as.data.frame(graphInput()$tab)

    tab_df$percentual <- (tab_df$Freq / sum(tab_df$Freq)) * 100

    tab_df$percentual <- paste0(round(tab_df$percentual, digits = 2), " %")

    kable(tab_df, format = "html") %>%
      kable_styling("striped", full_width = F)
  })

  observeEvent(graphInput(), {
    req(graphInput())

    updateSliderInput(session, "k_selected", max = graphInput()$k_number)
    updateSelectInput(session, "selected_doc",
      choices = seq_len(graphInput()$k_number)
    )
  })

  output$rainette_plot <- renderPlot({
    req(graphInput())

    lista_cluster <- graphInput()
    rainette_plot(lista_cluster$res1, lista_cluster$dtm, k = input$k_selected)
  })

  output$dynamic_docs_ui <- renderUI({
    req(graphInput())
    docs_sample_ui("rainette1", graphInput()$res1)
  })

  docs_sample_server("rainette1", graphInput()$res1, rv$corpus_slipt, graphInput()$k_number)

  observeEvent(input$get_r_code, {
    codigo <- paste0(
      "
      corpus <- corpus(df, text_field = '", rv$texto, "')
      corpus_split <- split_segments(corpus, segment_size = 40)

      dtm <- dfm(tokens(corpus_split))
      dtm <- dfm_trim(dtm, min_docfreq = 50)
      res1 <- rainette(dtm, k = ", graphInput()$k_number, ", min_segment_size = 50, min_split_members = 100)"
    )
    showModal(
      modalDialog(
        title = gettext("Codigo R"), size = "l",
        HTML(paste0(
          "Codigo em R da sistematização da clusterização :",
          "<pre><code>",
          paste(highr::hi_html(codigo), collapse = "\n"),
          "</code></pre>"
        )),
        easyClose = TRUE
      )
    )
  })

  observe({
    selected_tab <- input$cluster_tabs

    if (selected_tab == "Documentos dos Clusters") {
      shinyjs::hide(id = "sidebar")
      shinyjs::addClass(selector = "#main_panel", class = "expanded-panel")  # Expanda o mainPanel
    } else {
      shinyjs::show(id = "sidebar")
      shinyjs::removeClass(selector = "#main_panel", class = "expanded-panel")  # Restaure o tamanho original
    }
  })

  output$cluster_exist <- reactive({
    return(!is.null(graphInput()))
  })

  outputOptions(output, "cluster_exist", suspendWhenHidden = FALSE)

  ###########################################################################
  # GRAFICOS DE REDE ########################################################
  ###########################################################################

  observeEvent(input$run_network_graph, {
    req(graphInput())

    shiny::showNotification("Gerando graficos...", type = "message")
    print("Gerando graficos...")

    # df_lemmatizado <- criar_df_lemmatizado(
    #   df                = rv$df,
    #   texto_lemmatizado = rv$texto,
    #   corpus            = rv$corpus,
    #   corpus_slipt      = rv$corpus_slipt,
    #   res1              = graphInput()$res1,
    #   k_number          = graphInput()$k_number
    # )
    #

    clusters  <- cutree(graphInput()$res1, k = graphInput()$k_number)

    df <- data.frame(
      doc_id  = names(graphInput()$corpus_split),
      texto   = sapply(graphInput()$corpus_split, as.character),
      cluster = clusters
    )

    listas_k_data <- listas_k(
      df                = df,
      k                 = graphInput()$k_number,
      texto             = "texto",
    )

    rv$N <- seq_len(length(listas_k_data))
    termo <- tolower(input$termo)

    if (termo %in% c("true", "", " ", "  ")){
      termo <- TRUE
    } else {
      termo <- input$termo
    }

    termos_remove <- strsplit(input$termos_remove, ",\\s*")[[1]]

    rv$data_plots <- data_plot(
      lista_data             = listas_k_data,
      texto_var              = rv$texto,
      termo                  = termo,
      numberOfCoocs          = input$numberOfCoocs,
      termos_remove          = termos_remove,
      all                    = TRUE
    )

    rv$dados_plot_nuvem <- listas_k_data

    print("Graficos prontos")
    shiny::showNotification("Graficos prontos", type = "message")
  })

  observeEvent(rv$data_plots, {
    req(rv$data_plots)

    updateSelectInput(session, "pallete",
      choices = palletas
    )
    updateSelectInput(session, "selected_cluster",
      choices = c("clust_0", paste0("clust_", rv$N))
    )
  })

  output$networkPlot <- renderVisNetwork({
    req(rv$data_plots)
    req(input$selected_cluster)

    if (input$graph_tabs == "Grafico de Rede"){
      data_grafs <- rv$data_plots
      selected_cluster <- input$selected_cluster

      gerador_plot(
        graphNetwork            = data_grafs[[selected_cluster]]$graphNetwork,
        coocTerm                = data_grafs[[selected_cluster]]$coocTerm,
        pallete                 = input$pallete
      )

    }

  })

  output$wordcloudPlot <- renderWordcloud2({
    req(rv$data_plots)
    req(input$selected_cluster)

    if (input$graph_tabs == "Grafico de Nuvem"){
      wordcloud_data <- rv$dados_plot_nuvem[[input$selected_cluster]]$word_freq

      nuvem_plot(data = wordcloud_data, pallete = input$pallete)

    }
  })

  output$wordtreePlot <- renderUI({
    req(rv$data_plots)
    req(input$selected_cluster)


    if (input$graph_tabs == "Grafico de Arvore de palavras"){

      wordtree_data <- rv$dados_plot_nuvem[[input$selected_cluster]]$text

      html_tree <- wordtree(
        wordtree_data,
        targetWord   = tolower(input$termo),
        direction    = "suffix",
        Number_words = 5
      )

      HTML(html_tree)

    }

  })

}
