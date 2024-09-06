
# server.R

server <- function(input, output, session)
{
  ###########################################################################
  # DADOS ###################################################################
  ###########################################################################

  rv <- reactiveValues(df = NULL, file_name = NULL)

  volumes <- getVolumes()()
  shinyFileChoose(input, "browser", roots = volumes, session = session)

  observeEvent(input$import_browser, {
    req(input$browser)

    file_selected <- parseFilePaths(volumes, input$browser)
    file_path <- as.character(file_selected$datapath)

    if (length(file_path) > 0) {
      print(file_selected)

      rv$file_name <- basename(file_path)

      rv$df <- data.table::fread(file_path)
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

  observeEvent(input$lemmatizar, {
    updateSelectInput(session, "coluna_texto", choices = names(rv$df))
    updateSelectInput(session, "coluna_id", choices = names(rv$df))
  })

  observeEvent(input$run_lemmatizar, {
    df <- rv$df

    df <- df |>
      lemmatização(coluna_texto = "texto", coluna_id = "V1")

    rv$df <- df

  })

  # CLUSTERS ################################################################
  # GRAFICOS DE REDE ########################################################

}
