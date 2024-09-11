
# ui.R

ui <- fluidPage(
    theme = shinytheme("sandstone"),
    useShinyjs(),

    tags$head(
      tags$style(HTML("
      .expanded-panel {
        width: 100% !important;  /* Expande o mainPanel */
      }
    "))
    ),

  navbarPage(title = "CLUSTERS VIZ",position = "static-top",
  ###########################################################################
  # DADOS ###################################################################
  ###########################################################################

    tabPanel(title = "Dados", icon = icon("database", "fa-2x"),
      actionButton("import_data",  "Importação de dados", icon = icon("download")),

      bsModal(
        id = "import_dataset",
        title = "Importação de dados",
        trigger = "import_data",
        size = "large",

        tabsetPanel(type = "pills",

          tabPanel("Datasets Salvos",
            br(),
            selectInput(
              label = "Importar dataset usado anteriormente",
              inputId = "import_saved_df",
              choices = c()
            ),
            actionButton(inputId = "refresh_saved_df", label = "Refresh", icon = icon("sync-alt"))
          ),

          tabPanel("Arquivos",
            br(),

            shinyFilesButton("browser", "Escolha o Arquivo", "Selecione", multiple = FALSE),
            actionButton(inputId = "import_browser", label = "Importar dados")
          ),

          tabPanel("Ambiente R",
            br(),
            selectInput(
              label = "Data.frame aberto no ambiente R",
              inputId = "r_envi_df",
              choices = r_environment()
            ),
          actionButton(inputId = "import_r_envi", label = "Importar dados"),
          actionButton(inputId = "refresh_r_envi", label = "Refresh", icon = icon("sync-alt"))
          )
        )
      ),
      hr(),
      mainPanel(
        htmlOutput("df_summary"),
      ),

      conditionalPanel(
        condition = "output.df_exists == true", # Condição para mostrar os botões
        actionButton("view_data", "Visualizar dados", icon = icon("download")),
        bsModal(id = "view_data_bs",
                trigger = "view_data",
                title = "Visualizar dados",
                size = "large",
                DT::dataTableOutput("view_table")
        ),
        hr(),
        actionButton("lemmatizar", "Lemmatização"),
        bsModal(
          id = "lemmatização_page",
          title = "Lemmatização de dados",
          trigger = "lemmatizar",
          size = "large",

          tabsetPanel(type = "pills",
            tabPanel("Seleção",
              selectInput(
                label = "Coluna com ids",
                inputId = "coluna_id",
                choices = c()
              ),
              selectInput(
                label = "Coluna com texto",
                inputId = "coluna_texto",
                choices = c()
              ),
              actionButton("run_lemmatizar", "Rodar Lematização no data.frame carregado")
            )
          )
        ),
        hr(),
        actionButton("corpus_split", "Corpus split"),
        bsModal(
          id = "corpus_spilt_page",
          title = "Corpus split",
          trigger = "corpus_split",
          size = "large",
          tabsetPanel(type = "pills",
            tabPanel("Seleção",
              selectInput(
                label = "Coluna com o texto",
                inputId = "texto",
                choices = c()
              ),
              numericInput("segment_size", label = "segment_size", value = 40),
              actionButton("run_corpus_split", "Split")
            )
          )
        )
      )
    ),
  ###########################################################################
  # CLUSTERS ################################################################
  ###########################################################################

  # tags$head(
  #   tags$style(
  #     HTML("
  #     #mainPanelExpanded {
  #       width: 100% !important;
  #     }
  #     #mainPanelNormal {
  #       width: 70% !important;
  #     }
  #   ")
  #   )
  # ),

    tabPanel(title = "Clusters", icon = icon("chart-bar", "fa-2x"),

      conditionalPanel(
        condition = "output.corpus_slipt_exist == true",

        sidebarLayout(
          sidebarPanel(
            id = "sidebar",
            conditionalPanel(
              condition = "input.cluster_tabs == 'Manipulação dos Clusters'",
              shiny::numericInput(
                inputId = "k",
                label = "Numero de clusters",
                value = 10,
                min = 2,
                max = 16
              ),
              actionButton("run_cluster_graph", "Gerar Clusters"),
            ),
            conditionalPanel(
              condition = "input.cluster_tabs == 'Exploração dos Clusters'",
              sliderInput(
                label = "Clusters selecionados",
                inputId = "k_selected",
                value = 2,
                min = 2,
                max = 16,
              ),
            ),
          ),

          mainPanel(
            id = "main_panel",
            class = "mainPanelNormal",
            tabsetPanel(
              id = "cluster_tabs",
              tabPanel("Manipulação dos Clusters",
                htmlOutput("cluster_tab")
              ),
              tabPanel("Exploração dos Clusters",
                plotOutput("rainette_plot")
              ),
              tabPanel("Documentos dos Clusters",
                uiOutput("dynamic_docs_ui")
              ),
            )
          )
        )
      ),
    ),

  ###########################################################################
  # GRAFICOS DE REDE ########################################################
  ###########################################################################

    tabPanel(title = "Graficos de Rede", icon = icon("project-diagram", "fa-2x"),
      conditionalPanel(
        condition = "output.df_exists == true", # Condição para mostrar os botões
        sidebarLayout(
          sidebarPanel(
            actionButton("run_network_graph", "Gerar Graficos de rede"),
            selectInput(
              "choice_df2", "Selecione o cluster que deseja visualizar",
              choices = c()
            )
          ),
          mainPanel(

          )
        )
      )
    )
  )
)
