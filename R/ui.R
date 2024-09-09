
# ui.R

ui <- fluidPage(
    theme = shinytheme("sandstone"),
    useShinyjs(),
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
    tabPanel(title = "Clusters", icon = icon("chart-bar", "fa-2x"),
      conditionalPanel(
        condition = "output.corpus_slipt_exist == true", # Condição para mostrar os botões
        #sidebarLayout(
          sidebarPanel(
            id = "sidebar",
            shiny::numericInput(
              inputId = "k",
              label = "Numero de clusters",
              value = 10,
              min = 2,
              max = 16
            ),
            actionButton("run_cluster_graph", "Gerar Grafico de Clusters"),
            hr(),
            sliderInput(
              label = "Clusters selecionados",
              inputId = "k_selected",
              value = 2,
              min = 2,
              max = 16,
            ),
          ),
          mainPanel(
            fluidRow(
              column(
                height = 12,
                width = 12,

            tabsetPanel(
              id = "cluster_tabs",  # Defina um id para o tabsetPanel interno
              tabPanel("Manipulação dos Clusters"),
              tabPanel("Exploração dos Clusters",
                plotOutput("rainette_plot")
              ),
              tabPanel("Documentos dos Clusters",
                       div(
                         style = "width: 100%; height: 100%;",  # Largura e altura completas
                         uiOutput("dynamic_docs_ui")
                       )
              )
              )
              ),
            )
          )
        #)
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
            selectInput(
              "choice_df2", "Carrege seu data.frame clusterizado",
              choices = c()
            )
          ),
          mainPanel()
        )
      )
    )
  )
)
