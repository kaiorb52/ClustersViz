
# ui.R

ui <- fluidPage(
    theme = shinytheme("sandstone"),
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
            actionButton(inputId = "import_browser", label = "Importar dados"),
            actionButton(inputId = "refresh_browser", label = "Refresh", icon = icon("sync-alt"))
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
        br(),
        actionButton("lemmatizar", "Lemmatização", icon = icon("download")),
        br(),
        actionButton("corpus_spilt_button", "Corpus split", icon = icon("download"))
      )
    ),
  ###########################################################################
  # CLUSTERS ################################################################
  ###########################################################################

    tabPanel(title = "Clusters", icon = icon("chart-bar", "fa-2x"),
      sidebarLayout(
        sidebarPanel(
          sliderInput(
            label = "Numero de clusters",
            inputId = "cluster_slider",
            value = 2,
            min = 2,
            max = 16,
          ),
          selectInput(
            label = "Cluster",
            inputId = "selected_doc",
            choices = c()
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Exploração dos Clusters"),
            tabPanel("Documentos dos Clusters"),
            tabPanel("Manipulação dos Clusters"),
          )
        )
      )
    ),
  ###########################################################################
  # GRAFICOS DE REDE ########################################################
  ###########################################################################

    tabPanel(title = "Graficos de Rede", icon = icon("project-diagram", "fa-2x"),
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
