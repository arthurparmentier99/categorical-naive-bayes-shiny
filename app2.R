# Installer les packages nécessaires
install.packages(c("shiny", "shinydashboardPlus", "DT", "dplyr", "readxl","glmnet","shinyjs"))
install.packages("shinydashboard", type = "source")

# Charger les bibliothèques
library(shiny)
library(readr)
library(dplyr)
library(glmnet)
library(shiny)
library(shinydashboard)
library(DT)
library(shinydashboardPlus)
library(readxl)
library(shinyjs)



# Définir le thème personnalisé
customCSS <- "
  body {
    background-color: rgb(255,255,255);
  }
  /* Ajoutez d'autres styles personnalisés si nécessaire */
"

# Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Naive Baiyes"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "home", icon = icon("home")),
      menuItem("Import de données", tabName = "dataset", icon = icon("database")),
      menuItem("Explorer", tabName = "explore", icon = icon("th")),
      menuItem("Predict", tabName = "predict", icon = icon("chart-line")),
      menuItem("Fit", tabName = "fit", icon = icon("check-square")),
      menuItem("Graphique", tabName = "graph", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tags$style(HTML(customCSS)),
    tabItems(
      tabItem(
        tabName = "home",
        mainPanel(
          img(src = "logo.PNG", width = 100, height = 100),
          h1("Bienvenue sur notre application R Shiny !"),
          h2("Découvrez les fonctionnalités de notre package."),
          p("Cette application a pour objectif de vous offrir une expérience interactive pour explorer vos données avec notre package R."),
        )
      ),
      tabItem(
        tabName = "dataset",
        sidebarPanel(
          h3("Importez un ensemble de données"),
          radioButtons(
            "formt", "Format du fichier",
            choices = c(CSV = "csv", Excel = "excel"),
            selected = "csv"
          ),
          fileInput("file1", "Choisissez un fichier", accept = c(".csv", ".txt", ".xls")),
          checkboxInput("header", "Header", TRUE),
          radioButtons(
            "sep",
            "Separator",
            choices = c(
              Comma = ",",
              Semicolon = ";",
              Tab = "\t"
            ),
            selected = ","
          ),
          radioButtons(
            "quote",
            "Quote",
            choices = c(
              None = "",
              "Double Quote" = '"',
              "Single Quote" = "'"
            ),
            selected = '"'
          ),
          actionButton("report", label = "Afficher le rapport instantané")
        ),
        mainPanel(
          DT::dataTableOutput("data1", width = "90%"),
          verbatimTextOutput("c.show")
        )
      ),
      tabItem(
        tabName = "explore",
        fluidRow(
          # ... Votre contenu pour l'onglet "Explorer" va ici ...
        )
      ),
      tabItem(
        tabName = "predict",
        tabPanel(
          "Predict",
          sidebarLayout(
            sidebarPanel(
              fileInput("file_predict", "Sélectionner le fichier pour la prédiction :",
                        accept = c(".csv", ".tsv", ".xlsx")),
              selectInput("target_predict", "Variable cible :", ""),
              selectInput("features_predict", "Variables explicatives :", "", multiple = TRUE),
              actionButton("run_predict", "Lancer la prédiction"),
              width = 3
            ),
            mainPanel(
              textOutput("predict_output")
            )
          )
        )
      ),
      tabItem(
        tabName = "fit",
        tabPanel(
          "Fit",
          sidebarLayout(
            sidebarPanel(
              fileInput("file_fit", "Sélectionner le fichier pour l'ajustement :",
                        accept = c(".csv", ".tsv", ".xlsx")),
              selectInput("target_fit", "Variable cible :", ""),
              selectInput("features_fit", "Variables explicatives :", "", multiple = TRUE),
              actionButton("run_fit", "Lancer l'ajustement"),
              checkboxInput("indicator", "Indicateur de pertinence des variables"),
              actionButton("auto_select", "Sélection automatique des variables"),
              actionButton("export_pmml", "Exporter en PMML"),
              width = 3
            ),
            mainPanel(
              tableOutput("fit_output"),
              plotOutput("fit_indicator_plot")
            )
          )
        )
      ),
      tabItem(
        tabName = "graph",
        tabPanel(
          "Graphique",
          sidebarLayout(
            sidebarPanel(
              fileInput("file_graph", "Sélectionner le fichier pour le graphique :",
                        accept = c(".csv", ".tsv", ".xlsx")),
              actionButton("run_graph", "Lancer le graphique"),
              width = 3
            ),
            mainPanel(
              plotOutput("graph_output")
            )
          )
        )
      )
    ),
    skin = "blue-light"
  )
)

# Server
server <- function(input, output, session) {
  # Fonction réactive pour lire les données
  dados <- reactive({
    if (input$formt == "excel") {
      file <- input$file1
      req(file)
      df <- data.frame(read.xlsx(file$datapath, 1, startRow = 1))
      return(df)
    } else {
      if (input$formt == "csv") {
        file <- input$file1
        req(file)
        df <- data.frame(
          read.csv(
            file$datapath,
            header = input$header,
            sep = input$sep,
            quote = input$quote,
            stringsAsFactors = TRUE
          )
        )
        return(df)
      }
    }
  })
  
  # Affichage des 10 premières lignes des données
  output$data1 <- DT::renderDataTable({
    DT::datatable(dados(), options = list(scrollX = TRUE))
  })
  
  output$select <- renderUI({
    dattA <- data.frame(dados())
    num <- dattA %>% select_if(is.numeric)
    
    selectizeInput(
      inputId = "variable1",
      label = "Choose variable to plot",
      choices = names(num),
      multiple = FALSE
    )
  })
  
  observeEvent(input$report, {
    withProgress(message = "Writing report...", value = 0, {
      dd.r <- data.frame(dados())
      create_report(dd.r)
    })
  })
  
  output$sumariz1 <- renderUI({
    print(
      summarytools::dfSummary(dados(), graph.magnif = 0.75, headings = FALSE),
      method = 'render'
    )
  })
  
  output$tabcl <- renderUI({
    dattA <- data.frame(dados()) %>% select_if(purrr::negate(is.numeric))
    
    selectizeInput(
      inputId = "tabclass",
      label = "Categorical variable",
      choices = names(dattA),
      multiple = FALSE
    )
  })
  
  output$sumarizB <- renderUI({
    print(
      data.frame(dados()) %>%
        tbl_summary(by = input$tabclass) %>%
        add_p(),
      method = 'render'
    )
  })
}

shinyApp(ui, server)
