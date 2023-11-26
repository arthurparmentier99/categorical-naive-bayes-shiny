# Installer les packages nécessaires
install.packages(c("shiny", "shinydashboardPlus","rvest", "DT", "dplyr","plotly","ggplot2", "readxl","glmnet","shinyjs"))
install.packages("shinydashboard", type = "source")
install.packages("devtools")
library(devtools)
#devtools::install_github(Pioterr/nbsisemhp)
#library(NaiveBayes)
# Charger le package
devtools::install_github("NdeyeFatou8/NaivebayesCategorial")
library(NaiveBayes)



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
library(ggplot2)
library(plotly)
library(cluster)
library(rvest)


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
      menuItem("Info Naive Bayes", tabName = "naive_bayes_info", icon = icon("info-circle")),
      menuItem("Import de données", tabName = "dataset", icon = icon("database")),
      menuItem("Explorer", tabName = "explore", icon = icon("th")),
      menuItem("Fit", tabName = "fit", icon = icon("check-square")),
      menuItem("Predict", tabName = "predict", icon = icon("chart-line")),
      menuItem("Graphique", tabName = "graph", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tags$style(HTML(  "
        .center-text {
          text-align: center;
        }

        .center-content {
          margin: 0 auto;
        }
        "
    )

        ),
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css")
    ),

    tabItems(
      tabItem(
        tabName = "home",
        mainPanel(
          style = "text-align: center; margin: 0 auto;",
          div(img(id="image",src = "logo.PNG", width = 300, height = 300,style = "text-align: center;", class = "hidden")),
          div(h1(id="text1", "Bienvenue sur notre application R Shiny !", style = "text-align: center;", class = "hidden")),
          div(h3(id="text2", "Cette application a pour objectif de vous offrir une expérience interactive pour explorer vos données avec notre package R Naive Bayes.", class = "hidden")),
          div(h4(id="text3", "Enjoy ;)",style = "text-align: center;", class = "hidden"))
        )
      ),
      tabItem(
        tabName = "naive_bayes_info",
        mainPanel(tags$style(HTML("

  ")),
          h1("Naive Bayes"),
          htmlOutput("info_content")
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
            column(
              width = 12,
              tabsetPanel(
                navlistPanel(
                  HTML("<b>Summaries</b>"),
                  widths = c(2, 10),
                  tabPanel("General Summary",
                           mainPanel(htmlOutput("sumariz1"))),
                  HTML("<b>Visualizations</b>"),
                  tabPanel(
                    "2D plots",
                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        h4("Select variables for 2D plots"),
                        uiOutput("select_2d"),
                        uiOutput("select_2d_group"),
                        radioButtons(
                          inputId = "gbuttons_2d",
                          "Add Groups?",
                          c("Yes", "No"),
                          selected = "No",
                          inline = TRUE
                        )
                      ),
                      mainPanel(
                        conditionalPanel(
                          condition = "input.gbuttons_2d == 'Yes'",
                          plotlyOutput("plot_2d_explore", width = '90%', height = "80%"),
                          plotlyOutput("histogram_2d_explore", width = '90%', height = "80%")
                        ),
                        conditionalPanel(
                          condition = "input.gbuttons_2d == 'No'",
                          plotlyOutput("plot_2d_explore_0", width = '90%', height = "80%"),
                          plotlyOutput("histogram_2d_explore_0", width = '90%', height = "80%")
                        )
                      )
                    )
                  ),
                tabPanel("Correlation heatmap",
                         mainPanel(
                           h3("Interactive Correlation Heatmap"),
                           plotlyOutput("heatmap_explore", width = "100%", height = "600px")
                         )),
                tabPanel("Clustering",
                         mainPanel(
                           h3("Clustering"),
                           plotlyOutput("clustering_explore", width = "100%", height = "600px")
                         )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "predict",
        tabPanel(
          "Predict",
          sidebarLayout(
            sidebarPanel(
              fileInput("file_predict", "Sélectionner le fichier pour la prédiction :", accept = c(".csv", ".tsv", ".xlsx")),
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
              #fileInput("file_fit", "Sélectionner le fichier pour l'ajustement :", accept = c(".csv", ".tsv", ".xlsx")),
              selectInput("target_fit", "Variable cible :",choices = ""),
              selectInput("features_fit", "Variables explicatives :",choices = "",multiple = TRUE),
              sliderInput(inputId = "split",label = h3("Train/Test Split %"),min = 0,max = 100,value = 75),
              actionButton("run_fit", "Lancer l'ajustement"),
              checkboxInput("indicator", "Indicateur de pertinence des variables"),
              actionButton("auto_select", "Sélection automatique des variables"),
              actionButton("export_pmml", "Exporter en PMML"),

              width = 3
            ),
            mainPanel(
              verbatimTextOutput("message"),
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
              fileInput("file_graph", "Sélectionner le fichier pour le graphique :", accept = c(".csv", ".tsv", ".xlsx")),
              actionButton("run_graph", "Lancer le graphique"),
              width = 3
            ),
            mainPanel(
              plotOutput("graph_output")
            )
          )
        )
      )
    )
  )
)
  # Server
server <- function(input, output, session) {

    ########PAGE D'ACCUEIL#########

    # Premier délai pour le premier message
    shinyjs::delay(
      0,
      {
        shinyjs::removeClass("image", "hidden")
        shinyjs::addClass("image", "animate__animated animate__fadeInDown")
      }
    )

    # Deuxième délai pour le deuxième message après 5 secondes du premier délai
    shinyjs::delay(
      1000,
      {
        shinyjs::removeClass("text1", "hidden")
        shinyjs::addClass("text1", "animate__animated animate__fadeInDown")
      }
    )
    #3 ieme delay
    shinyjs::delay(
      2000,
      {
        shinyjs::removeClass("text2", "hidden")
        shinyjs::addClass("text2", "animate__animated animate__fadeInDown")
      }
    )

    #4 ieme delay
    shinyjs::delay(
      3000,
      {
        shinyjs::removeClass("text3", "hidden")
        shinyjs::addClass("text3", "animate__animated animate__fadeInDown")
      }
    )

    ########PAGE INFOS#########

    extractTextContent <- function() {
      info_text <- "
  Documentation de « Scikit-Learn » à propos du Naive Bayes - https://scikit-learn.org/stable/modules/naive_bayes.html

  • Livre « Pratique de l’Analyse Discriminante Linéaire » (https://cours-machine-learning.blogspot.com/p/analyse-discriminante.html), qui comporte un chapitre dédié au « Naive Bayes » (chapitre 6)
  • Tutoriels (https://tutoriels-data-science.blogspot.com/p/tutoriels-en-francais.html), j’ai écrit pas mal de choses sur Naive bayes.
  "
      return(info_text)
    }

    # Affichage du contenu extrait
    output$info_content <- renderText({
      extractTextContent()
    })

    #########PAGE DATASET ########

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
      DT::datatable(dados(), options = list(scrollX = TRUE, pageLength = 10))
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

    #######2D###

    output$select_2d <- renderUI({
      dattA <- data.frame(dados())
      num <- dattA %>% select_if(is.numeric)

      selectizeInput(
        inputId = "variable_2d",
        label = "Choose variable for 2D plot",
        choices = names(num),
        multiple = FALSE
      )
    })

    output$select_2d_group <- renderUI({
      dattA <- data.frame(dados())
      num <- dattA %>% select_if(is.numeric)

      selectizeInput(
        inputId = "variable_2d_group",
        label = "Choose grouping variable",
        choices = names(num),
        multiple = FALSE
      )
    })

    output$plot_2d_explore <- renderPlotly({
      # Créer le graphique 2D avec groupement
      plot_ly(
        data = dados(),
        x = ~get(input$variable_2d),
        y = ~get(input$variable_2d_group),
        color = ~get(input$variable_2d_group),
        type = "scatter",
        mode = "markers",
        marker = list(size = 10)
      ) %>%
        layout(
          title = "2D Plot with Grouping",
          xaxis = list(title = input$variable_2d),
          yaxis = list(title = input$variable_2d_group)
        )
    })

    output$plot_2d_explore_0 <- renderPlotly({
      # Créer le graphique 2D sans groupement
      plot_ly(
        data = dados(),
        x = ~get(input$variable_2d),
        y = ~get(input$variable_2d),
        type = "scatter",
        mode = "markers",
        marker = list(size = 10)
      ) %>%
        layout(
          title = "2D Plot without Grouping",
          xaxis = list(title = input$variable_2d),
          yaxis = list(title = input$variable_2d)
        )
    })

    # Render the histograms
    output$histogram_2d_explore <- renderPlotly({
      dats <- data.frame(dados())

      # Your logic to generate histogram here
      histogram_data <- plot_ly(dats, x = ~get(input$variable_2d), type = "histogram")

      layout <- list(
        title = "Histogram",
        xaxis = list(title = input$variable_2d),
        yaxis = list(title = "Frequency")
      ) %>%
        add_trace(marker = list(line = list(color = "black", width = 1)))

      histogram_data %>% layout(layout)
    })

    output$histogram_2d_explore_0 <- renderPlotly({
      dats <- data.frame(dados())

      # Your logic to generate histogram here for the case when groups are not added
      histogram_data <- plot_ly(dats, x = ~get(input$variable_2d), type = "histogram")

      layout <- list(
        title = "Histogram",
        xaxis = list(title = input$variable_2d),
        yaxis = list(title = "Frequency")
      ) %>%
        add_trace(marker = list(line = list(color = "black", width = 1)))

      histogram_data %>% layout(layout)
    })

    # Render the correlation heatmap
    output$heatmap_explore <- renderPlotly({
      dats <- data.frame(dados())

      # Filter out non-numeric columns
      numeric_cols <- sapply(dats, is.numeric)
      numeric_data <- dats[, numeric_cols]

      correlation_matrix <- cor(numeric_data, use = "complete.obs")

      heatmap_data <- plot_ly(
        x = colnames(correlation_matrix),
        y = colnames(correlation_matrix),
        z = as.matrix(correlation_matrix),
        type = "heatmap",
        colorscale = "Blues",
        showscale = TRUE
      )

      layout <- list(
        title = "Correlation Heatmap",
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )

      heatmap_data %>% layout(layout)
    })

    ######CLUSTER#########

    # Render the clustering plot
    output$clustering_explore <- renderPlotly({
      dats <- data.frame(dados())

      # Filter out non-numeric columns
      numeric_cols <- sapply(dats, is.numeric)
      numeric_data <- dats[, numeric_cols]

      # Perform k-means clustering (you can adjust the number of clusters)
      k <- 3
      kmeans_result <- kmeans(numeric_data, centers = k)

      # Add cluster assignment to the original data
      dats$cluster <- as.factor(kmeans_result$cluster)

      # Create a scatter plot with color-coded clusters
      plot_data <- plot_ly(
        x = dats[, 1],
        y = dats[, 2],
        color = dats$cluster,
        colors = c("red", "green", "blue"),  # Define cluster colors
        type = "scatter",
        mode = "markers",
        marker = list(size = 10)
      )

      layout <- list(
        title = "Clustering Plot",
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        showlegend = TRUE
      )

      plot_data %>% layout(layout)
    })


    # Créez une instance de votre modèle NaiveBayes
    my_naive_bayes <- naiveBayes$new(data = dados(), features = c("feature1", "feature2", ...), target = "target_variable")

    observe({
      # Mettre à jour les choix de target_fit avec les noms des variables
      updateSelectInput(session, "target_fit", choices = names(dados()))
    })

    observeEvent(input$target_fit, {
      # Mettre à jour les choix de features_fit avec toutes les variables sauf la variable sélectionnée dans target_fit
      selected_target <- input$target_fit
      all_variables <- names(dados())
      available_features <- setdiff(all_variables, selected_target)

      updateSelectInput(session, "features_fit", choices = available_features, selected = available_features)
    })

    ###ajOUT

    observeEvent(input$run_fit, {
      # Récupérez les données à partir du dataset sélectionné
      selected_data <- dados()
      selected_features <- selected_data[, input$features_fit, drop = FALSE]
      selected_target <- selected_data[, input$target_fit, drop = FALSE]

      # Exécutez l'ajustement avec votre modèle NaiveBayes
      my_naive_bayes$fit(X = selected_features, y = selected_target)

      # Affichez le résultat de l'ajustement
      output$message <- renderText("Le modèle a été entraîné avec succès")
      output$fit_result <- renderPrint({
        my_naive_bayes$summary()
      })
    })

}

shinyApp(ui, server)

