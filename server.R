# install.packages(c(
#   "shiny",
#   "readr",
#   "dplyr",
#   "glmnet",
#   "shinydashboard",
#   "DT",
#   "shinydashboardPlus",
#   "readxl",
#   "shinyjs",
#   "ggplot2",
#   "plotly",
#   "cluster",
#   "rvest",
#   "parallel",
#   "packagenaivebayes",
#   "stringr",
#   "caret",
#   "writexl",
#   "summarytools"
# ))

source("NaiveBayes.R")
# Charger les bibliothèques
library(devtools)
#install_github("arthurparmentier99/categorical-naive-bayes-package", dependencies = TRUE);
library(shiny)
library(readr)
library(dplyr)
library(glmnet)
library(shinydashboard)
library(DT)
library(shinydashboardPlus)
library(readxl)
library(shinyjs)
library(ggplot2)
library(plotly)
library(cluster)
library(rvest)
library(parallel)
library(packagenaivebayes)
library(stringr)
library(caret)
library(summarytools)
library(writexl)

reactiveNB <- reactiveValues(my_naive_bayes = NULL)
server <- function(input, output, session) {
  library(devtools) # Make sure that the devtools library is loaded
  # install_github("arthurparmentier99/categorical-naive-bayes-package", dependencies = TRUE);
  library(parallel)
  library(packagenaivebayes)
  library(stringr)
  library(summarytools)

  ######## PAGE D'ACCUEIL #########

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

  # 3ème délai
  shinyjs::delay(
    2000,
    {
      shinyjs::removeClass("text2", "hidden")
      shinyjs::addClass("text2", "animate__animated animate__fadeInDown")
    }
  )

  # 4ème délai
  shinyjs::delay(
    3000,
    {
      shinyjs::removeClass("text3", "hidden")
      shinyjs::addClass("text3", "animate__animated animate__fadeInDown")
    }
  )

  ######## PAGE INFOS #########

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

  ######## PAGE DATASET ########

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


  ### PAGE 2
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

  ### PAGE FIT
  observe({
    # Mettre à jour les choix de target_fit avec les noms des variables
    updateSelectInput(session, "target_fit", choices = names(dados()))
  })

  observeEvent(input$target_fit, {
    # Mettre à jour les choix de features_fit avec toutes les variables sauf la variable sélectionnée dans target_fit
    selected_target <- input$target_fit
    all_variables <- names(dados())
    available_features <- setdiff(all_variables, selected_target)

    updateSelectInput(session, "features_fit", choices = available_features)
    cat(input$features_fit)
  })

  observeEvent(input$run_fit, {
    # Récupérez les données à partir du dataset sélectionné
    selected_data <- dados()

    selected_features <- selected_data[, input$features_fit]
    selected_target <- selected_data[, input$target_fit]

    my_naive_bayes <- NaiveBayes$new(data = selected_data, features = input$features_fit, target = input$target_fit)
    reactiveNB$my_naive_bayes <- NaiveBayes$new(data = selected_data, features = input$features_fit, target = input$target_fit)
    reactiveNB$my_naive_bayes$fit(X = selected_features, y = selected_target)

    # Exécutez l'ajustement avec votre modèle NaiveBayes
    my_naive_bayes$fit(X = selected_features, y = selected_target)

    # Affichez le résultat de l'ajustement
    output$message <- renderText("Le modèle a été entraîné avec succès")
    output$fit_result <- renderPrint({
      my_naive_bayes$summary()
    })
  })


  ### PAGE PREDICT
  predictions <- reactive({
    if (input$formt == "excel") {
      file2 <- input$file_predict
      req(file2)
      df <- data.frame(read.xlsx(file2$datapath, 1, startRow = 1))
      return(df)
    } else {
      if (input$formt == "csv") {
        file2 <- input$file_predict
        req(file2)
        cat(input$header, "\n")
        cat(input$sep, "\n")
        cat(input$quote, "\n")
        df <- data.frame(
          read.csv(
            file2$datapath,
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

  observeEvent(input$run_predict, {
    # Récupérez les données à partir du dataset sélectionné
    selected_data <- predictions()
    selected_features <- selected_data[, input$features_fit]

    # Exécutez l'ajustement avec votre modèle NaiveBayes
    df_predict <- reactiveNB$my_naive_bayes$predict(X = selected_features)
    df_predict_proba <- reactiveNB$my_naive_bayes$predict_proba(X = selected_features)

    # Affichez le résultat de l'ajustement
    output$predict_done <- renderText("Le modèle a été prédit avec succès, voici les prédictions")

    output$datapredict <- DT::renderDataTable({
      DT::datatable(df_predict, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$proba_done <- renderText("Voici les probabilités d'appartenance aux classes")

    output$probapredict <- DT::renderDataTable({
      DT::datatable(df_predict_proba, options = list(scrollX = TRUE, pageLength = 10))
    })

  })

  output$exportButton <- downloadHandler(
    filename = function() {
      paste("predictions", input$exportFormat, sep = ".")
    },
    content = function(file) {
      selected_data <- predictions()
      selected_features <- selected_data[, input$features_fit]
      df_predict <- reactiveNB$my_naive_bayes$predict(X = selected_features)
      if (input$exportFormat == "csv") {
        write.csv(df_predict, file, row.names = FALSE)
      } else if (input$exportFormat == "xlsx") {
        writexl::write_xlsx(df_predict, file)
      }
    }
  )
}
