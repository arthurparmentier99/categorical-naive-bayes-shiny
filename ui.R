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

# Charger les bibliothèques
source("NaiveBayes.R")
library(devtools)
# install_github("arthurparmentier99/categorical-naive-bayes-package", dependencies = TRUE);
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

ui <- dashboardPage(
  dashboardHeader(title = "Naive Bayes"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "home", icon = icon("home")),
      menuItem("Info Naive Bayes", tabName = "naive_bayes_info", icon = icon("info-circle")),
      menuItem("Import de données", tabName = "dataset", icon = icon("database")),
      menuItem("Explorer", tabName = "explore", icon = icon("th")),
      menuItem("Fit", tabName = "fit", icon = icon("check-square")),
      menuItem("Predict", tabName = "predict", icon = icon("chart-line"))
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
        tabName = "fit",
        tabPanel(
          "Fit",
          sidebarLayout(
            sidebarPanel(
              selectInput("target_fit", "Variable cible :",choices = ""),
              selectInput("features_fit", "Variables explicatives :", choices = NULL, multiple = TRUE),
              # sliderInput(inputId = "split",label = h3("Train/Test Split %"),min = 0,max = 100,value = 75),
              actionButton("run_fit", "Lancer l'ajustement"),
              # checkboxInput("indicator", "Indicateur de pertinence des variables"),
              # actionButton("auto_select", "Sélection automatique des variables"),
              # actionButton("export_pmml", "Exporter en PMML"),

              width = 3
            ),
            mainPanel(
              tableOutput("show_data"),
              verbatimTextOutput("message"),
              verbatimTextOutput("fit_result"),
              tableOutput("fit_output"),
              plotOutput("fit_indicator_plot")
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
              actionButton("run_predict", "Lancer la prédiction"),
              width = 3
            ),
            mainPanel(
              verbatimTextOutput("predict_done"),
              DT::dataTableOutput("datapredict", width = "90%"),
              selectInput("exportFormat", "Choose export format:", choices = c("csv", "xlsx")),
              downloadButton("exportButton", "Export predictions to CSV/Excel")
            )
          )
        ),
        tabPanel(
          "Probability",
          sidebarLayout(
            sidebarPanel(
              width = 0
            ),
            mainPanel(
              verbatimTextOutput("proba_done"),
              DT::dataTableOutput("probapredict", width = "90%")
            )
          )
        )
      )
    )
  )
)
