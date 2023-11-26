# install.packages("R6", repos="https://cloud.r-project.org/")
library(R6)
library(parallel)
library(stringr)

#' Categorical Naive Bayes Classifier
#'
#' This class implements the Categorical Naive Bayes algorithm for categorical data.
#'
#' @section Usage:
#' \preformatted{nb <- NaiveBayes$new(data, features, target)
#' nb$fit(X, y)
#' predictions <- nb$predict(new_data)}
#'
#' @field data The training dataset.
#' @field features The explanatory variables.
#' @field target The target variable.
#' @field prior.probabilities Prior probabilities of classes.
#' @field conditional.probabilities Conditional probabilities of features.
#' @field X Training data.
#' @field y Target variable in the training data.
#' @field classes Unique classes of the target variable.
#' @field groupes Groups used for transforming numerical variables into categorical.
#' @field nb.is.numeric Indicator for the presence of numerical columns.
#' @field epsilon Epsilon parameter used in calculations.
#'
#' @section Methods:
#' \preformatted{initialize(data, features, target)
#' fit(X, y, epsilon = 0.001)
#' predict(X)
#' predict_proba(X)
#' print()
#' summary()
#' check.numerical()
#' numerical.to.categorical(column, column_number)
#' transform.categorical(X)}
#'
#' @section Author:
#' Arthur Parmentier
#' Fatou NDIENG
#'
#' @examples
#' \dontrun{
#' # Example usage
#' nb <- NaiveBayes$new(data, features, target)
#' nb$fit(X, y)
#' predictions <- nb$predict(new_data)
#' }
#'
#' @seealso
#' \url{https://your_documentation_link}
#'
#' @export
NaiveBayes <- R6Class("NaiveBayes",
                      public = list(
                        data = NA,
                        features = NA,
                        target = NA,
                        prior.probabilities = NA,
                        conditional.probabilities = list(),
                        X = NA,
                        y = NA,
                        classes = NA,
                        groupes = NA,
                        nb.is.numeric = FALSE,
                        epsilon = NA,

                        initialize = function(data, features, target) {
                          # check if there is data
                          if(missing(data)) {
                            stop("You requires a data file")
                          }

                          # check if we have at least one feature
                          if (missing(features)) {
                            stop("There must be at least one explanatory variable.")
                          }

                          # check if the target is qualitative
                          if(!is.character(target) & !is.factor(target)){
                            stop("The target variable is necessarily qualitative.")
                          }

                          self$data <- data
                          self$features <- features
                          self$target <- target
                        },

                        # the fit function is used to calculate the various probabilities
                        #' Fit the Categorical Naive Bayes Classifier
                        #'
                        #' This method fits the Categorical Naive Bayes classifier to the training data.
                        #'
                        #' @param X The training data.
                        #' @param y The target variable.
                        #' @param epsilon Epsilon parameter for smoothing (default is 0.001).
                        #'
                        #' @return NULL
                        #'
                        #' @section Usage:
                        #' \preformatted{nb <- NaiveBayes$new(data, features, target)
                        #' nb$fit(X, y)}
                        #'
                        #' @examples
                        #' \dontrun{
                        #' # Example usage
                        #' nb <- NaiveBayes$new(data, features, target)
                        #' nb$fit(X, y)
                        #' }
                        #'
                        #' @export
                        fit = function(X, y, epsilon = 0.001) {
                          # check if there is data
                          if(missing(X)){
                            stop("Fit method requires a dataframe")
                          }

                          if(any(is.na(X))) {
                            stop("Dataframe can't contains NA.")
                          }

                          # check if the target is qualitative
                          if(!is.character(y) & !is.factor(y)){
                            stop("The target variable is necessarily qualitative.")
                          }

                          if(any(is.na(y))) {
                            stop("y can't contains NA.")
                          }

                          # get the epsilon (useful for the summary)
                          self$epsilon <- epsilon

                          # get the predictor variables (will be useful for checking type of columns)
                          self$X <- X

                          # check if their is a numerical column in the dataframe
                          self$check.numerical()

                          # transform the target variable in factor if not
                          if (!is.factor(y)) {
                            y <- as.factor(y)
                          }
                          # get the target variable (will be useful for predict)
                          self$y <- y
                          # get the possible predictions classes (will be useful for predict)
                          self$classes <- levels(y)
                          # calculate prior probabilities
                          self$prior.probabilities <- table(y) / nrow(X)


                          ## calculate conditional probabilities
                          #self$conditional.probabilities <- lapply(self$features, function(feature) {
                          #  # apply the prop.table on each feature with the target to get their conditional probabilities
                          #  feature.conditional.prob <- prop.table(table(self$X[[feature]], self$y) + epsilon, margin = 2)
                          #  return(feature.conditional.prob)
                          #})


                          # detect the number of cores available for the parallelisation
                          num_cores <- detectCores()
                          cl <- makeCluster(num_cores)
                          # calculate conditional probabilities
                          self$conditional.probabilities <- parLapply(cl, self$features, function(feature) {
                            # apply the prop.table on each feature with the target to get their conditional probabilities
                            feature.conditional.prob <- prop.table(table(self$X[[feature]], self$y) + epsilon, margin = 2)
                            return(feature.conditional.prob)
                          })
                          stopCluster(cl)

                        },

                        #' Make Predictions using the Categorical Naive Bayes Classifier
                        #'
                        #' This method makes predictions using the trained Categorical Naive Bayes classifier.
                        #'
                        #' @param X New data for prediction.
                        #'
                        #' @return A data frame with predictions added as a new column.
                        #'
                        #' @section Usage:
                        #' \preformatted{predictions <- nb$predict(new_data)}
                        #'
                        #' @examples
                        #' \dontrun{
                        #' # Example usage
                        #' predictions <- nb$predict(new_data)
                        #' }
                        #'
                        #' @export
                        predict = function(X) {
                          if(missing(X)){
                            stop("Predict method requires a dataframe")
                          }
                          if (self$nb.is.numeric) {
                            X <- self$transform.categorical(X)
                          }

                          # vector of the predicted classes
                          predicted.classes <- c()

                          # for each row of the dataframe
                          for (i in 1:nrow(X)) {
                            # create a vector of the size of all possible probabilities
                            probabilities <- numeric(length(self$classes))
                            # initialize the counter
                            counter <- 1
                            # for each possible class
                            for (class in levels(self$y)) {
                              # initialize the probability with the prior_probability of the class
                              probabilities[counter] <- self$prior.probabilities[class]
                              # for each predictor variable
                              for (j in seq_along(X)) {
                                # multiply with the probability of P(feature | class)
                                probabilities[counter] <- probabilities[counter] * self$conditional.probabilities[[j]][X[[i, j]], class]
                              }
                              counter <- counter + 1
                            }

                            # normalize the probabilities
                            probabilities <- probabilities / sum(probabilities)

                            # get the predicted class with the max probabilities
                            predict.classe <- self$classes[which.max(probabilities)]
                            # add it to the vector
                            predicted.classes <- c(predicted.classes, predict.classe)
                          }
                          # add a column prediction to X
                          X$Prediction <- predicted.classes
                          # return X with the predictions
                          return(X)
                        },

                        #' Get Class Membership Probabilities using the Categorical Naive Bayes Classifier
                        #'
                        #' This method calculates the class membership probabilities for each observation in the given data using the trained Categorical Naive Bayes classifier.
                        #'
                        #' @param X New data for which to calculate class membership probabilities.
                        #'
                        #' @return A data frame with class membership probabilities for each observation.
                        #'
                        #' @section Usage:
                        #' \preformatted{probs <- nb$predict_proba(new_data)}
                        #'
                        #' @examples
                        #' \dontrun{
                        #' # Example usage
                        #' probs <- nb$predict_proba(new_data)
                        #' }
                        #'
                        #' @export
                        predict_proba = function(X) {
                          if(missing(X)){
                            stop("Predict_proba method requires a dataframe")
                          }
                          if (self$nb.is.numeric) {
                            X <- self$transform.categorical(X)
                          }

                          # dataframe of class membership probabilities
                          probabilities.classes <- data.frame(matrix(NA, nrow = 0, ncol = length(self$classes)))

                          # for each row of the dataframe
                          for (i in 1:nrow(X)) {
                            # create a vector of the size of all possible probabilities
                            probabilities <- numeric(length(self$classes))
                            # initialize the counter
                            counter <- 1
                            # for each possible class
                            for (class in levels(self$y)) {
                              # initialize the probability with the prior_probability of the class
                              probabilities[counter] <- self$prior.probabilities[class]
                              # for each predictor variable
                              for (j in seq_along(X)) {
                                # multiply with the probability of P(feature | class)
                                probabilities[counter] <- probabilities[counter] * self$conditional.probabilities[[j]][X[[i, j]], class]
                              }
                              counter <- counter + 1
                            }

                            # normalize the probabilities
                            probabilities <- probabilities / sum(probabilities)
                            # add the probabilities to the dataframe
                            probabilities.classes <- rbind(probabilities.classes, probabilities)
                          }
                          # change the name of the columns with the classes names
                          colnames(probabilities.classes) <- self$classes
                          # return the dataframe of class membership probabilities
                          return(probabilities.classes)
                        },

                        #' Print Summary Information for Categorical Naive Bayes Classifier
                        #'
                        #' This method prints a summary of the Categorical Naive Bayes classifier, including information about observations, predictive variables, and the target variable.
                        #'
                        #' @return Prints a summary to the console.
                        #'
                        #' @section Usage:
                        #' \preformatted{nb$print()}
                        #'
                        #' @examples
                        #' \dontrun{
                        #' # Example usage
                        #' nb$print()
                        #' }
                        #'
                        #' @export
                        print = function() {
                          cat("Categorical Naive Bayes\n")
                          cat("---------PRINT---------\n")
                          cat("\n")

                          cat("OBSERVATIONS ----------\n")
                          cat("Number of observations in the training dataset: ", nrow(self$X), "\n")
                          cat("\n")

                          cat("PREDICTIVE VARIABLE ---\n")
                          cat("Predictive variables: ", paste(self$features, collapse = ", "), "\n")
                          cat("Number of predictive variables: ", length(self$features), "\n")
                          cat("\n")

                          cat("TARGET VARIABLE -------\n")
                          cat("Target variable: ", self$target, "\n")
                          cat("Classes in the target variable: ", paste(self$classes, collapse = ", "), "\n")
                          cat("Number of classes in the target variable: ", length(self$classes), "\n")
                          cat("\n")
                        },

                        #' Generate Summary Statistics for Categorical Naive Bayes Classifier
                        #'
                        #' This method generates summary statistics for the Categorical Naive Bayes classifier, including information about parameters, preprocessing, and statistics on the features.
                        #'
                        #' @return Prints a summary to the console.
                        #'
                        #' @section Usage:
                        #' \preformatted{nb$summary()}
                        #'
                        #' @examples
                        #' \dontrun{
                        #' # Example usage
                        #' nb$summary()
                        #' }
                        #'
                        #' @export
                        summary = function() {
                          cat("Categorical Naive Bayes\n")
                          cat("--------SUMMARY--------\n")
                          cat("\n")

                          cat("PARAMETERS ------------\n")
                          cat("Epsilon: ", self$epsilon, "\n")
                          cat("\n")

                          cat("PREPROCESSING ---------\n")
                          cat("Dataframe contains numeric variables :", self$nb.is.numeric, "\n")
                          cat("Preprocessing done : ", self$nb.is.numeric, "\n")
                          cat("\n")

                          cat("STATISTICS ------------\n")
                          # get the number of the maximum modality for all features
                          max.mods <- sapply(self$X, function(feature) {
                            return(max(table(feature)))
                          })
                          # get the feature with the highest modality
                          max.feature <- self$features[which.max(max.mods)]
                          # get the value of this modality
                          max.mod <- max.mods[[max.feature]]
                          # get the name of this modality
                          name.max.mod <- names(table(self$X[max.feature])[which.max(max.mod)])
                          cat("Max. observations:", name.max.mod, "\n")
                          cat("Number of observations: ", max.mod, "\n")

                          # get the number of the minimum modality for all features
                          min.mods <- sapply(self$X, function(feature) {
                            return(min(table(feature)))
                          })
                          # get the feature with the lowest modality
                          min.feature <- self$features[which.min(min.mods)]
                          # get the value of this modality
                          min.mod <- min.mods[[min.feature]]
                          # get the name of this modality
                          name.min.mod <- names(table(self$X[min.feature])[which.min(min.mod)])
                          cat("Min. observations:", name.min.mod, "\n")
                          cat("Number of observations: ", min.mod, "\n")
                          cat("\n")

                          # get the unique values for each features
                          unique.values <- lapply(self$X, unique)
                          # get the number of unique values for each features
                          unique.values.number <- lapply(unique.values, length)
                          # for each unique value
                          for (i in seq_along(unique.values.number)) {
                            cat("Feature: ", names(unique.values.number)[[i]], "\n")
                            cat("Number of unique value: ", unique.values.number[[i]], "\n")
                          }
                          cat("\n")

                          cat("PROBABILITIES ---------\n")
                          cat("Prior probabilities: ", self$prior.probabilities, "\n")
                          cat("Conditional probabilities: can be accessed using the $conditional.probabilities variable in your object \n")
                        },

                        # check if columns are numerical and transform them in categorical
                        check.numerical = function() {
                          # if its the first time (train in general)
                          if (!self$nb.is.numeric) {
                            # create a dataframe for the groupes with the number of explanatory variables
                            # the number of row will always be 4 because we use quantiles and median
                            self$groupes <- as.data.frame(matrix(ncol = 0, nrow = 4))
                          }

                          for (i in 1:ncol(self$X)) {
                            if (is.numeric(self$X[, i])) {
                              self$nb.is.numeric <- TRUE
                              self$X[, i] <- self$numerical.to.categorical(self$X[, i], i)
                            }
                          }
                        },

                        # transform a numeric column in a categorical column
                        numerical.to.categorical = function(column, column_number) {

                          # create the breaks for the quantiles and the median
                          breaks <- quantile(column, probs = seq(0, 1, length.out = 5))
                          # create the groupes on the columns with the breaks
                          groupes <- cut(column, breaks = breaks, include.lowest = TRUE)

                          # intermediate variable to stock the groupes in a list
                          groupes_list <- vector("list", length = 4)
                          # add the groupes to the groupes dataframe (useful for encoding test values)
                          # for each levels of groupes (will always be 4 because we use quantiles and median)
                          for (i in seq_along(levels(groupes))) {
                            # extraction of levels for each column
                            groupes_list[[i]] <- levels(groupes)[i]
                          }

                          # add the groupes to the dataframe in the right column name
                          self$groupes[[names(self$X[column_number])]] <- groupes_list

                          # renames the groupes with letter (categorical)
                          levels(groupes)[1] <- "A"
                          levels(groupes)[2] <- "B"
                          levels(groupes)[3] <- "C"
                          levels(groupes)[4] <- "D"

                          # we return the column with the groupes
                          return(groupes)
                        },

                        transform.categorical = function(X) {
                          # for each column of our groupes
                          for (i in 1:length(self$groupes)) {
                            # we get the intervals of the groupes that we created
                            intervals <- unlist(self$groupes[[i]])
                            # for each row of the dataframe X
                            for (k in 1:nrow(X)) {
                              # for each interval
                              for (j in 1:length(intervals)) {
                                # cleaning the intervals
                                intervals[j] <- str_replace(intervals[j], "\\[", "")
                                intervals[j] <- str_replace(intervals[j], "\\]", "")
                                intervals[j] <- str_replace(intervals[j], "\\(", "")
                                intervals[j] <- str_replace(intervals[j], ",", " ")
                                # get the cleaned interval in numeric
                                numeric.interval <- as.numeric(unlist(strsplit(intervals[j], " ")))

                                # check if the value in the dataframe is in the interval of our groups
                                is.in.interval <- X[k ,i] >= numeric.interval[1] & X[k, i] < numeric.interval[2]

                                # if he is in the interval
                                if (is.in.interval) {
                                  if (j == 1) {
                                    # first interval will always be category A
                                    X[k, i] <- "A"
                                    break
                                  } else if (j == 2) {
                                    # second interval will always be category B
                                    X[k, i] <- "B"
                                    break
                                  } else if (j == 3) {
                                    # third interval will always be category C
                                    X[k, i] <- "C"
                                    break
                                  } else {
                                    # last interval will always be category D
                                    X[k, i] <- "D"
                                    break
                                  }
                                }
                              }
                            }

                          }
                          return(X)
                        }

                      )
)
