install.packages("R6", repos="https://cloud.r-project.org/")
library(R6)

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
    
    initialize = function(data, features, target) {
      self$data <- data
      self$features <- features
      self$target <- target
    },
    
    # the fit function is used to calculate the various probabilities
    fit = function(X, y, epsilon = 0.01) {
      # get the predictor variables (will be usefull for checking type of columns)
      self$X <- X
      
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
      
      # calculate conditional probabilities
      self$conditional.probabilities <- lapply(self$features, function(feature) {
        # apply the prop.table on each feature with the target to get their conditional probabilities
        feature.conditional.prob <- prop.table(table(self$data[[feature]], self$data[[self$target]]) + epsilon)
        return(feature.conditional.prob)
      })

    },
    
    predict = function(X) {
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
    
    predict_proba = function(X) {
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
  
    print = function() {
      print("TEST DU PRINT")
    },
    
    summary = function() {
      print("TEST DU SUMMARY")
    },
    
    # check if columns are numerical and transform them in categorical
    check.numerical = function() {
      for (i in 1:ncol(self$X)) {
        if (is.numeric(self$X[, i])) {
          self$X[, i] <- self$numerical.to.categorical(self$X[, i]) 
        }
      }
      print(self$X)
    },
    
    # transform a numeric column in a categorical column
    numerical.to.categorical = function(column) {
      # create the breaks for the quantiles and the median
      breaks <- quantile(column, probs = seq(0, 1, length.out = 5))
      # create the groupes on the columns with the breaks
      groupes <- cut(column, breaks = breaks, include.lowest = TRUE)
      # new.column <- creer_groupes(column)
      levels(groupes)[1] <- "A"
      levels(groupes)[2] <- "B"
      levels(groupes)[3] <- "C"
      levels(groupes)[4] <- "D"
      # we return the column with the groups
      return(groupes)
    },
    
    # create different groups when transforming with quantiles and median
    create_groups = function(column) {
      # create the breaks for the quantiles and the median
      breaks <- quantile(column, probs = seq(0, 1, length.out = 5))
      # create the groupes on the columns with the breaks
      groupes <- cut(column, breaks = breaks, include.lowest = TRUE)
      return(groupes)
    }
    
    
  )
)

# train data
data <- data.frame(
  Feature1 = c("A", "B", "C","B","C","C"),
  Feature2 = c("X", "X", "Z","Y","Z","Z"),
  Class = c("Positive", "Negative","Positive","Positive","Negative","Positive")
)

X=data[, c("Feature1", "Feature2")]
y=data[, "Class"]

# test data
new_data <- data.frame(
  Feature1 = c("A", "B", "C"),
  Feature2 = c("X", "Y", "Z")
)



nb <- NaiveBayes$new(data, c("Feature1", "Feature2"), "Class")
nb$fit(X, y)
nb$conditional.probabilities
p <- nb$predict(X)
pb <- nb$predict_proba(X)
pb

nb <- NaiveBayes$new(iris, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), "Species")
nb$fit(X, y)