# Libraries used
library(shiny)
library(shinydashboard)
library(ggplot2)
library(reshape)
library(wordcloud)

# Defining work environments
dashboard_location <- getwd()
base_location <- dirname(getwd())
data_location <- paste0(base_location, "/Data Outputs")
algorithms_location <- paste0(base_location, "/Algorithms")
svr_location <- paste0(base_location, "/Algorithms/SVR")
rf_location <- paste0(base_location, "/Algorithms/RandomForest")

# Loading data
setwd(data_location)
data <- read.csv("combined_processed_data.csv", check.names = FALSE)

# Processing data
colnames(data)[1] <- "Date"
data$Date <- as.Date(data$Date, format = "%m/%d/%y")

# Source Algorithms (i.e. cross validation)
setwd(algorithms_location)
source('rolling_cross_validation.R')

# Source SVR
setwd(svr_location)
source('svr_functions.R')

### Sourcing RF R files ----
setwd(rf_location)
source('randomForestFunctions.R')

# Libraries for RF
library(foreach)
library(doParallel)

### Sourcing KNN R files ----
# No files

# Libraries for KNN
library(tidyverse)
library(caret)

# Source UI
setwd(dashboard_location)
ui <- source("ui.R")

# ----------------------------------------------------------------------
# Defining backend functionality
server <- function(input, output){
  
  # Plotting Confirmed cases time series
  output$covid_timeline <- renderPlot({

    states <- input$timeline_state
    timeline <- seq(from = input$timeline_date[1], to = input$timeline_date[2],
                    by = "day")
    
    if(length(states) == 1){
      
      # Subsetting data based on dates selected by user
      state_data <- data[data$Date %in% timeline, input$timeline_state]
      date_subset_data <- data[data$Date %in% timeline, 1]
      
      # Combining data into a single dataset
      new_data <- as.data.frame(cbind(Date = as.Date(date_subset_data,
                                                     origin = "1970-01-01"), State = state_data))
      
      # Plotting
      ggplot(new_data, aes(x = as.Date(Date, origin = "1970-01-01"),
                           y = State)) + geom_line() +
        ggtitle(paste0("Daily confirmed cases in ", states)) +
        xlab("Date") + ylab("Confirmed cases")
      
    } else if(length(states) > 1){
      
      # Subsetting data based on dates selected by user
      state_data <- data[data$Date %in% timeline, input$timeline_state]
      date_subset_data <- data[data$Date %in% timeline, 1]
      
      # Combining data into a single dataset
      new_data <- as.data.frame(cbind(Date = date_subset_data, State = state_data))
      colnames(new_data) <- c("Date", states)
      
      # Converting data from long to tall to plot multiple lines in a single plot
      test_new_data <- melt(new_data, id = "Date")
      colnames(test_new_data) <- c("Date", "States", "value")
      # Plotting
      ggplot(test_new_data, aes(x = Date, y = value, colour = States)) +
        geom_line() +
        ggtitle(paste0("Daily confirmed cases in chosen states")) +
        xlab("Date") + ylab("Confirmed cases")
      
    }
  })
  
  # Plotting Wordmap
  output$wordmap <- renderPlot({
    
    set.seed(50)
    
    if(input$remove_covid == TRUE){
      
      twitter_dataset <- data[, c(54:ncol(data))]
      twitter_dataset <- twitter_dataset[, 
                    -which(colnames(twitter_dataset) %in% c("covid", 
                                                            "coronavirus",
                                                            "covid19"))]
      
    } else {
      
      # Dataset with only words
      twitter_dataset <- data[, c(54:ncol(data))]
      
    }

    
    # Defining timeframe to subset from data
    timeline <- seq(from = input$wordmap_date[1], to = input$wordmap_date[2],
                    by = "day")
    
    # Subsetting. 52 (date + 51 states -- we only want words)
    # word_data <- twitter_dataset[data$Date %in% timeline, 1:input$numb_words]
    word_data <- twitter_dataset[data$Date %in% timeline, 
                                 sample(colnames(twitter_dataset), input$numb_words,
                                        replace = FALSE)]
    
    # Now sum each column
    words <- colnames(word_data)
    word_sums <- colMeans(word_data)
    
    wordcloud(words = words, freq = word_sums, max.words = 411, random.order = FALSE,
              colors = brewer.pal(8, "Dark2"))
    
  })
  
  output$svr_cross <- renderTable({
    
    # ------------------------------------------
    # First, defining data and cross validation parameters
    
    # Extracting data from state selected
    state <- input$svr_state
    Y <- data[, state]
    
    # Scaling Y
    Y <- scale(Y)
    
    # Extracting X
    numb_predictors <- input$svr_num_predict
    X <- data[, 54:numb_predictors]
    
    # Scaling X
    X <- scale(X)
    
    # Start size, k, and model
    start_size <- input$svr_start_size
    K <- input$svr_k
    model = "SVR"
    
    # ------------------------------------------
    # Now defining the hyperparameter grid
    
    # Extracting values from dashboard
    max_poly <- input$svr_poly
    max_C <- input$svr_cost
    max_e <- input$svr_e
    ke <- input$svr_ke
    
    # Defining the sequences
    p1 <- seq(from = 1, to = max_poly, by = 1)
    C <- seq(from = 1, to = max_C, by = 1)
    e <- seq(from = 0.1, to = max_e, by = 0.1)
    
    # Creating the grid
    hyperparameters <- expand.grid(poly_deg = p1, ke = ke, 
                                   C = C, e = e)
    
    # --------------------------------------------
    # Passing values to cross validation function
    
    cross_outputs <- roll_cross_validation(X, Y, start_size, K, model, hyperparameters)

    cross_outputs
    
  })
  
  # output$svr_hist <- renderPlot({
  #   
  #   # ------------------------------------------
  #   # First, defining data and cross validation parameters
  #   
  #   # Extracting data from state selected
  #   state <- input$svr_state
  #   Y <- data[, state]
  #   
  #   # Scaling Y
  #   Y <- scale(Y)
  #   
  #   # Extracting X
  #   numb_predictors <- input$svr_num_predict
  #   X <- data[, 54:numb_predictors]
  #   
  #   # Scaling X
  #   X <- scale(X)
  #   
  #   # Start size, k, and model
  #   start_size <- input$svr_start_size
  #   K <- input$svr_k
  #   model = "SVR"
  #   
  #   # ------------------------------------------
  #   # Now defining the hyperparameter grid
  #   
  #   # Extracting values from dashboard
  #   max_poly <- input$svr_poly
  #   max_C <- input$svr_cost
  #   max_e <- input$svr_e
  #   ke <- input$svr_ke
  #   
  #   # Defining the sequences
  #   p1 <- seq(from = 1, to = max_poly, by = 1)
  #   C <- seq(from = 1, to = max_C, by = 1)
  #   e <- seq(from = 0.1, to = max_e, by = 0.1)
  #   
  #   # Creating the grid
  #   hyperparameters <- expand.grid(poly_deg = p1, ke = ke, 
  #                                  C = C, e = e)
  #   
  #   # --------------------------------------------
  #   # Passing values to cross validation function
  #   
  #   cross_outputs <- roll_cross_validation(X, Y, start_size, K, model, hyperparameters)
  #   
  #   hist(cross_outputs[, 5],
  #        main = "Histogram of Average RMSE",
  #        xlab = "Average RMSE")
  #  
  #   })
  
    output$rf_cross <- renderTable({
  
      # Extracting chosen state
      state <- input$rf_state
      Y <- as.data.frame(data[, state])
      colnames(Y) <- colnames(data[state])
  
      # X is not UI dependent
      X <- data[, 54:ncol(data)]
      
      # Extracting values for cross validation
      start_size = input$rf_start_size
      K = input$rf_k
      model = "RF"
      
      # Extracting values for hyperparameter grid
      n_trees_max <- input$rf_ntrees
      feature_frac_max <- input$rf_featurefrac
      minnode_max <- input$rf_minnode
      
      # Defining the hyperparameter grid
      n_trees <- seq(from = 5, to = n_trees_max, by = 1)
      feature_frac <- seq(from = 0.5, to = feature_frac_max, by = 0.05)
      min_node <- seq(from = 2, to = minnode_max, by = 1)
      
      # Chosing only the first 5, otherwise it'd take forever.
      hyperparameters <- head(expand.grid(n_trees = n_trees, feature_frac = feature_frac,
                                          min_node = min_node), 5)
      
      cross_outputs <- roll_cross_validation(X, Y, start_size, K, model, hyperparameters)
      
      cross_outputs
      
    })
    
    # output$rf_hist <- renderPlot({
    #   
    #   # Extracting chosen state
    #   state <- input$rf_state
    #   Y <- as.data.frame(data[, state])
    #   colnames(Y) <- colnames(data[state])
    #   
    #   # X is not UI dependent
    #   X <- data[, 54:ncol(data)]
    #   
    #   # Extracting values for cross validation
    #   start_size = input$rf_start_size
    #   K = input$rf_k
    #   model = "RF"
    #   
    #   # Extracting values for hyperparameter grid
    #   n_trees_max <- input$rf_ntrees
    #   feature_frac_max <- input$rf_featurefrac
    #   minnode_max <- input$rf_minnode
    #   
    #   # Defining the hyperparameter grid
    #   n_trees <- seq(from = 5, to = n_trees_max, by = 1)
    #   feature_frac <- seq(from = 0.5, to = feature_frac_max, by = 0.05)
    #   min_node <- seq(from = 2, to = minnode_max, by = 1)
    #   
    #   # Chosing only the first 5, otherwise it'd take forever.
    #   hyperparameters <- head(expand.grid(n_trees = n_trees, feature_frac = feature_frac,
    #                                       min_node = min_node), 5)
    #   
    #   cross_outputs <- roll_cross_validation(X, Y, start_size, K, model, hyperparameters)
    #   
    #   hist(cross_outputs[, 4],
    #        main = "Histogram of Average RMSE",
    #        xlab = "Average RMSE")
    #   
    # })
    
    output$knn_cross <- renderTable({
  
      # Extracting chosen state
      state <- input$knn_state
      Y <- data[, state]
  
      # X is not UI dependent
      X <- data[, 54:ncol(data)]
      
      # Extracting cross validation parameters
      start_size <- input$knn_start_size
      K <- input$knn_k
      
      # Extracting KNN parameters
      knn_numb <- input$knn_n
      
      # Defining hyperparameter grid
      hyperparameters <- data.frame(start_size = start_size, K = K, knn_numb = knn_numb)
      
      cross_outputs <- roll_cross_validation(X, Y, start_size = 120, K = 40, model = "KNN", hyperparameters)
      
      cross_outputs <- as.data.frame(cross_outputs)
      colnames(cross_outputs) <- "Average RMSE"
      
      cross_outputs
      
    })
  
  # This is the structure for any plot
  # output$svr_cross <- renderPlot({
  #   
  #   
  #   
  # })
  
}

# Loading the dashboard
shinyApp(ui, server)
