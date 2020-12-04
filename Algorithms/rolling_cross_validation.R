
roll_cross_validation <- function(X, Y, start_size = 40, K = 10, model, hyperparameters){
  
  ####################################################################
  #
  # This function takes X (data matrix), Y (input vector),
  # a start size, a K (test size), the model, and the hyper
  # parameter grid to find an optimal combination.
  #
  ####################################################################
  
  if(model == "SVR"){
    
    # Creating a list to store average RMSE values from each iteration
    average_RMSE_vector <- c()
    
    for(i in 1:nrow(hyperparameters)){
      
      # Extracting hyperparameters to be used in each iteration
      poly_deg <- hyperparameters[i, 1]
      ke <- as.character(hyperparameters[i, 2])
      C <- hyperparameters[i, 3]
      e <- hyperparameters[i, 4]
      
      # Defining a list that will then be used to calculate average RMSE
      RMSE_vector <- c()
      indx = 1
      
      # Resetting parameters
      start_size_svr = start_size
      
      while(start_size_svr < nrow(X)){
        
        ## Splitting data on a rolling basis
        # Defining the train validation
        train_X <- X[1:start_size_svr, ]
        train_Y <- Y[1:start_size_svr, ]
        
        # Increasing the counter
        start_size_svr <- start_size_svr + K
        
        # Defining the test validation
        test_X <- X[(dim(train_X)[1] + 1):start_size_svr, ]
        test_Y <- Y[(length(train_Y) + 1):start_size_svr, ]
        
        ########################################################
        ########################################################
        
        ## Training model
        # Defining number of train observations
        n = dim(train_X)[1]
        
        # Passing parameters to SVR function
        model <- svr_model(train_X, train_Y, n, ke, poly_deg, C, e)
        
        # Extracting outputs from model
        beta <- as.matrix(model[[1]])
        bias <- model[[3]]
        
        ########################################################
        ########################################################
        
        ## Testing model
        # Defining number of test observations
        n = dim(test_X)[1]
        
        # Finding Hessian Matrix of test X
        H_test <- hess_mat_test(test_X, train_X, poly_deg, ke)
        
        # Finding predicted values
        forecasted_vals <- H_test %*% beta + bias
        
        # Calculating RMSE
        RMSE_val <- RMSE(forecasted_vals, test_Y)
        
        # Adding to list, plus increasing counter
        RMSE_vector[indx] <- RMSE_val
        indx = indx + 1
        
      }
      
      print(i)
      # Calculating average RMSE and storing
      average_RMSE_vector[i] <- mean(RMSE_vector)
      
    }
    
  } else if(model == "RF"){
    
    # Creating a list to store average RMSE values from each iteration
    average_RMSE_vector <- c()
    
    for(i in 1:nrow(hyperparameters)){
      
      # Defining parallels
      registerDoParallel(5)
      
      # Extracting parameters
      n_trees <- hyperparameters[i, 1]
      feature_frac <- hyperparameters[i, 2]
      min_node <- hyperparameters[i, 3]
      
      # Defining a list that will then be used to calculate average RMSE
      RMSE_vector <- c()
      indx = 1
      
      # Resetting parameters
      start_size_svr = start_size
      
      while(start_size_svr < nrow(X)){
        
        # Defining the formula from X and Y.
        formula <- as.formula(paste(colnames(Y), "~", paste(colnames(X), collapse = "+")))
        
        ## Splitting data on a rolling basis
        # Defining the train validation
        train_X <- X[1:start_size_svr, ]
        train_Y <- as.data.frame(Y[1:start_size_svr, ])
        colnames(train_Y) <- colnames(Y)
        
        train_data <- cbind(train_Y, train_X)
        
        # Increasing the counter
        start_size_svr <- start_size_svr + K
        
        # Defining the test validation
        test_X <- X[(dim(train_X)[1] + 1):start_size_svr, ]
        test_Y <- as.data.frame(Y[(nrow(train_Y) + 1):start_size_svr, ])
        colnames(test_Y) <- colnames(Y)
        
        test_data <- cbind(test_Y, test_X)
        
        ########################################################
        ########################################################
        
        # Training 
        mod.1 <- run_rf(formula = formula,
                        n_trees = n_trees,
                        feature_frac = feature_frac,
                        data = train_data,
                        min_node = min_node)
        
        # Testing
        mod.1.pred <- pred_new_rf(rf_fit = mod.1,
                                  data = test_data)
        
        # Calculating RMSE
        RMSE_val <- RMSE(mod.1.pred, test_Y[, 1])
        
        # Adding to list, plus increasing counter
        RMSE_vector[indx] <- RMSE_val
        indx = indx + 1
        
      }
      
      print(i)
      # Calculating average RMSE and storing
      average_RMSE_vector[i] <- mean(RMSE_vector)
      
    }
  
    
  } else if(model == "KNN"){
    
      start_size = hyperparameters[1, 1]
      K_wind = hyperparameters[1, 2]
      knn_numb = hyperparameters[1, 3]
      
      # This was done with Caret, so no need for my convoluted cross_validation_process.
      # Thus, just copy pasted the code in KNN.R and adapted it to my function
      # Anyways, it would be the same, I think - Ricardo
      
      crtl = trainControl(method = "timeslice", initialWindow = start_size, 
                          horizon = K_wind, fixedWindow = FALSE)
      
      fit = train(X, Y, method = "knn", trControl = crtl, 
                  preProcess = c("center", "scale"), 
                  tuneGrid = expand.grid(k = seq(from = 1, to = knn_numb, by = 1)))
    
      output_data <- fit$results$RMSE
      return(output_data)
      
  }
  
  output_data <- cbind(hyperparameters, average_RMSE_vector)
  return(output_data)
  
}

# -----------------------------------------------------------------------------------

RMSE <- function(forecasted, observed){
  
  ####################################################################
  #
  # This function takes the forecasted values, as well as the 
  # observed ones, and calculates the root mean squared error.
  #
  ####################################################################
  
  val <- sqrt(mean((forecasted - observed)^2))
  return(val)
  
}