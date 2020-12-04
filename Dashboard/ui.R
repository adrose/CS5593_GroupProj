# Libraries used
library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Exploratory Data Analysis", tabName = "EDA", icon = icon("dashboard")),
    menuItem("Data Modeling", tabName = "DM", icon = icon("dashboard")),
    menuItem("Forecasting", tabName = "FOR", icon = icon("dashboard"))
  )
)

body <- dashboardBody(
  tabItems(
    # EDA Tab
    tabItem(tabName = "EDA",
            h1("Exploratory Data Analysis"),
            h4("The goal of this tab is to allow users to explore the relationship between our curated
               COVID-19 data set, which includes all 50 states plus the District of Columbia and
               several keywords extract from Twitter."),
      fluidPage(
      # Row 1
      fluidRow(
        box(title = "Confirmed cases over time", solidHeader = TRUE,
            collapsible = TRUE, status = "primary",
            
            plotOutput("covid_timeline", height = 300)),
        
        box(title = "Inputs", solidHeader = TRUE,
            collapsible = TRUE, status = "primary", height = 362,
            
            helpText("This plot represents the daily confirmed",
            "COVID-19 cases in each state plus the District of Columbia.",
            "You may plot up to 5 states at the same time. To delete",
            "a selected state, click on it and press 'backspace' on your",
            "keyboard.",
            "In addition, you can also modify the timeline between two",
            "particular dates from 04/13/2020 to 09/19/2020 to investigate",
            "changes over time"),
            
            selectizeInput("timeline_state", "Choose up to five states:",
                        c("USA","Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
                          "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
                          "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                          "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                          "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                          "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                          "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                          "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
                          "West Virginia", "Wisconsin", "Wyoming"),
                        options = list(maxItems = 5),
                        selected = "Alabama"),
            
            dateRangeInput("timeline_date", "Choose date range:",
                           start = "2020-04-13",
                           end = "2020-09-19",
                           min = "2020-04-13",
                           max = "2020-09-19",
                           format = "yy/mm/dd",
                           separator = " - "),
            
            submitButton("Submit")
        )
      ),
      # Row 2
      fluidRow(
        box(title = "Twitter words frequency over time", solidHeader = TRUE,
            collapsible = TRUE, status = "success",
                   
            plotOutput("wordmap", height = 500)),
      
      box(title = "Inputs", solidHeader = TRUE,
          collapsible = TRUE, status = "success", height = 560,
          
          helpText("This plot represents the most commonly used words in",
                   "COVID-19 related tweets averaged over time.",
                   "You may plot up to 411 words at the same time. They are drawn",
                   "in no particular order from the dataset.",
                   "In addition, you can also modify the timeline between two",
                   "particular dates from 04/13/2020 to 09/19/2020 to investigate",
                   "changes over time.",
                   "Lastly, you can remove COVID-19 references from the wordmap",
                   "(e.g. covid, covid19, coronavirus) from the wordmap."),
          
          numericInput("numb_words", "Number of words to populate map",
                       min = 10, max = 411, value = 10),
          
          dateRangeInput("wordmap_date", "Choose date range:",
                         start = "2020-04-13",
                         end = "2020-09-19",
                         min = "2020-04-13",
                         max = "2020-09-19",
                         format = "yy/mm/dd",
                         separator = " - "),
          
          checkboxInput("remove_covid", "Remove COVID-19 references", value = FALSE),
          
          submitButton("Submit")
          
          )
      )
    )
  ),
  
  # DM Tab
  tabItem(tabName = "DM",
          h1("Data Modeling"),
          h4("This tab allows the user to fit several hyperparameters of each model to distinct sets of data to identify the best fit"),
          fluidPage(
            # SVR Model
            fluidRow(box(title = "Support Vector Regression", solidHeader = TRUE,
                         collapsible = TRUE, status = "primary",
                         
                         tableOutput("svr_cross")),
                         # plotOutput("svr_hist")),
                     
                     box(title = "Inputs", solidHeader = TRUE,
                         collapsible = TRUE, status = "primary",
                         
                         helpText("These parameters control/modify the SVR model"),
                         helpText("First, choose cross validation settings"),
                         
                         selectizeInput("svr_state", "Choose a single state:",
                                        c("USA","Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
                                          "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
                                          "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                                          "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                                          "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                                          "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                                          "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                                          "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
                                          "West Virginia", "Wisconsin", "Wyoming"),
                                        options = list(maxItems = 1),
                                        selected = "Alabama"),
                         
                         numericInput("svr_num_predict",
                                      "Choose number of predictors",
                                      value = 40, min = 10, max = 410),
                         
                         numericInput("svr_start_size",
                                      "Choose size of starting train set",
                                      value = 120, min = 100, max = 120),
                         
                         numericInput("svr_k",
                                      "Choose size of rolling test set",
                                      value = 40, min = 10, max = 40),
                         
                         helpText("Defining the hyperparameter grid"),
                         
                         numericInput("svr_poly",
                                      "Choose Max. number of polynomial degree (between 1 to 2).
                                      Has no effect if kernel is linear",
                                      min = 1, max = 2, value = 1),
                         
                         numericInput("svr_cost",
                                      "Choose Max. value for Cost factor (between 1 to 5)",
                                      min = 1, max = 5, value = 1),
                         
                         numericInput("svr_e",
                                      "Choose Max. value for e factor (between 0.1 to 0.5)",
                                      min = 0.1, max = 0.5, value = 0.1),
                         
                         selectInput("svr_ke",
                                     "Select Kernel",
                                     choices = list("linear" = "linear",
                                                    "polynomial" = "polynomial"),
                                     selected = "polynomial"),
                         
                         submitButton("Submit")
                         
                         )
                     ),
            # Some other model - Ranfom Forest
            fluidRow(box(title = "Random Forest", solidHeader = TRUE,
                         collapsible = TRUE, status = "success",
                         
                         tableOutput("rf_cross")),
                         # plotOutput("rf_hist")),
                     
                     box(title = "Inputs", solidHeader = TRUE,
                         collapsible = TRUE, status = "success",
                         
                         helpText("These parameters control/modify the RF model"),
                         helpText("Only the first 5 parameters are used to showcase the model,
                                  as it takes quite a while to run!"),
                         helpText("First, choose cross validation settings"),
                         
                         selectizeInput("rf_state", "Choose a single state:",
                                        c("USA","Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
                                          "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
                                          "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                                          "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                                          "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                                          "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                                          "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                                          "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
                                          "West Virginia", "Wisconsin", "Wyoming"),
                                        options = list(maxItems = 1),
                                        selected = "Alabama"),
                         
                         numericInput("rf_start_size",
                                      "Choose size of starting train set",
                                      value = 120, min = 100, max = 120),
                         
                         numericInput("rf_k",
                                      "Choose size of rolling test set",
                                      value = 40, min = 10, max = 40),
                         
                         helpText("Defining the hyperparameter grid"),
                         
                         numericInput("rf_ntrees",
                                      "Choose Max. number of trees (between 5 to 10).",
                                      min = 5, max = 10, value = 5),
                         
                         numericInput("rf_featurefrac",
                                      "Choose Max. feature frac (between 0.5 to 0.75)",
                                      min = 0.5, max = 0.75, value = 0.5),
                         
                         numericInput("rf_minnode",
                                      "Choose Max. min number of nodes (between 2 to 5)",
                                      min = 2, max = 5, value = 2),
                         
                         submitButton("Submit")
                         
                     )),
            # Last model - KNN
            fluidRow(box(title = "KNN", solidHeader = TRUE,
                         collapsible = TRUE, status = "warning",
                         
                         tableOutput("knn_cross")),
                         # plotOutput("rf_hist")),
                     
                     box(title = "Inputs", solidHeader = TRUE,
                         collapsible = TRUE, status = "warning",
                         
                         helpText("These parameters control/modify the KNN model"),
                         helpText("Only the first 5 parameters are used to showcase the model,
                                  as it takes quite a while to run!"),
                         helpText("First, choose cross validation settings"),
                         
                         selectizeInput("knn_state", "Choose a single state:",
                                        c("USA","Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
                                          "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
                                          "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                                          "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                                          "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                                          "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                                          "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                                          "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
                                          "West Virginia", "Wisconsin", "Wyoming"),
                                        options = list(maxItems = 1),
                                        selected = "Alabama"),
                         
                         numericInput("knn_start_size",
                                      "Choose size of starting train set",
                                      value = 120, min = 100, max = 120),
                         
                         numericInput("knn_k",
                                      "Choose size of rolling test set",
                                      value = 40, min = 10, max = 40),
                         
                         helpText("Defining the hyperparameter grid"),
                         
                         numericInput("knn_n",
                                      "Choose Max. number of neighboors (between 2 to 15).",
                                      min = 2, max = 15, value = 2),
                         
                         submitButton("Submit")
                         
                     )),
          )
      ),
  
  # FOR tab
    tabItem(tabName = "FOR",
            h1("Forecasting"),
            h4("This tab contains three rows (one for each model), and outputs a plot containing the forecasted values
               of COVID-19 cases for the chosen state."),
            fluidPage(
              # Row 1 - SVR Predictions
              fluidRow(box(title = "SVR", solidHeader = TRUE,
                           collapsible = TRUE, status = "primary",
  
                           plotOutput("svr_pred")),
                       
                       box(title = "Inputs", solidHeader = TRUE,
                           collapsible = TRUE, status = "primary",
                           
                           helpText("These inputs control the parameters used to predict cases using
                                    an Support Vector Regression model"),
                           helpText("First, choose Data and train/test split"),
                           
                           selectizeInput("svr_pred_state", "Choose a single state:",
                                          c("USA","Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
                                            "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
                                            "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                                            "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                                            "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                                            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                                            "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                                            "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
                                            "West Virginia", "Wisconsin", "Wyoming"),
                                          options = list(maxItems = 1),
                                          selected = "USA"),
                           
                           numericInput("svr_pred_train",
                                        "Choose size of train data (between 100 to 128)",
                                        value = 128, min = 100, max = 128),
                           
                           numericInput("svr_pred_test",
                                        "Choose size of test data (between 32 to 60)",
                                        value = 32, min = 32, max = 60),
                           
                           numericInput("svr_pred_num_predict",
                                        "Choose number of predictors (between 10 to 410)",
                                        value = 200, min = 10, max = 410),
                           
                           helpText("Train + Test must add up to 160"),
                           helpText("Defining the hyperparameter grid"),
                           
                           numericInput("svr_pred_poly",
                                        "Choose polynomial degree (between 1 to 2).
                                      Has no effect if kernel is linear",
                                        min = 1, max = 2, value = 2),
                           
                           numericInput("svr_pred_cost",
                                        "Choose Cost factor (between 1 to 5)",
                                        min = 1, max = 5, value = 1),
                           
                           numericInput("svr_pred_e",
                                        "Choose e factor (between 0.1 to 0.5)",
                                        min = 0.1, max = 0.5, value = 0.35),
                           
                           selectInput("svr_pred_ke",
                                       "Select Kernel",
                                       choices = list("linear" = "linear",
                                                      "polynomial" = "polynomial"),
                                       selected = "polynomial"),
                           
                           submitButton("Submit"))
                           
                       )),
  
              # Row 2 - RF Predictions
              fluidRow(box(title = "RF", solidHeader = TRUE,
                           collapsible = TRUE, status = "success",
  
                           plotOutput("rf_pred")),
                       
                       box(title = "Inputs", solidHeader = TRUE,
                           collapsible = TRUE, status = "success",
                           
                           helpText("These inputs control the parameters used to predict cases using
                                    an Random Forest model"),
                           helpText("First, choose Data and train/test split"),
                           
                           selectizeInput("rf_pred_state", "Choose a single state:",
                                          c("USA","Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
                                            "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
                                            "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                                            "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                                            "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                                            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                                            "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                                            "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
                                            "West Virginia", "Wisconsin", "Wyoming"),
                                          options = list(maxItems = 1),
                                          selected = "USA"),
                           
                           numericInput("rf_pred_train",
                                        "Choose size of train data (between 100 to 128)",
                                        value = 128, min = 100, max = 128),
                           
                           numericInput("rf_pred_test",
                                        "Choose size of test data (between 32 to 60)",
                                        value = 32, min = 32, max = 60),
                           
                           helpText("Train + Test must add up to 160"),
                           helpText("Defining the hyperparameter grid"),
                           
                           numericInput("rf_pred_ntrees",
                                        "Choose number of trees (between 5 to 10).",
                                        min = 5, max = 10, value = 5),
                           
                           numericInput("rf_pred_featurefrac",
                                        "Choose feature frac (between 0.5 to 0.75)",
                                        min = 0.5, max = 0.75, value = 0.5),
                           
                           numericInput("rf_pred_minnode",
                                        "Choose min number of nodes (between 2 to 5)",
                                        min = 2, max = 5, value = 2),
                           
                           submitButton("Submit")
                           
                       )),
  
              # Row 3 - KNN Predictions
              fluidRow(box(title = "KNN", solidHeader = TRUE,
                           collapsible = TRUE, status = "warning",
  
                           plotOutput("knn_pred")),
                       
                       box(title = "Inputs", solidHeader = TRUE,
                           collapsible = TRUE, status = "warning",
                           
                           helpText("These inputs control the parameters used to predict cases using
                                    an KNN model"),
                           helpText("First, choose Data and train/test split"),
                           
                           selectizeInput("knn_pred_state", "Choose a single state:",
                                          c("USA","Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
                                            "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
                                            "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                                            "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                                            "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                                            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                                            "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                                            "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
                                            "West Virginia", "Wisconsin", "Wyoming"),
                                          options = list(maxItems = 1),
                                          selected = "USA"),
                           
                           numericInput("knn_pred_train",
                                        "Choose size of train data (between 100 to 128)",
                                        value = 128, min = 100, max = 128),
                           
                           numericInput("knn_pred_test",
                                        "Choose size of test data (between 32 to 60)",
                                        value = 32, min = 32, max = 60),
                           
                           helpText("Train + Test must add up to 160"),
                           helpText("Defining the hyperparameter grid"),
                           
                           numericInput("knn_pred_n",
                                        "Choose number of neighboors (between 2 to 15).",
                                        min = 2, max = 15, value = 2),
                           
                           submitButton("Submit")
                           
                       ))
        )
      )
  )

  
dashboardPage(skin = "black",
  dashboardHeader(title = "COVID-19 Dashboard"),
  sidebar,
  body
)