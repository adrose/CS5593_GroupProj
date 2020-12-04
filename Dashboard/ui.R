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
          h4("Placeholder for brief tab description"),
          fluidPage(
            # SVR Model
            fluidRow(box(title = "Support Vector Regression", solidHeader = TRUE,
                         collapsible = TRUE, status = "primary",
                         
                         tableOutput("svr_cross")),
                         # plotOutput("svr_hist")),
                     
                     box(title = "Inputs", solidHeader = TRUE,
                         collapsible = TRUE, status = "primary",
                         
                         helpText("Placeholder text for SVR Cross"),
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
                         
                         helpText("Placeholder text for RF Cross. Only the first 5 parameters are used to showcase the model,
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
                         
                         helpText("Placeholder text for KNN Cross. Only the first 5 parameters are used to showcase the model,
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
          h4("Placeholder for brief tab description"),
          fluidPage(
            # Row 1
            fluidRow(),
            # Row 2
            fluidRow()
      )
    )
  )
)
  
dashboardPage(skin = "black",
  dashboardHeader(title = "COVID-19 Dashboard"),
  sidebar,
  body
)