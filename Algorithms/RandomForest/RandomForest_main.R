## Load library(s)/Declare statics
# Parallel backend functions
library(foreach)
library(doParallel)
source("./Algorithms/RandomForest/RandomForestFunctions.R")

source("./Algorithms/RandomForest/RandomForestFunctions.R")
states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
            "Connecticut", "Delaware", "District.of.Columbia", "Florida", "Georgia",
            "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
            "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
            "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New.Hampshire",
            "New.Jersey", "New.Mexico", "New.York", "North.Carolina", "North.Dakota",
            "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode.Island", "South.Carolina",
            "South.Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
            "West.Virginia", "Wisconsin", "Wyoming")

## Read data
in.dat <- read.csv("./Data Outputs/combined_processed_data.csv")

## Train the model
# Declare outcome state of interest
Y <- "Alabama"
x.vars <-colnames(in.dat)[which(colnames(in.dat) %in%   states == FALSE)[-1]]
form.val <- as.formula(paste(Y, "~", paste(x.vars, collapse = "+")))

# Declare the model
registerDoParallel(5)
mod.1 <- run_rf(formula=form.val, n_trees = 250, feature_frac = .75, data=in.dat[1:100,], min_node = 3)
mod.1.pred <- pred_new_rf(rf_fit = mod.1, data = in.dat[101:130,])
mod.2 <- run_rf(formula=form.val, n_trees = 250, feature_frac = .75, data=in.dat[1:130,], min_node = 3)
mod.2.pred <- pred_new_rf(rf_fit = mod.2, data=in.dat[131:161,])
