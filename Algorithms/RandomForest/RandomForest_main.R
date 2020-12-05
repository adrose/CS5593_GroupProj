## Clear cache
rm(list=ls())

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
Y <- "Colorado"
x.vars <-colnames(in.dat)[which(colnames(in.dat) %in%   states == FALSE)[-1]]
form.val <- as.formula(paste(Y, "~", paste(x.vars, collapse = "+")))

## Now run all of the states
## Create a function which will run the model given the states
run.mod <- function(inState, n_trees=25, feature_frac=.5, data=in.dat, min_node=3){
  ## First declare the model
  x.vars <-colnames(in.dat)[which(colnames(in.dat) %in%   states == FALSE)[-1]]
  form.val <- as.formula(paste(inState, "~", paste(x.vars, collapse = "+")))
  
  ## Now train the model
  mod.out <- run_rf(formula=form.val, n_trees = n_trees, feature_frac = feature_frac, data=data, min_node=min_node)
  ## Return the model
  return(mod.out)
}
states <- as.list(states)
n_trees <- c(15, 25, 50)
feature_fracs <- c(.25, .50,.75)
min_node <- c(5,10)
all.perm <-expand.grid(n_trees, feature_fracs, min_node)
for(i in 2:nrow(all.perm)){
  n_tree <- all.perm$Var1[i]
  feature_frac <- all.perm$Var2[i]
  min_node <- all.perm$Var3[i]
  all_states1 <- mclapply(states, function(x)run.mod(inState = x, data=in.dat[1:40,], n_trees = n_tree, feature_frac = feature_frac,min_node = min_node), mc.cores = 4)
  all_states2 <- mclapply(states, function(x)run.mod(inState = x, data=in.dat[1:80,], n_trees = n_tree, feature_frac = feature_frac,min_node = min_node), mc.cores = 4)
  all_states3 <- mclapply(states, function(x)run.mod(inState = x, data=in.dat[1:120,], n_trees= n_tree, feature_frac = feature_frac,min_node = min_node), mc.cores = 4)
  ## Now validate them
  all_pred1 <- mclapply(all_states1, function(x) pred_new_rf(x, in.dat[41:80,]), mc.cores=6)
  all_pred2 <- mclapply(all_states1, function(x) pred_new_rf(x, in.dat[81:120,]), mc.cores=6)
  all_pred3 <- mclapply(all_states1, function(x) pred_new_rf(x, in.dat[121:160,]), mc.cores=6)
  cor_vals = NULL
  for(i in 1:length(all_pred1)){
    # Pred vals
    all_pred_vals <- c(all_pred1[[i]], all_pred2[[i]], all_pred3[[i]])
    ## All target vals
    targ.vals <- in.dat[41:160,i+1]
    #Now return the cor
    cor_vals <- c(cor_vals, cor(all_pred_vals, targ.vals))
  }
  cor_vals <- cbind(states, cor_vals)
  # Now write these
  out_name <- paste("ntree_", n_tree, "feature_frac_", feature_frac, "min_node_", min_node, ".csv", sep='')
  write.csv(cor_vals, file = out_name)
}
