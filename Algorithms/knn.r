library(tidyverse)
library(caret)

#creating the dataframe 
#----------------------------
data <- read.csv("C:\\Users\\osbo3\\Dropbox\\data mining\\combined_processed_data.csv")

#summing all the row to make the USA total
#------------------------------------------
data2 = data %>% 
  mutate(US_total = select(., Alabama:Wyoming) %>% rowSums(na.rm = TRUE))

#removing the all the states
#-------------------------------------------------------
datum = subset(data2, select = c(X,able:US_total))

#plotting the original data
#----------------------------------------------
plot.ts(datum$X, datum$US_total)

#creating the time sloces to validate on
#---------------------------------------------------
crtl = trainControl(method = "timeslice", initialWindow = 150, horizon = 10, fixedWindow = FALSE)

#fitting the USA data
#--------------------------------------------------
USA_fit = train(US_total ~. - X, data = datum, method = "knn", trControl=crtl, preProcess = c("center", "scale"), tuneGrid = expand.grid(k = c(2,3,4,5,6,7,8,9,10,15,20)))
USA_fit
plot(USA_fit)

#Creating the OKlahoma data
#---------------------------------------------------------
oklahoma_datum = subset(data2, select = c(X,Oklahoma, able:research))

ok_fit = train(Oklahoma ~. - X, data = oklahoma_datum, method = "knn", trControl=crtl, preProcess = c("center", "scale"), tuneGrid = expand.grid(k = c(2,3,4,5,6,7,8,9,10,15,20)))
ok_fit

plot(ok_fit)


#Creating the Texas data
#---------------------------------------------------------
texas_datum = subset(data2, select = c(X, Texas, able:research))

tx_fit = train(Texas ~. - X, data = texas_datum, method = "knn", trControl=crtl, preProcess = c("center", "scale"), tuneGrid = expand.grid(k = c(2,3,4,5,6,7,8,9,10,15,20)))
tx_fit

plot(tx_fit)

#80% data train 20% test
#-------------------------------------

train_partition <- createDataPartition(y=datum$US_total, p=.8, list=FALSE)
train_data <- datum[train_partition, ]
test_data <- datum[-train_partition, ]


train_crtl = trainControl(method = "timeslice", initialWindow = 110, horizon = 18, fixedWindow = FALSE)
train_fit = train(US_total ~. - X, data = train_data, method = "knn", trControl=train_crtl, preProcess = c("center", "scale"), tuneGrid = expand.grid(k = c(2,3,4,5,6,7,8,9,10,15,20)))
train_fit
plot(train_fit)

prediciton <- predict(train_fit, newdata = test_data)
prediciton


#plotting the data
plot(prediciton, type="l", col="red")
lines(test_data$US_total, col="green")
