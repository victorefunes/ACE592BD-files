setwd("C:/Users/victorf2/Box/ACE 592 BD/Lab 5")

ipak <- function(pkg){
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("rgdal", "sp", "xgboost")
ipak(packages)

# Load the data from /home/zhang303/Apartments
# Here, we read the data crawled from apartments on 2017-06-13 (rents-2017-06-13.csv)
# Raw is a dataframe that consists of 305278 observations.
raw <- read.csv("rents_2017-06-13.csv")

# Grab all the data that meets the criteria
data = raw[raw$state == "IL" & raw$city == "Champaign",]
states = "IL"

data[4,]

nrow(data)
ncol(data)
rm(raw)

# A function that translates a state into a code.
# For example, codeForState(IL) = 17
codeForState <- function (state_name) {
  if (!exists('stateCodeMap')) {
    stateCodeMap = read.csv('state_code_map.csv')
  }
  code = stateCodeMap[which(stateCodeMap$state==state_name),"code"]
  if (nchar(code) < 2) {
    code = paste0("0",code)
  }
  return(code)
}

# example
codeForState("IL")

# get POP data for Illinois
getPOPDataForState <- function (data,state_name) {
  # Get layer name & path
  layer_name = paste0("tl_2016_",codeForState(state_name),"_tabblock10")
  shape_object_path = paste0(codeForState(state_name),"_shp.rds")
  
  if (!file.exists(shape_object_path)) {
    return(NA)
  }
  
  # Grab just the rows for the state we're processing
  state_subset = subset(data,state==state_name)
  
  # If there are no rows for this state, do nothing
  if(dim(state_subset)[1]==0) {
    return(NA)
  } else {
    points_raw = state_subset
    points_raw$lat = as.numeric(points_raw$lat)
    points_raw$lon = as.numeric(points_raw$lon)
      
    source("PointsOverPolygons.R",local = TRUE)
    if (!exists("aug_points")) {
      stop("PointsOverPolygons.R failed to create membership data.")
    }
  }
  return(aug_points)
}

# usage in the next cell
# example output in the 3 cells later

# In a real project we run every state in the U.S., but here we only run Illinois.
# Don't worry about the warning messages
aug_data_by_state = lapply("IL",function(state_name) {
  print(paste0("Working on ",state_name,"..."))
    
  if (state_name == "") {
    return(NA)
  }
    
  aug_data = getPOPDataForState(data,state_name)
    
  return(aug_data)
})

# Remove any states that had no rows in them.
aug_data_by_state_no_NAs = aug_data_by_state[which(!is.na(aug_data_by_state))]
aug_data = Reduce(rbind,aug_data_by_state_no_NAs,aug_data_by_state_no_NAs[[1]][c(),])

aug_data[4,]

# a function that removes useless variables and categorizes by square ft
cleanChunk = function (chunk_old) {

  chunk=data.frame(matrix(nrow=nrow(chunk_old),ncol=0))
    
  #chunk$id <- chunk_old$csv_id
  chunk$Lat_Num <- as.numeric(chunk_old$lat)
  chunk$Lon_Num <- as.numeric(chunk_old$lon)
  chunk$Sqft_Num <- as.numeric(chunk_old$sqft)
  chunk$Score_Num <- as.numeric(chunk_old$score)
  chunk_old$rent1 <- sub("$",'',as.character(chunk_old$rent),fixed=TRUE)
  chunk$Rent_Num <- as.numeric(sub(",",'',as.character(chunk_old$rent1),fixed=TRUE))
  chunk$Zip_Fac <- as.factor(chunk_old$zipcode)
  chunk$Beds_Num <- as.numeric(chunk_old$beds)
  chunk$Baths_Num <- as.numeric(chunk_old$baths)
  chunk$Beds_Fac <- as.factor(chunk_old$beds)
  chunk$Baths_Fac <- as.factor(chunk_old$baths)
  chunk$DateAdded_Str <- as.character(chunk_old$Date.Added)

  chunk$State_Fac <- as.factor(chunk_old$STATEFP10)
  chunk$Tract_Fac <- as.factor(chunk_old$TRACTCE10)
  chunk$Block_Fac <- as.factor(chunk_old$BLOCKCE10)
  chunk$Date_Fac <- as.factor(chunk_old$Date.Added)

  chunk$Sqft0500 <- as.factor(chunk$Sqft_Num<500)
  chunk$Sqft500600 <- as.factor(chunk$Sqft_Num>=500 & chunk$Sqft_Num<600)
  chunk$Sqft600700 <- as.factor(chunk$Sqft_Num>=600 & chunk$Sqft_Num<700)
  chunk$Sqft700800 <- as.factor(chunk$Sqft_Num>=700 & chunk$Sqft_Num<800)
  chunk$Sqft800900 <- as.factor(chunk$Sqft_Num>=800 & chunk$Sqft_Num<900)
  chunk$Sqft9001000 <- as.factor(chunk$Sqft_Num>=900 & chunk$Sqft_Num<1000)
  chunk$Sqft10001100 <- as.factor(chunk$Sqft_Num>=1000 & chunk$Sqft_Num<1100)
  chunk$Sqft11001200 <- as.factor(chunk$Sqft_Num>=1100 & chunk$Sqft_Num<1200)
  chunk$Sqft12001300 <- as.factor(chunk$Sqft_Num>=1200 & chunk$Sqft_Num<1300)
  chunk$Sqft13001400 <- as.factor(chunk$Sqft_Num>=1300 & chunk$Sqft_Num<1400)
  chunk$Sqft14001500 <- as.factor(chunk$Sqft_Num>=1400 & chunk$Sqft_Num<1500)
  chunk$Sqft15001600 <- as.factor(chunk$Sqft_Num>=1500 & chunk$Sqft_Num<1600)
  chunk$Sqft16001700 <- as.factor(chunk$Sqft_Num>=1600 & chunk$Sqft_Num<1700)
  chunk$Sqft17001800 <- as.factor(chunk$Sqft_Num>=1700 & chunk$Sqft_Num<1800)
  chunk$Sqft18001900 <- as.factor(chunk$Sqft_Num>=1800 & chunk$Sqft_Num<1900)
  chunk$Sqft19002000 <- as.factor(chunk$Sqft_Num>=1900 & chunk$Sqft_Num<2000)
  chunk$Sqft2000 <- as.factor(chunk$Sqft_Num>=2000)
  
  chunk$Sqft20002100 <- as.factor(chunk$Sqft_Num>=2000 & chunk$Sqft_Num<2100)
  chunk$Sqft21002200 <- as.factor(chunk$Sqft_Num>=2100 & chunk$Sqft_Num<2200)
  chunk$Sqft22002300 <- as.factor(chunk$Sqft_Num>=2200 & chunk$Sqft_Num<2300)
  chunk$Sqft23002400 <- as.factor(chunk$Sqft_Num>=2300 & chunk$Sqft_Num<2400)
  chunk$Sqft24002500 <- as.factor(chunk$Sqft_Num>=2400 & chunk$Sqft_Num<2500)
  chunk$Sqft25002600 <- as.factor(chunk$Sqft_Num>=2500 & chunk$Sqft_Num<2600)
  chunk$Sqft26002700 <- as.factor(chunk$Sqft_Num>=2600 & chunk$Sqft_Num<2700)
  chunk$Sqft27002800 <- as.factor(chunk$Sqft_Num>=2700 & chunk$Sqft_Num<2800)
  chunk$Sqft28002900 <- as.factor(chunk$Sqft_Num>=2800 & chunk$Sqft_Num<2900)
  chunk$Sqft29003000 <- as.factor(chunk$Sqft_Num>=2900 & chunk$Sqft_Num<3000)
  chunk$Sqft3000 <- as.factor(chunk$Sqft_Num>=3000)
  chunk$Sqft2000 <- as.factor(chunk$Sqft_Num>=2000)
  chunk$Sqft01000 <- as.factor(chunk$Sqft_Num<1000)
  
  chunk$Location_GeoJSON = data.frame(matrix(NA,nrow=nrow(chunk_old),ncol=2,dimnames = list(c(),c("type","coordinates"))))
  chunk$Location_GeoJSON$coordinates = as.list(data.frame(apply(chunk_old[c("lat","lon")],1,function(f){
    return(c(as.numeric(f["lon"]),as.numeric(f["lat"])))
  })))
    
  chunk$Location_GeoJSON$type = "Point"
  
  chunk <- subset(chunk, Rent_Num!="NA")

  print("finished")
  return(subset(chunk, Rent_Num <= 5035))
}

clean <- cleanChunk(aug_data)
# It's fine if you see the folling warning message
# Warning message in cleanChunk(aug_data):
# “NAs introduced by coercion”

## Setting 80% as train and 20% as test
sample_size <- floor(0.8 * nrow(clean))

## Set the seed to make your partition reproducible
set.seed(123)

train_ind <- sample(seq_len(nrow(clean)), size = sample_size)

train <- clean[train_ind,]
test <- clean[-train_ind,]

## Print first 5 rows of the train data.
print(train[1:5,])

# Import library
library(xgboost)

# Get rid of Location_GeoJSON because we don't need them
train_no_json = train[,!names(train) %in% c("Location_GeoJSON")]
test_no_json = test[,!names(test) %in% c("Location_GeoJSON")]

# Change type of DateAdded_Str to Date
train_no_json$DateAdded_Str <- as.Date(train_no_json$DateAdded_Str, format = "%m-%d-%Y")
test_no_json$DateAdded_Str <- as.Date(test_no_json$DateAdded_Str, format = "%m-%d-%Y")

# From columns 14 to 42, if the value is true then change it to 1, otherwise 0
train_no_json[, 14:42] <- as.integer(train_no_json[, 14:42] == "TRUE")
test_no_json[, 14:42] <- as.integer(test_no_json[, 14:42] == "TRUE")

# Store the rent we are going to predict and delete it from the train data set
output_vector = train_no_json[,"Rent_Num"]
train_no_json = train_no_json[,!names(train_no_json) %in% c("Rent_Num")]

# Store the rent we are going to predict and delete it from the test data set
true_vector = test_no_json[, "Rent_Num"]
test_no_json = test_no_json[,!names(test_no_json) %in% c("Rent_Num")]

# Change to matrix because XGBoost takes matrices
train_numeric <- data.matrix(train_no_json)
test_numeric <- data.matrix(test_no_json)

dtrain <- xgb.DMatrix(data = train_numeric, label = output_vector)
dtest <- xgb.DMatrix(data = test_numeric, label = true_vector)

# We can see the rmse of train and test via using watchlist parameter 
watchlist <- list(train = dtrain, test = dtest)

# data: training data
# max.depth: maximum depth of the tree
# eta: step size
# gamma: minimum loss reduction
# subsample: subsample ratio of the training instance
# nround: number of rounds
# watchlist: watchlist
# objective: regression or classification
model <- xgb.train(data = dtrain,
                   max.depth = 300, 
                   eta = 0.1, 
                   gamma = 0.01,
                   subsample = 0.5,
                   nround = 50,
                   watchlist = watchlist, 
                   print_every_n = 10,
                   objective = "reg:linear")

# xgb.importance is a function that calculates the importance of each variable
importance_matrix <- xgb.importance(feature_names = colnames(train_numeric), model = model)
print(importance_matrix)

# Now we can start predicting the testing set and visualize the result.
model$pred <- predict(model, test_numeric)
test$res <- test$Rent_Num - model$pred

# visualize
library(ggplot2)
name <- "xgboost"
ggplot() + ggtitle(paste(name,".png",sep="")) +
      labs(x="Rent", y="Residuals") +
      geom_point(aes(test$Rent_Num, test$res),
                 colour='blue', fill = "blue") +
      geom_smooth(aes(test$Rent_Num, test$res), se=TRUE, fullrange=FALSE,
                  colour='firebrick1', fill = "firebrick1")


# eta = 0.5
model2 <- xgb.train(data = dtrain,
                   max.depth = 300, 
                   eta = 0.5, 
                   gamma = 0.01,
                   subsample = 0.5,
                   nround = 50,
                   watchlist = watchlist, 
                   print_every_n = 10,
                   objective = "reg:linear")

# xgb.importance is a function that calculates the importance of each variable
importance_matrix <- xgb.importance(feature_names = colnames(train_numeric), model = model2)
print(importance_matrix)

# Now we can start predicting the testing set and visualize the result.
model2$pred <- predict(model2, test_numeric)
test$res <- test$Rent_Num - model2$pred

ggplot() + ggtitle(paste(name,".png",sep="")) +
  labs(x="Rent", y="Residuals") +
  geom_point(aes(test$Rent_Num, test$res),
             colour='blue', fill = "blue") +
  geom_smooth(aes(test$Rent_Num, test$res), se=TRUE, fullrange=FALSE,
              colour='firebrick1', fill = "firebrick1")

# eta = 0.05
model3 <- xgb.train(data = dtrain,
                    max.depth = 300, 
                    eta = 0.05, 
                    gamma = 0.01,
                    subsample = 0.5,
                    nround = 50,
                    watchlist = watchlist, 
                    print_every_n = 10,
                    objective = "reg:linear")

# xgb.importance is a function that calculates the importance of each variable
importance_matrix <- xgb.importance(feature_names = colnames(train_numeric), model = model3)
print(importance_matrix)

# Now we can start predicting the testing set and visualize the result.
model3$pred <- predict(model3, test_numeric)
test$res <- test$Rent_Num - model3$pred

ggplot() + ggtitle(paste(name,".png",sep="")) +
  labs(x="Rent", y="Residuals") +
  geom_point(aes(test$Rent_Num, test$res),
             colour='blue', fill = "blue") +
  geom_smooth(aes(test$Rent_Num, test$res), se=TRUE, fullrange=FALSE,
              colour='firebrick1', fill = "firebrick1")


# max depth = 900
model4 <- xgb.train(data = dtrain,
                    max.depth = 900, 
                    eta = 0.1, 
                    gamma = 0.01,
                    subsample = 0.5,
                    nround = 50,
                    watchlist = watchlist, 
                    print_every_n = 10,
                    objective = "reg:linear")

# xgb.importance is a function that calculates the importance of each variable
importance_matrix <- xgb.importance(feature_names = colnames(train_numeric), model = model4)
print(importance_matrix)

# Now we can start predicting the testing set and visualize the result.
model4$pred <- predict(model4, test_numeric)
test$res <- test$Rent_Num - model4$pred

ggplot() + ggtitle(paste(name,".png",sep="")) +
  labs(x="Rent", y="Residuals") +
  geom_point(aes(test$Rent_Num, test$res),
             colour='blue', fill = "blue") +
  geom_smooth(aes(test$Rent_Num, test$res), se=TRUE, fullrange=FALSE,
              colour='firebrick1', fill = "firebrick1")

# max depth = 2700
model5 <- xgb.train(data = dtrain,
                    max.depth = 2700, 
                    eta = 0.1, 
                    gamma = 0.01,
                    subsample = 0.5,
                    nround = 50,
                    watchlist = watchlist, 
                    print_every_n = 10,
                    objective = "reg:linear")

# xgb.importance is a function that calculates the importance of each variable
importance_matrix <- xgb.importance(feature_names = colnames(train_numeric), model = model5)
print(importance_matrix)

# Now we can start predicting the testing set and visualize the result.
model5$pred <- predict(model5, test_numeric)
test$res <- test$Rent_Num - model5$pred

ggplot() + ggtitle(paste(name,".png",sep="")) +
  labs(x="Rent", y="Residuals") +
  geom_point(aes(test$Rent_Num, test$res),
             colour='blue', fill = "blue") +
  geom_smooth(aes(test$Rent_Num, test$res), se=TRUE, fullrange=FALSE,
              colour='firebrick1', fill = "firebrick1")