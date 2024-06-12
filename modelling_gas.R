#### MODELLING GAS
setwd("C:/Users/billo/OneDrive/Desktop/FAU/Thesis/data")

#### library calls ####
ssh <- suppressPackageStartupMessages
ssh(library(timeSeries))
ssh(library(tseries))
ssh(library(aTSA))
ssh(library(forecast))
ssh(library(rugarch))
ssh(library(ModelMetrics))
ssh(library(keras)) # neural networks

source("code/functions.R")


#### IMPORT DATA FROM LOCAL TO AVOID RELOADING EVERYTHING EVERYTIME ####
data_gas <- read_excel("data/GAS_firstdifferenced.xlsx")
data_gas = drop_na(data_gas)

## train-validation-test split 
set.seed(122)
n = nrow(data_gas)
train_proportion <- 0.7
validation_proportion <- 0.10
test_proportion <- 0.20

n_train <- round(train_proportion * n)
n_validation <- round(validation_proportion * n)
n_test <- n - n_train - n_validation

train_data <- data_gas[1:n_train, ]
validation_data <- data_gas[(n_train + 1):(n_train + n_validation), ]
test_data <- data_gas[(n_train + n_validation + 1):n, ]

dim(train_data)
dim(validation_data)
dim(test_data)

## Benchmark models ##
accuracy_table= data.frame(
  col1 = rep(NA, 3) # Placeholder to merge later. Number corresponds to the 7 statistics well include
)

#### ARIMA ####
# Define rolling window size

# Iterate over the rolling window (VALIDATION)####
# arima_model <- auto.arima(train_data$HENRYHUB)
# summary(arima_model)
# rolling_windows(train_data, validation_data, auto.arima, 10)

# # Train
# arima_model <- auto.arima(train_data$HENRYHUB)
# 
# # Validate
# validation_forecast_arima <- predict(arima_model, newdata = validation_data)
# validation_forecast_arima$mean
# validation_accuracy_arima <- accuracy(validation_forecast_arima, validation_data$HENRYHUB[1:10])
# print(validation_accuracy_arima)

# Iterate over the rolling window (TESTING)####

# predictions_arima = rolling_windows(train_data, test_data, auto.arima, 50)

model_arima_GAS = auto.arima(train_data$HENRYHUB)
window_sizes <- seq(24, 60, by = 1)  # Example window sizes
forecast_horizon <- 1  # Number of steps to forecast ahead

# rolling_arima(train_data, 10, 1)
arima_windows_evaluation_gas = evaluate_window_size(validation_data, window_sizes, forecast_horizon, rolling_arima, model_arima_GAS)

# window_size = 10
# n_windows = nrow(test_data) - window_size
# predictions = c()
# for (i in 1:n_windows) {
#   # Define training and validation data
#   # if (i > 7){
#   #   browser()
#   #   
#   # }
#   train_subset <- train_data[i:(i + window_size - 1), ]
#   check_subset <- train_data[(i + window_size), ] # test or train
#   
#   model <- auto.arima(train_subset$HENRYHUB)
#   
#   # Make predictions
#   check_set_x = subset(check_subset, select = -HENRYHUB)
#   new_prediction <- predict(model, newdata = check_set_x)
#   class(new_prediction)
#   predictions = c(predictions, extract_forecast(new_prediction))
# }
# predictions
# 
# predictions

# Evaluate performance 
best_window_size_arima_gas = window_sizes[which.min(rowSums(arima_windows_evaluation_gas))]

predictions_arima_gas = rolling_arima(test_data, best_window_size_arima_gas, forecast_horizon, model_arima_GAS)
check_set_y = subset(test_data, select = HENRYHUB)
check_set_y = check_set_y$HENRYHUB[(best_window_size_arima_gas + 1):length(test_data$HENRYHUB)]

# return error metrics
acc_arima = accuracy(na.omit(predictions_arima_gas), na.omit(check_set_y))

ggplot(data_gas) +
  geom_line(aes(x = date, y = HENRYHUB, color = "Original")) +
  geom_line(data = test_data[-(1:best_window_size_arima_gas),], aes(x = date, y = predictions_arima_gas, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "ARIMA forecast", y = "HENRYHUB (% change)")


# # Testing
# test_forecast_arima <- predict(arima_model, newdata = test_data)
# test_accuracy_arima <- accuracy(test_forecast, test_data$HENRYHUB[1:10])
# print(test_accuracy_arima)

#### Exponential Smoothing ####

# exp_smooth_model <- ets(train_data$HENRYHUB)
# summary(exp_smooth_model)
# # Validate
# 
# rolling_windows(train_data, validation_data, exp_smooth_model , 10)

# Testing

# predictions_ets = rolling_windows(train_data, test_data, ets, 50)

model_ets_GAS = ets(train_data$HENRYHUB)

ets_windows_evaluation_gas = evaluate_window_size(validation_data, window_sizes, forecast_horizon, rolling_ets, model_ets_GAS)

best_window_size_ets = window_sizes[which.min(rowSums(ets_windows_evaluation_gas))]

predictions_ets_gas = rolling_ets(test_data, best_window_size_ets, forecast_horizon, model_ets_GAS)

ggplot(data_gas) +
  geom_line(aes(x = date, y = HENRYHUB, color = "Original")) +
  geom_line(data = test_data[-(1:best_window_size_ets),], aes(x = date, y = predictions_ets_gas, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Exponential Smoothing forecast", y = "HENRYHUB (% change)")

# 
# window_size = 10
# n_windows = nrow(test_data) - window_size
# predictions = c()
# for (i in 1:n_windows) {
#   # Define training and validation data
#   # browser()
#   train_subset <- train_data[i:(i + window_size - 1), ]
#   check_subset <- train_data[(i + window_size), ] # test or train
#   
#   model <- ets(train_subset$HENRYHUB)
#   
#   # Make predictions
#   check_set_x = subset(check_subset, select = -HENRYHUB)
#   new_prediction <- predict(model, newdata = check_set_x)
#   class(new_prediction)
#   predictions = c(predictions, extract_forecast(new_prediction))
# }
# 
# extract_forecast(new_prediction)

# Evaluate performance 
check_set_y = subset(test_data, select = HENRYHUB) 
check_set_y = check_set_y[[1]][(best_window_size_arima_gas + 1):length(test_data$HENRYHUB)]

# return error metrics
acc_ets = accuracy(replace_zero(predictions_ets_gas), replace_zero(check_set_y))


#### No change ####

no_change_forecast <- function(observations){
  most_recent_value = observations[length(observations)]
  nc_vector = rep(most_recent_value, length(observations))
  
  accuracy(nc_vector, observations)
}

acc_no_change = no_change_forecast(na.omit(data_gas$HENRYHUB))


#### Random forests ####
library(randomForest)
names_cols <- names(data_gas)
f <- as.formula(paste("HENRYHUB ~", paste(names_cols[!names_cols %in% c("HENRYHUB", "date")], collapse = " + ")))

# single estimation
install.packages("mlr") # for hyperparameter tuning
library(mlr)
mtry = floor(length(names(data_gas))/3) # for regression problems
rf = randomForest(f, data = data_gas, mtry = mtry)
rf
summary(rf)
importance(rf)
varImpPlot(rf)

#create a task
train_data_rf= train_data
validation_data_rf= validation_data
test_data_rf = test_data
train_data_rf[] <- lapply(train_data, as.numeric)
validation_data_rf[] <- lapply(validation_data, as.numeric)
test_data_rf[] <- lapply(test_data, as.numeric)
rdesc <- makeResampleDesc("CV",iters=5L)
traintask <- makeRegrTask(data = train_data_rf,target = "HENRYHUB") 
validationtask <- makeRegrTask(data = validation_data_rf, target = "HENRYHUB")
testtask <- makeRegrTask(data = test_data_rf,target = "HENRYHUB")

rf.lrn <- makeLearner("regr.randomForest")
rf.lrn$par.vals <- list(ntree = 100L, importance=TRUE)
r <- resample(learner = rf.lrn, task = traintask, resampling = rdesc, measures = list(mae, mse, rmse), show.info = T)

# hyperparameter tuning
getParamSet(rf.lrn)

params <- makeParamSet(makeIntegerParam("mtry",lower = 2,upper = 10),makeIntegerParam("nodesize",lower = 10,upper = 50),
                       makeIntegerParam("ntree",lower = 2,upper = 1000), makeIntegerParam("maxnodes", lower = 2,upper = 500))
#set optimization technique
ctrl <- makeTuneControlRandom(maxit = 5L)
tune <- tuneParams(learner = rf.lrn, task = traintask, 
                   resampling = rdesc, 
                   measures = list(mae, mse, rmse), par.set = params, 
                   control = ctrl, show.info = T)
tune

rf_windows_evaluation = evaluate_window_size(train_data, validation_data, window_sizes, forecast_horizon, rolling_rf, tune)

best_window_size_rf = window_sizes[which.min(rowSums(rf_windows_evaluation))]

predictions_rf = rolling_rf(test_data, validation_data, best_window_size_rf, forecast_horizon, tune)

# Evaluate performance 
check_set_y = subset(test_data, select = HENRYHUB) 
check_set_y = check_set_y$HENRYHUB[(best_window_size_rf + 1):length(test_data$HENRYHUB)]

# return error metrics
acc_rf = accuracy(replace_zero(predictions_rf), replace_zero(check_set_y))


ggplot(data_gas) +
  geom_line(aes(x = date, y = HENRYHUB, color = "Original")) +
  geom_line(data = test_data[-(1:best_window_size_rf),], aes(x = date, y = predictions_rf, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Random Forest forecast", y = "HENRYHUB (% change)")


library(randomForest)
n <- names(data_gas)
f <- as.formula(paste("HENRYHUB ~", paste(n[!n %in% c("HENRYHUB", "day")], collapse = " + ")))
# ml_data = train_data[, !grepl("day", names(train_data))]

# single estimation
f
mtry = floor(length(names(data_gas))/3) # for regression problems
rf = randomForest(f, data = data_gas, mtry = mtry)
rf
importance(rf)
varImpPlot(rf)

# # rw estimation
# rolling_windows(train_data, test_data, rf, 10)

window_size = 50
n_windows = nrow(test_data) - window_size
predictions_rf = c()

for (i in 1:n_windows) {
  # Define training and validation data
  # browser()
  train_subset <- train_data[i:(i + window_size - 1), ]
  check_subset <- train_data[(i + window_size), ] # test or train
  
  mtry = floor(length(names(data_gas))/3) # for regression problems
  model = randomForest(f, data = train_subset, mtry = mtry)
  
  # Make predictions
  check_set_x = subset(check_subset, select = -HENRYHUB)
  new_prediction <- predict(model, newdata = check_set_x)
  class(new_prediction)
  predictions_rf = c(predictions_rf, extract_forecast(new_prediction))
}

# Evaluate performance 
check_set_y = subset(test_data, select = HENRYHUB) 
check_set_y = check_set_y$HENRYHUB[(window_size + 1):length(test_data$HENRYHUB)]

# return error metrics
acc_rf = accuracy(na.omit(predictions_rf), na.omit(check_set_y))


ggplot(data_gas) +
  geom_line(aes(x = date, y = HENRYHUB, color = "Original")) +
  geom_line(data = test_data[-(1:50),], aes(x = date, y = predictions_rf, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Random Forest forecast", y = "HENRYHUB (% change)")


accuracy_table = rbind(acc_arima, acc_ets, acc_no_change, acc_rf)
rownames(accuracy_table) = c("ARIMA", "ETS", "No change", "Random Forest")

create_table_from_df(accuracy_table, "Accuracy measures Benchmark vs ML models - GAS")

#### using caret (reference: https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/) ####

library(randomForest)
library(mlbench)
library(caret)

# Determine the split point
split_point <- floor(0.8 * nrow(data_gas))

# Assign the first 80% to the training set and the remaining 20% to the test set

data_gas$train <- c(rep(TRUE, split_point), rep(FALSE, nrow(data_gas) - split_point))
istrain <- data_gas$train
data_gas$train <- NULL

control <- trainControl(method="repeatedcv", number=5, repeats=3)
seed <- 7
metric <- "RMSE"
set.seed(seed)
mtry <- sqrt(ncol(data_gas) - 1)
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(f, data=data_gas[istrain, ], method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)

predictions_rf2 <- predict(rf_default, newdata =  data_gas[!istrain, ])

ggplot(data_gas) +
  geom_line(aes(x = date, y = HENRYHUB, color = "Original")) +
  geom_line(data = data_gas[!istrain, ], aes(x = date, y = predictions_rf2, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Random Forest forecast", y = "HENRYHUB (% change)")

## compute accuracy
check_set_y =subset(data_gas[!istrain, ], select = HENRYHUB)
check_set_y = check_set_y$HENRYHUB

accuracy_rf_default = c(measureMAE(replace_zero(check_set_y), replace_zero(predictions_rf2)), measureRMSE(replace_zero(check_set_y), replace_zero(predictions_rf2)), measureMAPE(replace_zero(check_set_y), replace_zero(predictions_rf2)))
accuracy_rf_default

varImp(rf_default)
# #### using random Search ####
# control <- trainControl(method="repeatedcv", number=5, repeats=3, search="random")
# set.seed(seed)
# # mtry <- sqrt(ncol(x))
# rf_random <- train(HENRYHUB~., data=data_gas, method="rf", metric=metric, tuneLength=15, trControl=control, tunegrid = expand.grid(mtry = 1:num_features))
# print(rf_random)
# plot(rf_random)
# 
# rf_random$modelType


#### using grid search (reference: https://www.projectpro.io/recipes/tune-hyper-parameters-grid-search-r)####
num_features = ncol(data_gas) + 31 -2 # removing target and date

# specifying the CV technique which will be passed into the train() function later and number parameter is the "k" in K-fold cross validation
train_control = trainControl(method = "cv", number = 5, search = "grid")

set.seed(50)
# Customsing the tuning grid
rfGrid <-  expand.grid(.mtry = (1:num_features))# QUESTION: WHY IS THE RMSE DECREASING AND R SQUARED INCREASING AS I INCLUDE MORE VARIABLES? LIKE THIS THE RANDOM FOREST WILL COLLAPSE INTO A NORMAL TREE MODEL.

# training a random forest tree model while tuning parameters
model_grid = train(f, data = data_gas[istrain,], method = "rf", trControl = train_control, tuneGrid = rfGrid)
print(model_grid)

predictions_rf3 = predict(model_grid, newdata = data_gas[!istrain, ])

ggplot(data_gas) +
  geom_line(aes(x = date, y = HENRYHUB, color = "Original")) +
  geom_line(data = data_gas[!istrain, ], aes(x = date, y = predictions_rf3, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Random Forest forecast (grid search)", y = "HENRYHUB (% change)")

accuracy_rf_grid = c(measureMAE(replace_zero(check_set_y), replace_zero(predictions_rf3)), measureRMSE(replace_zero(check_set_y), replace_zero(predictions_rf3)), measureMAPE(replace_zero(check_set_y), replace_zero(predictions_rf3)))
accuracy_rf_grid

varImp(model_grid)
