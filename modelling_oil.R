#### MODELLING OIL

setwd("C:/Users/billo/OneDrive/Desktop/FAU/Thesis/data/data")

install.packages("mlr") # for hyperparameter tuning
#### library calls ####
ssh <- suppressPackageStartupMessages
ssh(library(timeSeries))
ssh(library(tseries))
ssh(library(aTSA))
ssh(library(forecast))
ssh(library(rugarch))
ssh(library(ModelMetrics))
ssh(library(keras)) # neural networks
ssh(library(readxl))
ssh(library(writexl))
ssh(library(tidyr))
ssh(library(ModelMetrics))
library(ggplot2)
library(mlr)

source("code/functions.R")


#### IMPORT DATA FROM LOCAL TO AVOID RELOADING EVERYTHING EVERYTIME ####
data_oil <- read_excel("data/OIL_firstdifferenced.xlsx")
data_oil = drop_na(data_oil)

#### train-validation-test split ####
set.seed(122)
n = nrow(data_oil)
train_proportion <- 0.7
validation_proportion <- 0.10
test_proportion <- 0.20

n_train <- round(train_proportion * n)
n_validation <- round(validation_proportion * n)
n_test <- n - n_train - n_validation

train_data <- data_oil[1:n_train, ]
validation_data <- data_oil[(n_train + 1):(n_train + n_validation), ]
test_data <- data_oil[(n_train + n_validation + 1):n, ]

dim(train_data)
dim(validation_data)
dim(test_data)


#### ARIMA ####

## training
model_arima = auto.arima(train_data$WTI) # automatically selects the best model
window_sizes <- seq(24, 60, by = 1)  # range of window sizes to try out
forecast_horizon <- 1  # Number of steps to forecast ahead

## validation
# selecting the best performing window size
arima_windows_evaluation = evaluate_window_size(train_data, validation_data, window_sizes, forecast_horizon, rolling_arima, model = model_arima)
best_window_size_arima = window_sizes[which.min(rowSums(arima_windows_evaluation))]

## testing
predictions_arima = rolling_arima(test_data, best_window_size_arima, forecast_horizon, model_arima)
# Evaluate performance 
check_set_y = subset(test_data, select = WTI) 
check_set_y = check_set_y$WTI[(best_window_size_arima + 1):length(test_data$WTI)]
acc_arima = accuracy(replace_zero(predictions_arima), replace_zero(check_set_y)) ###? why Nans and Inf?

ggplot(data_oil) +
  geom_line(aes(x = date, y = WTI, color = "Original")) +
  geom_line(data = test_data[-(1:best_window_size_arima),], aes(x = date, y = predictions_arima, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "ARIMA forecast", y = "WTI (% change)")




#### EXPONENTIAL SMOOTHING ####

## training
model_ets = ets(train_data$WTI) # also automatically selects the best alpha parameter

## validation
ets_windows_evaluation = evaluate_window_size(train_data, validation_data, window_sizes, forecast_horizon, rolling_ets, model_ets)

best_window_size_ets = window_sizes[which.min(rowSums(ets_windows_evaluation))]

## testing
predictions_ets = rolling_ets(test_data, best_window_size_ets, forecast_horizon, model_ets)

# Evaluate performance 
check_set_y = check_set_y$WTI[(best_window_size_ets + 1):length(test_data$WTI)]

# return error metrics
acc_ets = accuracy(replace_zero(predictions_ets), replace_zero(check_set_y))


ggplot(data_oil) +
  geom_line(aes(x = date, y = WTI, color = "Original")) +
  geom_line(data = test_data[-(1:best_window_size_ets),], aes(x = date, y = predictions_ets, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Exponential Smoothing forecast", y = "WTI (% change)")



#### NO CHANGE FORECAST ####

no_change_forecast <- function(observations){
  most_recent_value = observations[length(observations)]
  nc_vector = rep(most_recent_value, length(observations))
  
  accuracy(replace_zero(nc_vector), replace_zero(observations))
}

acc_no_change = no_change_forecast(na.omit(data_oil$WTI))




#### RANDOM FORESTS ####
library(randomForest)
library(mlr)

n <- names(data_oil)
f <- as.formula(paste("WTI ~", paste(n[!n %in% c("WTI", "date")], collapse = " + ")))

# single estimation
mtry = floor(length(names(data_oil))/3) # for regression problems
rf = randomForest(f, data = data_oil, mtry = mtry)
rf
summary(rf)
importance(rf)
varImpPlot(rf)

#### using the tools from mlr package to validate the model ####
train_data_rf= train_data
validation_data_rf= validation_data
test_data_rf = test_data
train_data_rf[] <- lapply(train_data, as.numeric)
validation_data_rf[] <- lapply(validation_data, as.numeric)
test_data_rf[] <- lapply(test_data, as.numeric)
rdesc <- makeResampleDesc("CV",iters=5L)
traintask <- makeRegrTask(data = train_data_rf,target = "WTI") 
validationtask <- makeRegrTask(data = validation_data_rf, target = "WTI")
testtask <- makeRegrTask(data = test_data_rf,target = "WTI")

rf.lrn <- makeLearner("regr.randomForest")
rf.lrn$par.vals <- list(ntree = 100L, importance=TRUE)
r <- resample(learner = rf.lrn, task = traintask, resampling = rdesc, measures = list(mae, mse, rmse), show.info = T)

# hyperparameter tuning
getParamSet(rf.lrn)

params <- makeParamSet(makeIntegerParam("mtry",lower = 2,upper = 10),makeIntegerParam("nodesize",lower = 10,upper = 50),
                       makeIntegerParam("ntree",lower = 2,upper = 500), makeIntegerParam("maxnodes", lower = 2,upper = 250))
#set optimization technique
ctrl <- makeTuneControlRandom(maxit = 5L)
tune <- tuneParams(learner = rf.lrn, task = traintask, 
                   resampling = rdesc, 
                   measures = list(mae, mse, rmse), par.set = params, 
                   control = ctrl, show.info = T)
tune
rf_windows_evaluation = evaluate_window_size(tr_data = train_data, val_data=validation_data, window_sizes = window_sizes, forecast_horizon = forecast_horizon, func = rolling_rf, model = tune)

best_window_size_rf = window_sizes[which.min(rowSums(rf_windows_evaluation))]

predictions_rf = rolling_rf(data = test_data, window_size = best_window_size_rf, forecast_horizon = forecast_horizon, model = tune)


# Evaluate performance 
check_set_y = subset(test_data, select = WTI) 
check_set_y = check_set_y$WTI[(best_window_size_rf + 1):length(test_data$WTI)]

# return error metrics
acc_rf = accuracy(replace_zero(predictions_rf), replace_zero(check_set_y))


ggplot(data_oil) +
  geom_line(aes(x = date, y = WTI, color = "Original")) +
  geom_line(data = test_data[-(1:best_window_size_rf),], aes(x = date, y = predictions_rf, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Random Forest forecast (various hyperparameters tuning)", y = "WTI (% change)")

#### using caret (reference: https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/) ####

library(randomForest)
library(mlbench)
library(caret)


# Create model with default paramters
# Determine the split point
split_point <- floor(0.8 * nrow(data_oil))

# Assign the first 80% to the training set and the remaining 20% to the test set

data_oil$train <- c(rep(TRUE, split_point), rep(FALSE, nrow(data_oil) - split_point))
istrain <- data_oil$train
data_oil$train <- NULL

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "RMSE"
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(WTI ~., data=data_oil, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)


predictions_rf2 <- predict(rf_default, newdata =  validation_data)

ggplot(data_oil) +
  geom_line(aes(x = date, y = WTI, color = "Original")) +
  geom_line(data = test_data[-(1:125),], aes(x = date, y = predictions_rf2, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Random Forest forecast", y = "WTI (% change)")

#### using random Search ####
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(WTI~., data=data_oil, method="rf", metric=metric, tuneLength=15, trControl=control, tunegrid = expand.grid(mtry = 1:num_features))
print(rf_random)
plot(rf_random)

rf_random$modelType


#### using grid search (reference: https://www.projectpro.io/recipes/tune-hyper-parameters-grid-search-r)####
num_features = ncol(data_oil) - 1 # removing target and date

# specifying the CV technique which will be passed into the train() function later and number parameter is the "k" in K-fold cross validation
train_control = trainControl(method = "cv", number = 5, search = "grid")

set.seed(50)
# Customsing the tuning grid
rfGrid <-  expand.grid(mtry = (1:num_features)) # QUESTION: WHY IS THE RMSE DECREASING AND R SQUARED INCREASING AS I INCLUDE MORE VARIABLES? LIKE THIS THE RANDOM FOREST WILL COLLAPSE INTO A NORMAL TREE MODEL.

# training a random forest tree model while tuning parameters
model = train(WTI~., data = data_oil[istrain,], method = "rf", trControl = train_control, tuneGrid = rfGrid)

# summarising the results
print(model)

rf_windows_evaluation = evaluate_window_size(val_data = validation_data, window_sizes = window_sizes, forecast_horizon = forecast_horizon, func = rolling_rf, model = model_gridsearch)
best_window_size_rf = window_sizes[which.min(rowSums(rf_windows_evaluation))]
predictions_rf = rolling_rf(data = test_data, window_size = best_window_size_rf, forecast_horizon = forecast_horizon, model = model_gridsearch)

ggplot(data_oil) +
  geom_line(aes(x = date, y = WTI, color = "Original")) +
  geom_line(data = test_data[-(1:best_window_size_rf),], aes(x = date, y = predictions_rf, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Random Forest forecast (Grid search)", y = "WTI (% change)")

# QUESTION: SAME 