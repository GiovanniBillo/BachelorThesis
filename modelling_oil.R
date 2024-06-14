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

#### global ####
window_sizes <- seq(30, 100, by = 10)  # range of window sizes to try out
forecast_horizon <- 1  # Number of steps to forecast ahead
accuracy_metrics <- data.frame(
  Model = c(),
  MSE = c(),
  RMSE = c(),
  MAPE = c()
)


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
### messing around
predictions_arima = rolling_arima(train_data, validation_data, test_data, window_sizes, forecast_horizon)
check_set_y = test_data[, 2]
check_set_y = check_set_y[[1]]

plot_arima = ggplot(data_oil) +
  geom_line(aes(x = date, y = WTI, color = "Original")) +
  geom_line(data = test_data, aes(x = date, y = predictions_arima, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "ARIMA forecast", y = "WTI (% change)")
ggsave(filename = "plot_rf_arima.png", plot = plot_arima, width = 6, height = 4, dpi = 300)

ARIMA = c(measureMAE(replace_zero(check_set_y), replace_zero(predictions_arima)), measureRMSE(replace_zero(check_set_y), replace_zero(predictions_arima)), measureMAPE(replace_zero(check_set_y), replace_zero(predictions_arima)))
ARIMA

### OLDarima ####
## training
model_arima = auto.arima(train_data$WTI) # automatically selects the best model

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
  geom_line(data = test_data, aes(x = date, y = predictions_arima, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "ARIMA forecast", y = "WTI (% change)")

#### EXPONENTIAL SMOOTHING ####

## messing around ##
## training, validation and test 
predictions_ets = rolling_ets(train_data, validation_data, test_data, window_sizes, forecast_horizon)

plot_ets = ggplot(data_oil) +
  geom_line(aes(x = date, y = WTI, color = "Original")) +
  geom_line(data = test_data, aes(x = date, y = predictions_ets, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Exponential Smoothing forecast", y = "WTI (% change)")
ggsave(filename = "plot_ets.png", plot = plot_ets, width = 6, height = 4, dpi = 300)

ETS = c(measureMAE(replace_zero(check_set_y), replace_zero(predictions_ets)), measureRMSE(replace_zero(check_set_y), replace_zero(predictions_ets)), measureMAPE(replace_zero(check_set_y), replace_zero(predictions_ets)))
ETS

# old ets ####
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
  
  return(nc_vector)
}

no_change_forecast=no_change_forecast(test_data[[2]])

plot_nochange = ggplot(data_oil) +
  geom_line(aes(x = date, y = WTI, color = "Original")) +
  geom_line(data = test_data, aes(x = date, y = no_change_forecast, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "No change forecast", y = "WTI (% change)")
ggsave(filename = "plot_nochange.png", plot = plot_nochange, width = 6, height = 4, dpi = 300)

NO_CHANGE = c(measureMAE(replace_zero(check_set_y), replace_zero(no_change_forecast)), measureRMSE(replace_zero(check_set_y), replace_zero(no_change_forecast)), measureMAPE(replace_zero(check_set_y), replace_zero(no_change_forecast)))
NO_CHANGE

accuracy_benchmark = data.frame(ARIMA, ETS, NO_CHANGE)
rownames(accuracy_benchmark) = c('MAE', 'RMSE', 'MAPE')



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
mtry <- sqrt(ncol(data_oil) - 1)
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(WTI ~., data=data_oil[istrain, ], method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)

predictions_rf2 <- predict(rf_default, newdata =  data_oil[!istrain, ])

plot_rf_default = ggplot(data_oil) +
  geom_line(aes(x = date, y = WTI, color = "Original")) +
  geom_line(data = data_oil[!istrain, ], aes(x = date, y = predictions_rf2, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Random Forest forecast (default)", y = "WTI (% change)")
ggsave(filename = "plot_rf_default.png", plot = plot_rf_default, width = 6, height = 4, dpi = 300)

## compute accuracy
check_set_y = as.numeric(subset(data_oil[!istrain, ], select = WTI))
check_set_y = check_set_y$WTI

RANDOM_FOREST_DEFAULT = c(measureMAE(replace_zero(check_set_y), replace_zero(predictions_rf2)), measureRMSE(replace_zero(check_set_y), replace_zero(predictions_rf2)), measureMAPE(replace_zero(check_set_y), replace_zero(predictions_rf2)))
RANDOM_FOREST_DEFAULT

# #### using random Search ####
# control <- trainControl(method="repeatedcv", number=5, repeats=3, search="random")
# set.seed(seed)
# # mtry <- sqrt(ncol(x))
# rf_random <- train(WTI~., data=data_oil, method="rf", metric=metric, tuneLength=15, trControl=control, tunegrid = expand.grid(mtry = 1:num_features))
# print(rf_random)
# plot(rf_random)
# 
# rf_random$modelType


#### using grid search (reference: https://www.projectpro.io/recipes/tune-hyper-parameters-grid-search-r)####
num_features = ncol(data_oil) - 1 +  31

# specifying the CV technique which will be passed into the train() function later and number parameter is the "k" in K-fold cross validation
train_control = trainControl(method = "cv", number = 5, search = "random")

set.seed(50)
# Customsing the tuning grid
rfGrid <-  expand.grid(.mtry = (1:num_features))# QUESTION: WHY IS THE RMSE DECREASING AND R SQUARED INCREASING AS I INCLUDE MORE VARIABLES? LIKE THIS THE RANDOM FOREST WILL COLLAPSE INTO A NORMAL TREE MODEL.

# training a random forest tree model while tuning parameters
model_grid = train(WTI~., data = data_oil[istrain,], method = "rf", trControl = train_control, tuneGrid = rfGrid)
print(model_grid)

predictions_rf3 = predict(model_grid, newdata = data_oil[!istrain, ])

plot_rf_random = ggplot(data_oil) +
  geom_line(aes(x = date, y = WTI, color = "Original")) +
  geom_line(data = data_oil[!istrain, ], aes(x = date, y = predictions_rf3, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Random Forest forecast (grid search)", y = "WTI (% change)")

ggsave(filename = "plot_rf_random.png", plot = plot_rf_random, width = 6, height = 4, dpi = 300)
RANDOM_FOREST_RANDOM = c(measureMAE(replace_zero(check_set_y), replace_zero(predictions_rf3)), measureRMSE(replace_zero(check_set_y), replace_zero(predictions_rf3)), measureMAPE(replace_zero(check_set_y), replace_zero(predictions_rf3)))
RANDOM_FOREST_RANDOM

accuracy_ml = data.frame(RANDOM_FOREST_DEFAULT, RANDOM_FOREST_RANDOM)
rownames(accuracy_ml) = c('MAE', 'RMSE', 'MAPE')

cbind(accuracy_benchmark, accuracy_ml)

## rolling windows evaluation ##

rf_windows_evaluation = evaluate_window_size(val_data = validation_data, window_sizes = window_sizes, forecast_horizon = forecast_horizon, func = rolling_rf, model = model_grid)
best_window_size_rf = window_sizes[which.min(rowSums(rf_windows_evaluation))]
predictions_rf = rolling_rf(data = test_data, window_size = best_window_size_rf, forecast_horizon = forecast_horizon, model = model_grid)

ggplot(data_oil) +
  geom_line(aes(x = date, y = WTI, color = "Original")) +
  geom_line(data = test_data[-(1:best_window_size_rf),], aes(x = date, y = predictions_rf, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Random Forest forecast (Grid search)", y = "WTI (% change)")

# QUESTION: THE RESULT IS THE SAME IN ALL CASES, WHAT COULD BE CAUSING THIS? PERHAPS ALSO RANDOM FOREST DATA NEEDS RESCALING AFTER BEING FITTED TO THE MODEL