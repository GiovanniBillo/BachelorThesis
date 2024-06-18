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


#### ARIMA ####
predictions_arima_gas = rolling_arima(train_data, validation_data, test_data, window_sizes, forecast_horizon)
check_set_y = test_data[, 2]
check_set_y = check_set_y[[1]]

plot_arima_gas = ggplot(data_gas) +
  geom_line(aes(x = date, y = HENRYHUB, color = "Original")) +
  geom_line(data = test_data, aes(x = date, y = predictions_arima, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "ARIMA forecast", y = "HENRYHUB (% change)")
ggsave(filename = "plot_arima_gas.png", plot = plot_arima_gas, width = 6, height = 4, dpi = 300)

ARIMA = c(measureMAE(replace_zero(check_set_y), replace_zero(predictions_arima)), measureRMSE(replace_zero(check_set_y), replace_zero(predictions_arima)))
ARIMA

#### Exponential Smoothing ####
predictions_ets_gas = rolling_ets(train_data, validation_data, test_data, window_sizes, forecast_horizon)

plot_ets_gas = ggplot(data_gas) +
  geom_line(aes(x = date, y = HENRYHUB, color = "Original")) +
  geom_line(data = test_data, aes(x = date, y = predictions_ets, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Exponential Smoothing forecast", y = "HENRYHUB (% change)")
ggsave(filename = "plot_ets_gas.png", plot = plot_ets, width = 6, height = 4, dpi = 300)

ETS = c(measureMAE(replace_zero(check_set_y), replace_zero(predictions_ets)), measureRMSE(replace_zero(check_set_y), replace_zero(predictions_ets)))
ETS

#### No change ####

no_change_forecast <- function(observations){
  most_recent_value = observations[1]
  nc_vector = rep(most_recent_value, length(observations))
  
  return(nc_vector)
}

no_change_gas = no_change_forecast(test_data$HENRYHUB)
NO_CHANGE = c(measureMAE(replace_zero(check_set_y), replace_zero(no_change_gas)), measureRMSE(replace_zero(check_set_y), replace_zero(no_change_gas)))


#### RANDOM FOREST ####
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
rf_default <- train(HENRYHUB ~., data=data_gas[istrain, ], method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)

predictions_rf_default_gas <- predict(rf_default, newdata =  data_gas[!istrain, ])

plot_rf_default_gas = ggplot(data_gas) +
  geom_line(aes(x = date, y = HENRYHUB, color = "Original")) +
  geom_line(data = data_gas[!istrain, ], aes(x = date, y = predictions_rf_default_gas, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Random Forest forecast (default)", y = "HENRYHUB (% change)")
ggsave(filename = "plot_rf_default_gas.png", plot = plot_rf_default_gas, width = 6, height = 4, dpi = 300)

## compute accuracy
check_set_y = subset(data_gas[!istrain, ], select = HENRYHUB)
check_set_y = check_set_y$HENRYHUB

RANDOM_FOREST_DEFAULT = c(measureMAE(replace_zero(check_set_y), replace_zero(predictions_rf_default_gas)), measureRMSE(replace_zero(check_set_y), replace_zero(predictions_rf_default_gas)))
RANDOM_FOREST_DEFAULT

#### using random search (reference: https://www.projectpro.io/recipes/tune-hyper-parameters-grid-search-r)####
set.seed(50)
num_features = ncol(data_gas) - 1 +  31

# specifying the CV technique which will be passed into the train() function later and number parameter is the "k" in K-fold cross validation
train_control = trainControl(method = "cv", number = 5, search = "random")

# Customsing the tuning grid
rfGrid <-  expand.grid(.mtry = (1:num_features))

# training a random forest tree model while tuning parameters
model_random = train(HENRYHUB~., data = data_gas[istrain,], method = "rf", trControl = train_control, tuneGrid = rfGrid)
print(model_random)

predictions_rf_random_gas = predict(model_random, newdata = data_gas[!istrain, ])

plot_rf_random_gas = ggplot(data_gas) +
  geom_line(aes(x = date, y = HENRYHUB, color = "Original")) +
  geom_line(data = data_gas[!istrain, ], aes(x = date, y = predictions_rf_random_gas, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Random Forest forecast (random search)", y = "HENRYHUB (% change)")

ggsave(filename = "plot_rf_random_gas.png", plot = plot_rf_random_gas, width = 6, height = 4, dpi = 300)
RANDOM_FOREST_RANDOM = c(measureMAE(replace_zero(check_set_y), replace_zero(predictions_rf_random_gas)), measureRMSE(replace_zero(check_set_y), replace_zero(predictions_rf_random_gas)))
RANDOM_FOREST_RANDOM

accuracy_ml = data.frame(RANDOM_FOREST_DEFAULT, RANDOM_FOREST_RANDOM)
rownames(accuracy_ml) = c('MAE', 'RMSE')

predictions_benchmark_gas <- data.frame(
  date = test_data$date,
  actual = test_data$HENRYHUB,
  ARIMA = predictions_arima,
  ETS = predictions_ets,
  NoChange = no_change_gas
)

predictions_ml_gas <- data.frame(
  date = test_data$date,
  actual = test_data$HENRYHUB,
  RandomForestDefault = predictions_rf2,
  RandomForestRandom = predictions_rf3,
  RecurrentNeuralNetwork = predictions_rnn
)

#### Convert Data to Long Format ####
predictions_long_benchmark_gas <- predictions_benchmark_gas %>%
  pivot_longer(cols = -date, names_to = "Model", values_to = "Value")

predictions_long_ml_gas <- predictions_ml_gas %>%
  pivot_longer(cols = -date, names_to = "Model", values_to = "Value")

#### Plot ####
combined_plot_benchmark_gas <- ggplot(predictions_long_benchmark_gas, aes(x = date, y = Value, color = Model)) +
  geom_line() +
  scale_color_manual(values = c(
    "actual" = "black",
    "ARIMA" = "red",
    "ETS" = "green",
    "NoChange" = "blue"
  )) +
  labs(title = "Forecast Models Comparison (Benchmark)", y = "HENRYHUB (% change)") 

combined_plot_benchmark_gas
ggsave(filename = "combined_plot_benchmark_gas.png", plot = combined_plot_benchmark_gas, width = 6, height = 4, dpi = 300)


combined_plot_ml <- ggplot(predictions_long_ml_gas, aes(x = date, y = Value, color = Model)) +
  geom_line() +
  scale_color_manual(values = c(
    "actual" = "black",
    "RandomForestDefault" = "blue",
    "RandomForestRandom" = "red",
    "RecurrentNeuralNetwork" = "green"
  )) +
  labs(title = "Forecast Models Comparison (ML)", y = "HENRYHUB (% change)")

combined_plot_ml


## accuracy metrics 

# Combine into a data frame
accuracy_df_gas <- data.frame(
  Model = c("ARIMA", "ETS", "No Change", "RF Default", "RF Random"),
  MAE = c(ARIMA[1], ETS[1], NO_CHANGE[1], RANDOM_FOREST_DEFAULT[1], RANDOM_FOREST_RANDOM[1]),
  RMSE = c(ARIMA[2], ETS[2], NO_CHANGE[2], RANDOM_FOREST_DEFAULT[2], RANDOM_FOREST_RANDOM[2])
  #  MAPE = c(accuracy_arima[4], accuracy_ets[4], accuracy_no_change[4], accuracy_rf_default[3], accuracy_rf_random[3], accuracy_rnn_randomsearch[3])
)
a