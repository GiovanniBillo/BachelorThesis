#### MODELLING OIL

setwd("C:/Users/billo/OneDrive/Desktop/FAU/Thesis/data/data")

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
source("functions.R")


#### IMPORT DATA FROM LOCAL TO AVOID RELOADING EVERYTHING EVERYTIME ####
data_oil <- read_excel("OIL_firstdifferenced.xlsx")
data_oil = drop_na(data_oil)

## train-validation-test split 
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

## Benchmark models ##
accuracy_table= data.frame(
  col1 = rep(NA, 3) # Placeholder to merge later. Number corresponds to the 7 statistics well include
)

# ARIMA
# Define rolling window size

# Iterate over the rolling window (VALIDATION)####
# arima_model <- auto.arima(train_data$WTI)
# summary(arima_model)
# rolling_windows(train_data, validation_data, auto.arima, 10)

# # Train
# arima_model <- auto.arima(train_data$WTI)
# 
# # Validate
# validation_forecast_arima <- predict(arima_model, newdata = validation_data)
# validation_forecast_arima$mean
# validation_accuracy_arima <- accuracy(validation_forecast_arima, validation_data$WTI[1:10])
# print(validation_accuracy_arima)

# Iterate over the rolling window (TESTING)####

# predictions_arima = rolling_windows(train_data, test_data, auto.arima, 50)

# window_size = 10
# SELECTS THE BEST MODEL
model_arima = auto.arima(train_data$WTI)
window_sizes <- seq(24, 60, by = 1)  # Example window sizes
forecast_horizon <- 1  # Number of steps to forecast ahead

rolling_arima(train_data, 10, 1)
arima_windows_evaluation = evaluate_window_size(validation_data, window_sizes, forecast_horizon, rolling_arima, model_arima)


for (i in 1:n_windows) {
  # Define training and validation data
  # if (i > 7){
  #   browser()
  #
  # }
  train_subset <- train_data[i:(i + window_size - 1), ]
  check_subset <- train_data[(i + window_size), ] # test or train

  model <- arima(train_subset$WTI, order = c(3, 0, 0))

  # Make predictions
  check_set_x = subset(check_subset, select = -WTI)
  new_prediction <- predict(model, newdata = check_set_x)
  class(new_prediction)
  predictions = c(predictions, extract_forecast(new_prediction))
}
predictions_arima = predictions

# return error metrics

best_window_size_arima = window_sizes[which.min(arima_windows_evaluation$MAE)]

predictions_arima = rolling_arima(test_data, best_window_size_arima, forecast_horizon, order = c(3, 0, 0))
# Evaluate performance 
check_set_y = subset(test_data, select = WTI) 
check_set_y = check_set_y$WTI[(best_window_size_arima + 1):length(test_data$WTI)]
acc_arima = accuracy(replace_zero(predictions_arima), replace_zero(check_set_y)) ###? why Nans and Inf?

ggplot(data_oil) +
  geom_line(aes(x = date, y = WTI, color = "Original")) +
  geom_line(data = test_data[-(1:best_window_size_arima),], aes(x = date, y = predictions_arima, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "ARIMA forecast", y = "WTI (% change)")


# # Testing
# test_forecast_arima <- predict(arima_model, newdata = test_data)
# test_accuracy_arima <- accuracy(test_forecast, test_data$WTI[1:10])
# print(test_accuracy_arima)

# Exponential Smoothing

# exp_smooth_model <- ets(train_data$WTI)
# summary(exp_smooth_model)
# # Validate
# 
# rolling_windows(train_data, validation_data, exp_smooth_model , 10)

# Testing
model_ets = ets(train_data$WTI)
predictions_ets = rolling_windows(train_data, test_data, ets, 50)

ets_windows_evaluation = evaluate_window_size(validation_data, window_sizes, forecast_horizon, rolling_ets, model_ets)

best_window_size_ets = window_sizes[which.min(ets_windows_evaluation$MAE)]

predictions_ets = rolling_ets(test_data, best_window_size_ets, forecast_horizon)

ggplot(data_oil) +
  geom_line(aes(x = date, y = WTI, color = "Original")) +
  geom_line(data = test_data[-(1:best_window_size_ets),], aes(x = date, y = predictions_ets, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Exponential Smoothing forecast", y = "WTI (% change)")


# window_size = 10
# n_windows = nrow(test_data) - window_size
# predictions = c()
# for (i in 1:n_windows) {
#   # Define training and validation data
#   # browser()
#   train_subset <- train_data[i:(i + window_size - 1), ]
#   check_subset <- train_data[(i + window_size), ] # test or train
#   
#   model <- ets(train_subset$WTI)
#   
#   # Make predictions
#   check_set_x = subset(check_subset, select = -WTI)
#   new_prediction <- predict(model, newdata = check_set_x)
#   class(new_prediction)
#   predictions = c(predictions, extract_forecast(new_prediction))
# }
# 
# extract_forecast(new_prediction)

# Evaluate performance 
check_set_y = subset(test_data, select = WTI) 
check_set_y = check_set_y$WTI[(window_size + 1):length(test_data$WTI)]

# return error metrics
acc_ets = accuracy(na.omit(predictions_ets), na.omit(check_set_y))

# No change

no_change_forecast <- function(observations){
  most_recent_value = observations[length(observations)]
  nc_vector = rep(most_recent_value, length(observations))
  
  accuracy(nc_vector, observations)
}

acc_no_change = no_change_forecast(na.omit(data_oil$WTI))

## ML Models ##


# Random forests 
library(randomForest)
n <- names(data_oil)
f <- as.formula(paste("WTI ~", paste(n[!n %in% c("WTI", "day")], collapse = " + ")))
ml_data = train_data[, !grepl("day", names(train_data))]

# single estimation
f
mtry = floor(length(names(data_oil))/3) # for regression problems
rf = randomForest(f, data = data_oil, mtry = mtry)
rf
importance(rf)
varImpPlot(rf)

# rw estimation
rolling_windows(train_data, test_data, rf, 10)

window_size = 50
n_windows = nrow(test_data) - window_size
predictions_rf = c()

for (i in 1:n_windows) {
  # Define training and validation data
  # browser()
  train_subset <- train_data[i:(i + window_size - 1), ]
  check_subset <- train_data[(i + window_size), ] # test or train
  
  mtry = floor(length(names(data_oil))/3) # for regression problems
  model = randomForest(f, data = train_subset, mtry = mtry)
  
  # Make predictions
  check_set_x = subset(check_subset, select = -WTI)
  new_prediction <- predict(model, newdata = check_set_x)
  class(new_prediction)
  predictions_rf = c(predictions_rf, extract_forecast(new_prediction))
}

# Evaluate performance 
check_set_y = subset(test_data, select = WTI) 
check_set_y = check_set_y$WTI[(window_size + 1):length(test_data$WTI)]

# return error metrics
acc_rf = accuracy(na.omit(predictions_rf), na.omit(check_set_y))


ggplot(data_oil) +
  geom_line(aes(x = date, y = WTI, color = "Original")) +
  geom_line(data = test_data[-(1:50),], aes(x = date, y = predictions_rf, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Random Forest forecast", y = "WTI (% change)")

# Recurrent Neural Network
library(caret)
# set some parameters for our model
max_len <- 6 # the number of previous examples we'll look at
batch_size <- 32 # number of sequences to look at at one time during training
total_epochs <- 15 # how many times we'll look @ the whole dataset while training our model

# set a random seed for reproducability
set.seed(123)

wti = data_oil$WTI

# get a list of start indexes for our (overlapping) chunks
start_indexes <- seq(1, length(wti) - (max_len + 1), by = 3)

# create an empty matrix to store our data in
wti_matrix <- matrix(nrow = length(start_indexes), ncol = max_len + 1)

# fill our matrix with the overlapping slices of our dataset
for (i in 1:length(start_indexes)){
  wti_matrix[i,] <- wti[start_indexes[i]:(start_indexes[i] + max_len)]
}
# make sure it's numeric
wti_matrix <- wti_matrix * 1

# remove na's if you have them
if(anyNA(wti_matrix)){
  wti_matrix <- na.omit(wti_matrix)
}
# split our data into the day we're predict (y), and the 
# sequence of days leading up to it (X)
X <- wti_matrix[,-ncol(wti_matrix)]
y <- wti_matrix[,ncol(wti_matrix)]

training_index <- createDataPartition(y, p = .7, 
                                      list = FALSE, 
                                       times = 1)

# training data
X_train <- array(X[training_index,], dim = c(length(training_index), max_len, 1))
y_train <- y[training_index]

remotes::install_github("rstudio/tensorflow")
reticulate::install_python()
library(tensorflow)
install_tensorflow(envname = "r-tensorflow")

install.packages("keras")
library(keras)
install_keras()
# NNtrain <- NNdata[1:n_train, ]
# NNvalidation <- NNdata[(n_train + 1):(n_train + n_validation), ]
# NNtest <- NNdata[(n_train + n_validation + 1):n, ]
# # preprocessing
# mean = apply(NNtrain, 2, mean) # 2 parameter indicates that function will be applied column-wise
# std = apply(NNtrain, 2, sd)
# NNdata <- scale(NNdata, center = mean, scale = std)
# 
# accuracy_table = rbind(acc_arima, acc_ets, acc_no_change, acc_rf)
# rownames(accuracy_table) = c("ARIMA", "ETS", "No change", "Random Forest")
# 
# create_table_from_df(accuracy_table, "Accuracy measures Benchmark vs ML models - OIL")
`
# pakcage RNN`


