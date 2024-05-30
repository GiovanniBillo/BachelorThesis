#### MODELLING OIL

#### library calls ####
ssh <- suppressPackageStartupMessages
ssh(library(timeSeries))
ssh(library(tseries))
ssh(library(aTSA))
ssh(library(forecast))
ssh(library(rugarch))
ssh(library(ModelMetrics))
ssh(library(keras)) # neural networks

source("functions.R")


#### CONSTANTS ####
window_size = 50

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

predictions_arima = rolling_windows(train_data, test_data, auto.arima, 50)

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
#   model <- auto.arima(train_subset$WTI)
#   
#   # Make predictions
#   check_set_x = subset(check_subset, select = -WTI)
#   new_prediction <- predict(model, newdata = check_set_x)
#   class(new_prediction)
#   predictions = c(predictions, extract_forecast(new_prediction))
# }
# predictions
# 
# predictions

# Evaluate performance 
check_set_y = subset(test_data, select = WTI) 
check_set_y = check_set_y$WTI[(window_size + 1):length(test_data$WTI)]

# return error metrics
acc_arima = accuracy(na.omit(predictions_arima), na.omit(check_set_y))

ggplot(data_oil) +
  geom_line(aes(x = date, y = WTI, color = "Original")) +
  geom_line(data = test_data[-(1:50),], aes(x = date, y = predictions_arima, color = "Forecast")) +
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

predictions_ets = rolling_windows(train_data, test_data, ets, 50)

ggplot(data_oil) +
  geom_line(aes(x = date, y = WTI, color = "Original")) +
  geom_line(data = test_data[-(1:50),], aes(x = date, y = predictions_ets, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Exponential Smoothing forecast", y = "WTI (% change)")
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

NNdata = data.matrix(data[, -1]) # remove date column
NNtrain <- NNdata[1:n_train, ]
NNvalidation <- NNdata[(n_train + 1):(n_train + n_validation), ]
NNtest <- NNdata[(n_train + n_validation + 1):n, ]
# preprocessing
mean = apply(NNtrain, 2, mean) # 2 parameter indicates that function will be applied column-wise
std = apply(NNtrain, 2, sd)
NNdata <- scale(NNdata, center = mean, scale = std)

accuracy_table = rbind(acc_arima, acc_ets, acc_no_change, acc_rf)
rownames(accuracy_table) = c("ARIMA", "ETS", "No change", "Random Forest")

create_table_from_df(accuracy_table, "Accuracy measures Benchmark vs ML models - OIL")
`
# pakcage RNN`
#### archive ####
# # Pad shorter vectors with NA to make them equal length
# data <- lapply(data, function(x) {
#   if (length(x) < max_length) {
#     c(x, rep(NA, max_length - length(x)))
#   } else {
#     x
#   }
# })
# 
# # Convert the list of padded vectors into a dataframe
# data <- data.frame(data)
# Find the maximum length among the vectors
# max_length <- max(sapply(data, length))

