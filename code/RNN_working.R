#### Library Calls ####
packages <- c("timeSeries", "tseries", "aTSA", "forecast", "rugarch", 
              "ModelMetrics", "keras", "readxl", "writexl", "tidyr", "ggplot2", "caret", "dplyr")
install.packages(packages)

# Suppress package startup messages
ssh <- suppressPackageStartupMessages
ssh(library(timeSeries))
ssh(library(tseries))
ssh(library(aTSA))
ssh(library(forecast))
ssh(library(rugarch))
ssh(library(ModelMetrics))
ssh(library(keras))
ssh(library(readxl))
ssh(library(writexl))
ssh(library(tidyr))
ssh(library(ggplot2))
ssh(library(caret))
ssh(library(dplyr))

source("functions.R")

#### Import Data ####
data_oil <- read_excel("data_cop/OIL_firstdifferenced.xlsx")
data_oil <- drop_na(data_oil)

#### Train-Test Split ####
set.seed(123)  # For reproducibility
# Determine the split point
split_point <- floor(0.8 * nrow(data_oil))

# Assign the first 80% to the training set and the remaining 20% to the test set
data_oil$train <- c(rep(TRUE, split_point), rep(FALSE, nrow(data_oil) - split_point))

# train_index <- createDataPartition(data_oil$WTI, p = 0.8, list = FALSE)
istrain = data_oil$train
data_oil$train = NULL

#### Prepare Data ####
noday <- which(names(data_oil) %in% c("day", "date"))
xdata <- data.matrix(data_oil[-noday])

# Scaling the data
xdata_scaled <- scale(xdata)
scaling_params <- attributes(xdata_scaled)

# Function to unscale data
unscale <- function(scaled_data, center, scale) {
  browser()
  sweep(sweep(scaled_data, 2, center, "+"), 2, scale, "*")
}

lagm <- function(x, k = 1) {
  n <- nrow(x)
  pad <- matrix(NA, k, ncol(x))
  rbind(pad, x[1:(n - k), ])
}

columns_for_lags <- c("WTI", "wti_futures", "brent", "dj", "sp", "brent_futures", "dubai_futures")
arframe <- data.frame(
  wti = xdata_scaled[, "WTI"], 
  xdata_scaled[, !(colnames(xdata) %in% columns_for_lags)],
  L1 = lagm(xdata_scaled[, columns_for_lags], 1),
  L2 = lagm(xdata_scaled[, columns_for_lags], 2), 
  L3 = lagm(xdata_scaled[, columns_for_lags], 3), 
  L4 = lagm(xdata_scaled[, columns_for_lags], 4),
  L5 = lagm(xdata_scaled[, columns_for_lags], 5)
)

#### Remove NAs ####
arframe <- arframe[-(1:5), ]
istrain <- istrain[-(1:5)]
data_oil <- data_oil[-(1:5), ]

#### Linear Model ####
arfit <- lm(wti ~ ., data = arframe[istrain, ])
arpred <- predict(arfit, arframe[!istrain, ])

V0 <- var(arframe[!istrain, "wti"])
linear_model_accuracy <- 1 - mean((arpred - arframe[!istrain, "wti"])^2) / V0

#### Including Day of the Month ####
arframed <- data.frame(day = data_oil[, noday], arframe)
arfitd <- lm(wti ~ ., data = arframed[istrain, ])
arpredd <- predict(arfitd, arframed[!istrain, ])
linear_model_day_accuracy <- 1 - mean((arpredd - arframe[!istrain, "wti"])^2) / V0

#### Prepare Data for RNN ####
n <- nrow(arframe)
xrnn <- data.matrix(arframe[, -1])
xrnn <- array(xrnn, c(n, 5, 5))
xrnn <- xrnn[, , 5:1]
xrnn <- aperm(xrnn, c(1, 3, 2))

#### Build and Train RNN Model ####
model <- keras_model_sequential() %>% 
  layer_simple_rnn(units = 12, 
                   input_shape = list(5, 5), 
                   activation = "relu",
                   dropout = 0.1, recurrent_dropout = 0.1) %>% 
  layer_dense(units = 1)

model %>% compile(optimizer = optimizer_rmsprop(), loss = "mse")

history <- model %>% fit(
  xrnn[istrain,, ], arframe[istrain, "wti"],
  batch_size = 64, epochs = 200,
  validation_data = list(xrnn[!istrain,, ], arframe[!istrain, "wti"])
)

#### Evaluate RNN Model ####
kpred_scaled <- predict(model, xrnn[!istrain, , ])
kpred <- unscale(kpred_scaled, scaling_params$`scaled:center`["WTI"], scaling_params$`scaled:scale`["WTI"])

differences = kpred - data_oil[!istrain, "WTI"]
rnn_model_accuracy <- 1 - mean(differences$WTI^2) / V0

#### Plot Results ####
data_oil$predicted <- NA
data_oil$predicted[!istrain] <- kpred

ggplot(data_oil, aes(x = as.Date(data_oil$date))) +
  geom_line(aes(y = WTI, color = "Original")) +
  geom_line(aes(y = predicted, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Recurrent Neural Network Forecast (single prediction)", y = "WTI (% change)")

unscale1d <- function(scaled_data, center, scale) {
  return(scaled_data * scale + center)
}

#### Rolling windows Prediction with RNN ####
window_sizes <- seq(20, 60, by = 1)  # range of window sizes to try out
forecast_horizon <- 1
windows_evaluation_rnn = evaluate_window_size(val_data = xrnn[!istrain], window_sizes = window_sizes, forecast_horizon = forecast_horizon, func = rolling_rnn, model = model)
predictions_rnn <- rolling_rnn(xrnn[!istrain, , ], 20, forecast_horizon, model)
predictions_rnn_scaled = unscale1d(predictions_rnn, scaling_params$`scaled:center`["WTI"], scaling_params$`scaled:scale`["WTI"])


ggplot(data_oil, aes(x = as.Date(data_oil$date))) +
  geom_line(aes(y = WTI, color = "Original")) +
  geom_line(aes(y = predicted, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Recurrent Neural Network Forecast", y = "WTI (% change)")

test_data = data_oil[!istrain, ]
ggplot(data_oil) +
  geom_line(aes(x = date, y = WTI, color = "Original")) +
  geom_line(data = test_data[-(1:20),], aes(x = date, y = predictions_rnn_scaled, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Recurrent neural network forecast (rolling windows)", y = "WTI (% change)")
# Display accuracy
list(
  Linear_Model_Accuracy = linear_model_accuracy,
  Linear_Model_Day_Accuracy = linear_model_day_accuracy,
  RNN_Model_Accuracy = rnn_model_accuracy
)
