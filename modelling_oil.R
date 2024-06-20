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

rnn_predictions <- read_excel("C:/Users/billo/OneDrive/Desktop/FAU/Thesis/RNN_gas/RNN_predictions_all.xlsx")

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

ARIMA_oil = c(measureMAE(replace_zero(check_set_y), replace_zero(predictions_arima)), measureRMSE(replace_zero(check_set_y), replace_zero(predictions_arima)), measureMAPE(replace_zero(check_set_y), replace_zero(predictions_arima)))
ARIMA_oil


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

ETS_oil = c(measureMAE(replace_zero(check_set_y), replace_zero(predictions_ets)), measureRMSE(replace_zero(check_set_y), replace_zero(predictions_ets)), measureMAPE(replace_zero(check_set_y), replace_zero(predictions_ets)))
ETS_oil


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

NO_CHANGE_oil = c(measureMAE(replace_zero(check_set_y), replace_zero(no_change_forecast)), measureRMSE(replace_zero(check_set_y), replace_zero(no_change_forecast)), measureMAPE(replace_zero(check_set_y), replace_zero(no_change_forecast)))
NO_CHANGE_oil

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
RANDOM_FOREST_DEFAULT_oil = c(measureMAE(replace_zero(check_set_y), replace_zero(predictions_rf2)), measureRMSE(replace_zero(check_set_y), replace_zero(predictions_rf2)), measureMAPE(replace_zero(check_set_y), replace_zero(predictions_rf2)))
RANDOM_FOREST_DEFAULT_oil

#### using random search (reference: https://www.projectpro.io/recipes/tune-hyper-parameters-grid-search-r)####
set.seed(50)
num_features = ncol(data_oil) - 1 +  31

# specifying the CV technique which will be passed into the train() function later and number parameter is the "k" in K-fold cross validation
train_control = trainControl(method = "cv", number = 5, search = "random")


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
RANDOM_FOREST_RANDOM_oil = c(measureMAE(replace_zero(check_set_y), replace_zero(predictions_rf3)), measureRMSE(replace_zero(check_set_y), replace_zero(predictions_rf3)), measureMAPE(replace_zero(check_set_y), replace_zero(predictions_rf3)))
RANDOM_FOREST_RANDOM_oil

accuracy_ml = data.frame(RANDOM_FOREST_DEFAULT, RANDOM_FOREST_RANDOM)
rownames(accuracy_ml) = c('MAE', 'RMSE', 'MAPE')

cbind(accuracy_benchmark, accuracy_ml)

#### Gather Predictions ####


predictions_benchmark <- data.frame(
  date = test_data$date,
  actual = check_set_y,
  ARIMA = predictions_arima,
  ETS = predictions_ets,
  NoChange = no_change_forecast
)

predictions_ml <- data.frame(
  date = test_data$date,
  actual = test_data$WTI,
  RandomForestDefault = predictions_rf2,
  RandomForestRandom = predictions_rf3,
  RecurrentNeuralNetwork = predictions_rnn
)

#### Convert Data to Long Format ###
predictions_long_benchmark <- predictions_benchmark %>%
  pivot_longer(cols = -date, names_to = "Model", values_to = "Value")

predictions_long_ml <- predictions_ml %>%
  pivot_longer(cols = -date, names_to = "Model", values_to = "Value")

#### Plot ###
combined_plot_benchmark_oil <- ggplot(predictions_long_benchmark, aes(x = date, y = Value, color = Model)) +
  geom_line() +
  scale_color_manual(values = c(
    "actual" = "black",
    "ARIMA" = "red",
    "ETS" = "green",
    "NoChange" = "blue"
  )) +
  labs(title = "Forecast Models Comparison (Benchmark)", y = "WTI (% change)") 
ggsave(filename = "combined_plot_benchmark_oil.png", plot = combined_plot_benchmark_oil, width = 6, height = 4, dpi = 300)


combined_plot_benchmark

combined_plot_ml <- ggplot(predictions_long_ml, aes(x = date, y = Value, color = Model)) +
  geom_line() +
  scale_color_manual(values = c(
    "actual" = "black",
    "RandomForestDefault" = "blue",
    "RandomForestRandom" = "red",
    "RecurrentNeuralNetwork" = "green"
  )) +
  labs(title = "Forecast Models Comparison (ML)", y = "WTI (% change)")

combined_plot_ml

# Save the plot
ggsave(filename = "combined_forecast_plot.png", plot = combined_plot, width = 10, height = 6, dpi = 300)

actuals <- data_oil[!istrain, "WTI"]
actuals <- actuals$WTI

RNNRANDOMSEARCH_oil <- c(
  measureMAE(replace_zero(actuals), replace_zero(rnn_predictions$`Predictions oil w/GPRD`)),
  measureRMSE(replace_zero(actuals), replace_zero(rnn_predictions$`Predictions oil w/GPRD`))
)


# accuracy_rnn_randomsearch = c(4.960123e-01, 7.157093e-01, 4.028281e+07)

accuracy_rf_default = RANDOM_FOREST_DEFAULT
accuracy_rf_random = RANDOM_FOREST_RANDOM
# Combine into a data frame
accuracy_df_oil_gprd <- data.frame(
  Model = c("ARIMA", "ETS", "No Change", "RF Default", "RF Random", "RNN Random Search"),
  MAE = c(ARIMA_oil[1], ETS_oil[1], NO_CHANGE_oil[1], RANDOM_FOREST_DEFAULT_oil[1], RANDOM_FOREST_RANDOM_oil[1], RNNRANDOMSEARCH_oil[1]),
  RMSE = c(ARIMA_oil[2], ETS_oil[2], NO_CHANGE_oil[2], RANDOM_FOREST_DEFAULT_oil[2], RANDOM_FOREST_RANDOM_oil[2], RNNRANDOMSEARCH_oil[2])
)

print(accuracy_df)

# Load the xtable library
library(xtable)

# Create LaTeX table
latex_table_accuracy <- xtable(accuracy_df, caption = "Accuracy Metrics for Different Models")
latex_table_accuracy_nogprd <- xtable(accuracy_df, caption = "Accuracy Metrics for Different Models")

# Print the LaTeX table code
# Save the LaTeX table to a file
sink("FINAL_accuracy_metrics_table_nogprd.tex")
print(latex_table_accuracy_nogprd, type = "latex", include.rownames = TRUE, caption.placement = "top")
sink()

### select best model ####
which.min(accuracy_df$MAE)
which.min(accuracy_df$RMSE)

#### without GPRD #### 
data_oil_nogprd = subset(data_oil, select = -GPRD)

#### RANDOM FORESTS ####

#### using caret (reference: https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/) ####

library(randomForest)
library(mlbench)
library(caret)

# Determine the split point
split_point_nogprd <- floor(0.8 * nrow(data_oil_nogprd))

# Assign the first 80% to the training set and the remaining 20% to the test set
data_oil_nogprd$train_nogprd <- c(rep(TRUE, split_point_nogprd), rep(FALSE, nrow(data_oil_nogprd) - split_point_nogprd))
istrain_nogprd <- data_oil_nogprd$train_nogprd
data_oil_nogprd$train_nogprd <- NULL

control_nogprd <- trainControl(method="repeatedcv", number=10, repeats=3)
seed_nogprd <- 7
metric_nogprd <- "RMSE"
set.seed(seed_nogprd)
mtry_nogprd <- sqrt(ncol(data_oil_nogprd) - 1)
tunegrid_nogprd <- expand.grid(.mtry=mtry_nogprd)
rf_default_nogprd <- train(WTI ~., data=data_oil_nogprd[istrain_nogprd, ], method="rf", metric=metric_nogprd, tuneGrid=tunegrid_nogprd, trControl=control_nogprd)
print(rf_default_nogprd)

predictions_rf2_nogprd <- predict(rf_default_nogprd, newdata = data_oil_nogprd[!istrain_nogprd, ])

# plot_rf_default_nogprd = ggplot(data_oil_nogprd) +
#   geom_line(aes(x = date, y = WTI, color = "Original")) +
#   geom_line(data = data_oil_nogprd[!istrain_nogprd, ], aes(x = date, y = predictions_rf2_nogprd, color = "Forecast")) +
#   scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
#   labs(title = "Random Forest forecast (default)", y = "WTI (% change)")
# ggsave(filename = "plot_rf_default_nogprd.png", plot = plot_rf_default_nogprd, width = 6, height = 4, dpi = 300)

## compute accuracy
check_set_y_nogprd <- data_oil_nogprd[!istrain_nogprd, "WTI"]  # Ensure you have this variable in your context
check_set_y_nogprd = check_set_y_nogprd$WTI
RANDOM_FOREST_DEFAULT_oil_nogprd = c(measureMAE(replace_zero(check_set_y_nogprd), replace_zero(predictions_rf2_nogprd)), measureRMSE(replace_zero(check_set_y_nogprd), replace_zero(predictions_rf2_nogprd)), measureMAPE(replace_zero(check_set_y_nogprd), replace_zero(predictions_rf2_nogprd)))
RANDOM_FOREST_DEFAULT_oil_nogprd

#### using random search (reference: https://www.projectpro.io/recipes/tune-hyper-parameters-grid-search-r)####
set.seed(50)
num_features_nogprd = ncol(data_oil_nogprd) - 1 +  31

# specifying the CV technique which will be passed into the train() function later and number parameter is the "k" in K-fold cross validation
train_control_nogprd = trainControl(method = "cv", number = 5, search = "random")

# Customising the tuning grid
rfGrid_nogprd <-  expand.grid(.mtry = (1:num_features_nogprd))

# training a random forest tree model while tuning parameters
model_grid_nogprd = train(WTI ~., data = data_oil_nogprd[istrain_nogprd, ], method = "rf", trControl = train_control_nogprd, tuneGrid = rfGrid_nogprd)
print(model_grid_nogprd)

predictions_rf3_nogprd = predict(model_grid_nogprd, newdata = data_oil_nogprd[!istrain_nogprd, ])

# plot_rf_random_nogprd = ggplot(data_oil_nogprd) +
#   geom_line(aes(x = date, y = WTI, color = "Original")) +
#   geom_line(data = data_oil_nogprd[!istrain_nogprd, ], aes(x = date, y = predictions_rf3_nogprd, color = "Forecast")) +
#   scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
#   labs(title = "Random Forest forecast (grid search)", y = "WTI (% change)")
# 
# ggsave(filename = "plot_rf_random_nogprd.png", plot = plot_rf_random_nogprd, width = 6, height = 4, dpi = 300)

RANDOM_FOREST_RANDOM_oil_nogprd = c(measureMAE(replace_zero(check_set_y_nogprd), replace_zero(predictions_rf3_nogprd)), measureRMSE(replace_zero(check_set_y_nogprd), replace_zero(predictions_rf3_nogprd)), measureMAPE(replace_zero(check_set_y_nogprd), replace_zero(predictions_rf3_nogprd)))
RANDOM_FOREST_RANDOM_oil_nogprd

# Compile accuracy results
accuracy_ml_nogprd = data.frame(RANDOM_FOREST_DEFAULT_oil_nogprd, RANDOM_FOREST_RANDOM_oil_nogprd)
rownames(accuracy_ml_nogprd) = c('MAE', 'RMSE', 'MAPE')

# Assuming accuracy_benchmark_nogprd exists in your environment
cbind(accuracy_benchmark_nogprd, accuracy_ml_nogprd)

RNNRANDOMSEARCH_oil_nogprd <- c(
  measureMAE(replace_zero(actuals), replace_zero(rnn_predictions$`Predictions oil w.o/GPRD`)),
  measureRMSE(replace_zero(actuals), replace_zero(rnn_predictions$`Predictions oil w.o/GPRD`))
)


accuracy_df_oil_nogprd <- data.frame(
  Model = c("RF Default", "RF Random", "RNN Random Search"),
  MAE = c(RANDOM_FOREST_DEFAULT_oil_nogprd[1], RANDOM_FOREST_RANDOM_oil_nogprd[1], RNNRANDOMSEARCH_oil_nogprd[1]),
  RMSE = c(RANDOM_FOREST_DEFAULT_oil_nogprd[2], RANDOM_FOREST_RANDOM_oil_nogprd[2], RNNRANDOMSEARCH_oil_nogprd[2])
)
