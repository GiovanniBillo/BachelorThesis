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


#### Gather Predictions ####

## from cloud ##
#### ####
predictions_rnn <- c(
  0.0276881483, -0.0067576611, -0.0166237018, 0.0075864084, -0.0280962016, 0.0019587780, -0.0147362586,
  0.0151597549, 0.0152619317, 0.0113054051, 0.0221554452, 0.0031588713, 0.0084855822, -0.0044158779,
  -0.0011951285, -0.0235005537, -0.0001089081, 0.0174407667, -0.0037944408, -0.0114910726, 0.0115887869,
  0.0014088039, -0.0014959093, 0.0068220974, -0.0122679703, 0.0135364292, 0.0003945715, 0.0169587722,
  -0.0010089077, -0.0030487070, 0.0075090050, -0.0002531801, 0.0132193370, -0.0057904519, -0.0140243769,
  -0.0019099279, 0.0151704255, -0.0062782467, -0.0026277047, 0.0114627128, 0.0180567183, 0.0087344991,
  -0.0040495267, -0.0043878373, 0.0124828260, 0.0118423270, -0.0056734995, 0.0105092502, -0.0132199011,
  0.0067305984, -0.0104334071, 0.0069792708, -0.0073589547, 0.0014102228, 0.0212004227, 0.0052205013,
  -0.0043153377, -0.0066137215, -0.0151289513, -0.0024722872, 0.0046657888, -0.0051083854, -0.0191282213,
  0.0014423520, 0.0012202682, -0.0198211849, 0.0073703202, -0.0053951725, -0.0014893984, -0.0093737225,
  0.0022301022, -0.0018508109, 0.0083588948, -0.0066248104, -0.0012976062, -0.0011659247, -0.0177179870,
  -0.0394790095, 0.0072186220, 0.0096134237, -0.0082301731, -0.0239286622, -0.0315192750, -0.0596898977,
  0.0089159585, -0.0274366149, 0.0146369427, 0.0256764268, -0.0162993653, 0.0014950506, -0.0354615151,
  0.0175664438, -0.0028668478, -0.0159302098, 0.0179837700, -0.0080350165, 0.0399456971, 0.0053499779,
  -0.0098965558, -0.0029522298, 0.0148345424, -0.0045899510, -0.0049798527, -0.0346203377, 0.0205772551,
  0.0015771298, 0.0017782841, 0.0387438390, -0.0066180268, 0.0025771928, -0.0128330016, -0.0274839660,
  -0.0111727511, -0.0184676823, 0.0031059666, 0.0090723675, 0.0093053872, -0.0083915515, -0.0017503223,
  0.0023049649, 0.0051664620, 0.0135273142, 0.0040465662, -0.0682358749, 0.0229445889, -0.0330713776,
  -0.0151658889, -0.0403357634, 0.0209910941, 0.0204581624, 0.0001715788, 0.0383937924, -0.0341968030,
  -0.0158264759, 0.0045360632, 0.0171568805, 0.0028139272, -0.0039498155, -0.0090367853, -0.0167476410,
  -0.0096403113, 0.0072161507, 0.0128258291, 0.0110972803, -0.0152041660, -0.0352154482, 0.0383852185,
  0.0002086122, 0.0053649136, 0.0170368269, -0.0081598670, -0.0221731866, -0.0099706091, -0.0079705431,
  0.1188977347, -0.0351116267, -0.0117190565, 0.0054787508, -0.0025790950, 0.0063939889, -0.0201132917,
  -0.0079359538, -0.0026060366, -0.0113330542, -0.0158181903, -0.0346556419, -0.0240564164, -0.0017024388,
  0.0084637265, -0.0013484558, -0.0038006264, 0.0002494538, 0.0110379572, 0.0184452950, -0.0193777896,
  -0.0085632459, 0.0024258828, 0.0068072981, -0.0066506379, -0.0064682460, 0.0110454911, 0.0217657325,
  0.0044750696, 0.0027145413, -0.0083768510, -0.0028858624, -0.0097878215, -0.0095766066, 0.0270303448,
  0.0048940186, 0.0101138037, -0.0179706143, 0.0100025133, 0.0013440852, -0.0096361713, 0.0027379689,
  -0.0064852354, 0.0144130338, -0.0105312532, -0.0243705456, 0.0213393718, 0.0191502024, -0.0125486448,
  0.0012477076, 0.0055699560, -0.0045366312, -0.0182988647, -0.0249942572, -0.0020806981, 0.0338120805,
  0.0027630338, 0.0132082806, -0.0063867139, 0.0006324188, -0.0099980685, 0.0068503820, 0.0123662978,
  0.0005296862, 0.0086230293, -0.0003786683, 0.0035517406, -0.0114475308, -0.0017000645, 0.0075621465,
  0.0071977695, 0.0007116184, 0.0012346096, -0.0112013183, 0.0007321245, 0.0299986062, -0.0019883414,
  -0.0039721558, -0.0312485519, -0.0041150054, -0.0071974689, -0.0097487221, 0.0007059314, -0.0075829753,
  0.0080414649, -0.0004866257, -0.0070156831, -0.0205672842, -0.0170860604, -0.0235495311, -0.0219978510,
  0.0042046443, -0.0022915043, -0.0133191361, -0.0116105991
)

plot_rnn = ggplot(data_oil) +
  geom_line(aes(x = date, y = WTI, color = "Original")) +
  geom_line(data = data_oil[!istrain, ], aes(x = date, y = predictions_rnn, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Recurrent Neural Network forecast", y = "WTI (% change)")
ggsave(filename = "plot_rnn_default.png", plot = plot_rnn_default, width = 6, height = 4, dpi = 300)

# ####
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

#### Convert Data to Long Format ####
predictions_long_benchmark <- predictions_benchmark %>%
  pivot_longer(cols = -date, names_to = "Model", values_to = "Value")

predictions_long_ml <- predictions_ml %>%
  pivot_longer(cols = -date, names_to = "Model", values_to = "Value")

#### Plot ####
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

accuracy_rnn_randomsearch <- c(
  measureMAE(replace_zero(actuals), replace_zero(predictions_rnn)),
  measureRMSE(replace_zero(actuals), replace_zero(predictions_rnn))
)
accuracy_rnn_randomsearch

# accuracy_rnn_randomsearch = c(4.960123e-01, 7.157093e-01, 4.028281e+07)

accuracy_rf_default = RANDOM_FOREST_DEFAULT
accuracy_rf_random = RANDOM_FOREST_RANDOM
# Combine into a data frame
accuracy_df <- data.frame(
  Model = c("ARIMA", "ETS", "No Change", "RF Default", "RF Random", "RNN Random Search"),
  MAE = c(accuracy_arima[2], accuracy_ets[2], accuracy_no_change[2], accuracy_rf_default[1], accuracy_rf_random[1], accuracy_rnn_randomsearch[1]),
  RMSE = c(accuracy_arima[3], accuracy_ets[3], accuracy_no_change[3], accuracy_rf_default[2], accuracy_rf_random[2], accuracy_rnn_randomsearch[2])
 #  MAPE = c(accuracy_arima[4], accuracy_ets[4], accuracy_no_change[4], accuracy_rf_default[3], accuracy_rf_random[3], accuracy_rnn_randomsearch[3])
)

print(accuracy_df)

# Load the xtable library
library(xtable)

# Create LaTeX table
latex_table_accuracy <- xtable(accuracy_df, caption = "Accuracy Metrics for Different Models")

# Print the LaTeX table code
# Save the LaTeX table to a file
sink("FINAL_accuracy_metrics_table.tex")
print(latex_table_accuracy, type = "latex", include.rownames = TRUE, caption.placement = "top")
sink()

### select best model ####
which.min(accuracy_df$MAE)
which.min(accuracy_df$RMSE)
