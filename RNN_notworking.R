setwd("C:/Users/billo/anaconda3/envs/Thesis")
## reference: https://www.kaggle.com/code/rtatman/beginner-s-intro-to-rnn-s-in-r

### only damn conda installation working
install.packages("remotes")
remotes::install_github(sprintf("rstudio/%s", c("reticulate", "tensorflow", "keras")))
reticulate::miniconda_uninstall() # start with a blank slate
reticulate::install_miniconda()
keras::install_keras()

# install_tensorflow(method = "conda", conda = "C:/Users/billo/anaconda3/_conda.exe")


#### library calls ####
library(tensorflow)
library(keras)
library(caret)
source("functions.R")

# Recurrent Neural Network
#### IMPORT DATA FROM LOCAL TO AVOID RELOADING EVERYTHING EVERYTIME ####
data_oil <- read_excel("C:/Users/billo/OneDrive/Desktop/FAU/Thesis/data/OIL_firstdifferenced.xlsx")
data_oil = drop_na(data_oil)

train_index = createDataPartition(data_oil$WTI, p = 0.8)
data_oil$train <- ifelse(row_number(data_oil) %in% train_index$Resample1, TRUE, FALSE)

istrain = data_oil$train
data_oil$train <- NULL
#### try 2 ####
noday = which(names(data_oil) %in% c("day", "date"))
xdata = data.matrix(data_oil[-noday])
# nyse = NYSE
xdata = scale(xdata)

lagm <- function (x , k = 1) {
     n <- nrow (x )
     pad <- matrix (NA , k , ncol ( x))
     rbind ( pad , x [1:( n - k) , ])
}

columns_for_lags = c("WTI", "wti_futures", "brent", "dj", "sp", "brent_futures", "dubai_futures")
arframe <- data.frame(wti = xdata[, "WTI"], 
                      xdata[, !(colnames(xdata) %in% columns_for_lags)],
                      L1 = lagm(xdata[, columns_for_lags], 1),
                      L2 = lagm(xdata[, columns_for_lags], 2), 
                      L3 = lagm(xdata[, columns_for_lags], 3), 
                      L4 = lagm(xdata[, columns_for_lags], 4),
                      L5 = lagm(xdata[, columns_for_lags], 5))

### remove NAs
arframe <- arframe[-(1:5), ]
istrain <- istrain[-(1:5)]

arfit <- lm(wti ~ ., data = arframe[istrain, ])
arpred <- predict(arfit, arframe[!istrain, ])

V0 <- var(arframe [!istrain , "wti"])
1 - mean (( arpred - arframe [! istrain , "wti"])^2) / V0

# including day of the month
arframed <- data.frame(day = data_oil[-(1:5), noday], arframe)
arfitd <- lm(wti ~ ., data = arframed[istrain, ])
arpredd <- predict(arfit, arframed[!istrain, ])
1 - mean (( arpredd - arframe [! istrain , "wti"])^2) / V0


n <- nrow ( arframe )
xrnn <- data.matrix ( arframe [ , -1])
xrnn <- array ( xrnn , c(n , 5, 5) )
xrnn <- xrnn [, , 5:1]
xrnn <- aperm ( xrnn , c(1 , 3, 2) )
dim ( xrnn )


model <- keras_model_sequential() %>% 
  layer_simple_rnn(units = 12, 
                   input_shape = list(5, 5), 
                   dropout = 0.1, recurrent_dropout = 0.1) %>% 
  layer_dense((units = 1))

history = model %>% fit(
  xrnn[istrain,, ], arframe[istrain, "wti"],
  batch_size = 64, epochs = 200,
  validation_data = 
    list(xrnn[!istrain,, ], arframe[!istrain, "wti"]))

#### try 1 ####
# set some parameters for our model
max_len <- 12 # the number of previous examples we'll look at
batch_size <- 64 # number of sequences to look at at one time during training
total_epochs <- 30 # how many times we'll look @ the whole dataset while training our model


# set a random seed for reproducability
set.seed(123)

wti = data_oil$WTI

table(wti)

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

training_index <- createDataPartition(y, p = .9, 
                                      list = FALSE, 
                                      times = 1)


# training data
X_train <- array(X[training_index,], dim = c(length(training_index), max_len, 1))
y_train <- y[training_index]

#testing data
X_test <- array(X[-training_index,], dim = c(length(y) - length(training_index), max_len, 1))
y_test <- y[-training_index]

model = keras_model_sequential()
dim(X_train)

model %>%
  layer_dense(input_shape = dim(X_train)[2:3], units = max_len)
model %>% 
  layer_simple_rnn(units = 6)
model %>%
  layer_dense(units = 1, activation = 'sigmoid') # output
summary(model)

model %>% compile(loss = 'mae', 
                  optimizer = 'RMSprop', 
                  metrics = c('accuracy'))
# Actually train our model! This step will take a while
trained_model <- model %>% fit(
  x = X_train, # sequence we're using for prediction 
  y = y_train, # sequence we're predicting
  batch_size = batch_size, # how many samples to pass to our model at a time
  epochs = total_epochs, # how many times we'll look @ the whole dataset
  validation_split = 0.1) # how much data to hold out for testing as we go along

trained_model
plot(trained_model)

# testing the model
classes <- model %>% predict(X_test, batch_size = batch_size)

table(y_test, classes)

model %>% evaluate (X_test, y_test, batch_size = batch_size)
