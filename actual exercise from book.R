library(ISLR2)
library(keras)
library(tensorflow)
 xdata <- data.matrix (
  NYSE [, c("DJ_return", "log_volume","log_volatility")]
)
 istrain <- NYSE [, "train"]
 xdata <- scale ( xdata )

 lagm <- function (x , k = 1) {
   n <- nrow (x )
   pad <- matrix (NA , k , ncol ( x))
   rbind ( pad , x [1:( n - k) , ])
   }

 arframe <- data.frame ( log_volume = xdata [, "log_volume"] ,
                            L1 = lagm ( xdata , 1) , L2 = lagm ( xdata , 2) ,
                            L3 = lagm ( xdata , 3) , L4 = lagm ( xdata , 4) ,
                            L5 = lagm ( xdata , 5)
)

 arframe <- arframe [ -(1:5) , ]
 istrain <- istrain [ -(1:5) ]
 arframed <-
  data.frame ( day = NYSE [ -(1:5) , "day_of_week"], arframe )
 arfitd <- lm(log_volume ~ ., data = arframed[istrain, ])
arpredd <- predict ( arfitd , arframed [! istrain , ])
V0 <- var ( arframe [! istrain , "log_volume"])

 1 - mean (( arpredd - arframe [! istrain , "log_volume"]) ^2) / V0
 n <- nrow ( arframe )
 xrnn <- data.matrix ( arframe [ , -1])
 xrnn <- array ( xrnn , c(n , 3, 5) )
 xrnn <- xrnn [, , 5:1]
 xrnn <- aperm ( xrnn , c(1 , 3, 2) )
 dim ( xrnn )


 model <- keras_model_sequential () %>% 
   layer_simple_rnn ( units = 12 ,
                           input_shape = list (5 , 3) ,
                           dropout = 0.1 , recurrent_dropout = 0.1) %>%
   layer_dense ( units = 1)
model %>% compile (optimizer = optimizer_rmsprop(),
                       loss = "mse")
history <- model %>% fit (
  xrnn [ istrain ,, ], arframe [ istrain , "log_volume"],
  batch_size = 64 , epochs = 200 ,
  validation_data =
    list ( xrnn [! istrain ,, ], arframe [! istrain , " log_volume"])
)
