## test git command line commit


assign_day_month <-function(dates_vector){
  day_vector <- c()
  for (i in seq_along(dates_vector)){
    day_of_month = c(substr(as.character(dates_vector[i]), start=c(9), stop =c(10)))
    day_vector <- c(day_vector, day_of_month)
  }
  return(day_vector)
}

extract_month <-function(dates_vector){
  c <- c()
  for (i in seq_along(dates_vector)){
    month = c(substr(as.character(dates_vector[i]), start=c(6), stop =c(7)))
    c <- c(c, month)
  }
  return(as.numeric(c))
}

format_date <- function(dates_vector){
  for (i in seq_along(dates_vector)){
    month <- dates_vector[i]
    # print(dates_vector[i])
    dates_vector[i] <- paste(month, "-01", sep="")
  }
  dates_vector <-as.Date(dates_vector, format = "%Y-%m-%d")
  return(dates_vector)
}

oil_demand_request <- function(facet){
  # condition = ifelse(units == c("MBBL/D"), value * 30, value)
  oil_demand <- eia_data(
    dir = "petroleum/cons/psup",
    data = "value",
    freq = "monthly",
    start =  START_DATE_MONTHLY,
    end =  END_DATE_MONTHLY,
    facets = list(product = facet),
    sort = list(cols = "period", order = "asc")
  ) %>%
    select(
      period,
      value,
      units
    ) %>%
    drop_na() %>% 
    mutate(
      period = format_date(period), # Convert 'period' column to date format
      value = as.numeric(value) # Convert 'value' column to numeric
    ) %>% 
    group_by(period, units)  %>%  # Group by 'period' and 'units'
    summarise(total_demand = sum(value))  # Calculate sum of 'value' for each group
  
  # operations to aggregate demand based on the different units, MBBL and MBBL/D
  aggregate_units = ifelse(oil_demand$units == "MBBL/D", oil_demand$total_demand * 30, oil_demand$total_demand)
  oil_demand$total_demand = aggregate_units
  oil_demand <- oil_demand %>% group_by(period) %>% summarize(total_demand = sum(total_demand))
  
  # aggregate different units of measurement
  # month_factor = 30  
  # oil_demand <- oil_demand %>% group_by(date)  %>%  # Group by 'period' and 'units'
  #   summarise(total_demand = sum(value)) %>%  # Calculate sum of 'value' for each group
  
  return(oil_demand)
}

assemble_oil_demand<- function(facets){
  oil_demand_assembled <- list()
  for (i in facets){
    series_name =  paste("demand_", i, sep = "")
    
    oil_demand_assembled[[series_name]] <- oil_demand_request(i)
  }
  
  
  return(oil_demand_assembled)
}

broadcast_monthly_data <- function(monthly_vector, df){
  # browser()
  ##### broadcasting monthly variable values
  daily_vector <- rep(NA, length(df$date))
  #                 # Ja Feb Mar Apr  May Ju Jul Au  Se  Ot  Nov Dec
  # months_length = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  month_vector <- extract_month(df$date)
  current_month = 1
  counter = 1
  
  # monthly_vector <- na.omit(monthly_vector)
  
  for (i in monthly_vector){
    while (current_month == month_vector[counter]){
      daily_vector[counter] = i
      if (counter != length(df$date))
        counter = counter + 1
      else
        break
    }
    if (current_month != 12)
      current_month = current_month + 1
    else
      current_month = 1
  }
  
  return(daily_vector)
}

rolling_windows <- function(train_data, check_data, model_function, window_size){
  n_windows = nrow(check_data) - window_size
  predictions = c()
  for (i in 1:n_windows) {
    # Define training and validation data
    # browser()
    train_subset <- train_data[i:(i + window_size - 1), ]
    check_subset <- check_data[(i + window_size), ] # test or train
    
    model <- model_function(train_subset[[2]])
    
    # Make predictions
    # check_set_x = subset(check_subset, select = -WTI)
    check_set_x = check_subset[, -2]
    new_prediction <- predict(model, newdata = check_set_x)
    class(new_prediction)
    predictions = c(predictions, extract_forecast(new_prediction))
  }
  
  # Evaluate performance 
  # check_set_y = subset(check_data, select = WTI) 
  check_set_y = check_data[[2]]
  check_set_y = check_set_y[(window_size + 1):length(check_data[[2]])] # objective (Y) column is always at position 2 after date
  
  # return error metrics
  accuracy(predictions, check_set_y)
  
  return (predictions)
}
## should be turned into a function as the procedure is largely the same
## across models. However cant figure out how to differentiate it for different models 
## requiring different parameters

extract_forecast <- function(pred){
  # Extract point forecasts
  # browser()
  if (inherits(pred, "forecast")) {
    # For exponential smoothing model
    return(pred$mean[1])
  } else if (inherits(pred, "list")){
    # for ARIMA model
    return(pred$pred)
  }
  else
    # For other models (e.g., random forest)
    return(pred)
}

create_table_from_df <- function(table, caption){
  # Create table image for presentation
  table_html <- table %>%
    kable("html", caption = caption) %>%
    kable_styling("striped", full_width = F)
  # Define the file paths
  html_file <- paste0(caption, ".html")
  pdf_file <- paste0(caption, ".pdf")
  png_file <- paste0(caption, ".png")
  
  # Save the table as an HTML file
  save_kable(table_html, file = html_file)
  
  # Render HTML to PDF
  rmarkdown::render(html_file, output_format = "pdf_document", output_file = pdf_file)
  
  # Convert PDF to PNG
  webshot2::webshot(pdf_file, file = png_file, vwidth = 1600, vheight = 900, delay = 0.2)
}

#### SUMMARY STATISTICS AND FIRST DIFFERENCING ####
summary_statistics <- function(data, commodity, data_state){
  
  if (!commodity %in% c("OIL", "GAS")) {
    stop("Invalid commodity. Please choose either 'OIL' or 'GAS'.")
  }
  
  stats_list = data.frame(
    col1 = rep(NA, 8) # Placeholder to merge later. Number corresponds to the 7 statistics well include
  )
  
  plots_acf <- list()
  plots_pacf <- list()
  
  for (i in seq_along(data)){
    ## descriptive statistics
    # browser()
    serie = data[i]
    # serie = drop_na(serie)
    name = colnames(serie)
    serie = serie[[colnames(serie)[1]]] # it's one in order for the series to "select itself" (it has just one column)
    ## ADF, JB test
    adf = tseries::adf.test(serie)$statistic
    pvalue = tseries::adf.test(serie)$p.value
    kpss= tseries::kpss.test(serie)$statistic
    jb = jarque.bera.test(serie)$statistic
    stats = c(min(serie), max(serie), mean(serie), sd(serie), adf, pvalue, jb, kpss)
    if (any(sapply(serie, is.null))) {
      stop("Error: The series contains NULL values.")
    }
    stats <- data.matrix(stats)
    ## this other option stores a table object
    # stats = summary(serie)
    # assign(name, stats)
    stats_list = cbind(stats_list, stats)
    # length(stats_list)
    colnames(stats_list)[i + 1] = name
    
    # clearing the dataframe
    stats_list$stats <- NULL
    
    ## ACF and PACF 
    acf = ggAcf(serie, main = name, lag.max = 50) + ggtitle(name)
    plots_acf[[i]] = acf
    pacf = ggPacf(serie, main = name, lag.max = 50) + ggtitle(name)
    plots_pacf[[i]] = pacf
    
    rm(name)
    
    # final_plot_acf_pacf <- final_plot_acf_pacf + acf + pacf
  }
  
  # browser()
  
  combined_acf = plot_grid(plotlist = plots_acf, ncol = 4)
  combined_pacf = plot_grid(plotlist = plots_pacf, ncol = 4)
  
  ggsave(paste0(commodity, data_state, "_combined_acf.png"), combined_acf, width = 10, height = 8)
  ggsave(paste0(commodity, data_state, "_combined_pacf.png"), combined_pacf, width = 10, height = 8)
  
  # clearing the initializing column
  stats_list$col1 <- NULL 
  
  # assigning names to rows
  rownames(stats_list) = c("min", "max", "mean", "sd", "ADF", "pvalue", "JB", "KPSS")
  
  # # Create table image for presentation
  # table_html <- stats_list %>%
  #   kable("html", caption = paste0(commodity, " Summary Statistics ", data_state)) %>%
  #   kable_styling("striped", full_width = F)
  # # Define the file paths
  # html_file <- paste0(commodity, "_summary_statistics_", data_state, ".html")
  # png_file <- paste0(commodity, "_summary_statistics_", data_state, ".png")
  # 
  # # Save the table as an HTML file
  # save_kable(table_html, file = html_file)
  # 
  # # Convert the HTML file to a PNG image
  # webshot(html_file, file = png_file, vwidth = 1600, vheight = 900)
  caption = paste0(commodity, " Summary Statistics ", data_state)
  create_table_from_df(stats_list, caption)
  
  # Create a latex table using kableExtra
  table <- kable(stats_list, "latex", caption = paste0(commodity, " Summary Statistics (pre-differencing)", data_state))
  latex_table = kable_styling(table)
  writeLines(as.character(latex_table), paste0(commodity, "_summary_statistics_", data_state, ".tex"))
  
  return(stats_list)
}
apply_first_differencing <- function(df, stats_list, commodity){
  ##### apply first differencing #####
  # browser()
  #subset non-stationary columns
  pvalues = stats_list[c('pvalue'), ]
  pvalues = t(pvalues)[1:length(pvalues)]
  non_stationary = which(pvalues > 0.1)
  non_stationary = non_stationary + 1 # skip the date column (index 1)
  names(df)[non_stationary]
  non_stationary_columns = names(df)[non_stationary]
  df_copy = df
  df[non_stationary_columns]
  
  for (i in seq_along(df[non_stationary_columns])){
    
    serie = df[non_stationary_columns][i]
    serie = drop_na(serie)
    name = paste0(colnames(serie), "_diff")
    serie = serie[[colnames(serie)[1]]]
    if (name != "gas_demand_gr_diff"){ #kinda hardcoded to avoid it differencing a broadcasted column
      serie = diff(log(serie))
      df[non_stationary_columns][i] = c(NA, serie)
      colnames(df[non_stationary_columns])[i] = name
    }
    
    rm(name)
    
  }
  if (commodity == "GAS"){
    ## apply log to HENRYHUB
    df$HENRYHUB = c(NA, diff(log(df$HENRYHUB)))
    df$HENRYHUB = replace_zero(df$HENRYHUB)
  }
  
  write_xlsx(df, paste0("data/", commodity, "_firstdifferenced.xlsx"))
  print(paste0(commodity, "_firstdifferenced.xlsx", " created"))
}
assembled_plots <- function(df, commodity){
  # browser()
  plots <- list()
  data = data.frame(df)
  data = drop_na(data)
  names = colnames(data)
  for (i in seq_along(data)){
    # browser()
    if (class(data[[i]])[1] == "numeric"){
      # var_name = names[i]
      plot = ggplot(data = data, aes_string(x = "date", y = colnames(df)[i]))+
        geom_line() +
        labs(
          title = names[i],  # Title
          x = "Date",  # X-axis label
          y = names[i]  # Y-axis label
        )
      plot
      plots[[i]] <- plot
    }
  }
  combined_plot = plot_grid(plotlist = plots,ncol = 5)
  combined_plot
  ggsave(paste0("combined_plot_", commodity, ".png"), combined_plot, width = 10, height = 8)
}

#### FUNCTIONS FOR MODELLING ####
mape <- function(actual, forecast) {
  # Define the very small value to substitute
  small_value <- 1e-10  # Adjust this value as needed
  
  # Substitute 0 with very small value
  actual <- ifelse(actual == 0, small_value, actual)
  forecast <- ifelse(forecast == 0, small_value, forecast)
  r = abs((actual - forecast) / actual)
  return(mean(r) * 100)
}

get_params <- function(model){
  if (class(model)[1] %in% c("forecast_ARIMA", "ARIMA", "Arima")){
    params = c(model$arma[1], model$arma[3], model$arma[2])
    return(params)
    
  }else if((class(model)[1] == "ets")){
    params = model$par["alpha"]
    return(params)
  }else if(class(model)[1] %in% c("TuneResult", "OptResult")){ 
    params= c(model$x["mtry"], model$x["nodesize"], model$x["ntree"], model$x["maxnodes"])
    names(params) <- c("mtry", "nodesize", "ntree", "maxnodes")
    return(params)
  }else if(class(model)[1] %in% c("train", "train.formula")){ # random forest grid search
    return(model$bestTune$mtry)
  }else{
    print("Error: unknown model class. Can't extract parameters")
  }
}

rolling_arima <- function(data, window_size, forecast_horizon, model) {
  n <- length(data[[2]])
  forecasts <- numeric(n - window_size - forecast_horizon + 1)
  
  for (i in 1:(n - window_size - forecast_horizon + 1)) {
    tryCatch({
      window_data <- data[i:(i + window_size - 1), ]
      fit <- arima(window_data[[2]], order = get_params(model))  # parameters extracted by the function from the best given model chosen by auto.arima()
      forecast <- predict(fit, n.ahead = forecast_horizon)
      forecasts[i] <- extract_forecast(forecast)
    }, error = function(e) {
      # Error-handling block
      # Print error message
      print(paste("Error:", e))
      
      # First-level error handling
      tryCatch({
        # Apply differencing to make the data stationary
        window_data[[2]] <- c(NA, diff(window_data[[2]], lag = 1, differences = 1))  # First-order differencing
        
        # Retry fitting the ARIMA model with differenced data
        fit <- arima(window_data[[2]], order = c(3, 0, 0))
        
        # Proceed with forecasting
        forecast <- predict(fit, n.ahead = forecast_horizon)
        forecasts[i] <- extract_forecast(forecast)
      }, error = function(e) {
        # Second-level error handling
        print(paste("Error (second level):", e))
        browser()  # Enter browser mode to debug
        
        # Apply differencing to make the data stationary again
        window_data[[2]] <- c(NA, diff(window_data[[2]], lag = 1, differences = 1))  # First-order differencing
        
        # Retry fitting the ARIMA model with differenced data
        fit <- arima(window_data[[2]], order = c(3, 0, 0))
        
        # Proceed with forecasting
        forecast <- predict(fit, n.ahead = forecast_horizon, newdata = )
        forecasts[i] <- extract_forecast(forecast)
      })
    })
  }
  
  return(forecasts)
}

rolling_ets <- function(data, window_size, forecast_horizon, model) {
  # browser()
  n <- length(data[[2]])
  forecasts <- numeric(n - window_size - forecast_horizon + 1)
  
  for (i in 1:(n - window_size - forecast_horizon + 1)) {
    # browser()
    window_data <- data[i:(i + window_size - 1), ]
    alpha = get_params(model)
    fit <- ets(window_data[[2]], alpha = alpha) 
    forecast <- predict(fit, n.ahead = forecast_horizon)
    forecasts[i] <- extract_forecast(forecast)
  }
  
  return(forecasts)
}

# QUESTION: should I use just validation data in the rolling windows where I am reestimating the random forest
# or would it be better also given the short length of the dataset to include training data in this step and then
# use the validation data in the predict function as newdata parameter?

rolling_rf <- function(tr_data = NULL, data, window_size, forecast_horizon, model) {
  # browser()
  n <- nrow(data)
  forecasts <- numeric(n - window_size - forecast_horizon + 1)
  
  # random forest specs
  names <- names(data)
  f <- as.formula(paste(names[2], "~", paste(names[!names %in% c(names[2], names[1])], collapse = " + ")))
  
  # j = 0
  for (i in 1:(n - window_size - forecast_horizon + 1)) {
    # browser()
    window_data <- data[i:(i + window_size - 1), ]
    # params = get_params(model)
    # fit <- randomForest(f, data = data, mtry = params$mtry,
    #                     nodesize = params$nodesize, ntree = params$ntree, maxnodes = params$maxnodes)
    fit <- randomForest(f, data = data, mtry = get_params(model))
    # fit <- randomForest(f, data = tr_data, mtry = mtry)
    
    forecast <- predict(fit, n.ahead = forecast_horizon) # newdata = data[, -c(1, 2)# removing target and date column before giving the predictor variables for the new datapoint
    # j = j + 1 
    forecasts[i] <- extract_forecast(forecast)
  }
  
  return(forecasts)
}

rolling_rnn <- function(valdata, window_size, forecast_horizon, model) {
  # browser()
  # Check if valdata is a 3D array
  if (length(dim(valdata)) != 3) {
    stop("valdata must be a 3D array")
  }
  
  if (model == '<pointer: 0x0>'){ # abort if model is NULL
    stop("Model is a NULL pointer")
  } 
  
  n <- dim(valdata)[1]
  num_features <- dim(valdata)[3]
  forecasts <- numeric(n - window_size - forecast_horizon + 1)
  
  for (i in 1:(n - window_size - forecast_horizon + 1)) {
    window_data <- valdata[i:(i + window_size - 1), ,]
    
    # Reshape window_data for Keras model prediction
    x_new <- array(window_data, dim = c(1, window_size, num_features))
    
    # Make prediction using the Keras model
    forecast <- predict(model, x_new)
    
    # Extract the forecast value (assuming forecast returns a numeric value)
    forecasts[i] <- forecast[1]
  }
  
  return(forecasts)
}


evaluate_window_size <- function(tr_data = NULL, val_data, window_sizes, forecast_horizon, func, model) {
  # browser()
  actual_values <- val_data[(max(window_sizes) + forecast_horizon):length(data), ]
  results <- data.frame(WindowSize = integer(), MAE = numeric(), MSE = numeric(), MAPE = numeric())
  
  
  for (window_size in window_sizes) {
    # browser()
      forecasts <- func(data = val_data, window_size = window_size, forecast_horizon = forecast_horizon, model = model)
      forecasts <- forecasts[1:length(actual_values[[1]])]
      
    
    mae <- mae(actual_values[[2]], forecasts)
    mse <- mse(actual_values[[2]], forecasts)
    rmse <- rmse(actual_values[[2]], forecasts)
    mape <- mape(actual_values[[2]], forecasts)
    
    results <- rbind(results, data.frame(WindowSize = window_size, MAE = mae, MSE = mse, RMSE = rmse, MAPE = mape))
  }
  
  return(results)
}

# having found Inf or Na values in the accuracy table, I have created a function to replace 0s with very small numbers
# in order not to have division by 0
replace_zero = function(numeric_vector){
  # Define the very small value to substitute
  small_value <- 1e-10  # Adjust this value as needed
  
  # Substitute 0 with very small value
  numeric_vector <- ifelse(numeric_vector == 0, small_value, numeric_vector)
  
  return(numeric_vector)
}
