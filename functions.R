

assign_day_month <-function(dates_vector){
  day_vector <- c()
  for (i in seq_along(dates_vector)){
    day_of_month = c(substr(as.character(dates_vector[i]), start=c(9), stop =c(10)))
    day_vector <- c(day_vector, day_of_month)
  }
  return(day_vector)
}
# dfs$day = assign_day_month(dfs$date)
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
      if (counter != length(dfs$date))
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

## should be turned into a function as the procedure is largely the same
## across models. However cant figure out how to differentiate it for different models 
## requiring different parameters

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
  png_file <- paste0(caption, ".png")
  
  # Save the table as an HTML file
  save_kable(table_html, file = html_file)
  
  # Convert the HTML file to a PNG image
  webshot(html_file, file = png_file, vwidth = 1600, vheight = 900)
}
