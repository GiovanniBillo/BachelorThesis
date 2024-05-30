devtools::install_github("jcizel/FredR")
library(FredR)
library(pipeR)
library(dplyr)
library(readr)

assign_day_month <-function(dates_vector){
  df <- data.frame()
  for (i in seq_along(dates_vector)){
    day_of_month = c(substr(as.character(dates_vector[i]), start=c(9), stop =c(10)))
    df <- rbind(df, day_of_month)
  }
  return(df)
}

    apiKey = "7fad28cf6bb7cdbe771756fbe0730fc3"
    fred <- FredR(apiKey)
    str(fred,1)
    oil.series <- fred$series.search("Oil")
    # we want WTI (WTISPLC), Brent (DCOILBRENTEU)
    WTI <- fred$series.observations(series_id = 'DCOILWTICO')
    WTI %>>%
      select(
        date,
        value
      ) %>>%
      mutate(
        date = as.Date(date),
        value = as.numeric(value)
      ) %>% filter(date >= '2019-01-01' & date < '2024-01-01')->WTI
    # oil_sub <- oil %>% filter(date >= '2019-01-01' & date < '2024-01-01')
    require(ggplot2)
    qplot(data = WTI, x = date, y = value, geom = 'line')
    # explanatory variables
    brent <- fred$series.observations(series_id = 'DCOILBRENTEU')
    stock.series <- fred$series.search("Dow Jones") # DJIA
    dj <- fred$series.observations(series_id = 'DJIA')
    series_fred = list(WTI= WTI, brent = brent, dj = dj)
    names(series_fred)[1]
    
    # filtering and renaming the series from FRED
    for (i in seq_along(series_fred)){
      series_fred[[i]] %>>%
        select(
          date,
          value
        ) %>>%
        mutate(
          date = as.Date(date),
          value = as.numeric(value)
        ) %>% filter(date >= '2019-01-01' & date < '2024-01-01') %>%
        rename_with(~ names(series_fred)[i], value) ->series_fred[[i]]
    }
    
    #importing series from local
    brent_futures <- read_csv("oil data/Brent oil futures historical data-01-01-2019.csv")
    wti_futures <- read_csv("oil data/Crude Oil WTI Futures Historical Data 01-01-2019.csv")
    dubai_futures <- read_csv("oil data/Dubai Crude Oil (Platts) Financial Futures Historical Data.csv")
    futures = list(brent_futures = brent_futures, wti_futures= wti_futures, dubai_futures = dubai_futures)
    
    # filtering and renaming the series from local
    for (i in seq_along(futures)) {
      # Check if the 'Date' column exists
      if ("Date" %in% names(futures[[i]])) {
        # Convert the Date column to Date objects
        futures[[i]]$Date <- as.Date(futures[[i]]$Date, format="%m/%d/%Y")
        print("Date Converted!")
        # Check the class of the Date column after conversion
        print(class(futures[[i]]$Date))
      } else {
        # If 'Date' column doesn't exist, print a message
        print("Date column not found in the data frame.")
      }
      futures[[i]] <- futures[[i]] %>%
        select(
          Date,
          Price
        ) %>% filter(Date >= '2019-01-01' & Date < '2024-01-01') %>%
        rename(date = Date) %>% rename_with(~ names(futures)[i], Price)
    }
    
    # merging all the series together 
#     dfs <- list(series_fred[1], series_fred[2], series_fred[3], futures[1], futures[2], futures[3])
# merged_df <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), dfs)
#     merged_df <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), dfs)
    
    dfs <- list(series_fred[[1]], series_fred[[2]], series_fred[[3]], futures[[1]], futures[[2]], futures[[3]])
    merged_df <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), dfs)
    
    # useful test to see if all dataframes have a common column
    # that is if they can be merged
    lapply(dfs, function(df) "date" %in% names(df))
    
# testing out the function
day_of_month = c(substr(as.character(merged_df$date[1]), start=c(9), stop =c(10)))
assign_day_month(merged_df$date)

# adding the day of month to the dataset
merged_df$day_of_month <-assign_day_month(merged_df$date)

