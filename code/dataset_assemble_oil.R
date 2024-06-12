#### working directory ####
setwd("C:/Users/billo/OneDrive/Desktop/FAU/Thesis/data")
#### packages ####
# CRAN packages
install.packages(c(
  "eia",          # Access to US Energy Administration data
  "patchwork",    # Combine multiple ggplots
  "kableExtra",   # Create complex tables
  "writexl",      # Write Excel files
  "TimeSeries",   # Time series analysis
  "aTSA",         # Advanced Time Series Analysis
  "cowplot",      # Plotting
  "png",          # Read and write PNG images
  "gt",           # Create tables
  "webshot2",     # Take screenshots of web pages
  "readxl"        # Read Excel files
))

# GitHub packages
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("jcizel/FredR")  # Access to FRED data

#### library calls ####
ssh <- suppressPackageStartupMessages
ssh(library(OECD))
ssh(library(FredR))
ssh(library(eia))
ssh(library(pipeR))
ssh(library(dplyr))
ssh(library(readr))
ssh(library(tidyr))
ssh(library(ggplot2))
ssh(library(patchwork))
ssh(library(tseries)) # adf test
ssh(library(kableExtra)) # save df as image
ssh(library(forecast))
ssh(library(far))
ssh(library(cowplot))
ssh(library(png))
ssh(library(gt))
ssh(library(webshot2))
ssh(library(readxl))
ssh(library(writexl))

source("code/functions.R")
#### setup for EIA data ####
eia_key = "VXgY9PMtxMquGidojwzaA20dWBXV2Na3fhLlD6be"
eia_set_key(eia_key)


#### CONSTANTS ####
START_DATE_MONTHLY <- "2015-01"
END_DATE_MONTHLY <- "2024-01"

START_DATE_DAILY = as.Date("2015-01-01")
END_DATE_DAILY = as.Date("2024-01-01")

facets = list("EP00",
              "EPC0",
              "EPD0",
              "EPD00H",
              "EPDM10",
              "EPDXL0")
facets_all <- list("EP00", "EPC0", "EPD0", "EPD00H", "EPDM10", "EPDXL0", "EPJK", "EPL0", "EPL2", "EPLL", "EPLLB0I", "EPLLB0N", "EPLLBAI", "EPLLBAN", "EPLLBYI", "EPLLBYN", "EPLLE", "EPLLEA", "EPLLEY", "EPLLNG", "EPLLPA", "EPLLPY", "EPLLPZ", "EPLOLE", "EPLP", "EPM0C", "EPM0F", "EPM0R", "EPOBG", "EPOBGC0", "EPOBGR0", "EPOBV", "EPOL", "EPOO", "EPOORD", "EPOORDB", "EPOORDO", "EPOORO", "EPP2", "EPPA", "EPPC", "EPPCC", "EPPCM", "EPPK", "EPPL", "EPPM", "EPPNS", "EPPP0", "EPPPN", "EPPPO", "EPPR", "EPPS", "EPPU", "EPPV", "EPPW")

####FRED DATA####
fred_apiKey = "7fad28cf6bb7cdbe771756fbe0730fc3"

fred <- FredR(fred_apiKey)

# we want WTI (WTISPLC), Brent (DCOILBRENTEU), Dow Jones, S&P 500, Dubai Crude(POILDUBUSDM)
  
# explained variable
WTI <- fred$series.observations(series_id = 'DCOILWTICO')
# explanatory variables
brent <- fred$series.observations(series_id = 'DCOILBRENTEU')
stock.series <- fred$series.search("Dow Jones") # DJIA
# dj <- fred$series.observations(series_id = 'DJIA')
sp <- fred$series.observations(series_id = 'SP500')
dubai <- fred$series.observations(series_id = 'POILDUBUSDM') # monthly data

series_fred = list(WTI= WTI, brent = brent, sp = sp, dubai = dubai)

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
        ) %>% 
        subset(date >= START_DATE_DAILY & date <=  END_DATE_DAILY) %>% 
        rename_with(~ names(series_fred)[i], value) ->series_fred[[i]]
    }

    
#importing series from local
brent_futures <- read_csv("data/oil data/Brent oil futures historical data.csv")
wti_futures <- read_csv("data/oil data/Crude Oil WTI Futures Historical Data.csv")
dubai_futures <- read_csv("data/oil data/Dubai Crude Oil (Platts) Financial Futures Historical Data.csv")
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
      # browser()
      } 
    else {
        # If 'Date' column doesn't exist, print a message
        print("Date column not found in the data frame.")
    }
    futures[[i]] <- futures[[i]] %>% 
      select(
        Date,
        Price
      ) %>% subset(Date >= START_DATE_DAILY & Date<  END_DATE_DAILY) %>%
        rename(date = Date) %>% rename_with(~ names(futures)[i], Price)
    }

#### Merging all series all together ####
dfs <- c()
for (i in seq_along(series_fred)){
  dfs <- c(dfs, series_fred[i])
}
for (i in seq_along(futures)){
  dfs <- c(dfs, futures[i])
}

dubai_monthly_growth_rate <- c(c(NA, diff(dfs$dubai$dubai))/lag(dfs$dubai$dubai, default = NA)) * 100 # the default NA option substitutes the c(NA, ...)
# df$dubai_growth_rate <- c(NA, diff(dubai_monthly) / lag(dubai_monthly, default = NA)) * 100

dfs = Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), dfs)
dfs <- as.data.frame(dfs)
# removing duplicate date columns
duplicates <- duplicated(names(dfs))
dfs <- dfs[!duplicates]

dfs$day <-assign_day_month(dfs$date)

names(dfs)[names(dfs) == "day$X.01."] <- "day"

##### broadcasting monthly variable values
dfs$daily_dubai_growth_rate = broadcast_monthly_data(dubai_monthly_growth_rate, dfs)

# broadcasted values
dfs$daily_dubai

# removing monthly values
dfs$dubai <- NULL

# Print the resulting dataframe
print(dfs)

# drop NAs
dfs <- drop_na(dfs)

#### Geopolitical Risk Data ####
gpr <- read_csv("data/gpr/data_gpr_daily_recent.csv")
gpr <- gpr %>% 
  select(
    DAY,
    GPRD) %>>%
  mutate(
    date = as.Date(as.character(DAY), format = "%Y%m%d"))%>% subset(date >= START_DATE_DAILY & date <=  END_DATE_DAILY)->gpr
dfs = merge(dfs, gpr, by = "date", all.x = TRUE)
dfs$DAY <- NULL

#### EIA data about oil ####

## usage example ##
# (d <- eia_data(
#   dir = "electricity/retail-sales",
#   data = "sales",
#   facets = list(stateid = "OH", sectorid = "RES"),
#   freq = "annual",
#   start = "2010",
#   sort = list(cols = "period", order = "asc"),
# ))
# rm(d)

##### oil supply ###
oil_supply <- eia_data(
  dir = "petroleum/crd/crpdn",
  data = "value",
  freq = "monthly",
  start =  START_DATE_MONTHLY,
  end =  END_DATE_MONTHLY,
  sort = list(cols = "period", order = "asc"),
) %>>%
  select(
    period,
    value,
    units) %>%
  mutate(period = format_date(period), value = as.numeric(value)) %>% # Convert 'period' column to date format
  group_by(period, units) %>% # Extract year-month
  summarise(total_production = sum(value)) # Aggregate data (replace 'production_column' with the actual name of your production column)

# operations to aggregate supply based on the different units, MBBL and MBBL/D (analogous to those done for demand)
aggregate_units = ifelse(oil_supply$units == "MBBL/D", oil_supply$total_production * 30, oil_supply$total_production)
oil_supply$total_production = aggregate_units
oil_supply <- oil_supply %>% group_by(period) %>% summarize(total_production = sum(total_production)) %>% rename(date = period)

##### oil demand
oil_demand = assemble_oil_demand(facets)
merged_demand <- Reduce(function(x, y) merge(x, y, by = c("period"), all = TRUE), oil_demand)

col_names = c("date")
for (facet in facets){
  col_name = paste0("demand_", facet)
  col_names = c(col_names, col_name)
}
colnames(merged_demand) = col_names

# get the vector index of demand columns ( all starting with the string "demand_")
s = grep("demand_", colnames(merged_demand))
colnames(merged_demand)[s]

# aggregate over all products
merged_demand$total_demand = rowSums(merged_demand[, c(colnames(merged_demand)[s])])

# remove unnecessary columns
merged_demand[, c(colnames(merged_demand)[s])] <- NULL

# Finally unite supply and demand
SD = merge(oil_supply,  merged_demand, by = 'date')
SD$oil_demand_gr = c(c(NA, diff(SD$total_demand))/lag(SD$total_demand, default = NA)) * 100
SD$oil_supply_gr = c(c(NA, diff(SD$total_production))/lag(SD$total_production, default = NA)) * 100

## merge demand and supply data to the main dataframe

dfs$oil_demand_gr <- broadcast_monthly_data(SD$oil_demand_gr, dfs)
dfs$oil_supply_gr <- broadcast_monthly_data(SD$oil_supply_gr, dfs)

dfs <- drop_na(dfs)

# Write the dataframe to an Excel file
write_xlsx(dfs, "data/complete_dataframe_oil.xlsx")
