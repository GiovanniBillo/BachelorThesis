## GAS data analysis

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
ssh(library(writexl))

source("code/functions.R")

#### setup for EIA data ####
eia_key = "VXgY9PMtxMquGidojwzaA20dWBXV2Na3fhLlD6be"
eia_set_key(eia_key)


#### CONSTANTS ####
START_DATE_MONTHLY <- "2017-01"
END_DATE_MONTHLY <- "2022-01"

START_DATE_DAILY = as.Date("2017-01-01")
END_DATE_DAILY = as.Date("2022-01-01") # eia has no supply/demand data after the end of 2022

####FRED DATA####
fred_apiKey = "7fad28cf6bb7cdbe771756fbe0730fc3"

fred <- FredR(fred_apiKey)

# explained variable
HENRYHUB <- fred$series.observations(series_id = 'DHHNGSP')
# explanatory variables
heating_oil <- fred$series.observations(series_id = 'DHOILNYH')
wti <- fred$series.observations(series_id = 'DCOILWTICO')
# dj <- fred$series.observations(series_id = 'DJIA') # not necessary as follows sp
sp <- fred$series.observations(series_id = 'SP500')

series_fred = list(HENRYHUB= HENRYHUB, heating_oil = heating_oil, sp = sp, wti= wti)

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

#### Local Data ####
natural_gas_futures <- read_csv("data/gas data/Natural Gas Futures Historical Data.csv")
natural_gas_futures$Date = as.Date(natural_gas_futures$Date, format="%m/%d/%Y") 
natural_gas_futures = natural_gas_futures %>% select(
      Date,
      Price
    ) %>% subset(Date >= START_DATE_DAILY & Date<  END_DATE_DAILY) %>%
    rename(date = Date, 
           gas_future = Price)


#### EIA DATA ####
# facets are necessary for the api request
# they subset the search for specific parameters
facets_gas_process = list(
    "FGC",
    "FGG",
    "FGO",
    "FGS",
    "FGW")
facets_gas_series = list(
    "N9011US2",
    "N9012US2",
    "NGM_EPG0_FGC_NUS_MMCF",
    "NGM_EPG0_FGS_NUS_MMCF")
facets_states = list(
  "N3010AL2", "N3010AR2", "N3010AZ2", "N3010CA2", "N3010CO2",
  "N3010CT2", "N3010DC2", "N3010FL2", "N3010GA2", "N3010IA2",
  "N3010ID2", "N3010IL2", "N3010IN2", "N3010KS2", "N3010KY2",
  "N3010LA2", "N3010MA2", "N3010MD2", "N3010MI2", "N3010MN2",
  "N3010MO2", "N3010MS2", "N3010NC2", "N3010NE2", "N3010NJ2",
  "N3010NM2", "N3010NV2", "N3010NY2", "N3010OH2", "N3010OK2",
  "N3010OR2", "N3010PA2", "N3010SC2", "N3010TN2", "N3010TX2",
  "N3010US2", "N3010UT2", "N3010VA2", "N3010WA2", "N3010WI2",
  "N3010WV2"
)


eia_dir('natural-gas/prod')
gas_supply = eia_data(
  dir = "natural-gas/prod/sum",
  data = "value",
  freq = "monthly",
  facets = list(process = facets_gas_process, series = facets_gas_series),
  start =  START_DATE_MONTHLY,
  end =  END_DATE_MONTHLY,
  sort = list(cols = "period", order = "asc"))

gas_supply_tot = gas_supply %>% select(period,value) %>% 
  mutate(
    period = format_date(period), # Convert 'period' column to date format
    value = as.numeric(value) # Convert 'value' column to numeric
  ) %>% 
  group_by(period)  %>%  # Group by 'period' and 'units'
  summarise(total_supply = sum(value))  # Calculate sum of 'value' for each group

gas_demand_residential = eia_data(
  dir = "natural-gas/cons/sum",
  data = "value",
  freq = "monthly",
  facets = list(series = facets_states),
  start =  START_DATE_MONTHLY,
  end =  END_DATE_MONTHLY,
  sort = list(cols = "period", order = "asc"))

gas_demand_residential = gas_demand_residential %>% select(period,value) %>% drop_na() %>% 
  mutate(
    period = format_date(period), # Convert 'period' column to date format
    value = as.numeric(value) # Convert 'value' column to numeric
  ) %>% 
  group_by(period)  %>%  # Group by 'period' and 'units'
  summarise(total_demand = sum(value))  # Calculate sum of 'value' for each group

gas_demand_industrial = eia_data(
  dir = "natural-gas/cons/sum",
  data = "value",
  freq = "monthly",
  facets = list(series = "N3035US2"),
  start =  START_DATE_MONTHLY,
  end =  END_DATE_MONTHLY,
  sort = list(cols = "period", order = "asc"))

gas_demand_industrial = gas_demand_industrial %>% select(period,value) %>%  drop_na() %>% 
  mutate(
    period = format_date(period), # Convert 'period' column to date format
    value = as.numeric(value)) # Convert 'value' column to numeric)

gas_demand_tot = merge(gas_demand_industrial, gas_demand_residential, by = "period", all = T)
gas_demand_tot$demand = rowSums(gas_demand_tot[, c("value", "total_demand")], na.rm = TRUE)
gas_demand_tot$value = NULL
gas_demand_tot$total_demand = NULL

rm(gas_demand_industrial)
rm(gas_demand_residential)
rm(gas_supply)

#### Geopolitical Risk Data ####
gpr <- read_csv("data/gpr/data_gpr_daily_recent.csv")
gpr <- gpr %>% 
  select(
    DAY,
    GPRD) %>>%
  mutate(
    date = as.Date(as.character(DAY), format = "%Y%m%d"))%>% subset(date >= START_DATE_DAILY & date <=  END_DATE_DAILY)->gpr
gpr$DAY <- NULL


#### merging the data together
series_fred = Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), series_fred)
df = merge(series_fred, natural_gas_futures, by = "date", all = TRUE)
#### !!!!!
gas_demand_tot$demand_growth_rate = c(c(NA, diff(gas_demand_tot$demand))/lag(gas_demand_tot$demand, default = NA)) * 100
gas_supply_tot$supply_growth_rate = c(c(NA, diff(gas_supply_tot$total_supply))/lag(gas_supply_tot$total_supply, default = NA)) * 100

df$gas_supply_gr = broadcast_monthly_data(gas_supply_tot$total_supply, df)
df$gas_demand_gr = broadcast_monthly_data(gas_demand_tot$demand, df)

# add day of the month column 
df$day = assign_day_month(df$date)

df = merge(df, gpr, by = 'date', all = TRUE)
df = data.frame(df)
df = drop_na(df)

# Write the dataframe to an Excel file
write_xlsx(df, "data/complete_dataframe_gas.xlsx")

