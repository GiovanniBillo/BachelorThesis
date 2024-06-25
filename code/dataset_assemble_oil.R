#### working directory ####
setwd("C:/Users/billo/OneDrive/Desktop/FAU/Thesis/data")
#### packages ####
install.packages("eia") # access to US energy administration data
devtools::install_github("jcizel/FredR")
install.packages("oecd")
install.packages("patchwork")
install.packages("kableExtra")
install.packages('writexl')
install.packages("TimeSeries")
install.packages("aTSA")
install.packages("keras")
install.packages("cowplot")
install.packages("png")
install.packages("gt")
install.packages("webshot2")
webshot2::install_phantomjs()
install.packages("readxl")

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

source("functions.R")

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
dj <- fred$series.observations(series_id = 'DJIA')
sp <- fred$series.observations(series_id = 'SP500')
dubai <- fred$series.observations(series_id = 'POILDUBUSDM') # monthly data

series_fred = list(WTI= WTI, brent = brent, dj = dj, sp = sp, dubai = dubai)

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
brent_futures <- read_csv("oil data/Brent oil futures historical data.csv")
wti_futures <- read_csv("oil data/Crude Oil WTI Futures Historical Data.csv")
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

# graphing 
plots <- list()

for (i in seq_along(dfs)){
  # browser()
  data = data.frame(dfs[[i]])
  data = drop_na(data)
  names = colnames(data)
  plot = ggplot(data = data, aes(x = date, y = .data[[names[2]]]))+
    geom_line() +
    labs(
      title = paste(names[2], "prices"),  # Title
      x = "Date",  # X-axis label
      y = names[2]  # Y-axis label
    )
    plots[[i]] <- plot
}

combined_plot = plot_grid(plotlist = plots, ncol = 4)
combined_plot
ggsave("combined_plot.png", combined_plot, width = 10, height = 8)


# length(diff(dfs$dubai$dubai))
# length(lag(dfs$dubai$dubai, default = NA))

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
gpr <- read_csv("gpr/data_gpr_daily_recent.csv")
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

# dubai_monthly_growth_rate <- c(diff(dfs$dubai$dubai)/lag(dfs$dubai$dubai, default = NA)) * 100 # the default NA option substitutes the c(NA, ...)

SD$oil_demand_gr = c(c(NA, diff(SD$total_demand))/lag(SD$total_demand, default = NA)) * 100
SD$oil_supply_gr = c(c(NA, diff(SD$total_production))/lag(SD$total_production, default = NA)) * 100

## merge demand and supply data to the main dataframe

dfs$oil_demand_gr <- broadcast_monthly_data(SD$oil_demand_gr, dfs)
dfs$oil_supply_gr <- broadcast_monthly_data(SD$oil_supply_gr, dfs)

dfs <- drop_na(dfs)

# Write the dataframe to an Excel file
write_xlsx(dfs, "complete_dataframe_oil.xlsx")

####OLD####
## SUMMARY STATISTICS (PRE-DIFFERENCING) ####

## removing insignificant columns
pos = which(names(dfs) %in% c("month", "day_of_month", "day", "date"))
values = names(dfs)[-pos]

stats_list = data.frame(
  col1 = rep(NA, 8)
)

plots_acf <- list()
plots_pacf <- list()

for (i in seq_along(dfs[values])){
  ## descriptive statistics
  # browser()
  serie = dfs[values][i]
  serie = drop_na(serie)
  name = colnames(serie)
  serie = serie[[colnames(serie)[1]]] # it's one in order for the series to "select itself" (it has just one column)
  
  
  ## ADF, JB test
  adf = c(adf.test(serie)$statistic, adf.test(serie)$p.value)
  kpss = kpss.test(serie)$statistic
  jb = jarque.bera.test(serie)$statistic
  stats = c(min(serie), max(serie), mean(serie), sd(serie), adf, jb, kpss)
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
combined_acf = plot_grid(plotlist = plots_acf, ncol = 4)
combined_pacf = plot_grid(plotlist = plots_pacf, ncol = 4)

ggsave("combined_acf.png", combined_acf, width = 10, height = 8)
ggsave("combined_pacf.png", combined_pacf, width = 10, height = 8)

# clearing the initializing column
stats_list$col1 <- NULL 

# assigning names to rows
rownames(stats_list) = c("min", "max", "mean", "sd", "ADF", "pvalue", "JB", "KPSS")

# Create table image for presentation
table_html <- stats_list %>%
  kable("html", caption = "Oil Summary Statistics (pre-differencing)") %>%
  kable_styling("striped", full_width = F)
# Define the file paths
html_file <- "oil_summary_statistics_prediff.html"
png_file <- "oil_summary_statistics_prediff.png"

# Save the table as an HTML file
save_kable(table_html, file = html_file)

# Convert the HTML file to a PNG image
webshot(html_file, file = png_file, vwidth = 1600, vheight = 900)
# Create a latex table using kableExtra
table <- kable(stats_list, "latex", caption = "oil summary statistics (pre-differencing")
latex_table = kable_styling(table)
writeLines(as.character(latex_table), "OIL_summary_statistics_pre_diff.tex")


### HOW IS IT POSSIBLE THAT THERE IS A NEGATIVE PRICE OF PETROLEUM? ####
subset(dfs, WTI < 0)


apply_first_differencing <- function(df, stats_list){
  ##### apply first differencing #####
  
  #subset non-stationary columns
  pvalues = stats_list[c('pvalue'), ]
  pvalues = t(pvalues)[1:length(pvalues)]
  non_stationary = which(pvalues > 0.1)
  non_stationary = non_stationary + 1 # skip the date column (index 1)
  names(dfs)[non_stationary]
  non_stationary_columns = names(dfs)[non_stationary]
  dfs_copy = dfs
  dfs[non_stationary_columns]
  
  for (i in seq_along(df[non_stationary_columns])){
    
    serie = dfs[non_stationary_columns][i]
    serie = drop_na(serie)
    name = paste0(colnames(serie), "_diff")
    serie = serie[[colnames(serie)[1]]]
    serie = diff(log(serie))
    
    dfs[non_stationary_columns][i] = c(NA, serie)
    colnames(dfs[non_stationary_columns])[i] = name
    
    rm(name)
    
    # final_plot_acf_pacf <- final_plot_acf_pacf + acf + pacf
  }
  
  write_xlsx(dfs, "OIL_firstdifferenced.xlsx")
}

#### apply first differencing #####

#subset non-stationary columns
pvalues = stats_list[c('pvalue','min'), ]
pvalues = t(pvalues)[1:length(pvalues)]
non_stationary = which(pvalues > 0.1)
non_stationary = non_stationary + 1 # skip the date column (index 1)
names(dfs)[non_stationary]
non_stationary_columns = names(dfs)[non_stationary]
dfs_copy = dfs
dfs[non_stationary_columns]

for (i in seq_along(dfs[non_stationary_columns])){
  
  serie = dfs[non_stationary_columns][i]
  serie = drop_na(serie)
  name = paste0(colnames(serie), "_diff")
  serie = serie[[colnames(serie)[1]]]
  serie = diff(log(serie))

  dfs[non_stationary_columns][i] = c(NA, serie)
  colnames(dfs[non_stationary_columns])[i] = name
  
  rm(name)

  # final_plot_acf_pacf <- final_plot_acf_pacf + acf + pacf
}

write_xlsx(dfs, "OIL_firstdifferenced.xlsx")

#### SUMMARY STATISTICS (POST-DIFFERENCING) ####
## removing insignificant columns
pos = which(names(dfs) %in% c("month", "day_of_month", "day", "date"))
values = names(dfs)[-pos]

stats_list_pd = data.frame(
  col1 = rep(NA, 8)
)

plots_acf <- list()
plots_pacf <- list()

for (i in seq_along(dfs[values])){
  ## descriptive statistics
  # browser()
  serie = dfs[values][i]
  serie = drop_na(serie)
  name = colnames(serie)
  serie = serie[[colnames(serie)[1]]] # it's one in order for the series to "select itself" (it has just one column)
  
  
  ## ADF, JB test
  adf = c(adf.test(serie)$statistic, adf.test(serie)$p.value)
  kpss = kpss.test(serie)$statistic
  jb = jarque.bera.test(serie)$statistic
  stats = c(min(serie), max(serie), mean(serie), sd(serie), adf, jb, kpss)
  stats <- data.matrix(stats)
  ## this other option stores a table object
  # stats = summary(serie)
  # assign(name, stats)
  stats_list_pd = cbind(stats_list_pd, stats)
  length(stats_list)
  colnames(stats_list_pd)[i + 1] = name
  
  # clearing the dataframe
  stats_list_pd$stats <- NULL
  
  
  ## ACF and PACF 
  acf = ggAcf(serie, main = name, lag.max = 50) + ggtitle(name)
  plots_acf[[i]] = acf
  pacf = ggPacf(serie, main = name, lag.max = 50) + ggtitle(name)
  plots_pacf[[i]] = pacf
  
  rm(name)
  
  # final_plot_acf_pacf <- final_plot_acf_pacf + acf + pacf
}
combined_acf_pd = plot_grid(plotlist = plots_acf, ncol = 5)
combined_pacf_pd = plot_grid(plotlist = plots_pacf, ncol = 5)

ggsave("combined_acf_pd.png", combined_acf_pd, width = 10, height = 8)
ggsave("combined_pacf_pd.png", combined_pacf_pd, width = 10, height = 8)
  
# final_plot_acf_pacf <- final_plot_acf_pacf + acf + pacf

# clearing the initializing column
stats_list_pd$col1 <- NULL 

# assigning names to rows
rownames(stats_list_pd) = c("min", "max", "mean", "sd", "ADF", "pvalue", "JB", "KPSS")
stats_list_pd

# Create table image for presentation
table_html <- stats_list_pd %>%
  kable("html", caption = "Oil Summary Statistics(post-differencing") %>%
  kable_styling("striped", full_width = F)
# Define the file paths
html_file <- "oil_summary_statistics_postdiff.html"
png_file <- "oil_summary_statistics_postdiff.png"

# Save the table as an HTML file
save_kable(table_html, file = html_file)

# Convert the HTML file to a PNG image
webshot(html_file, file = png_file, vwidth = 1600, vheight = 900)
# Create a table using kableExtra
table <- kable(stats_list_pd, "latex", caption = "summary statistics")
latex_table = kable_styling(table)
writeLines(as.character(latex_table), "OIL_summary_statistics_post_differencing.tex")
# we can now see both in the ADF test statistic and in the ACF graphs that the series are stationary