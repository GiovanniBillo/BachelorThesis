#### SUMMARY STATISTICS 
setwd("C:/Users/billo/OneDrive/Desktop/FAU/Thesis/data")

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


## Read in data (assembled beforehand) ##
data_oil <- read_excel("data/complete_dataframe_oil.xlsx")
data_gas <- read_excel("data/complete_dataframe_gas.xlsx")

#### SUMMARY STATISTICS (PRE-DIFFERENCING) ####
# remove insignificant columns
pos_oil = which(names(data_oil) %in% c("day_of_month", "day", "date"))
pos_gas = which(names(data_gas) %in% c("day_of_month", "day", "date"))

values_oil = names(data_oil)[-pos_oil]
stats_oil = summary_statistics(data_oil[values_oil], "OIL", "(Pre-differencing)")

values_gas = names(data_gas)[-pos_gas]
stats_gas = summary_statistics(data_gas[values_gas], "GAS", "(Pre-differencing)")

apply_first_differencing(data_oil, stats_oil, "OIL")
apply_first_differencing(data_gas, stats_gas, "GAS")

#### SUMMARY STATISTICS (POST-DIFFERENCING) ####
data_oil_fd <- read_excel("OIL_firstdifferenced.xlsx")
data_oil_fd = drop_na(data_oil_fd)
stats_oil_pd = summary_statistics(data_oil_fd[values_oil], "OIL", "(Post-differencing)")

data_gas_fd <- read_excel("data/GAS_firstdifferenced.xlsx")
data_gas_fd = drop_na(data_gas_fd)
stats_gas_pd = summary_statistics(data_gas_fd[values_gas], "GAS", "(Post-differencing)")
