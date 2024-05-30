#### SUMMARY STATISTICS 
##### TEST github

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



## function definitions ##
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
  
  # Create table image for presentation
  table_html <- stats_list %>%
    kable("html", caption = paste0(commodity, " Summary Statistics ", data_state)) %>%
    kable_styling("striped", full_width = F)
  # Define the file paths
  html_file <- paste0(commodity, "_summary_statistics_", data_state, ".html")
  png_file <- paste0(commodity, "_summary_statistics_", data_state, ".png")
  
  # Save the table as an HTML file
  save_kable(table_html, file = html_file)
  
  # Convert the HTML file to a PNG image
  webshot(html_file, file = png_file, vwidth = 1600, vheight = 900)
  # Create a latex table using kableExtra
  table <- kable(stats_list, "latex", caption = paste0(commodity, " Summary Statistics (pre-differencing)", data_state))
  latex_table = kable_styling(table)
  writeLines(as.character(latex_table), paste0(commodity, "_summary_statistics_", data_state, ".tex"))
  
  return(stats_list)
}
apply_first_differencing <- function(df, stats_list, commodity){
  ##### apply first differencing #####
  
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
    serie = diff(log(serie))
    
    df[non_stationary_columns][i] = c(NA, serie)
    colnames(df[non_stationary_columns])[i] = name
    
    rm(name)
    
    # final_plot_acf_pacf <- final_plot_acf_pacf + acf + pacf
  }
  
  write_xlsx(df, paste0(commodity, "_firstdifferenced.xlsx"))
  print(paste0(commodity, "_firstdifferenced.xlsx", " created"))
}

## Read in data (assembled beforehand) ##
data_oil <- read_excel("complete_dataframe.xlsx")
data_gas <- read_excel("complete_dataframe_gas.xlsx")

#### SUMMARY STATISTICS (PRE-DIFFERENCING) ####
# remove insignificant columns
pos = which(names(data_oil) %in% c("month", "day_of_month", "day", "date"))

values_oil = names(data_oil)[-pos]
stats_oil = summary_statistics(data_oil[values_oil], "OIL", "(Pre-differencing)")

values_gas = names(data_gas)[-pos]
stats_gas = summary_statistics(data_gas[values_gas], "GAS", "(Pre-differencing)")

apply_first_differencing(data_oil, stats_oil, "OIL")
apply_first_differencing(data_gas, stats_gas, "GAS")

data_gas_fd <- read_excel("GAS_firstdifferenced.xlsx")
data_gas_fd = drop_na(data_gas_fd)
stats_gas_pd = summary_statistics(data_gas_fd[values_gas], "GAS", "(Post-differencing)")



#### removing insignificant columns ####
pos = which(names(dfs) %in% c("month", "day_of_month", "day", "date"))
values = names(dfs)[-pos]

stats_list = data.frame(
  col1 = rep(NA, )
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
