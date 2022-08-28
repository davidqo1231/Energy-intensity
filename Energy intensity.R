library(tidyverse)
library(openxlsx)
library(readxl)
library(zoo)
library(data.table)
library(lubridate)
library(sjmisc)
library(stringr)

# List of countries
countries <- c("AUS", "AUT", "BEL", "BGR", "BRA", "CAN", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN",
               "FRA", "GBR", "GRC", "HRV", "HUN", "CHE", "CHN", "IDN", "IND", "IRL", "ITA", "JPN", "KOR", 
               "LTU", "LUX", "LVA", "MEX", "MLT", "NOR", "POL", "PRT", "ROU", "RUS", "SVK", "SVN", "SWE", 
               "TUR", "TWN", "USA")

# Loading data
energy_datasets <- list()
electricity_data <- list() 
for(i in countries){
  path <- paste("https://github.com/davidqo1231/Energy-intensity/blob/main/Environmental-accounts/NAMEA_GEU5610_", i, ".xls", sep = "")
  energy_datasets[[i]] <- read_excel(path, sheet = "2014")
  electricity_data[[i]] <- energy_datasets[[i]][["ELECTR_HEATPROD"]]
}

df_electricity <- as.data.table(electricity_data) %>%
  bind_cols(as.data.table(energy_datasets[["AUS"]][["...1"]])) %>%
  rename(industry = V1) %>%
  relocate(industry) %>%
  slice(1:56)

industry_names <- df_electricity %>%
  select(industry) 

read.xlsx("https://github.com/davidqo1231/Energy-intensity/blob/main/", sheet = "National IO-tables", rows = (c(1, 1683:1738)), cols = c(5:60)

m <- readr::read_csv2("https://raw.githubusercontent.com/davidqo1231/Energy-intensity/main/NIOT/AUS_NIOT_nov16.csv")



