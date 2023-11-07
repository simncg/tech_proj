#===========================================================================#
# Date:    October, 2023                                                 #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  firms_less_than_1000USD_IDN.R                                    #
#                                                                           #
# This program identifies Indonesia firms that trade less than 1000 usd per #                 
# year. This filter is needed for the regression analysis.                  #                                                                         
#                                                                           #
#                                                                           #           -                                                                                                                                                  #
#===========================================================================#

# Libraries to be used
library(tidyverse)
library(writexl)
library(haven)
library(readr)

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Load exports data 
export_data_firm_month <-
  read_dta("../../Data/Indonesia/IDN_Exports_Monthly_domestic_level_dropdup.dta") %>%
  filter(!str_starts(hs, "27"))

# Load imports data 
import_data_firm_month <-
  read_dta("../../Data/Indonesia/IDN_Imports_Monthly_domestic_level_dropdup.dta") %>%
  filter(!str_starts(hs, "27"))

# Identify firms that import less than 1000 usd in both 2019 and 2020
less_than_1000USD_import <- import_data_firm_month %>% 
  group_by(company_id, year) %>% 
  summarise(sum_import = sum(import, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(less_than_1000USD_import_year = sum_import < 1000) %>% 
  group_by(company_id) %>% 
  mutate(# Number of years appearing in the data
         n_years = n(), 
         # Number of years in which the company imported less than 1000 usd
         n_years_import_less_1000usd = sum(less_than_1000USD_import_year),
         # Indicate if firm in all years imported less than 1000 usd
         import_always_less_1000usd = n_years_import_less_1000usd == n_years, 
         # Indicate if firm imported less than 1000 usd in 2020
         less_than_1000USD_import_2020 = sum_import < 1000 & year == 2020 & n_years == 1, 
         # Indicate if firm imported less than 1000 usd in 2019
         less_than_1000USD_import_2019 = sum_import < 1000 & year == 2019 & n_years == 1, 
         # Indicate if firm imported less than 1000 usd in 2019 and also in 2020
         less_than_1000USD_import_19_20 = sum(less_than_1000USD_import_year[year %in% c(2019, 2020)]) == 2, 
         # If a firm only appears once the correct value in this variable for this case is NA 
         less_than_1000USD_import_19_20 = ifelse(n_years > 1, less_than_1000USD_import_19_20, NA)
         ) %>% 
  select(company_id, year, n_years, less_than_1000USD_import_19_20, less_than_1000USD_import_2020, 
         less_than_1000USD_import_2019, sum_import)

  
# Identify firms that export less than 1000 usd per year
less_than_1000USD_export <- export_data_firm_month %>% 
  group_by(company_id, year) %>% 
  summarise(sum_export = sum(export, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(less_than_1000USD_export_year = sum_export < 1000) %>% 
  group_by(company_id) %>% 
  mutate(# Number of years appearing in the data
    n_years = n(), 
    # Number of years in which the company imported less than 1000 usd
    n_years_export_less_1000usd = sum(less_than_1000USD_export_year),
    # Indicate if firm imported less than 1000 usd in 2020 and only appears in this year
    less_than_1000USD_export_2020 = sum_export < 1000 & year == 2020 & n_years == 1 , 
    # Indicate if firm imported less than 1000 usd in 2019 and only appears in this year
    less_than_1000USD_export_2019 = sum_export < 1000 & year == 2019 & n_years == 1, 
    # Indicate if firm imported less than 1000 usd in 2019 and also in 2020
    less_than_1000USD_export_19_20 = sum(less_than_1000USD_export_year[year %in% c(2019, 2020)]) == 2, 
    less_than_1000USD_export_19_20 = ifelse(n_years > 1, less_than_1000USD_export_19_20, NA)
  ) %>% 
  select(company_id, year, n_years, sum_export, less_than_1000USD_export_19_20, 
         less_than_1000USD_export_2020, 
         less_than_1000USD_export_2019)
  

# Save results 
write_csv(less_than_1000USD_export, "../../Data/Indonesia/processed_data/export_firms_trade_less_than_1000usd.csv")
write_csv(less_than_1000USD_import, "../../Data/Indonesia/processed_data/import_firms_trade_less_than_1000usd.csv")
