#===========================================================================#
# Date:    January 25, 2022                                                 #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  firms_less_than_1000USD_IND.R                                    #
#                                                                           #
# This program identify firms that in ALL years (where they trade)          #
# export/import always less than 1000 usd.                                  #                                                
#                                                                           #
#                                                                           #                                                                                                                                                         #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)


# Libraries to be used ----
source("../src/packages.R")
library(dtplyr)

# Load exports data at the firm month hs level 
export_data_firm_month <-
  read_parquet("../../Data/Mexico/processed_data/export_summaries_by_firm_month_HS_code_complete.parquet") %>% 
  filter(!str_starts(hs6, "27"), 
         date >= ymd("2018-01-01"), # We take advantage that we have 2018, 2019, 2020, 2021  complete for Mexico to do this filter
         date < ymd("2022-01-01")) %>% 
  rename(company_id = domestic_company_id)

# Load imports data at the firm-month-hs level hs
import_data_firm_month <-
  read_parquet("../../Data/Mexico/processed_data/import_summaries_by_firm_month_HS_code_complete.parquet")%>% 
  filter(!str_starts(hs6, "27"), 
         date >= ymd("2018-01-01"), # We take advantage that we have 2018, 2019, 2020 and 2021 complete for Mexico
         date < ymd("2022-01-01")) %>% 
  rename(company_id = domestic_company_id)

gc()

# Identify firms that import less than 1000 usd in 2018, 2019, 2020 and 2021
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
    # Indicate if firm imported less than 1000 usd in all years in which it trades
    less_than_1000usd = n_years_import_less_1000usd == n_years
  ) %>% 
  select(company_id, year, n_years, less_than_1000usd, sum_import) %>% 
  as_tibble()


# Identify firms that export less than 1000 usd in 2018, 2019, 2020 and 2021
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
    # Indicate if firm exported less than 1000 usd in all years in which it trades
    less_than_1000usd = n_years_export_less_1000usd == n_years
  ) %>% 
  select(company_id, year, n_years, sum_export, less_than_1000usd) %>% 
  as_tibble()


# Save results 
write_csv(less_than_1000USD_export, "../../Data/Mexico/processed_data/export_firms_trade_less_than_1000usd.csv")
write_csv(less_than_1000USD_import, "../../Data/Mexico/processed_data/import_firms_trade_less_than_1000usd.csv")
