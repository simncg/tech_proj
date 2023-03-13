#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  sic_groups_distributions.R                                       #
#                                                                           #                 
#                                                                           #                                                                         
#                                                                           #
#                                                                           #           -                                                                                                                                                  #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")


# Load matched Aberdeen to Panjiva data ----
matched_data <- read_csv("../../Data/India/matched_Aberdeen_to_Panjiva_data_India.csv") %>%
  rename(company_id = our_ID)

#Load builtwith data ----

# Load technology data (only nod variables) ----
tech_data   <- read_parquet("../../Data/India/Builtwith_no_drop_long.parquet", 
                            col_select = c("our_ID", "year", "month", "tech", "value", "FI", "LI")) %>% 
  # For the moment, we are only going to analyze e-payment/e-commerce technologies
  filter(tech %in% c("payrobust_nod", "ecom_nod")) %>% 
  rename(company_id = our_ID)

gc()

# Add adoption date to tech data and date variables
tech_data <- 
  tech_data %>% 
  # Identify technology adoption date (first_adopted variable)
  left_join(
    tech_data %>% 
      select(-LI, -FI) %>% 
      arrange(company_id, tech, year, month) %>% 
      group_by(company_id, tech) %>%
      slice(which(value == 1)[1]) %>% 
      ungroup() %>% 
      mutate(first_adopted = ifelse(year < 2019, "before 2019", paste0(year, "_", month))) %>% 
      select(company_id, tech, first_adopted), 
    by = c("company_id", "tech")
  ) %>% 
  mutate(# If missing value in adopted date, then technology never adopted by the company 
    first_adopted = ifelse(is.na(first_adopted), "never adopted", first_adopted), 
    # Create date and data character (year-month) variables
    date = as.Date(paste0(year, "-", month, "-01")), 
    date_character = as.character(fct_reorder(as.factor(format(date, "%Y-%b")), date))
  ) %>% 
  select(-year, -month) %>% 
  # From long to wide to have one row per firm-month and to have tech columns and columns defining their adoption date
  pivot_wider(
    names_from = tech, 
    values_from = c(value, first_adopted)
  ) %>% 
  # Rename resulting columns of the pivot_wider, remove prefix "_value" and "_nod" from column names
  rename_if(str_detect(names(.), "_nod"), ~sub("_nod", "", .)) %>% 
  rename_if(str_detect(names(.), "value_"), ~sub("value_", "", .)) %>% 
  arrange(company_id, date) %>% 
  group_by(company_id) %>% 
  # Create lags for variables of e-commerce and e-payment technologies
  mutate(across(c(ecom, payrobust), list(t_1 = function(x){lag(x, 1)}, 
                                         t_2 = function(x){lag(x, 2)}, 
                                         t_3 = function(x){lag(x, 3)}))) %>% 
  # Create a variable indicating if e-commerce or e-payment technology is adopted.
  mutate(pay_or_ecomnod = ifelse((payrobust == 1 | ecom == 1),1, 0 ),
         # Create lagged variables for adoption of e-commerce or e-payment technologies
         pay_or_ecomnod_t_1 = ifelse((payrobust_t_1 == 1 | ecom_t_1 == 1),1, 0),
         pay_or_ecomnod_t_2 = ifelse((payrobust_t_2 == 1 | ecom_t_2 == 1),1, 0),
         pay_or_ecomnod_t_3 = ifelse((payrobust_t_3 == 1 | ecom_t_3 == 1),1, 0)
  )

gc()


# Imports data ----

# Firms that import less than 1000 usd in each one of the years (2019, 2020 and 2021) 
import_less_than_1000usd<-read_csv("../../Data/India/processed_data/import_firms_trade_less_than_1000usd.csv")

# Match with Aberdeen 
import_data <-fread("../../Data/India/import_summaries_by_firm_month_HS_code_complete.csv") %>% 
  select(company_id = domestic_company_id, year, month, date, date_character,hs6, import, log_import) %>% 
  # filter to analysis period
  filter(date >= ymd("2019-02-01"),
         date <= ymd("2021-06-01")) %>% 
  mutate(hs6  = str_pad(as.character(hs6) , 6, "left", "0")) %>% 
  # Drop firms that import less than 1000 usd
  left_join(import_less_than_1000usd, by = c("company_id", "year")) %>% 
  filter(!less_than_1000usd) %>% 
  select(-less_than_1000usd, -sum_import, -n_years) %>% 
  left_join(matched_data %>% 
              select(company_id, SITEID, SICGRP, NAICS6_CODE),
            by = c("company_id")
  ) %>% 
  mutate(#SICGRP = ifelse(is.na(SICGRP), "No Data", SICGRP), 
         import_export = "Imports Dataset") %>% 
  select(company_id, date_character, SICGRP, import_export)


# Match with Builtwith 
import_data <- import_data %>% 
  # Add the builtwith technology data 
  left_join(tech_data %>% 
              select(-LI, -FI),
            by = c("company_id", "date_character")
  ) 

import_data %>% 
  summarise(across(everything(), ~ sum(is.na(.x))))


non_na_import<-import_data %>% 
  na.omit()

# Percentage of firms that matched aberdeen and builtiwith
length(unique(non_na_import$company_id))/length(unique(import_data$company_id))


gc()

# Exports data ----

# Firms that export less than 1000 usd in each one of the years (2019, 2020 and 2021) 
export_less_than_1000usd<-read_csv("../../Data/India/processed_data/export_firms_trade_less_than_1000usd.csv")


export_data <-fread("../../Data/India/export_summaries_by_firm_month_HS_code_complete.csv") %>% 
  select(company_id = domestic_company_id, year, month, date, date_character,hs6, export, log_export) %>% 
  # filter to analysis period
  filter(date >= ymd("2019-02-01"),
         date <= ymd("2021-09-01")) %>% 
  mutate(hs6  = str_pad(as.character(hs6) , 6, "left", "0")) %>% 
  # Drop firms that import less than 1000 usd in a particular year
  left_join(export_less_than_1000usd, by = c("company_id", "year")) %>% 
  filter(!less_than_1000usd) %>% 
  select(-less_than_1000usd, -sum_export, -n_years) %>% 
  left_join(matched_data %>% 
              select(company_id, SITEID, SICGRP, NAICS6_CODE),
            by = c("company_id")
  ) %>% 
  mutate(#SICGRP = ifelse(is.na(SICGRP), "No Data", SICGRP), 
         import_export = "Exports Dataset") %>% 
  select(company_id, date_character, SICGRP, import_export)

gc()


export_data <- export_data %>% 
  # Add the builtwith technology data 
  left_join(tech_data %>% 
              select(-LI, -FI),
            by = c("company_id", "date_character")
  ) 

non_na_export <- export_data %>% 
  na.omit()

export_data %>% 
  summarise(across(everything(), ~ sum(is.na(.x))))

