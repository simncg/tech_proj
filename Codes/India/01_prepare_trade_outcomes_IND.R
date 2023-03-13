#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  prepare_trade_outcomes_IND.R                                     #
#                                                                           #
# This program generates the trade outcomes to be used in the regression    #
# analysis.                                                                 #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")
library(dtplyr)


# Read Panjiva Import Raw Data ---
panjiva_import_data <- read_dta("../../Data/India/raw_data/imports_collapsed_dom_hs6_onlyvalue.dta")%>%
  filter(!str_starts(hs6 , "27"))

# Read Panjiva Export Raw Data ---
panjiva_export_data <- read_dta("../../Data/India/raw_data/exports_collapsed_dom_hs6_onlyvalue.dta")%>%
  filter(!str_starts(hs6 , "27")) 

# Read data with income level of countries ---
income_level_countries <- readxl::read_excel("../../Data/Extra Data/OGHIST.xlsx", sheet = "2019_data")%>% 
  mutate(continent = countrycode::countrycode(country, origin = "iso3c", destination = "continent"))

gc()

# Create outcomes for imports ----
panjiva_import_data <- lazy_dt(panjiva_import_data)


panjiva_import_data_clean <- panjiva_import_data %>% 
  select(domestic_company_id = domestic_id, domestic_company_name = domestic_cleaned, year, month,  hs6, import, country = foreign_country_cleaned_iso3) %>%
  mutate(hs6 = substr(hs6, 1, 6)) %>%
  group_by(domestic_company_id, domestic_company_name, year, month,  hs6) %>%
  summarise(import = sum(import, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(log_import = log(import),
         import_dummy = TRUE) %>% 
  mutate(date = ymd(paste0(year, "-", month, "-01"))) %>%
  mutate(date_labels = format(date, "%Y-%b")) %>%
  mutate(date_fact = as.factor(date_labels)) %>%
  mutate(date_fact = fct_reorder(date_fact, date)) %>%
  mutate(date_character = as.character(date_fact)) %>%
  select(-date_labels, -date_fact) %>% 
  left_join(
    panjiva_import_data %>%
      select(domestic_company_id = domestic_id, year, month,  hs6, import, country = foreign_country_cleaned_iso3) %>%
      left_join(income_level_countries, by = c('country' = 'country')) %>%
      filter(!is.na(income_level)) %>%
      mutate(
        high_vs_rest = ifelse(income_level == "H", "high_import", "not_high_import")
      ) %>%
      group_by(domestic_company_id, year, month,  hs6, high_vs_rest) %>%
      summarise(import = sum(import, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(log_import = log(import)) %>%
      select(-import) %>%
      pivot_wider(names_from = high_vs_rest,
                  values_from = log_import),
    by = c("domestic_company_id", "year", "month", "hs6")
  ) %>%
  left_join(
    panjiva_import_data %>%
      select(domestic_company_id = domestic_id, year, month,  hs6, import, country = foreign_country_cleaned_iso3) %>%
      left_join(income_level_countries, by = c('country' = 'country')) %>%
      filter(!is.na(income_level)) %>%
      mutate(
        high_vs_rest = ifelse(income_level %in% c("H", "UM") , "H_UM_import", "L_LM_import")
      ) %>%
      group_by(domestic_company_id, year, month,  hs6, high_vs_rest) %>%
      summarise(import = sum(import, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(log_import = log(import)) %>%
      select(-import) %>%
      pivot_wider(names_from = high_vs_rest, 
                  values_from = log_import),
    by = c("domestic_company_id", "year", "month", "hs6")
  ) %>%
  left_join(
    panjiva_import_data %>%
      select(domestic_company_id = domestic_id, year, month,  hs6, import, country = foreign_country_cleaned_iso3) %>%
      left_join(income_level_countries, by = c('country' = 'country')) %>%
      filter(!is.na(income_level)) %>%
      mutate(
        Asia_vs_rest = ifelse(continent %in% c("Asia"), "asia_import", "non_Asia_import")
      ) %>%
      group_by(domestic_company_id, year, month,  hs6, Asia_vs_rest) %>%
      summarise(import = sum(import, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(log_import = log(import)) %>%
      select(-import) %>%
      pivot_wider(names_from = Asia_vs_rest, 
                  values_from = log_import),
    by = c("domestic_company_id", "year", "month", "hs6")
  ) %>% relocate(
    domestic_company_id ,
    year,
    month,
    date,
    date_character,
    hs6,
    import,
    log_import,
    import_dummy
  ) %>%
  arrange(domestic_company_id, date) %>% 
  as_tibble()


write_csv(panjiva_import_data_clean, "../../Data/India/processed_data/import_summaries_by_firm_month_HS_code_complete.csv")

# Create outcomes for exports ----
panjiva_export_data <- lazy_dt(panjiva_export_data)

panjiva_export_data_clean <- panjiva_export_data %>% 
  select(domestic_company_id = domestic_id, domestic_company_name = domestic_cleaned, year, month,  hs6, export, country = foreign_country_cleaned_iso3) %>%
  mutate(hs6 = substr(hs6, 1, 6)) %>%
  group_by(domestic_company_id, domestic_company_name, year, month,  hs6) %>%
  summarise(export = sum(export, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(log_export = log(export),
         export_dummy = TRUE) %>% 
  mutate(date = ymd(paste0(year, "-", month, "-01"))) %>%
  mutate(date_labels = format(date, "%Y-%b")) %>%
  mutate(date_fact = as.factor(date_labels)) %>%
  mutate(date_fact = fct_reorder(date_fact, date)) %>%
  mutate(date_character = as.character(date_fact)) %>%
  select(-date_labels, -date_fact) %>% 
  left_join(
    panjiva_export_data %>%
      select(domestic_company_id = domestic_id, year, month,  hs6, export, country = foreign_country_cleaned_iso3) %>%
      left_join(income_level_countries, by = c('country' = 'country')) %>%
      filter(!is.na(income_level)) %>%
      mutate(
        high_vs_rest = ifelse(income_level == "H", "high_export", "not_high_export")
      ) %>%
      group_by(domestic_company_id, year, month,  hs6, high_vs_rest) %>%
      summarise(export = sum(export, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(log_export = log(export)) %>%
      select(-export) %>%
      pivot_wider(names_from = high_vs_rest,
                  values_from = log_export),
    by = c("domestic_company_id", "year", "month", "hs6")
  ) %>%
  left_join(
    panjiva_export_data %>%
      select(domestic_company_id = domestic_id, year, month,  hs6, export, country = foreign_country_cleaned_iso3) %>%
      left_join(income_level_countries, by = c('country' = 'country')) %>%
      filter(!is.na(income_level)) %>%
      mutate(
        high_vs_rest = ifelse(income_level %in% c("H", "UM") , "H_UM_export", "L_LM_export")
      ) %>%
      group_by(domestic_company_id, year, month,  hs6, high_vs_rest) %>%
      summarise(export = sum(export, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(log_export = log(export)) %>%
      select(-export) %>%
      pivot_wider(names_from = high_vs_rest, 
                  values_from = log_export),
    by = c("domestic_company_id", "year", "month", "hs6")
  ) %>%
  left_join(
    panjiva_export_data %>%
      select(domestic_company_id = domestic_id, year, month,  hs6, export, country = foreign_country_cleaned_iso3) %>%
      left_join(income_level_countries, by = c('country' = 'country')) %>%
      filter(!is.na(income_level)) %>%
      mutate(
        Asia_vs_rest = ifelse(continent %in% c("Asia"), "asia_export", "non_Asia_export")
      ) %>%
      group_by(domestic_company_id, year, month,  hs6, Asia_vs_rest) %>%
      summarise(export = sum(export, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(log_export = log(export)) %>%
      select(-export) %>%
      pivot_wider(names_from = Asia_vs_rest, 
                  values_from = log_export),
    by = c("domestic_company_id", "year", "month", "hs6")
  ) %>% relocate(
    domestic_company_id ,
    year,
    month,
    date,
    date_character,
    hs6,
    export,
    log_export,
    export_dummy
  ) %>%
  arrange(domestic_company_id, date) %>% 
  as_tibble()

# Save results 
write_csv(panjiva_export_data_clean, "../../Data/India/processed_data/export_summaries_by_firm_month_HS_code_complete.csv")




