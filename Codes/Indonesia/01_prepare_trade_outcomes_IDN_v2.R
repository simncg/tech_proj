#===========================================================================#
# Date of last update: October 2023                                         #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  prepare_trade_outcomes_IDN.R                                     #
#                                                                           #
# This program generates the trade outcomes to be used in the regression    #
# analysis. In particular, we generate the following outcomes: Log. Import  # 
# and Log. Export, number of distinct destinations or sources. Since for    #
# Indonesia we only have data from 2019-Feb onwards it is a short period to #
# define a baseline year, therefore, for this country we do not define the  #
# outcome variable new sources/new destinations compared to a baseline year #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")


# Create outcomes for exports at the firm-month-hs6 level ----

## Read Panjiva Export Raw Data ----

# Read Domestic Exports data at the firm-month-hs8-destination country level
panjiva_export_data <-
  read_dta("../../Data/Indonesia/raw_data/exports_collapsed_dom_hs6.dta") %>%
  filter(!str_starts(hs6, "27")) 


## Collapse exports data to the firm-month-hs6 level ----
export_summary_by_firm_month_HS_code <- panjiva_export_data %>%
  select(company_id = domestic_id, year, month,  hs6, export, country = foreign_country_cleaned_iso3) %>%
  group_by(company_id, year, month,  hs6) %>%
  summarise(
    # # Sum export values by firm-month-hs6
    export = sum(export, na.rm = TRUE),
    # Number of distinct countries (distinct destinations) 
    n_countries = n_distinct(country)
    ) %>%
  ungroup() %>%
  mutate(
    log_export = log(export),
    export_dummy = TRUE
    ) %>%
  # Date variable
  mutate(date = ymd(paste0(year, "-", month, "-01"))) %>%
  mutate(date_labels = format(date, "%Y-%b")) %>%
  mutate(date_fact = as.factor(date_labels)) %>%
  mutate(date_fact = fct_reorder(date_fact, date)) %>%
  mutate(date_character = as.character(date_fact)) %>%
  select(-date_fact, -date_labels) %>%
  relocate(
    company_id,
    year,
    month,
    date,
    date_character,
    hs6,
    export,
    log_export,
    n_countries,
    export_dummy
  ) %>%
  arrange(company_id, date)


# Save exports data at the firm-month-HS6 level 
write_csv(export_summary_by_firm_month_HS_code, "../../Data/Indonesia/processed_data/export_summary_by_firm_month_HS_code.csv")


## Collapse exports data to the firm-month level ---- 
export_summary_by_firm_month <- panjiva_export_data %>%
  select(company_id = domestic_id, year, month,  hs6, export, country = foreign_country_cleaned_iso3) %>%
  group_by(company_id, year, month) %>%
  summarise(
    # # Sum export values by firm-month-hs6
    export = sum(export, na.rm = TRUE),
    # Number of distinct countries (distinct destinations) 
    n_countries = n_distinct(country), 
    # Number of distinct hs6 products 
    n_hs6_products = n_distinct(hs6)
  ) %>%
  ungroup() %>%
  mutate(
    log_export = log(export),
    export_dummy = TRUE
  ) %>%
  # Date variable
  mutate(date = ymd(paste0(year, "-", month, "-01"))) %>%
  mutate(date_labels = format(date, "%Y-%b")) %>%
  mutate(date_fact = as.factor(date_labels)) %>%
  mutate(date_fact = fct_reorder(date_fact, date)) %>%
  mutate(date_character = as.character(date_fact)) %>%
  select(-date_fact, -date_labels) %>%
  relocate(
    company_id,
    year,
    month,
    date,
    date_character,
    export,
    log_export,
    n_countries,
    export_dummy
  ) %>%
  arrange(company_id, date)

write_csv(export_summary_by_firm_month, "../../Data/Indonesia/processed_data/export_summary_by_firm_month.csv")


rm(export_summary_by_firm_month, export_summary_by_firm_month_HS_code, panjiva_export_data)
gc()

# Create outcomes for imports at the firm-month-hs6 level ----

# Read Domestic Imports data at the firm-month-hs6-destination country level-quantity unit
panjiva_import_data <-
  read_dta("../../Data/Indonesia/raw_data/imports_collapsed_dom_hs6.dta") %>%
  filter(!str_starts(hs6, "27"))

## Collapse exports data to the firm-month-hs6 level ----
import_summary_by_firm_month_HS_code<-
  panjiva_import_data %>%
  select(company_id = domestic_id, year, month,  hs6, import, country = foreign_country_cleaned_iso3) %>%
  group_by(company_id, year, month,  hs6) %>%
  summarise(
    # Sum import values by firm-month-hs6
    import = sum(import, na.rm = TRUE),
    # Number of distinct countries (distinct sources) 
    n_countries = n_distinct(country)
  ) %>%
  ungroup() %>%
  mutate(log_import = log(import),
         import_dummy = TRUE) %>%
  # Create date variables
  mutate(date = ymd(paste0(year, "-", month, "-01"))) %>%
  mutate(date_labels = format(date, "%Y-%b")) %>%
  mutate(date_fact = as.factor(date_labels)) %>%
  mutate(date_fact = fct_reorder(date_fact, date)) %>%
  mutate(date_character = as.character(date_fact)) %>%
  select(-date_fact, -date_labels) %>%
  relocate(
    company_id,
    year,
    month,
    date,
    date_character,
    hs6,
    import,
    log_import,
    n_countries,
    import_dummy
  ) %>%
  arrange(company_id, date)

# Save data at the firm-month-HS6 level
write_csv(import_summary_by_firm_month_HS_code, "../../Data/Indonesia/processed_data/import_summary_by_firm_month_HS_code.csv")


# Collapse imports data to the firm-month level ----
import_summary_by_firm_month<-
  panjiva_import_data %>%
  select(company_id = domestic_id, year, month, import, country = foreign_country_cleaned_iso3, hs6) %>%
  # Group by firm-year-month
  group_by(company_id, year, month) %>%
  summarise(
    # Sum import values by firm-month-hs6
    import = sum(import, na.rm = TRUE),
    # Number of distinct countries (distinct sources) 
    n_countries = n_distinct(country), 
    # Number of distinct hs6 products 
    n_hs6_products = n_distinct(hs6)
  ) %>%
  ungroup() %>%
  mutate(log_import = log(import),
         import_dummy = TRUE) %>%
  # Create date variables
  mutate(date = ymd(paste0(year, "-", month, "-01"))) %>%
  mutate(date_labels = format(date, "%Y-%b")) %>%
  mutate(date_fact = as.factor(date_labels)) %>%
  mutate(date_fact = fct_reorder(date_fact, date)) %>%
  mutate(date_character = as.character(date_fact)) %>%
  select(-date_fact, -date_labels) %>%
  relocate(
    company_id,
    year,
    month,
    date,
    date_character,
    import,
    log_import,
    n_countries,
    import_dummy
  ) %>%
  arrange(company_id, date)


write_csv(import_summary_by_firm_month, "../../Data/Indonesia/processed_data/import_summary_by_firm_month.csv")

