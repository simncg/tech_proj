#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  prepare_trade_outcomes_MEX.R                                     #
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


# Create Outcomes for Imports ----

## Collapsing the data at the firm-month-HS8-foreign partner level -----

# At this point, the Mexico raw data is at the firm-month-HS8-foreign partner-unit quantity level. 
# We will collapse the data to sum the export/import value by the firm-month-HS8-partner country.
# Namely, we “ignore” the different quantity units that may be there and we take the whole value of
# trade in that product by that firm to that country in that month.

# Read Panjiva Import Raw Data ---
panjiva_import_data <- read_dta("../../Data/Mexico/raw_data/MEX_imports_Monthly_dom_locationmode_hs8.dta")%>%
  lazy_dt() %>% 
  filter(!str_starts(hs8 , "27"), 
         country_iso!= "EUN") %>% 
  group_by(domestic_firm_id, domestic_firm_name, year, month, hs8, country_iso) %>% 
  summarize(import = sum(import)) %>% 
  mutate(hs6 = substr(hs8, 1, 6))
  

## Collapsing imports to the firm-month-hs6 level -----

panjiva_import_data_clean <- panjiva_import_data %>% 
  select(domestic_company_id = domestic_firm_id, domestic_company_name = domestic_firm_name, year, month,  hs8, hs6, import, country = country_iso) %>%
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
  relocate(
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
  as.data.table()

write_parquet(panjiva_import_data_clean, "../../Data/Mexico/processed_data/import_summaries_by_firm_month_HS_code_complete.parquet")

rm(panjiva_import_data_clean, panjiva_import_data)

# Create outcomes for exports ----

## Collapsing the data at the firm-month-HS8-foreign partner level -----

# At this point, the Mexico raw data is at the firm-month-HS8-foreign partner-unit quantity level. 
# We will collapse the data to sum the export/import value by the firm-month-HS8-partner country.
# Namely, we “ignore” the different quantity units that may be there and we take the whole value of
# trade in that product by that firm to that country in that month.

# Read Panjiva Import Raw Data ---
panjiva_export_data <- read_dta("../../Data/Mexico/raw_data/MEX_exports_Monthly_dom_locationmode_hs8.dta")%>%
  lazy_dt() %>% 
  filter(!str_starts(hs8 , "27"), 
         country_iso!= "EUN") %>% 
  group_by(domestic_firm_id, domestic_firm_name, year, month, hs8, country_iso) %>% 
  summarize(export = sum(export)) %>% 
  mutate(hs6 = substr(hs8, 1, 6)) 

## Collapsing exports to the firm-month-hs6 level -----


panjiva_export_data_clean <- panjiva_export_data %>% 
  select(domestic_company_id = domestic_firm_id, domestic_company_name = domestic_firm_name, year, month,  hs6, export, country = country_iso) %>%
  mutate(hs6 = substr(hs8, 1, 6)) %>%
  group_by(domestic_company_id,  domestic_company_name, year, month,  hs6) %>%
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
  relocate(
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
  as.data.table()

# Save results 
write_parquet(panjiva_export_data_clean, "../../Data/Mexico/processed_data/export_summaries_by_firm_month_HS_code_complete.parquet")

rm(panjiva_export_data_clean, panjiva_export_data)
