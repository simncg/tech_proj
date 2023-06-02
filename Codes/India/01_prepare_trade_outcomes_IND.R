#===========================================================================#
# Date of last update: May 2023                                             #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  prepare_trade_outcomes_IND.R                                     #
#                                                                           #
# This program generates the trade outcomes to be used in the regression    #
# analysis. In particular, we generate the following outcomes: Log. Import  # 
# and Log. Export, number of distinct destinations or sources, dummy for    #
# exporting to a new destination or importing from a new source (we take a  #
# a baseline year to define the existing destinations/sources).             #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")
library(dtplyr)

# Create outcomes for imports at the firm-month-hs6 level ----

## Read Panjiva Import Raw Data ----

# This dataset is at the firm-month-hs6-country level
panjiva_import_data <- read_dta("../../Data/India/raw_data/imports_collapsed_dom_hs6_onlyvalue.dta")%>%
  filter(!str_starts(hs6 , "27"))

# Use lazy_dt to optimize data processing 
#panjiva_import_data <- lazy_dt(panjiva_import_data)

# Define existing sources in the baseline year 2017 ----

##  Define existing sources in the baseline year 2017 at the FIRM-HS6 level. ----
existing_sources_f_hs6<-
  panjiva_import_data %>% 
  # Filter to the baseline year 
  filter(year == 2017) %>% 
  # Select relevant variables 
  select(domestic_id, hs6, foreign_country_cleaned_iso3) %>% 
  # Drop duplicates
  distinct() %>% 
  # Order data
  arrange(domestic_id, hs6) %>% 
  # Create variable to identify existing countries (sources) in the baseline year at the firm-hs6
  mutate(existing_country_f_hs6 = 1) %>% 
  # Convert to data.frame
  as.data.frame()




## Define existing sources in the baseline year 2017 at the FIRM level. ----
existing_sources_f<-
  panjiva_import_data %>% 
  # Filter to the baseline year 
  filter(year == 2017) %>% 
  # Select relevant variables 
  select(domestic_id, foreign_country_cleaned_iso3) %>% 
  # Drop duplicates
  distinct() %>% 
  # Order data
  arrange(domestic_id) %>% 
  # Create variable to identify existing countries (sources) in the baseline year at the firm level
  mutate(existing_country_f = 1) %>% 
  # Convert to data.frame
  as.data.frame()


## Join existing sources in the baseline year with the imports dataset ---- 
panjiva_import_data<-
  panjiva_import_data %>% 
  # Join with the existing sources at the firm-HS6 level
  left_join(
    existing_sources_f_hs6, 
    by = c("domestic_id", "hs6", "foreign_country_cleaned_iso3")
  ) %>%
  # Join with the existing sources (at the firm-product) again but by firm-product to check if the firm-product existed/had transactions in 2017
  left_join(
    existing_sources_f_hs6 %>% 
      distinct(domestic_id, hs6) %>% 
      mutate(firm_hs6_exist_base_year = 1), 
    by = c("domestic_id", "hs6")
  ) %>%
  mutate(
    # Dummy variable equal to TRUE if the source is a new country (at the firm-HS6 level) compared to the 
    # existing countries (sources) in the baseline year. We only define this variable for those combinations
    # of firms-products that existed in the baseline year
    is_new_country_f_hs6 = is.na(existing_country_f_hs6) & firm_hs6_exist_base_year == 1,
  ) %>% 
  # Join with the existing sources at the firm level 
  left_join(
    existing_sources_f, 
    by = c('domestic_id', "foreign_country_cleaned_iso3")
  ) %>% 
  # Join with the existing sources (at the firm-product) again but by firm-product to check if the firm-product existed/had transactions in 2017
  left_join(
    existing_sources_f %>% 
      distinct(domestic_id) %>% 
      mutate(firm_exist_base_year = 1), 
    by = c('domestic_id')
  ) %>% 
  mutate(
    # Dummy variable equal to TRUE if the source is a new country (at the firm level) compared to the 
    # existing countries (sources) in the baseline year. We only create this variable for those 
    # firms that existed in the baseline year.
    is_new_country_f = is.na(existing_country_f) & firm_exist_base_year == 1
  )
  



# Collapse imports dataset to the firm-hs6-month level ----
panjiva_import_data_clean <- panjiva_import_data %>% 
  select(domestic_company_id = domestic_id, domestic_company_name = domestic_cleaned,
         year, month,  hs6, import, country = foreign_country_cleaned_iso3, 
         is_new_country_f_hs6) %>%
  mutate(hs6 = substr(hs6, 1, 6)) %>%
  group_by(domestic_company_id, domestic_company_name, year, month,  hs6) %>%
  summarise(# Sum import values by firm-month-hs6
            import = sum(import, na.rm = TRUE), 
            # Number of distinct countries (distinct sources) 
            n_countries = n_distinct(country), 
            # Dummy equal to 1 if the firm in a particular year-month imported from at least one new country (source)
            # compared to the baseline year
            new_country = as.integer(any(is_new_country_f_hs6))
            ) %>%
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
  as_tibble()


write_csv(panjiva_import_data_clean, "../../Data/India/processed_data/import_summaries_by_firm_month_HS_code_complete.csv")


# Collapse imports dataset at the firm-month level ----

panjiva_import_firm_month <- panjiva_import_data %>% 
  select(domestic_company_id = domestic_id, domestic_company_name = domestic_cleaned,
         year, month, import, country = foreign_country_cleaned_iso3, 
         is_new_country_f, hs6) %>%
  # Group by firm-year-month
  group_by(domestic_company_id, domestic_company_name, year, month) %>%
  summarise(
    # Sum import values by firm-month
    import = sum(import, na.rm = TRUE), 
    # Number of distinct countries (distinct sources) 
    n_countries = n_distinct(country), 
    # Dummy equal to 1 if the firm in a particular year-month imported from a new country (source)
    # compared to the baseline year
    new_country = as.integer(any(is_new_country_f)), 
    # Number of distinct hs6 products 
    n_hs6_products = n_distinct(hs6)
    ) %>%
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
    import,
    log_import,
    import_dummy
  ) %>%
  arrange(domestic_company_id, date) %>% 
  as.data.table()

write_csv(panjiva_import_firm_month, "../../Data/India/processed_data/import_summaries_by_firm_month_complete.csv")

rm(panjiva_import_data, panjiva_import_firm_month, panjiva_import_data_clean, 
   existing_sources_f, existing_sources_f_hs6)
gc()
# Create outcomes for exports at the firm-month-hs6 level ----

# Read Panjiva Export Raw Data -----
panjiva_export_data <- read_dta("../../Data/India/raw_data/exports_collapsed_dom_hs6_onlyvalue.dta")%>%
  filter(!str_starts(hs6 , "27")) 

# Define existing destinations in the baseline year 2017 ----

##  Define existing destinations in the baseline year 2017 at the FIRM-HS6 level. ----
existing_destin_f_hs6<-
  panjiva_export_data %>% 
  # Filter to the baseline year 
  filter(year == 2017) %>% 
  # Select relevant variables 
  select(domestic_id, hs6, foreign_country_cleaned_iso3) %>% 
  # Drop duplicates
  distinct() %>% 
  # Order data
  arrange(domestic_id, hs6) %>% 
  # Create variable to identify existing countries (destinations) in the baseline year at the firm-hs6 level
  mutate(existing_country_f_hs6 = 1) %>% 
  # Convert to data.frame
  as.data.frame()




## Define existing destinations in the baseline year 2017 at the FIRM level. ----
existing_destin_f<-
  panjiva_export_data %>% 
  # Filter to the baseline year 
  filter(year == 2017) %>% 
  # Select relevant variables 
  select(domestic_id, foreign_country_cleaned_iso3) %>% 
  # Drop duplicates
  distinct() %>% 
  # Order data
  arrange(domestic_id) %>% 
  # Create variable to identify existing countries (destinations) in the baseline year at the firm level
  mutate(existing_country_f = 1) %>% 
  # Convert to data.frame
  as.data.frame()

## Join existing destinations in the baseline year with the exports dataset ---- 
panjiva_export_data<-
  panjiva_export_data %>% 
  # Join with the existing destinations at the firm-HS6 level
  left_join(
    existing_destin_f_hs6, 
    by = c("domestic_id", "hs6", "foreign_country_cleaned_iso3")
  ) %>%
  # Join with the existing destinations (at the firm-product) again but by firm-product to check if the firm-product existed/had transactions in baseline year
  left_join(
    existing_destin_f_hs6 %>% 
      distinct(domestic_id, hs6) %>% 
      mutate(firm_hs6_exist_base_year = 1), 
    by = c("domestic_id", "hs6")
  ) %>% 
  mutate(
    # Dummy variable equal to TRUE if the destination is a new country (at the firm-HS6 level) compared to the 
    # existing countries (destinations) in the baseline year. 
    is_new_country_f_hs6 = is.na(existing_country_f_hs6) & firm_hs6_exist_base_year == 1,
  ) %>% 
  # Join with the existing destinations at the firm level 
  left_join(
    existing_destin_f, 
    by = c('domestic_id', "foreign_country_cleaned_iso3")
  ) %>% 
  # Join with the existing destinations (at the firm-product) again but by firm-product to check if the firm-product existed/had transactions in 2017
  left_join(
    existing_destin_f %>% 
      distinct(domestic_id) %>% 
      mutate(firm_exist_base_year = 1), 
    by = c('domestic_id')
  ) %>% 
  mutate(
    # Dummy variable equal to TRUE if the destination is a new country (at the firm level) compared to the 
    # existing countries (destinations) in the baseline year. 
    is_new_country_f = is.na(existing_country_f) & firm_exist_base_year == 1
  )


# Collapse export data to the firm-month-hs6 level -----
panjiva_export_data <- lazy_dt(panjiva_export_data)

panjiva_export_data_clean <- panjiva_export_data %>% 
  select(domestic_company_id = domestic_id, domestic_company_name = domestic_cleaned, 
         year, month,  hs6, export, country = foreign_country_cleaned_iso3, 
         is_new_country_f_hs6) %>%
  mutate(hs6 = substr(hs6, 1, 6)) %>%
  group_by(domestic_company_id, domestic_company_name, year, month,  hs6) %>%
  summarise(# Sum of export value 
            export = sum(export, na.rm = TRUE), 
            # Number of distinct countries (distinct destinations) 
            n_countries = n_distinct(country), 
            # Dummy equal to 1 if the firm in a particular year-month exported to a new country (destination)
            # compared to the baseline year
            new_country = as.integer(any(is_new_country_f_hs6))
            ) %>%
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
  as_tibble()

# Save results 
write_csv(panjiva_export_data_clean, "../../Data/India/processed_data/export_summaries_by_firm_month_HS_code_complete.csv")


# Collapse export data to the firm-month level ----
panjiva_export_firm_month <- panjiva_export_data %>% 
  select(domestic_company_id = domestic_id, domestic_company_name = domestic_cleaned,
         year, month, export, country = foreign_country_cleaned_iso3, is_new_country_f, 
         hs6) %>%
  # Group by firm-year-month
  group_by(domestic_company_id, domestic_company_name, year, month) %>%
  summarise(export = sum(export, na.rm = TRUE), 
            # Number of distinct countries (distinct destinations) 
            n_countries = n_distinct(country), 
            # Dummy equal to 1 if the firm in a particular year-month exported to a new country (destination)
            # compared to the baseline year
            new_country = as.integer(any(is_new_country_f)), 
            # Number of distinct HS6 products 
            n_hs6_products = n_distinct(hs6)
            ) %>%
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
    export,
    log_export,
    export_dummy
  ) %>%
  arrange(domestic_company_id, date) %>% 
  as.data.table()
  
write_csv(panjiva_export_firm_month, "../../Data/India/processed_data/export_summaries_by_firm_month_complete.csv")
