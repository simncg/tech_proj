#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  gen_data_model_products_IND.R                                    #
#                                                                           #
# This program generates the data necessary to run the regressions with     # 
# different products categories specifications (eBay tradable,              #
# China e-commerce, BEC classification, Time-Sensitivity, Capital and       # 
# Intermediate products) for India. The first model measures if technology  #              
# adoption affects trade outcomes. The second model measures if tech adoption#                                                                         
# before 2019 helps to mitigate the impacts of COVID on trade outcomes      #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")

# Load HS Code Classification data with products information ----
hs_data <- read_csv("../../Data/Extra Data/HS_code_classifications.csv")%>% 
  mutate(hs_2017   = str_pad(hs_2017  , 6, "left", "0"))

# Read data with intermediate and capital HS products classification ----
int_cap_HS <- read_excel("../../Data/Extra Data/capital_intermediate_HS_code_classification.xlsx", 
                         sheet = "FromHSToISICToEC") %>% 
  # Keep only HS 2012 version
  filter(HS == 4) %>% 
  # Keep only the variable with the HS6 codes and the variable EUC with information of the type of product
  select(hs6 =`HS-6digit`, EUC) %>% 
  # Join with correspondence tables to convert from HS codes 2012 to HS codes 2017 
  left_join(., read_xlsx("../../Data/Extra Data/HS2017toHS2012ConversionAndCorrelationTables.xlsx") %>% 
              rename( "hs_2017" = "From HS 2017",  "hs_2012"="To HS 2012"), 
            by = c("hs6" = "hs_2012")) %>% 
  select(hs_2017, EUC) %>% 
  # Create an indicator variable 
  mutate(value = 1) %>% 
  # Remove na in hs codes 2017
  na.omit(hs_2017) %>% 
  # Long format to wide format: convert CAP and INT to two separate variables
  pivot_wider(names_from = EUC, values_from = value, values_fill = 0) %>% 
  # Keep only the identifiers variables for intermediate and capital products
  select(hs_2017, CAP, INT) %>% 
  # From dummy to logical 
  mutate(CAP = as.logical(CAP), 
         INT = as.logical(INT))

gc()


# SIC groups to analyze ----
included_SIC_Groups <- c('AG-M-C', # Agricultural, Construction and Mining
                         "MANUF",  # Manufacturing
                         "SVCS",   # Services
                         "F-I-RE", # Finance Insurance, Real Estate 
                         "TR-UTL", # Transport & Utilities
                         "WHL-RT"  # Wholesale-retaol
)


# Load matched Aberdeen to Panjiva data ----
aberdeen_data <- read_csv("../../Data/India/raw_data/matched_Aberdeen_to_Panjiva_data_India_v2.csv") %>%
  rename(company_id = our_ID)

# Read data with stringency index which measures severity of government restrictions ----
OxCGRT_timeseries_stringency_index <- readxl::read_excel("../../Data/Extra Data/OxCGRT_timeseries_all.xlsx", sheet ="stringency_index"   )

# Get mean stringency index during Covid period
covid_data <- OxCGRT_timeseries_stringency_index %>%
  filter(country_name == "India") %>%
  pivot_longer(
    cols = -c(country_code, country_name),
    names_to = "date",
    values_to = "stringency_index"
  ) %>%
  mutate(date = parse_date_time(date, orders = "%d%b%Y")) %>%
  mutate(month_year = ymd(glue(
    "{year(date)}-{month(date)}-01"
  ))) %>%
  group_by(month_year) %>%
  summarise(month_mean_stringency_index  = mean(stringency_index , na.rm = TRUE))



# Imports data ----

# Firms IDs to be replaced to the old firm IDS because they are different in the latest version of Panjiva
new_ids<-c("IND164276", "IND37751", "IND171809", "IND12967", "IND253050") # IDs to be replaced
old_ids<-c("IND38927", "IND41278", "IND6043", "IND23040", "IND46989")     # Old IDs (the ones we need)

# Firms that import yearly less than 1000 usd in the years they appear  
import_less_than_1000usd<-read_csv("../../Data/India/processed_data/import_firms_trade_less_than_1000usd.csv")

import_data <-fread("../../Data/India/processed_data/import_summaries_by_firm_month_HS_code_complete.csv") %>% 
  select(company_id = domestic_company_id, year, month, date, date_character,hs6, import, log_import, import_dummy, 
         n_countries_import = n_countries, new_source = new_country) %>% 
  # filter to analysis period
  filter(date >= ymd("2018-07-01"),
         date <= ymd("2021-12-31")) %>% 
  mutate(hs6  = str_pad(as.character(hs6) , 6, "left", "0")) %>% 
  #Drop firms that imported less than 1000 usd in all years between 2019-2021
  left_join(import_less_than_1000usd, by = c("company_id", "year")) %>% 
  filter(!less_than_1000usd) %>% 
  select(-less_than_1000usd, -sum_import, -n_years) %>% 
  # Replace some id firms that changed with the latest version of Panjiva 
  mutate(company_id = if_else(company_id %in% new_ids, old_ids[match(company_id, new_ids)], company_id))

gc()

# Exports data ----

# Firms that export yearly less than 1000 usd in the years they appear  
export_less_than_1000usd<-read_csv("../../Data/India/processed_data/export_firms_trade_less_than_1000usd.csv")


export_data <-fread("../../Data/India/processed_data/export_summaries_by_firm_month_HS_code_complete.csv") %>% 
  select(company_id = domestic_company_id, year, month, date, date_character,hs6, export, log_export, export_dummy, 
         n_countries_export = n_countries, new_destination = new_country
         ) %>% 
  # filter to analysis period
  filter(date >= ymd("2018-07-01"),
         date <= ymd("2021-12-31")) %>% 
  mutate(hs6  = str_pad(as.character(hs6) , 6, "left", "0")) %>% 
  # Drop firms that exported less than 1000 usd in all years between 2019-2021
  left_join(export_less_than_1000usd, by = c("company_id", "year"))%>% 
  filter(!less_than_1000usd) %>% 
  select(-less_than_1000usd, -sum_export, -n_years) %>% 
  # Replace some id firms that changed with the latest version of Panjiva 
  mutate(company_id = if_else(company_id %in% new_ids, old_ids[match(company_id, new_ids)], company_id))

gc()


# Load technology data (only nod variables) ----
tech_data <- read_parquet("../../Data/India/processed_data/tech_data_IND.parquet")


# Data for regressions of the model that measures if tech adoption affects trade outcomes ----


# Data for regressions are at the transaction level (firm-month-hs6 code)
# Join data for those observations that are matched to both BuiltWith and Aberdeen data
# We will analyze E-commerce (ecomnod) and E-payment technologies (payrobust)


# Imports data 
pay_ecom_import_data_IND<-import_data %>% 
  # Join information from Aberdeen to have SIC group and NAICS6 Code
  left_join(aberdeen_data %>% 
              select(company_id, SITEID, SICGRP, NAICS6_CODE),
            by = c("company_id")) %>% 
  # Keep firms that are part of analyzed SIC groups
  filter(SICGRP %in% included_SIC_Groups) %>%   
  # Select just the bare minimum variables
  select(company_id, year, month, date_character, hs6, log_import, import,
         new_source, n_countries_import,
         import_dummy, SITEID, SICGRP, NAICS6_CODE) %>% 
  # Add the HS code classification data with products information 
  left_join(hs_data, by = c('hs6' = 'hs_2017')) %>% 
  # Add the intermediate - capital products classification information
  left_join(int_cap_HS, by = c('hs6' = 'hs_2017')) %>% 
  # Add the builtwith technology data 
  left_join(tech_data %>% 
              select(-LI, -FI),
            by = c("company_id", "date_character")
            ) %>% 
  # Drop firms that adopted e-commerce/e-payments technology before period of analysis
  filter(first_adopted_payrobust != "before july 2018" & 
         first_adopted_ecom != "before july 2018") %>%
  # Keep firms that had website before period of analysis and are indexed after 2021
  filter(company_id %in% (tech_data %>%
                            group_by(company_id) %>%
                            summarise(FI_min = min(FI),
                                      LI_max = max(LI)) %>%
                                      filter(FI_min <  ymd("2018-07-01"),
                                             LI_max >  ymd("2021-12-31")) %>%
                                      pull(company_id))) %>%
  # Order data
  arrange(company_id, hs6, date) %>% 
  # Rename variable related to remote work ISIC
  rename(mean_remote_work_ISIC = mean_IND, 
         letter_credit_use = LCInt)



gc()



# Exports data 
pay_ecom_export_data_IND<-export_data %>% 
  # Join information from Aberdeen to have SIC group and NAICS6 Code
  left_join(aberdeen_data %>% 
              select(company_id, SITEID, SICGRP, NAICS6_CODE),
            by = c("company_id")) %>% 
  # Keep firms that are part of analyzed SIC groups
  filter(SICGRP %in% included_SIC_Groups) %>%   
  # Select just the bare minimum variables
  select(company_id, year, month, date_character, hs6, log_export, export,
         new_destination, n_countries_export,
         export_dummy, SITEID, SICGRP, NAICS6_CODE) %>% 
  # Add the HS code classification data with products information 
  left_join(hs_data, by = c('hs6' = 'hs_2017')) %>% 
  # Add the intermediate - capital products classification information
  left_join(int_cap_HS, by = c('hs6' = 'hs_2017')) %>% 
  # Add the builtwith technology data 
  left_join(tech_data %>% 
              select(-LI, -FI),
            by = c("company_id", "date_character")) %>% 
  # Drop firms that adopted e-commerce/e-payments technology before period of analysis
  filter(first_adopted_payrobust != "before july 2018" & 
         first_adopted_ecom != "before july 2018") %>%
  # Keep firms that had website before july 2018 and are indexed after end 2021
  filter(company_id %in% (tech_data %>%
                            group_by(company_id) %>%
                            summarise(FI_min = min(FI),
                                      LI_max = max(LI)) %>%
                            filter(FI_min <  ymd("2018-07-01"),
                                   LI_max >  ymd("2021-12-31")) %>%
                            pull(company_id))) %>%
  # Order data
  arrange(company_id, hs6, date) %>% 
  rename(mean_remote_work_ISIC = mean_IND)%>% 
  rename(letter_credit_use = LCInt)

gc()



# Data for regressions of the model that measures if tech adoption mitigates COVID impacts on trade outcomes ----

# Imports data
import_tech_mitig<- import_data %>% 
  # Join information from Aberdeen to have SIC group and NAICS6 Code
  left_join(aberdeen_data %>% 
              select(company_id, SITEID, SICGRP, NAICS6_CODE),
            by = c("company_id")) %>% 
  # Keep firms that are part of analyzed SIC groups
  filter(SICGRP %in% included_SIC_Groups) %>%   
  # Select just the bare minimum variables
  select(company_id, year, month, date, date_character, hs6, log_import, import,
         new_source, n_countries_import,
         import_dummy, SITEID, SICGRP, NAICS6_CODE) %>% 
  # Add the HS code classification data with products information 
  left_join(hs_data, by = c('hs6' = 'hs_2017')) %>% 
  # Add the intermediate - capital products classification information
  left_join(int_cap_HS, by = c('hs6' = 'hs_2017')) %>% 
  # Add COVID data to have stringency index 
  mutate(date = as.Date(date)) %>% 
  left_join(covid_data, by= c("date" = "month_year")) %>% 
  # Join tech data
  left_join(tech_data %>% 
              select(company_id, date_character, adopted_pay_or_ecom_before_2019), 
            by = c('company_id', "date_character")) %>% 
  # Keep firms that had website before july 2018 and are indexed after end 2021
  filter(company_id %in% (tech_data %>%
                            group_by(company_id) %>%
                            summarise(FI_min = min(FI),
                                      LI_max = max(LI)) %>%
                            filter(FI_min <  ymd("2018-07-01"),
                                   LI_max >  ymd("2021-12-31")) %>%
                            pull(company_id))) %>%
  # Modify existing variables
  mutate(Ebay_tradable = as.numeric(Ebay_tradable), 
         adopted_pay_or_ecom_before_2019 = as.numeric(adopted_pay_or_ecom_before_2019)) %>% 
  # Rename some variables
  rename(mean_remote_work_ISIC = mean_IND, letter_credit_use = LCInt)%>% 
  # Fill monthly stringency index with 0s in the pre-covid period 
  mutate(month_mean_stringency_index = ifelse(is.na(month_mean_stringency_index), 0, month_mean_stringency_index))



# Exports data
export_tech_mitig<- export_data %>% 
  # Join information from Aberdeen to have SIC group and NAICS6 Code
  left_join(aberdeen_data %>% 
              select(company_id, SITEID, SICGRP, NAICS6_CODE),
            by = c("company_id")) %>% 
  # Keep firms that are part of analyzed SIC groups
  filter(SICGRP %in% included_SIC_Groups) %>%   
  # Select just the bare minimum variables
  select(company_id, year, month, date, date_character, hs6, log_export, export, 
         new_destination, n_countries_export,
         export_dummy, SITEID, SICGRP, NAICS6_CODE) %>% 
  # Add the HS code classification data with products information 
  left_join(hs_data, by = c('hs6' = 'hs_2017')) %>% 
  # Add the intermediate - capital products classification information
  left_join(int_cap_HS, by = c('hs6' = 'hs_2017')) %>% 
  # Add COVID data to have stringency index 
  mutate(date = as.Date(date)) %>% 
  left_join(covid_data, by= c("date" = "month_year")) %>% 
  # Join tech data
  left_join(tech_data %>% 
              select(company_id, date_character, adopted_pay_or_ecom_before_2019), 
            by = c('company_id', "date_character")) %>% 
  # Keep firms that had website before july 2018 and are indexed after end 2021
  filter(company_id %in% (tech_data %>%
                            group_by(company_id) %>%
                            summarise(FI_min = min(FI),
                                      LI_max = max(LI)) %>%
                            filter(FI_min <  ymd("2018-07-01"),
                                   LI_max >  ymd("2021-12-31")) %>%
                            pull(company_id))) %>%
  # Modify existing variables
  mutate(Ebay_tradable = as.numeric(Ebay_tradable), 
         adopted_pay_or_ecom_before_2019 = as.numeric(adopted_pay_or_ecom_before_2019)) %>% 
  # Rename some variables
  rename(mean_remote_work_ISIC = mean_IND, letter_credit_use = LCInt)%>% 
  # Fill monthly stringency index with 0s in the pre-covid period 
  mutate(month_mean_stringency_index = ifelse(is.na(month_mean_stringency_index), 0, month_mean_stringency_index))



# Remove no longer needed objects
rm(export_data, hs_data, import_data, tech_data, aberdeen_data, export_less_than_1000usd, 
   int_cap_HS, included_SIC_Groups, import_less_than_1000usd, covid_data, 
   OxCGRT_timeseries_stringency_index, new_ids, old_ids)

# Save data ---- 
write_csv(pay_ecom_import_data_IND, "../../Data/India/processed_data/imports_product_model_IND.csv")
write_csv(pay_ecom_export_data_IND, "../../Data/India/processed_data/exports_product_model_IND.csv")
write_csv(import_tech_mitig, "../../Data/India/processed_data/imports_tech_mitigation_model_IND.csv")
write_csv(export_tech_mitig, "../../Data/India/processed_data/exports_tech_mitigation_model_IND.csv")

gc()
