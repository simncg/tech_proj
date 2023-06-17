#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  gen_data_model_products_IDN.R                                    #
#                                                                           #
# This program generates the data necessary to run the regressions of the   #
# model with different products categories specifications (eBay tradable,   #
# China e-commerce, BEC classification, Time-Sensitivity) for Indonesia     #                 
#                                                                           #                                                                         
#                                                                           #
#                                                                           #           -                                                                                                                                                  #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)


# Load libraries ----
source("../src/packages.R")


# Read HS Code Classification data with products information ----
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

# SIC Groups to analyze ----
included_SIC_Groups <- c('AG-M-C', # Agricultural, Construction and Mining
                         "MANUF",  # Manufacturing
                         "SVCS",   # Services
                         "F-I-RE", # Finance Insurance, Real Estate 
                         "TR-UTL", # Transport & Utilities
                         "WHL-RT"  # Wholesale-retail
)

# Load Aberdeen data ----
aberdeen_data <- arrow::read_parquet("../../Data/Indonesia/raw_data/master_file_builtwith_updated.parquet",
                                 col_select = c("In_aberdeen","company_id","date", "date_character", 
                                                 "number_of_employees", "NAICS6_CODE", "SIC_group")) %>% 
  rename(EMPLE = number_of_employees) %>% 
  distinct(company_id, .keep_all = T) %>% 
  filter(In_aberdeen) %>%  
  select(-In_aberdeen, -date, -date_character)




# Load additional data of Aberdeen (to add more variables obtained from Aberdeen)
aberdeen_data_2 <- read_xlsx("../../Data/Indonesia/raw_data/matched_data_Indonesia.xlsx") 

# Correspondance table of IDs for Aberdeen of Indonesia
IDN_Domestic_Ids_Corresp <- read_xlsx("../../Data/Indonesia/raw_data/IDN_Domestic_Ids_Corresp_update.xlsx")

# Use correspondance, get SITEID and Aberdeen Company Name
aberdeen_data_2 <-IDN_Domestic_Ids_Corresp  %>% 
  select(prev_our_domestic_id, new_our_domestic_id ) %>% 
  distinct() %>% 
  filter(prev_our_domestic_id  %in% aberdeen_data_2$our_domestic_id) %>% 
  left_join(aberdeen_data_2,  by = c("prev_our_domestic_id" = "our_domestic_id")) %>% 
  rename(company_id = new_our_domestic_id) %>% 
  distinct(company_id, .keep_all = T) %>% 
  select(company_id, SITEID, Aberdeen_COMPANY)

# Join both datasets with Aberdeen data
aberdeen_data<-left_join(aberdeen_data, aberdeen_data_2, by = "company_id") %>% 
  relocate(company_id, SIC_group, SITEID) %>% 
  rename(SICGRP = SIC_group)

rm(aberdeen_data_2)


# Read data with stringency index, which measures severity of government restrictions ----
OxCGRT_timeseries_stringency_index <- readxl::read_excel("../../Data/Extra Data/OxCGRT_timeseries_all.xlsx", sheet ="stringency_index"   )

# Get mean stringency index during Covid period
covid_data <- OxCGRT_timeseries_stringency_index %>%
  filter(country_name == "Indonesia") %>%
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


# Read imports data ----

# First read data with variable that indicates if a firm import less than 1000 usd in both 2019 
# and 2020. Also includes a variable that indicates if a firm, that is only in 2020, imports
# less than 1000 usd in this year. Also, another variable that does the same but for 2019.  
imports_less_than_1000usd<-read_csv("../../Data/Indonesia/processed_data/import_firms_trade_less_than_1000usd.csv")

# Read imports data at the firm-month-HS6 level
import_data <- read_csv("../../Data/Indonesia/processed_data/import_summary_by_firm_month_HS_code.csv") %>% 
  select(company_id, date, date_character,hs6,log_import, import, 
         import_dummy, n_countries_import = n_countries)%>% 
  mutate(hs6  = str_pad(hs6 , 6, "left", "0"), 
         year = year(date)) %>% 
  # Drop firms that imported less than 1000 usd in both 2019 and 2020
  # Drop firms that only appears in 2019 and imported less than 1000 usd in this year
  # Drop firms that only appears in 2020 and imported less than 1000 usd in this year
  left_join(imports_less_than_1000usd, 
            by = c("company_id", "year")) %>% 
  filter(!less_than_1000USD_import_19_20, 
         !less_than_1000USD_import_2019, 
         !less_than_1000USD_import_2020) %>% 
  # Select variables to be analyzed 
  select(-year, -less_than_1000USD_import_19_20, -n_years,
         -less_than_1000USD_import_2020, 
         -less_than_1000USD_import_2019, 
         -sum_import)

# Read exports data ----


# First read data with variable that indicates if a firm export less than 1000 usd in both 2019 
# and 2020. Also includes a variable that indicates if a firm that only appears in 2020 exports
# less than 1000 usd in this year. Also, another variable that does the same but for 2019. 

exports_less_than_1000usd<-read_csv("../../Data/Indonesia/processed_data/export_firms_trade_less_than_1000usd.csv")


# Read exports data at the firm-month-HS6 level
export_data <- read_csv("../../Data/Indonesia/processed_data/export_summary_by_firm_month_HS_code.csv") %>% 
  select(company_id, date, date_character,hs6,log_export, export, export_dummy, 
         n_countries_export = n_countries) %>% 
  mutate(hs6  = str_pad(hs6 , 6, "left", "0"), 
         year = year(date)) %>% 
  # Drop firms that in a particular year export less than 1000 usd
  left_join(exports_less_than_1000usd, 
            by = c("company_id", "year")) %>% 
  filter(!less_than_1000USD_export_19_20,
         !less_than_1000USD_export_2019, 
         !less_than_1000USD_export_2020) %>% 
  # Select variables to be analyzed 
  select(-year, -less_than_1000USD_export_19_20, 
         -less_than_1000USD_export_2020, less_than_1000USD_export_2019, 
         -n_years, -sum_export, 
         -less_than_1000USD_export_2019)


# Read BuiltWith data for Indonesia ----
tech_data<-read_parquet("../../Data/Indonesia/processed_data/tech_data_IDN.parquet")

gc()


# Data for regressions of the model that measures if tech adoption affects trade outcomes ----


# Data for regressions are at the transaction level (firm-month-hs6 code)
# Join data for those observations that are matched to both BuiltWith and Aberdeen data
# We will analyze E-commerce (ecomnod) and E-payment technologies (payrobust)


# Imports data 
pay_ecom_import_data_IDN<-import_data %>% 
  # Join information from Aberdeen to have SIC group and NAICS6 Code
  left_join(aberdeen_data %>% 
              select(company_id, SITEID, SICGRP, NAICS6_CODE),
            by = c("company_id")) %>% 
  # Keep firms that are part of analyzed SIC groups
  filter(SICGRP %in% included_SIC_Groups) %>%   
  # Select just the bare minimum variables
  select(company_id, date_character, hs6, log_import, import, import_dummy, 
         SITEID, SICGRP, NAICS6_CODE, n_countries_import) %>% 
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
  filter(first_adopted_payrobust != "before february 2019" & 
           first_adopted_ecom != "before february 2019") %>%
  # Keep firms that had website before february 2019 and are indexed after June 2021
  filter(company_id %in% (tech_data %>%
                            group_by(company_id) %>%
                            summarise(FI_min = min(FI),
                                      LI_max = max(LI)) %>%
                            filter(FI_min <  ymd("2019-02-01"),
                                   LI_max >  ymd("2021-06-01")) %>%
                            pull(company_id))) %>%
  # Order data
  arrange(company_id, hs6, date) %>% 
  # Rename variable related to remote work ISIC
  rename(mean_remote_work_ISIC = mean_IDN, letter_credit_use = LCInt)



gc()


# Exports data 
pay_ecom_export_data_IDN<-export_data %>% 
  # Join information from Aberdeen to have SIC group and NAICS6 Code
  left_join(aberdeen_data %>% 
              select(company_id, SITEID, SICGRP, NAICS6_CODE),
            by = c("company_id")) %>% 
  # Keep firms that are part of analyzed SIC groups
  filter(SICGRP %in% included_SIC_Groups) %>%   
  # Select just the bare minimum variables
  select(company_id,date_character, hs6, log_export, export, export_dummy, 
         SITEID, SICGRP, NAICS6_CODE, n_countries_export) %>% 
  # Add the HS code classification data with products information 
  left_join(hs_data, by = c('hs6' = 'hs_2017')) %>% 
  # Add the intermediate - capital products classification information
  left_join(int_cap_HS, by = c('hs6' = 'hs_2017')) %>% 
  # Add the builtwith technology data 
  left_join(tech_data %>% 
              select(-LI, -FI),
            by = c("company_id", "date_character")) %>% 
  # Drop firms that adopted e-commerce/e-payments technology before period of analysis
  filter(first_adopted_payrobust != "before february 2019" & 
         first_adopted_ecom != "before february 2019") %>%
  # Keep firms that had website before february 2019 and are indexed after September 2021
  filter(company_id %in% (tech_data %>%
                            group_by(company_id) %>%
                            summarise(FI_min = min(FI),
                                      LI_max = max(LI)) %>%
                            filter(FI_min <  ymd("2019-02-01"),
                                   LI_max >  ymd("2021-09-01")) %>%
                            pull(company_id))) %>%
  # Order data
  arrange(company_id, hs6, date) %>% 
  rename(mean_remote_work_ISIC = mean_IDN)%>% 
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
  select(company_id, date, date_character, hs6, log_import, import, import_dummy,
         n_countries_import,
         SITEID, SICGRP, NAICS6_CODE) %>% 
  # Add the HS code classification data with products information 
  left_join(hs_data, by = c('hs6' = 'hs_2017')) %>% 
  # Add the intermediate - capital products classification information
  left_join(int_cap_HS, by = c('hs6' = 'hs_2017')) %>% 
  # Add COVID data to have stringency index 
  mutate(date = as.Date(date)) %>% 
  left_join(covid_data, by= c("date" = "month_year")) %>% 
  # Join tech data
  left_join(tech_data  %>% 
              select(-LI, -FI, -date), 
            by = c('company_id', "date_character")) %>% 
  # Keep firms that had website before february 2019 and are indexed after end 2021
  filter(company_id %in% (tech_data %>%
                            group_by(company_id) %>%
                            summarise(FI_min = min(FI),
                                      LI_max = max(LI)) %>%
                            filter(FI_min <  ymd("2019-02-01"),
                                   LI_max >  ymd("2021-06-01")) %>%
                            pull(company_id))) %>%
  # Modify existing variables
  mutate(Ebay_tradable = as.numeric(Ebay_tradable), 
         adopted_pay_or_ecom_before_2019 = as.numeric(adopted_pay_or_ecom_before_2019)) %>% 
  # Rename some variables
  rename(mean_remote_work_ISIC = mean_IDN, letter_credit_use = LCInt)%>% 
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
  select(company_id, date, date_character, hs6, log_export, export, export_dummy,
         n_countries_export, 
         SITEID, SICGRP, NAICS6_CODE) %>% 
  # Add the HS code classification data with products information 
  left_join(hs_data, by = c('hs6' = 'hs_2017')) %>% 
  # Add the intermediate - capital products classification information
  left_join(int_cap_HS, by = c('hs6' = 'hs_2017')) %>% 
  # Add COVID data to have stringency index 
  mutate(date = as.Date(date)) %>% 
  left_join(covid_data, by= c("date" = "month_year")) %>% 
  # Join tech data
  left_join(tech_data  %>% 
              select(-LI, -FI, -date), 
            by = c('company_id', "date_character")) %>% 
  # Keep firms that had website before july 2018 and are indexed after September 2021
  filter(company_id %in% (tech_data %>%
                            group_by(company_id) %>%
                            summarise(FI_min = min(FI),
                                      LI_max = max(LI)) %>%
                            filter(FI_min <  ymd("2019-02-01"),
                                   LI_max >  ymd("2021-09-01")) %>%
                            pull(company_id))) %>%
  # Modify existing variables
  mutate(Ebay_tradable = as.numeric(Ebay_tradable), 
         adopted_pay_or_ecom_before_2019 = as.numeric(adopted_pay_or_ecom_before_2019)) %>% 
  # Rename some variables
  rename(mean_remote_work_ISIC = mean_IDN, letter_credit_use = LCInt)%>% 
  # Fill monthly stringency index with 0s in the pre-covid period 
  mutate(month_mean_stringency_index = ifelse(is.na(month_mean_stringency_index), 0, month_mean_stringency_index))



# Remove no longer needed objects
rm(export_data, hs_data, import_data, tech_data, aberdeen_data, exports_less_than_1000usd, 
   int_cap_HS, included_SIC_Groups, imports_less_than_1000usd, covid_data, 
   OxCGRT_timeseries_stringency_index, IDN_Domestic_Ids_Corresp)

# Save data ---- 
write_csv(pay_ecom_import_data_IDN, "../../Data/Indonesia/processed_data/imports_product_model_IDN.csv")
write_csv(pay_ecom_export_data_IDN, "../../Data/Indonesia/processed_data/exports_product_model_IDN.csv")
write_csv(import_tech_mitig, "../../Data/Indonesia/processed_data/imports_tech_mitigation_model_IDN.csv")
write_csv(export_tech_mitig, "../../Data/Indonesia/processed_data/exports_tech_mitigation_model_IDN.csv")


gc()

