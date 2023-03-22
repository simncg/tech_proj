#===========================================================================#
# Date:    March 2023                                                       #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
#                                                                           #
# This program generates tables and graphs to analyze the trade partners of #
# Indonesia, i.e., the trade flow between Indonesia and another country     #
# (e.g., U.S).                                                              #                                         
#                                                                           #
#                                                                           #           -                                                                                                                                                  #
#===========================================================================#



# The task is to create descriptive statistics of the trade partners Indonesia-US. 

# Stats in table of presentation 18th october: 

# - Foregn firms overall (partners of firms matched to both Aberdeen and Builtwith)
# - Foreign firms in Aberdeen countries 
# - Median import partners per firm 
# - Median export partners per firm 
# - Median import partners per firm/hs6 product
# - Mean import partners per firm/hs6 product 
# - Median export partners per firm/hs6 product
# - Mean export partners per firm/hs6 product 
# - Median hs6 import products per firm 
# - Mean hs6 import products per firm 
# - Median hs6 export products per firm 
# - Mean hs6 export products per firm 


# Stats proposed by Devaki:

# Number of trading partners for firms in e-commerce or not 
# Number of ecommerce/non-ecommerce trading partners for firms that are in ecommerce or not. Basically, do (non) ecommerce firms trade more with other (non) ecommerce firms




# Focus only on 3 relations 

# wholesale/retail vs manufacturing vs the rest (3 categories).

# 1. Wholesale/Retail vs Manufacturing 
# 2. Wholesale/Retail vs The Rest of SIC
# 3. Manufacturing vs The Rest


# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
library(tidyverse)
library(haven)
library(data.table)
library(dtplyr)
library(readxl)

# Import functions to do the foreign partner analysis ----- 
source("../src/functions_foreign_partner_analysis.R")

# Read Data -----

## Aberdeen ----

### Aberdeen Foreign ----

# Read matched Aberdeen to Panjiva data for foreign firms trading with IDN
aberdeen_f_data<-fread("../../Data/Indonesia/raw_data/matched_foreign_firms_indonesia.csv") %>% 
  # Select foreign company ID, foreign country, siteid, Aberdeen firm name, Panjiva firm name, 
  # SIC industry, NAICS6 classficiation. 
  select(foreign_company_id = our_ID, foreign_country_aberdeen = country_iso3_01, foreign_SITEID = SITEID,
         foreign_aberdeen_firm_name = Aberdeen_COMPANY,  foreign_panjiva_firm_name = panjiva_cleaned_name_01,
         foreign_SICGRP = SICGRP, foreign_NAICS6_CODE = NAICS6_CODE)

### Aberdeen Domestic ----

# Read matched Aberdeen to Panjiva data for domestic firms
aberdeen_data <- arrow::read_parquet("../../Data/Indonesia/raw_data/master_file_builtwith_updated.parquet",
                                     col_select = c("In_aberdeen","company_id","date", "date_character", 
                                                    "NAICS6_CODE", "SIC_group")) %>% 
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
  select(company_id, SITEID, Aberdeen_COMPANY, panjiva_domestic_1)

# Join both datasets with Aberdeen data
aberdeen_data<-left_join(aberdeen_data, aberdeen_data_2, by = "company_id") %>% 
  relocate(company_id, SIC_group, SITEID)  %>% 
  rename(SICGRP = SIC_group, 
         domestic_company_id = company_id, 
         aberdeen_firm_name =Aberdeen_COMPANY, 
         panjiva_firm_name = panjiva_domestic_1) %>% 
  lazy_dt() 

rm(aberdeen_data_2, IDN_Domestic_Ids_Corresp)


## Panjiva Exports for foreign firms ----

# This data is at the domestic firm-foreign firm-year-month-hs8 level
exports_f_data<-read_dta("../../Data/Indonesia/raw_data/IDN_Exports_Monthly_foreign_domestic_level_dropdup.dta") %>% 
  lazy_dt() %>% 
  rename(foreign_country_panjiva = country) %>% 
  mutate(foreign_company_id = if_else(foreign_company_id == "", NA_character_, foreign_company_id), 
         foreign_country_panjiva = if_else(foreign_country_panjiva == "", NA_character_, foreign_country_panjiva), 
         hs6 = substr(hs, 1, 6),
         date = as.Date(paste0(year, "-", month, "-01"))) %>% 
  # Matching rates
  mutate(matched_aberdeen_dom = domestic_company_id %in% aberdeen_data$domestic_company_id, 
         matched_aberdeen_for = foreign_company_id %in% aberdeen_f_data$foreign_company_id, 
         matched_both_aberdeen = as.logical(matched_aberdeen_dom*matched_aberdeen_for)) %>% 
  # Join Aberdeen data for foreign firms
  left_join(aberdeen_f_data, by = "foreign_company_id") %>%
  # Join Aberdeen data for domestic firms
  left_join(aberdeen_data, by = "domestic_company_id") 

## Panjiva Imports for foreign firms ----

# This data is at the domestic firm-foreign firm-year-month-hs8 level
imports_f_data<-read_dta("../../Data/Indonesia/raw_data/IDN_Imports_Monthly_foreign_domestic_level_dropdup.dta") %>% 
  lazy_dt() %>% 
  rename(foreign_country_panjiva = country) %>% 
  mutate(foreign_company_id = if_else(foreign_company_id == "", NA_character_, foreign_company_id), 
         foreign_country_panjiva = if_else(foreign_country_panjiva == "", NA_character_, foreign_country_panjiva), 
         hs6 = substr(hs, 1, 6),
         date = as.Date(paste0(year, "-", month, "-01"))) %>% 
  # Join Aberdeen data for foreign firms
  left_join(aberdeen_f_data, by = "foreign_company_id") %>% 
  # Join Aberdeen data for domestic firms
  left_join(aberdeen_data, by = "domestic_company_id") %>% 
  # Matching rates
  mutate(matched_aberdeen_dom = domestic_company_id %in% aberdeen_data$domestic_company_id, 
         matched_aberdeen_for = foreign_company_id %in% aberdeen_f_data$foreign_company_id, 
         matched_both_aberdeen = as.logical(matched_aberdeen_dom*matched_aberdeen_for))


## HS products information ----

# Read HS Code Classification data with products information ----
hs_data <- read_csv("../../Data/Extra Data/HS_code_classifications.csv")%>% 
  mutate(hs_2017   = str_pad(hs_2017  , 6, "left", "0")) %>% 
  lazy_dt()

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
         INT = as.logical(INT)) %>% 
  lazy_dt()

gc()

# Tables ----

# Data of imports that we can analyze, so those observations that have both 
# non-missing values in SIC Group of aberdeen domestic and aberdeen foreign

included_SIC_Groups <- c('AG-M-C', # Agricultural, Construction and Mining
                         "MANUF",  # Manufacturing
                         "SVCS",   # Services
                         "F-I-RE", # Finance Insurance, Real Estate
                         "TR-UTL", # Transport & Utilities
                         "WHL-RT"  # Wholesale-retail
)

## Table 1 ----

# Create table with summary statistics between local industry and foreign 
# industry. We will analyze three categories of industries Wholesale-Retail, Manufacturing
# and the rest (Agricultural, Construction and Mining, Services, Finance Insurance and Real State,
# Transport & Utilities). 

imports_analysis<-imports_f_data %>% 
  na.omit(SICGRP, foreign_SICGRP) %>% 
  filter(SICGRP %in% included_SIC_Groups, 
         foreign_SICGRP %in% included_SIC_Groups) %>% 
  # Categorize in 3 industries (Wholesale-retail, Manufacturing and the rest)
  mutate(industry_local = if_else(!(SICGRP %in% c("MANUF", "WHL-RT")), "Rest", SICGRP), 
         industry_foreign = if_else(!(foreign_SICGRP %in% c("MANUF", "WHL-RT")), "Rest", foreign_SICGRP)
        ) %>% 
  # Filter for USA-IDN trade flow relationship
  filter(foreign_country_panjiva == "USA") %>% 
  # Group by local industry and foreign industry combinations (so 9 combinations)
  group_by(industry_local, industry_foreign) %>%
  # Create some summary statistics 
  mutate(
    # Total number of domestic companies in industry X that trade with foreign firms in industry X
    n_domestic_firms = n_distinct(domestic_company_id), 
    # Compute the total value of imports from domestic firms (sum of imports across firms) in industry X to all foreign firms in industry X.
    tot_imp_to_f_ind = sum(import), 
    # Average per IDN firm of total value of imports from domestic firms in industry X to all foreign firms in industry X
    avg_tot_imp_to_f_ind = tot_imp_to_f_ind/n_domestic_firms 
  ) %>% 
  ungroup() %>% 
  # Group by domestic firms, local industry and foreign industry
  group_by(domestic_company_id, industry_local, industry_foreign) %>% 
  # Number of foreign exporters by domestic firm (exporters because domestic companies are importing from them)
  mutate(foreign_n_exporters = n_distinct(foreign_company_id)) %>% 
  ungroup() %>% 
  # Group by local industry and foreign industry 
  group_by(industry_local, industry_foreign) %>% 
  mutate(
    # Average number of foreign partners per firm by local industry and foreign industry 
    avg_n_exporters = mean(foreign_n_exporters),  
    ) %>% 
  ungroup() %>% 
  # Group by domestic firm, foreign firm, local industry and foreign industry
  group_by(domestic_company_id, foreign_company_id, industry_local, industry_foreign) %>% 
  # Sum the total value of imports between two firms (firm-to-firm imports)
  mutate(imports_firm_to_firm = sum(import)) %>% 
  ungroup() %>% 
  # Group by local industry and foreign industry
  group_by(industry_local, industry_foreign) %>% 
  # Take the average of the firm-to-firm imports 
  mutate(mean_imp_firm_to_firm = mean(imports_firm_to_firm)) %>% 
  ungroup() %>% 
  select(industry_local, industry_foreign, n_domestic_firms, avg_tot_imp_to_f_ind, 
         avg_n_exporters, mean_imp_firm_to_firm) %>% 
  distinct(industry_local, industry_foreign, .keep_all = T) %>% 
  as_tibble()



exports_analysis<-exports_f_data %>% 
  na.omit(SICGRP, foreign_SICGRP) %>% 
  filter(SICGRP %in% included_SIC_Groups, 
         foreign_SICGRP %in% included_SIC_Groups) %>% 
  # Categorize in 3 industries (Wholesale-retail, Manufacturing and the rest)
  mutate(industry_local = if_else(!(SICGRP %in% c("MANUF", "WHL-RT")), "Rest", SICGRP), 
         industry_foreign = if_else(!(foreign_SICGRP %in% c("MANUF", "WHL-RT")), "Rest", foreign_SICGRP)
  ) %>% 
  # Filter for USA-IDN trade flow relationship
  filter(foreign_country_panjiva == "USA") %>% 
  # Group by local industry and foreign industry combinations (so 9 combinations)
  group_by(industry_local, industry_foreign) %>%
  # Create some summary statistics 
  mutate(
    # Total number of domestic companies in industry X that trade with foreign firms in industry X
    n_domestic_firms = n_distinct(domestic_company_id), 
    # Compute the total value of exports from domestic firms (sum of exports across firms) in industry X to all foreign firms in industry X.
    tot_exp_to_f_ind = sum(export), 
    # Average per IDN firm of total value of exports from domestic firms in industry X to all foreign firms in industry X
    avg_tot_exp_to_f_ind = tot_exp_to_f_ind/n_domestic_firms 
  ) %>% 
  ungroup() %>% 
  # Group by domestic firms, local industry and foreign industry
  group_by(domestic_company_id, industry_local, industry_foreign) %>% 
  # Number of foreign importers by domestic firm (importers because domestic companies are exporting to them)
  mutate(foreign_n_importers = n_distinct(foreign_company_id)) %>% 
  ungroup() %>% 
  # Group by local industry and foreign industry 
  group_by(industry_local, industry_foreign) %>% 
  mutate(
    # Average number of foreign partners per firm by local industry and foreign industry 
    avg_n_importers = mean(foreign_n_importers),  
  ) %>% 
  ungroup() %>% 
  # Group by domestic firm, foreign firm, local industry and foreign industry
  group_by(domestic_company_id, foreign_company_id, industry_local, industry_foreign) %>% 
  # Sum the total value of imports between two firms (firm-to-firm imports)
  mutate(exports_firm_to_firm = sum(export)) %>% 
  ungroup() %>% 
  # Group by local industry and foreign industry
  group_by(industry_local, industry_foreign) %>% 
  # Take the average of the firm-to-firm imports 
  mutate(mean_exp_firm_to_firm = mean(exports_firm_to_firm)) %>% 
  ungroup() %>% 
  select(industry_local, industry_foreign, n_domestic_firms, avg_tot_exp_to_f_ind, 
         avg_n_importers, mean_exp_firm_to_firm) %>% 
  distinct(industry_local, industry_foreign, .keep_all = T) %>% 
  as_tibble()


## Table 2: Same as table 1 but using products classification ----

# NOTE: There is a double counting in the number of firms, but this is just to see patterns

# Imports 
imp_prod_analysis<-imports_f_data %>% 
  left_join(hs_data, by = c("hs6" = "hs_2017")) %>% 
  left_join(int_cap_HS, by = c("hs6" = "hs_2017")) %>% 
  na.omit(SICGRP, foreign_SICGRP) %>% 
  filter(SICGRP %in% included_SIC_Groups, 
         foreign_SICGRP %in% included_SIC_Groups) %>% 
  # Categorize in 3 industries (Wholesale-retail, Manufacturing and the rest)
  mutate(industry_local = if_else(!(SICGRP %in% c("MANUF", "WHL-RT")), "Rest", SICGRP), 
         industry_foreign = if_else(!(foreign_SICGRP %in% c("MANUF", "WHL-RT")), "Rest", foreign_SICGRP)
  )%>% 
  # Filter for USA-IDN trade flow relationship
  filter(foreign_country_panjiva == "USA") 


# Exports
exp_prod_analysis<-exports_f_data %>% 
  left_join(hs_data, by = c("hs6" = "hs_2017")) %>% 
  left_join(int_cap_HS, by = c("hs6" = "hs_2017")) %>% 
  na.omit(SICGRP, foreign_SICGRP) %>% 
  filter(SICGRP %in% included_SIC_Groups, 
         foreign_SICGRP %in% included_SIC_Groups) %>% 
  # Categorize in 3 industries (Wholesale-retail, Manufacturing and the rest)
  mutate(industry_local = if_else(!(SICGRP %in% c("MANUF", "WHL-RT")), "Rest", SICGRP), 
         industry_foreign = if_else(!(foreign_SICGRP %in% c("MANUF", "WHL-RT")), "Rest", foreign_SICGRP)
  )%>% 
  # Filter for USA-IDN trade flow relationship
  filter(foreign_country_panjiva == "USA") 


consumable_prod_imp<-foreign_prod(imp_prod_analysis, cons_BEC , "Consumable", "Non_Consumable")
consumable_prod_exp<-foreign_prod(exp_prod_analysis, cons_BEC , "Consumable", "Non_Consumable")
