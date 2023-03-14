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

# Read Data -----

## Aberdeen ----

### Aberdeen Foreign ----

# Read matched Aberdeen to Panjiva data for foreign firms trading with IDN
aberdeen_f_data<-fread("../../Data/Indonesia/raw_data/matched_foreign_firms_indonesia.csv") %>% 
  # Select foreign company ID, foreign country, siteid, Aberdeen firm name, Panjiva firm name, 
  # SIC industry, NAICS6 classficiation. 
  select(foreign_company_id = our_ID, foreign_country_aberdeen = country_iso3_01, foreign_SITEID = SITEID,
         foreign_aberdeen_firm_name = Aberdeen_COMPANY,  foreign_panjiva_firm_name = panjiva_cleaned_name_01,
         foreign_SICGRP = SICGRP, foreign_NAICS6_CODE = NAICS6_CODE)%>% 
  lazy_dt() %>% 
  as_tibble()

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
  lazy_dt() %>% 
  as_tibble()

rm(aberdeen_data_2, IDN_Domestic_Ids_Corresp)


## Panjiva Exports for foreign firms ----

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
  left_join(aberdeen_data, by = "domestic_company_id") %>%
  as_tibble()

## Panjiva Imports for foreign firms ----

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
         matched_both_aberdeen = as.logical(matched_aberdeen_dom*matched_aberdeen_for)) %>% 
  as_tibble()






# 
# match_imports<- data.frame(Data = "Panjiva Imports",
#                            n_dom_firms = length(unique(imports_f_data$domestic_company_id)), 
#                            n_for_firms = length(unique(imports_f_data$foreign_company_id))
#                 ) %>% 
#   mutate(n_firms_total = n_dom_firms + n_for_firms) %>% 
#   bind_cols(
#     imports_f_data %>% 
#       select(domestic_company_id,matched_aberdeen_dom) %>% 
#       distinct(domestic_company_id, .keep_all = T) %>% 
#       mutate(n_match_dom = sum(matched_aberdeen_dom)) %>% 
#       select(n_match_dom) %>% 
#       head(1)
#   ) %>% 
#   bind_cols(
#     imports_f_data %>% 
#       select(foreign_company_id, matched_aberdeen_for) %>% 
#       distinct(foreign_company_id, .keep_all = T) %>% 
#       mutate(n_match_for = sum(matched_aberdeen_for)) %>% 
#       select(n_match_for) %>% 
#       head(1)
#   ) %>% 
#   mutate(`% Matched to Aberdeen Domestic` = n_match_dom/n_dom_firms,
#          `% Matched to Aberdeen Foreign` = n_match_for/n_for_firms
#   )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # SIC Groups to analyze ----
# included_SIC_Groups <- c('AG-M-C', # Agricultural, Construction and Mining
#                          "MANUF",  # Manufacturing
#                          "SVCS",   # Services
#                          "F-I-RE", # Finance Insurance, Real Estate 
#                          "TR-UTL", # Transport & Utilities
#                          "WHL-RT"  # Wholesale-retail
# )
# 
# 
