#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in Indonesia,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# This script generates the data necessary to run the extensive margin      #
# analysis                                                                  #
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

# Load functions to blown-up dataset for preparing data to extensive margin regressions
source("../src/functions_gen_data_ext_marg.R")

# HS data ----

# Since we cannot create counts of the products classification variables Letter Credit Use, 
# Mean Remote Work ISIC, Relationship Stickiness and Fraction inputs not sold on exchange, 
# We take the data with just the products and their classifications (hs_data) and we
# identify the products with values of these variables above the median and create
# dummies indicating whether the product is above the median or not. Then, we join 
# this information to the data at the firm-month-product level and count the number of
# products traded above the median and the number of products traded below the median. 
hs_data <- read_csv("../../Data/Extra Data/HS_code_classifications.csv")%>% 
  mutate(hs_2017   = str_pad(hs_2017  , 6, "left", "0")) %>% 
  select(hs6 = hs_2017, letter_credit_use = LCInt, mean_remote_work_ISIC = mean_IDN,
         relationship_stickiness, frac_lib_diff) %>% 
  mutate(across(-hs6, ~ median(.,na.rm = T) < ., 
                .names = 'above_median_{col}')) %>% 
  mutate(hs6 = as.integer(hs6)) %>% 
  # Keep only variables indicating whether above the median or not
  select(hs6, contains("above"))


# Get number of country partners by month-firm. ---

# When computing the number of partner countries per firm-month we will do it only
# for firm-month where all rows have the partner country not missing. For firm-month
# where some of the rows is missing it is best to not compute this outcome because 
# it will be incomplete and wrong.

# Imports partner countries
import_partner_country<-read_dta("../../Data/Indonesia/raw_data/IDN_Imports_Monthly_domestic_level_dropdup.dta")%>%
  filter(!str_starts(hs , "27"))%>% 
  select(company_id, year, month,  hs, import, country) %>% 
  mutate(hs6 = substr(hs, 1, 6), 
         date = as.Date(paste0(year, "-", month, "-01")), 
         country = ifelse(country == "", NA, country)) %>%
  select(-year, -month, -hs) %>% 
  group_by(company_id, date) %>%
  # Check if all rows have the country partner by firm-month 
  mutate(non_na_count = sum(!is.na(country)), 
         count_obs = n(), 
         all_non_na = non_na_count == count_obs) %>% 
  # Keep only firm-month groups that all rows have the country partner
  filter(all_non_na) %>% 
  # Group by firm-month to count number of partner countries
  group_by(company_id, date) %>% 
  summarise(n_country_partners = n_distinct(country)) %>%
  mutate(date = as.IDate(date)) %>% 
  ungroup()

gc()

# Exports partner countries
export_partner_country<-read_dta("../../Data/Indonesia/raw_data/IDN_Exports_Monthly_domestic_level_dropdup.dta")%>%
  filter(!str_starts(hs , "27"))%>% 
  select(company_id, year, month,  hs, export, country) %>% 
  mutate(hs6 = substr(hs, 1, 6), 
         date = as.Date(paste0(year, "-", month, "-01")), 
         country = ifelse(country == "", NA, country)) %>%
  select(-year, -month) %>% 
  group_by(company_id, date) %>%
  # Check if all rows have the country partner by firm-month 
  mutate(non_na_count = sum(!is.na(country)), 
         count_obs = n(), 
         all_non_na = non_na_count == count_obs) %>% 
  # Keep only firm-month groups that all rows have the country partner
  filter(all_non_na) %>% 
  # Group by firm-month to count number of partner countries
  group_by(company_id, date) %>% 
  summarise(n_country_partners = n_distinct(country)) %>%
  mutate(date = as.IDate(date)) %>% 
  ungroup()

gc()

# Data for model that measures if tech adoption affects trade outcomes ---- 

# Imports data for model that measures if tech adoption affects trade outcomes (after processed in gen_data_model_products_IDN.R) ----
imports_data_IDN<- fread("../../Data/Indonesia/processed_data/imports_product_model_IDN.csv") %>% 
  arrange(company_id, date, hs6) %>% 
  # Join variables that indicate if value of the variables product for letter_credit_use,
  # mean_remote_work_ISIC, relationship_stickiness and frac_lib_diff is above the median  
  left_join(hs_data, by = "hs6") %>% 
  # Join number of country partners by firm-month
  left_join(import_partner_country, by = c("company_id", "date")) %>% 
  mutate(date = as.Date(date))

# Exports data for model that measures if tech adoption affects trade outcomes  (after processed in gen_data_model_products_IDN.R) ----
exports_data_IDN<- fread("../../Data/Indonesia/processed_data/exports_product_model_IDN.csv") %>% 
  arrange(company_id, date, hs6) %>% 
  # Join variables that indicate if value of the variables product for letter_credit_use,
  # mean_remote_work_ISIC, relationship_stickiness and frac_lib_diff is above the median  
  left_join(hs_data, by = "hs6") %>% 
  # Join number of country partners by firm-month
  left_join(export_partner_country, by = c("company_id", "date"))%>% 
  mutate(date = as.Date(date))

# Data for model that measures if tech adoption helps to mitigate covid impacts ---- 

# Imports 
imports_data_mitig_IDN<- fread("../../Data/Indonesia/processed_data/imports_tech_mitigation_model_IDN.csv") %>% 
  # Join variables that indicate if value of the variables product for letter_credit_use,
  # mean_remote_work_ISIC, relationship_stickiness and frac_lib_diff is above the median  
  left_join(hs_data, by = "hs6") %>% 
  # Join number of country partners by firm-month
  left_join(import_partner_country, by = c("company_id", "date")) %>% 
  mutate(date = as.Date(date))


# Exports 
exports_data_mitig_IDN<- fread("../../Data/Indonesia/processed_data/exports_tech_mitigation_model_IDN.csv") %>% 
  # Join variables that indicate if value of the variables product for letter_credit_use,
  # mean_remote_work_ISIC, relationship_stickiness and frac_lib_diff is above the median  
  left_join(hs_data, by = "hs6") %>% 
  # Join number of country partners by firm-month
  left_join(export_partner_country, by = c("company_id", "date")) %>% 
  mutate(date = as.Date(date))



#rm(import_partner_country, export_partner_country)
gc()


# Tech data ----
tech_data <- read_parquet("../../Data/Indonesia/processed_data/tech_data_IDN.parquet", 
                              col_select = c("company_id", "date", "date_character", "pay_or_ecomnod_t_1",
                                             "pay_or_ecomnod_t_2", "pay_or_ecomnod_t_3", 
                                             "adopted_pay_or_ecom_before_2019"))


gc()

# Create extensive margin data ---- 

# Create extensive margin data for products model
exports_ext_marg_IDN<-data_extensive_margin(exports_data_IDN)
imports_ext_marg_IDN<-data_extensive_margin(imports_data_IDN)

# Create extensive margin data for tech covid mitigation model 
exports_ext_marg_mitig_IDN<-data_extensive_margin(exports_data_mitig_IDN, covid_string_index = TRUE)
imports_ext_marg_mitig_IDN<-data_extensive_margin(imports_data_mitig_IDN, covid_string_index = TRUE)

# Save extensive margin data ----
write_csv(imports_ext_marg_IDN, "../../Data/Indonesia/processed_data/imports_product_model_extmarg_IDN.csv")
write_csv(exports_ext_marg_IDN, "../../Data/Indonesia/processed_data/exports_product_model_extmarg_IDN.csv")
write_csv(imports_ext_marg_mitig_IDN, "../../Data/Indonesia/processed_data/imports_mitig_model_extmarg_IDN.csv")
write_csv(exports_ext_marg_mitig_IDN, "../../Data/Indonesia/processed_data/exports_mitig_model_extmarg_IDN.csv")


