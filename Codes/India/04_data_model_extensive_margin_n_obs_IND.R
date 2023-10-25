#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# This script generates descriptive statistics of the number of HS6 products#
# per HS4 code and estimates the number of expected observations if the     #
# expansion of the dataset for the extensive margin is based on the HS4 code#
# approach: blown-up dataset where the data will be at the HS6-firm-month or#
# quarter level, but we won't use every HS6 possible, just a set of feasible#
# HS6 products for each firm based on the HS4 code                          #
#                                                                           #
#                                                                           #                                                                                                                                                  #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")


# Read HS6 products information ---- 
hs_data <- read_csv("../../Data/Extra Data/HS_code_classifications.csv")%>% 
  mutate(hs_2017  = str_pad(hs_2017  , 6, "left", "0")) %>% 
  rename(hs6 = hs_2017) %>% 
  mutate(hs4 =  substr(hs6, 1, 4)) %>% 
  relocate(hs6, hs4)

# Count the number of HS6 products for each HS4. 
hs4_n_prods<-hs_data %>% 
  count(hs4) %>% 
  rename(number_hs6 = n)


# Data for model that measures if tech adoption affects trade outcomes ---- 

## Exports data for model that measures if tech adoption affects trade outcomes (after processed in gen_data_model_products_IND.R) ----
exports_data_IND<- fread("../../Data/India/processed_data/exports_product_model_IND.csv") %>% 
  arrange(company_id, date, hs6) %>% 
  mutate(hs6  = str_pad(hs6, 6, "left", "0"), 
         hs4 = substr(hs6, 1, 4)) %>% 
  relocate(date, hs6, hs4, company_id) %>% 
  # Convert date variable to date type
  mutate(date = as.Date(date))

# Create a dataset at the firm-HS4 level 
exp_firm_hs4<-exports_data_IND %>% 
  group_by(company_id, hs4) %>% 
  summarize(exports = sum(export)) %>% 
  ungroup() %>% 
  # Join with data with number of HS6 products by HS4
  left_join(hs4_n_prods, by = "hs4") %>% 
  # Number of distinct HS4 products exported by the firm
  group_by(company_id) %>% 
  mutate(n_hs4 = n()) %>% 
  ungroup() %>% 
         # Possible number of observations in the blown-up dataset if we use months 
  # The process to identify the number of observations that we will have in this export (import) dataset is the following: First, we count the number of distinct HS6 products by HS4 code to have a dataset at the HS4 level- *Table 1*. Second, using the regression sample^[firms that didn't have the technology in 2019 for the model that measures if tech adoption affects trade outcomes and keeping only period of analysis.], we collapse the export or import data by HS4 (summing trade values across time) to have a dataset at the firm-HS4 level - *Table 2*. We then join the *Table 2* and *Table 1* to have a dataset at the firm-hs4 level with columns *Firm*, *HS4*, *Number of HS6 products in the HS4*, *Total value of exports of the HS4*. Third, in this dataset at the firm-HS4 level, we multiply the number of HS6 products in the HS4 by the number of months (quarters) and we sum this value across all rows. 
  mutate(# Number of observation for each firm-month-feasible hs6
         obs_firm_month_feas_hs6 = number_hs6*length(unique(exports_data_IND$date)), 
         # Total number of observation in the blown-up-dataset at the firm-month-feasible HS6
         total_obs_firm_month_feas_hs6 = sum(obs_firm_month_feas_hs6, na.rm = T), 
         # Possible number of observations in the blown-up dataset if we use quarters 
         # Number of observation for each firm-quarter-feasible hs6
         obs_firm_quarter_feas_hs6 =  number_hs6*length(unique(zoo::as.yearqtr(exports_data_IND$date))), 
         # Total number of observation in the blown-up-dataset at the firm-quarter-feasible HS6
         total_obs_firm_quarter_feas_hs6 = sum(obs_firm_quarter_feas_hs6, na.rm = T)
         ) %>% 
  # Percentage of total exports corresponding of a specific HS4
  group_by(company_id) %>% 
  mutate(perc_exp_hs4 = (exports/sum(exports))*100, 
         # Exports value corresponds to the same HS4 90% of the time?
         same_hs4_90perc = max(perc_exp_hs4>90), 
         same_hs4_80perc = max(perc_exp_hs4>80), 
         same_hs4_70perc = max(perc_exp_hs4>70), 
         ) %>% 
  ungroup()

# Descriptive statistics of the number of HS4 codes exported by each firm 
stats_n_hs4_exp<-
  exp_firm_hs4 %>% 
  select(company_id, n_hs4) %>% 
  distinct(company_id, .keep_all = T) %>% 
  summarize(Mean = round(mean(n_hs4), 2),
            Min = min(n_hs4), 
            p25 = quantile(n_hs4, probs = .25), 
            Median = median(n_hs4), 
            p75 = quantile(n_hs4, probs = .75), 
            Max = max(n_hs4)
  ) %>% 
  mutate(Data = 'Exports')


## Imports data for model that measures if tech adoption affects trade outcomes (after processed in gen_data_model_products_IND.R) ----
imports_data_IND<- fread("../../Data/India/processed_data/imports_product_model_IND.csv") %>% 
  arrange(company_id, date, hs6) %>% 
  mutate(hs6  = str_pad(hs6, 6, "left", "0"), 
         hs4 = substr(hs6, 1, 4)) %>% 
  relocate(date, hs6, hs4, company_id) %>% 
  # Convert date variable to date type
  mutate(date = as.Date(date))

# Create a dataset at the firm-HS4 level 
imp_firm_hs4<-imports_data_IND %>% 
  group_by(company_id, hs4) %>% 
  summarize(imports = sum(import)) %>% 
  ungroup() %>% 
  # Join with data with number of HS6 products by HS4
  left_join(hs4_n_prods, by = "hs4") %>% 
  # Number of distinct HS4 products exported by the firm
  group_by(company_id) %>% 
  mutate(n_hs4 = n()) %>% 
  ungroup() %>% 
  # Possible number of observations in the blown-up dataset if we use months 
  # The process to identify the number of observations that we will have in this export (import) dataset is the following: First, we count the number of distinct HS6 products by HS4 code to have a dataset at the HS4 level- *Table 1*. Second, using the regression sample^[firms that didn't have the technology in 2019 for the model that measures if tech adoption affects trade outcomes, firms in the SIC industries analyzed, firms that had website before period of analysis and are indexed after period of analysis and keeping only period of analysis.], we collapse the export or import data by HS4 (summing trade values across time) to have a dataset at the firm-HS4 level - *Table 2*. We then join the *Table 2* and *Table 1* to have a dataset at the firm-hs4 level with columns *Firm*, *HS4*, *Number of HS6 products in the HS4*, *Total value of exports of the HS4*. Third, in this dataset at the firm-HS4 level, we multiply the number of HS6 products in the HS4 by the number of months (quarters) and we sum this value across all rows. 
  mutate(# Number of observation for each firm-month-feasible hs6
    obs_firm_month_feas_hs6 = number_hs6*length(unique(imports_data_IND$date)), 
    # Total number of observation in the blown-up-dataset at the firm-month-feasible HS6
    total_obs_firm_month_feas_hs6 = sum(obs_firm_month_feas_hs6, na.rm = T), 
    # Possible number of observations in the blown-up dataset if we use quarters 
    # Number of observation for each firm-quarter-feasible hs6
    obs_firm_quarter_feas_hs6 =  number_hs6*length(unique(zoo::as.yearqtr(exports_data_IND$date))), 
    # Total number of observation in the blown-up-dataset at the firm-quarter-feasible HS6
    total_obs_firm_quarter_feas_hs6 = sum(obs_firm_quarter_feas_hs6, na.rm = T)
  ) %>% 
  # Percentage of total exports corresponding of a specific HS4
  group_by(company_id) %>% 
  mutate(perc_imp_hs4 = (imports/sum(imports))*100, 
         # Exports value corresponds to the same HS4 90% of the time?
         same_hs4_90perc = max(perc_imp_hs4>90), 
         same_hs4_80perc = max(perc_imp_hs4>80), 
         same_hs4_70perc = max(perc_imp_hs4>70), 
  ) %>% 
  ungroup()

# Descriptive statistics of the number of HS4 codes imported by each firm
stats_n_hs4_imp<-
  imp_firm_hs4 %>% 
  select(company_id, n_hs4) %>% 
  distinct(company_id, .keep_all = T) %>% 
  summarize(Mean = round(mean(n_hs4), 2),
            Min = min(n_hs4), 
            p25 = quantile(n_hs4, probs = .25), 
            Median = median(n_hs4), 
            p75 = quantile(n_hs4, probs = .75), 
            Max = max(n_hs4)
  ) %>% 
  mutate(Data = 'Imports')



## Percentage of firms for which x% of their total exports correspond to the same HS4 ----

# In terms of value of exports, are the firms exporting 90%, 80% and 70% of their total exports the same HS4?

perc_exp_HS4<-exp_firm_hs4 %>% 
  select(company_id, same_hs4_90perc, same_hs4_80perc, same_hs4_70perc) %>% 
  distinct(company_id, .keep_all = T) %>% 
  mutate(across(-company_id, ~ 100*(sum(.)/n()))) %>% 
  select(-company_id) %>% 
  head(1) %>% 
  mutate(Data = 'Exports') %>% 
  relocate(Data)

## Percentage of firms for which x% of their total imports correspond to the same HS4 ----

# In terms of value of imports, are the firms importing 90%, 80% and 70% of their total imports the same HS4?

perc_imp_HS4<-imp_firm_hs4 %>% 
  select(company_id, same_hs4_90perc, same_hs4_80perc, same_hs4_70perc) %>% 
  distinct(company_id, .keep_all = T) %>% 
  mutate(across(-company_id, ~ 100*(sum(.)/n()))) %>% 
  select(-company_id) %>% 
  head(1) %>% 
  mutate(Data = 'Imports') %>% 
  relocate(Data)

## Create table with number of observations in the expanded dataset (Does tech adoption affect trade outcomes) -----

# Using HS4
table1<-exp_firm_hs4 %>% 
  select(total_obs_firm_month_feas_hs6, total_obs_firm_quarter_feas_hs6) %>% 
  head(1) %>% 
  mutate(Data = "Exports") %>% 
  relocate(Data) %>% 
  # Join imports data
  bind_rows(
    imp_firm_hs4 %>% 
      select(total_obs_firm_month_feas_hs6, total_obs_firm_quarter_feas_hs6) %>% 
      head(1) %>% 
      mutate(Data = "Imports") %>% 
      relocate(Data) 
  )


# Data for model that measures if tech adoption mitigates COVID impacts ---- 
## Exports data for model that measures if tech adoption mitigates COVID impacts (after processed in gen_data_model_products_IND.R) ----
exports_data_mitig_IND<- fread("../../Data/India/processed_data/exports_tech_mitigation_model_IND.csv") %>% 
  arrange(company_id, date, hs6) %>% 
  mutate(hs6  = str_pad(hs6, 6, "left", "0"), 
         hs4 = substr(hs6, 1, 4)) %>% 
  relocate(date, hs6, hs4, company_id) %>% 
  # Convert date variable to date type
  mutate(date = as.Date(date))

# Create a dataset at the firm-HS4 level 
exp_firm_hs4_mitig<-exports_data_mitig_IND %>% 
  group_by(company_id, hs4) %>% 
  summarize(exports = sum(export)) %>% 
  ungroup() %>% 
  # Join with data with number of HS6 products by HS4
  left_join(hs4_n_prods, by = "hs4") %>% 
  # Number of distinct HS4 products exported by the firm
  group_by(company_id) %>% 
  mutate(n_hs4 = n()) %>% 
  ungroup() %>% 
  # Possible number of observations in the blown-up dataset if we use months 
  # The process to identify the number of observations that we will have in this export (import) dataset is the following: First, we count the number of distinct HS6 products by HS4 code to have a dataset at the HS4 level- *Table 1*. Second, using the regression sample^[, we collapse the export or import data by HS4 (summing trade values across time) to have a dataset at the firm-HS4 level - *Table 2*. We then join the *Table 2* and *Table 1* to have a dataset at the firm-hs4 level with columns *Firm*, *HS4*, *Number of HS6 products in the HS4*, *Total value of exports of the HS4*. Third, in this dataset at the firm-HS4 level, we multiply the number of HS6 products in the HS4 by the number of months (quarters) and we sum this value across all rows. 
  mutate(# Number of observation for each firm-month-feasible hs6
    obs_firm_month_feas_hs6 = number_hs6*length(unique(exports_data_mitig_IND$date)),
    # Total number of observation in the blown-up-dataset at the firm-month-feasible HS6
    total_obs_firm_month_feas_hs6 = sum(obs_firm_month_feas_hs6, na.rm = T), 
    # Possible number of observations in the blown-up dataset if we use quarters 
    # Number of observation for each firm-quarter-feasible hs6
    obs_firm_quarter_feas_hs6 =  number_hs6*length(unique(zoo::as.yearqtr(exports_data_mitig_IND$date))), # There are 8 quarters from 2020-01-01 to 2021-12-31 (COVID period)
    # Total number of observation in the blown-up-dataset at the firm-quarter-feasible HS6
    total_obs_firm_quarter_feas_hs6 = sum(obs_firm_quarter_feas_hs6, na.rm = T)
  ) %>% 
  # Percentage of total exports corresponding of a specific HS4
  group_by(company_id) %>% 
  mutate(perc_exp_hs4 = (exports/sum(exports))*100, 
         # Exports value corresponds to the same HS4 90% of the time?
         same_hs4_90perc = max(perc_exp_hs4>90), 
         same_hs4_80perc = max(perc_exp_hs4>80), 
         same_hs4_70perc = max(perc_exp_hs4>70), 
  ) %>% 
  ungroup()


stats_n_hs4_exp_mitig<-exp_firm_hs4_mitig %>% 
  select(company_id, n_hs4) %>% 
  distinct(company_id, .keep_all = T) %>% 
  summarize(Mean = round(mean(n_hs4), 2),
            Min = min(n_hs4), 
            p25 = quantile(n_hs4, probs = .25), 
            Median = median(n_hs4), 
            p75 = quantile(n_hs4, probs = .75), 
            Max = max(n_hs4)
  ) %>% 
  mutate(Data = 'Exports')



## Imports data for model that measures if tech adoption mitigates COVID impacts (after processed in gen_data_model_products_IND.R) ----
imports_data_mitig_IND<- fread("../../Data/India/processed_data/imports_tech_mitigation_model_IND.csv") %>% 
  arrange(company_id, date, hs6) %>% 
  mutate(hs6  = str_pad(hs6, 6, "left", "0"), 
         hs4 = substr(hs6, 1, 4)) %>% 
  relocate(date, hs6, hs4, company_id) %>% 
  # Convert date variable to date type
  mutate(date = as.Date(date))

# Create a dataset at the firm-HS4 level 
imp_firm_hs4_mitig<-imports_data_mitig_IND %>% 
  group_by(company_id, hs4) %>% 
  summarize(imports = sum(import)) %>% 
  ungroup() %>% 
  # Join with data with number of HS6 products by HS4
  left_join(hs4_n_prods, by = "hs4") %>% 
  # Number of distinct HS4 products exported by the firm
  group_by(company_id) %>% 
  mutate(n_hs4 = n()) %>% 
  ungroup() %>% 
  # Possible number of observations in the blown-up dataset if we use months 
  # The process to identify the number of observations that we will have in this export (import) dataset is the following: First, we count the number of distinct HS6 products by HS4 code to have a dataset at the HS4 level- *Table 1*. Second, using the regression sample^[firms that didn't have the technology in 2019 for the model that measures if tech adoption affects trade outcomes, firms in the SIC industries analyzed, firms that had website before period of analysis and are indexed after period of analysis and keeping only period of analysis.], we collapse the export or import data by HS4 (summing trade values across time) to have a dataset at the firm-HS4 level - *Table 2*. We then join the *Table 2* and *Table 1* to have a dataset at the firm-hs4 level with columns *Firm*, *HS4*, *Number of HS6 products in the HS4*, *Total value of exports of the HS4*. Third, in this dataset at the firm-HS4 level, we multiply the number of HS6 products in the HS4 by the number of months (quarters) and we sum this value across all rows. 
  mutate(# Number of observation for each firm-month-feasible hs6
    obs_firm_month_feas_hs6 = number_hs6*length(unique(imports_data_mitig_IND$date)), 
    # Total number of observation in the blown-up-dataset at the firm-month-feasible HS6
    total_obs_firm_month_feas_hs6 = sum(obs_firm_month_feas_hs6, na.rm = T), 
    # Possible number of observations in the blown-up dataset if we use quarters 
    # Number of observation for each firm-quarter-feasible hs6
    obs_firm_quarter_feas_hs6 =  number_hs6*length(unique(zoo::as.yearqtr(imports_data_mitig_IND$date))), 
    # Total number of observation in the blown-up-dataset at the firm-quarter-feasible HS6
    total_obs_firm_quarter_feas_hs6 = sum(obs_firm_quarter_feas_hs6, na.rm = T)
  ) %>% 
  # Percentage of total exports corresponding of a specific HS4
  group_by(company_id) %>% 
  mutate(perc_imp_hs4 = (imports/sum(imports))*100, 
         # Exports value corresponds to the same HS4 90% of the time?
         same_hs4_90perc = max(perc_imp_hs4>90), 
         same_hs4_80perc = max(perc_imp_hs4>80), 
         same_hs4_70perc = max(perc_imp_hs4>70), 
  ) %>% 
  ungroup()


stats_n_hs4_imp_mitig<-imp_firm_hs4_mitig %>% 
  select(company_id, n_hs4) %>% 
  distinct(company_id, .keep_all = T) %>% 
  summarize(Mean = round(mean(n_hs4), 2),
            Min = min(n_hs4), 
            p25 = quantile(n_hs4, probs = .25), 
            Median = median(n_hs4), 
            p75 = quantile(n_hs4, probs = .75), 
            Max = max(n_hs4)
  ) %>% 
  mutate(Data = 'Imports')





## Percentage of firms for which x% of their total exports correspond to the same HS4 ----

# In terms of value of exports, are the firms exporting 90%, 80% and 70% of their total exports the same HS4?

perc_exp_HS4_mitig<-exp_firm_hs4_mitig %>% 
  select(company_id, same_hs4_90perc, same_hs4_80perc, same_hs4_70perc) %>% 
  distinct(company_id, .keep_all = T) %>% 
  mutate(across(-company_id, ~ 100*(sum(.)/n()))) %>% 
  select(-company_id) %>% 
  head(1) %>% 
  mutate(Data = 'Exports') %>% 
  relocate(Data)

## Percentage of firms for which x% of their total imports correspond to the same HS4 ----

# In terms of value of imports, are the firms importing 90%, 80% and 70% of their total imports the same HS4?

perc_imp_HS4_mitig<-imp_firm_hs4_mitig %>% 
  select(company_id, same_hs4_90perc, same_hs4_80perc, same_hs4_70perc) %>% 
  distinct(company_id, .keep_all = T) %>% 
  mutate(across(-company_id, ~ 100*(sum(.)/n()))) %>% 
  select(-company_id) %>% 
  head(1) %>% 
  mutate(Data = 'Imports') %>% 
  relocate(Data)

## Create table with number of observations in the expanded dataset (COVID mitigation model) -----
table1_mitig<-exp_firm_hs4_mitig %>% 
  select(total_obs_firm_month_feas_hs6, total_obs_firm_quarter_feas_hs6) %>% 
  head(1) %>% 
  mutate(Data = "Exports") %>% 
  relocate(Data) %>% 
  # Join imports data
  bind_rows(
    imp_firm_hs4_mitig %>% 
      select(total_obs_firm_month_feas_hs6, total_obs_firm_quarter_feas_hs6) %>% 
      head(1) %>% 
      mutate(Data = "Imports") %>% 
      relocate(Data) 
  )








