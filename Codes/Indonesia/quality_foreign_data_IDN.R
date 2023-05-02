#===========================================================================#
# Date:    March 2023                                                       #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
#                                                                           #
# This program creates different summary statistics to understand the       #
# quality of the foreign Panjiva data.                                      # 
#                                                                           #
#                                                                           #           -                                                                                                                                                  #
#===========================================================================#

# DO ALL THE ANALYSIS BY FIRM-YEAR. DO NOT DO ON THE ABERDEEN MATCHED DATA JUST IN THE COMPLETE PANJIVA DATA. 
# •	For Indian firms, what proportion of observation have a foreign firm not missing. 
# •	What proportion of total trade value (exports, imports( have the foreign firm not missing 
# •	For each Indian firm, compute the total number of observations that has in this dataset and count how many have a foreign firm and how many does not have a foreign firm. Summary statistics of this variable. 


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
library(arrow)
library(ggplot2)
library(kableExtra)

# Read functions for understanding quality of foreign data ----
source("../src/functions_quality_foreign_data.R")

# Read Data -----

## Indonesia ----

### Panjiva Exports for foreign firms ----

# This data is at the domestic firm-foreign firm-year-month-hs8 level
exports_f_data_IDN<-read_dta("../../Data/Indonesia/raw_data/IDN_Exports_Monthly_foreign_domestic_level_dropdup.dta") %>% 
  #lazy_dt() %>% 
  rename(foreign_country_panjiva = country) %>% 
  mutate(foreign_company_id = if_else(foreign_company_id == "", NA_character_, foreign_company_id), 
         foreign_country_panjiva = if_else(foreign_country_panjiva == "", NA_character_, foreign_country_panjiva), 
         hs6 = substr(hs, 1, 6),
         date = as.Date(paste0(year, "-", month, "-01"))) 

### Panjiva Imports for foreign firms ----

# This data is at the domestic firm-foreign firm-year-month-hs8 level
imports_f_data_IDN<-read_dta("../../Data/Indonesia/raw_data/IDN_Imports_Monthly_foreign_domestic_level_dropdup.dta") %>% 
  #lazy_dt() %>% 
  rename(foreign_country_panjiva = country) %>% 
  mutate(foreign_company_id = if_else(foreign_company_id == "", NA_character_, foreign_company_id), 
         foreign_country_panjiva = if_else(foreign_country_panjiva == "", NA_character_, foreign_country_panjiva), 
         hs6 = substr(hs, 1, 6),
         date = as.Date(paste0(year, "-", month, "-01"))) 


# Compute foreign-quality data ----
exp_quality_IDN<-quality_stats(exports_f_data_IDN, export)
imp_quality_IDN<-quality_stats(imports_f_data_IDN, import)

# Save results ----
save(exp_quality_IDN, imp_quality_IDN, file = "../../Data/Indonesia/processed_data/quality_foreign_data_IDN.RData")

