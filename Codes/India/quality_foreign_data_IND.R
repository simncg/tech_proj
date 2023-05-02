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

## India ----

### Panjiva Exports for foreign firms ----

exports_f_data_IND<-read_dta("../../Data/India/raw_data/exports_collapsed_fd_hs6_onlyvalue.dta") %>% 
  rename(foreign_country_panjiva = foreign_country_cleaned_iso3, foreign_company_id = foreign_id, domestic_company_id = domestic_id) %>% 
  mutate(foreign_company_id = if_else(foreign_company_id == "", NA_character_, foreign_company_id), 
         foreign_country_panjiva = if_else(foreign_country_panjiva == "", NA_character_, foreign_country_panjiva), 
         date = as.Date(paste0(year, "-", month, "-01"))
  ) 

### Panjiva Imports for foreign firms ----
imports_f_data_IND<-read_dta("../../Data/India/raw_data/imports_collapsed_fd_hs6_onlyvalue.dta") %>% 
  rename(foreign_country_panjiva = foreign_country_cleaned_iso3, foreign_company_id = foreign_id, domestic_company_id = domestic_id) %>% 
  mutate(foreign_company_id = if_else(foreign_company_id == "", NA_character_, foreign_company_id), 
         foreign_country_panjiva = if_else(foreign_country_panjiva == "", NA_character_, foreign_country_panjiva), 
         date = as.Date(paste0(year, "-", month, "-01"))
  ) 


# Quality of foreign data statistics ----
exp_quality_IND<-quality_stats(exports_f_data_IND, export)
imp_quality_IND<-quality_stats(imports_f_data_IND, import)

# Save results
save(exp_quality_IND, imp_quality_IND, file = "../../Data/India/processed_data/quality_foreign_data_IND.RData")
