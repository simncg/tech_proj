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

## Mexico ----

### Panjiva Exports for foreign firms ----

exports_f_data_MEX<-read_dta("../../Data/Mexico/raw_data/MEX_exports_Monthly_fd_locationmode_hs8.dta") %>%
  filter(!str_starts(hs8 , "27"), 
         country_iso!= "EUN") %>% 
  rename(foreign_country_panjiva = country_iso, foreign_company_id = foreign_id, domestic_company_id = domestic_firm_id) %>% 
  mutate(foreign_company_id = if_else(foreign_company_id == "", NA_character_, foreign_company_id), 
         foreign_country_panjiva = if_else(foreign_country_panjiva == "", NA_character_, foreign_country_panjiva), 
         date = as.Date(paste0(year, "-", month, "-01"))
  ) 


# Quality of foreign data statistics of exports ----
exp_quality_MEX<-quality_stats(exports_f_data_MEX, export)


rm(exports_f_data_MEX)
gc()

### Panjiva Imports for foreign firms ----
imports_f_data_MEX<-read_dta("../../Data/Mexico/raw_data/MEX_imports_Monthly_fd_locationmode_hs8.dta") %>% 
  filter(!str_starts(hs8 , "27"), 
         country_iso!= "EUN") %>% 
  rename(foreign_country_panjiva = country_iso, foreign_company_id = foreign_id, domestic_company_id = domestic_firm_id) %>% 
  mutate(foreign_company_id = if_else(foreign_company_id == "", NA_character_, foreign_company_id), 
         foreign_country_panjiva = if_else(foreign_country_panjiva == "", NA_character_, foreign_country_panjiva), 
         date = as.Date(paste0(year, "-", month, "-01"))
  ) 

# Quality of foreign data statistics of improts ----
imp_quality_MEX<-quality_stats(imports_f_data_MEX, import)


rm(imports_f_data_MEX)
gc()

# Save results
save(exp_quality_MEX, imp_quality_MEX, file = "../../Data/Mexico/processed_data/quality_foreign_data_MEX.RData")
