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
  #as_tibble()

### Panjiva Imports for foreign firms ----

# This data is at the domestic firm-foreign firm-year-month-hs8 level
imports_f_data_IDN<-read_dta("../../Data/Indonesia/raw_data/IDN_Imports_Monthly_foreign_domestic_level_dropdup.dta") %>% 
  #lazy_dt() %>% 
  rename(foreign_country_panjiva = country) %>% 
  mutate(foreign_company_id = if_else(foreign_company_id == "", NA_character_, foreign_company_id), 
         foreign_country_panjiva = if_else(foreign_country_panjiva == "", NA_character_, foreign_country_panjiva), 
         hs6 = substr(hs, 1, 6),
         date = as.Date(paste0(year, "-", month, "-01"))) 


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

# Summary statistics for measuring quality of foreign data -----

## Indonesia ----
exp_quality_IDN<-quality_stats(exports_f_data_IDN, export)
imp_quality_IDN<-quality_stats(imports_f_data_IDN, import)

## India ----
exp_quality_IND<-quality_stats(exports_f_data_IND, export)
imp_quality_IND<-quality_stats(imports_f_data_IND, import)


# Percentage of Firms with Complete Foreign Partners Data, Grouped by Year ----

## Indonesia ----
exp_quality_IDN %>% 
  filter(foreign_firm_missing == 0) %>% 
  group_by(year) %>%
  summarize(total_firms = n(),
            firms_complete = sum(prop_obs == 1, na.rm = TRUE),
            prop_complete = firms_complete /total_firms) %>% 
  select(Year = year, Exports = prop_complete) %>% 
  left_join(imp_quality_IDN %>% 
              filter(foreign_firm_missing == 0) %>% 
              group_by(year) %>%
              summarize(total_firms = n(),
                        firms_complete = sum(prop_obs == 1, na.rm = TRUE),
                        prop_complete = firms_complete /total_firms) %>% 
              select(Year = year, Imports = prop_complete),
            by="Year") %>% 
  kable(caption = "Indonesia - Proportion of Firms with Non-Missing Foreign Partners Data by Year", 
        booktabs = T, 
        col.names = c("Year", "Exports Dataset", "Imports Dataset")) %>% 
  add_header_above(c(" " = 1, "Proportion of Firms" = 2)) %>% 
  kable_styling(latex_options = c("HOLD_position")) 




exp_quality_IDN %>% 
  filter(foreign_firm_missing == 0) %>% 
  group_by(year) %>%
  summarize(total_firms = n(),
            firms_complete = sum(prop_obs == 1, na.rm = TRUE),
            prop_complete = firms_complete /total_firms) %>% 
  select(Year = year, Exports = prop_complete) %>% 
  left_join(imp_quality_IDN %>% 
              filter(foreign_firm_missing == 0) %>% 
              group_by(year) %>%
              summarize(total_firms = n(),
                        firms_complete = sum(prop_obs == 1, na.rm = TRUE),
                        prop_complete = firms_complete /total_firms) %>% 
              select(Year = year, Imports = prop_complete),
            by="Year") %>% 
  pivot_longer(cols = c(Exports, Imports), 
               names_to = "data", 
               values_to = "proportion") %>% 
  ggplot()+
  geom_col(aes(x = Year, y = proportion, fill = as.factor(Year)), alpha = 0.75)+
  facet_wrap(~data)+
  ggtitle("Proportion of Firms with Non-Missing Foreign Partners Data by Year", 
          subtitle = "Indonesia")+
  ylab("Proportion")+
  scale_fill_economist()+
  theme_minimal()+
  theme(legend.position  = "None", 
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic"))
  
  
  



## India ----
exp_quality_IND %>% 
  filter(foreign_firm_missing == 0) %>% 
  group_by(year) %>%
  summarize(total_firms = n(),
            firms_complete = sum(prop_obs == 1, na.rm = TRUE),
            prop_complete = firms_complete /total_firms) %>% 
  select(Year = year, Exports = prop_complete) %>% 
  left_join(imp_quality_IND %>% 
              filter(foreign_firm_missing == 0) %>% 
              group_by(year) %>%
              summarize(total_firms = n(),
                        firms_complete = sum(prop_obs == 1, na.rm = TRUE),
                        prop_complete = firms_complete /total_firms) %>% 
              select(Year = year, Imports = prop_complete),
            by="Year") %>% 
  kable(caption = "India - Proportion of Firms with Non-Missing Foreign Partners Data by Year", 
        booktabs = T, 
        col.names = c("Year", "Exports Dataset", "Imports Dataset")) %>% 
  add_header_above(c(" " = 1, "Proportion of Firms" = 2)) %>% 
  kable_styling(latex_options = c("HOLD_position")) 



# Summary Statistics of other quality data variables ----

## Indonesia ----
exp_quality_IDN %>% 
  filter(foreign_firm_missing == 0) %>% 
  group_by(year) %>% 
  # Summary Statistics of the Variable Measuring the Proportion of Observations by Firm-Year with Non-Missing Foreign Partner
  summarize(Mean = mean(prop_obs),
            Min = min(prop_obs), 
            p5 = quantile(prop_trade_volume, 0.05),
            p25 = quantile(prop_obs, 0.25), 
            Median = median(prop_obs), 
            p75 = quantile(prop_obs, 0.75), 
            Max = max(prop_obs)) %>% 
  # Summary Statistics of the Variable Measuring the Proportion of Trade Volume by Firm-Year with Non-Missing Foreign Partner
  bind_rows(exp_quality_IDN %>% 
              filter(foreign_firm_missing == 0) %>% 
              group_by(year) %>% 
              summarize(Mean = mean(prop_trade_volume),
                        Min = min(prop_trade_volume), 
                        p5 = quantile(prop_trade_volume, 0.05),
                        p25 = quantile(prop_trade_volume, 0.25), 
                        Median = median(prop_trade_volume), 
                        p75 = quantile(prop_trade_volume, 0.75), 
                        Max = max(prop_trade_volume))
              ) %>% 
  # Round values 
  mutate(across(everything(), ~ round(., 4))) %>% 
  # Rename year as Year
  rename(Year = year) %>% 
  # Create Table
  kable(caption = "Indonesian Foreign Partners Panjiva Exports - Data Quality Summary Statistics", 
        booktabs = T) %>% 
  pack_rows("Proportion of Firm Observations with Non-Missing Foreign Partners by Year", 1, 3) %>%
  pack_rows("Proportion of Firm Total Exports with Non-Missing Foreign Partners by Year", 4, 6) %>% 
  kable_styling(latex_options = c("HOLD_position"))



exp_quality_IDN %>% 
  filter(foreign_firm_missing == 0) %>% 
  group_by(year) %>% 
  # Summary Statistics of the Variable Measuring the Proportion of Observations by Firm-Year with Non-Missing Foreign Partner
  summarize(Mean = mean(prop_obs),
            Min = min(prop_obs), 
            p5 = quantile(prop_trade_volume, 0.05),
            p25 = quantile(prop_obs, 0.25), 
            Median = median(prop_obs), 
            p75 = quantile(prop_obs, 0.75), 
            Max = max(prop_obs)) %>% 
  # Round values 
  mutate(across(everything(), ~ round(., 4))) %>% 
  # Rename year as Year
  rename(Year = year) %>% 
  # Create Table
  kable(caption = "Panjiva Exports - Summary Statistics of Proportion of Firm Observations with Non-Missing Foreign Partners by Year", 
        booktabs = T) %>% 
  #pack_rows("Proportion of Firm Observations with Non-Missing Foreign Partners by Year", 1, 3) %>%
  #pack_rows("Proportion of Firm Total Exports with Non-Missing Foreign Partners by Year", 4, 6) %>% 
  kable_styling(latex_options = c("HOLD_position"))




exp_quality_IDN %>% 
  filter(foreign_firm_missing == 0) %>% 
  group_by(year) %>% 
  summarize(Mean = mean(prop_trade_volume),
            Min = min(prop_trade_volume), 
            p5 = quantile(prop_trade_volume, 0.05),
            p25 = quantile(prop_trade_volume, 0.25), 
            Median = median(prop_trade_volume), 
            p75 = quantile(prop_trade_volume, 0.75), 
            Max = max(prop_trade_volume)) %>% 
  # Round values 
  mutate(across(everything(), ~ round(., 4))) %>% 
  # Rename year as Year
  rename(Year = year) %>% 
  # Create Table
  kable(caption = "Panjiva Exports - Proportion of Firm Total Exports with Non-Missing Foreign Partners by Year", 
        booktabs = T) %>% 
  kable_styling(latex_options = c("HOLD_position"))

  



# Summary Statistics of the Variable Measuring the Proportion of Observations by Firm-Year with Non-Missing Foreign Partner
imp_quality_IDN %>% 
  filter(foreign_firm_missing == 0) %>% 
  group_by(year) %>% 
  summarize(Mean = mean(prop_obs),
            Min = min(prop_obs), 
            p5 = quantile(prop_trade_volume, 0.05),
            p25 = quantile(prop_obs, 0.25), 
            Median = median(prop_obs), 
            p75 = quantile(prop_obs, 0.75), 
            Max = max(prop_obs)) %>% 
  # Summary Statistics of the Variable Measuring the Proportion of Trade Volume by Firm-Year with Non-Missing Foreign Partner
  bind_rows(imp_quality_IDN %>% 
              filter(foreign_firm_missing == 0) %>% 
              group_by(year) %>% 
              summarize(Mean = mean(prop_trade_volume),
                        Min = min(prop_trade_volume), 
                        p5 = quantile(prop_trade_volume, 0.05),
                        p25 = quantile(prop_trade_volume, 0.25), 
                        Median = median(prop_trade_volume), 
                        p75 = quantile(prop_trade_volume, 0.75), 
                        Max = max(prop_trade_volume))
  ) %>% 
  # Round values 
  mutate(across(everything(), ~ round(., 4))) %>% 
  # Rename year as Year
  rename(Year = year) %>% 
  # Create Table
  kable(caption = "Indonesian Foreign Partners Panjiva Imports - Data Quality Summary Statistics", 
        booktabs = T) %>% 
  pack_rows("Proportion of Firm Observations with Non-Missing Foreign Partners by Year", 1, 3) %>%
  pack_rows("Proportion of Firm Total Imports with Non-Missing Foreign Partners by Year", 4, 6) %>% 
  kable_styling(latex_options = c("HOLD_position"))



## India ----
exp_quality_IND %>% 
  filter(foreign_firm_missing == 0) %>% 
  group_by(year) %>% 
  # Summary Statistics of the Variable Measuring the Proportion of Observations by Firm-Year with Non-Missing Foreign Partner
  summarize(Mean = mean(prop_obs),
            Min = min(prop_obs), 
            p5 = quantile(prop_trade_volume, 0.05),
            p25 = quantile(prop_obs, 0.25), 
            Median = median(prop_obs), 
            p75 = quantile(prop_obs, 0.75), 
            Max = max(prop_obs)) %>% 
  # Summary Statistics of the Variable Measuring the Proportion of Trade Volume by Firm-Year with Non-Missing Foreign Partner
  bind_rows(exp_quality_IND %>% 
              filter(foreign_firm_missing == 0) %>% 
              group_by(year) %>% 
              summarize(Mean = mean(prop_trade_volume),
                        Min = min(prop_trade_volume), 
                        p5 = quantile(prop_trade_volume, 0.05),
                        p25 = quantile(prop_trade_volume, 0.25), 
                        Median = median(prop_trade_volume), 
                        p75 = quantile(prop_trade_volume, 0.75), 
                        Max = max(prop_trade_volume))
  ) %>% 
  # Round values 
  mutate(across(everything(), ~ round(., 3))) %>% 
  # Rename year as Year
  rename(Year = year) %>% 
  # Create Table
  kable(caption = "Indian Foreign Partners Panjiva Exports - Data Quality Summary Statistics", 
        booktabs = T) %>% 
  pack_rows("Proportion of Firm Observations with Non-Missing Foreign Partners by Year", 1, 3) %>%
  pack_rows("Proportion of Firm Total Exports with Non-Missing Foreign Partners by Year", 4, 6) %>% 
  kable_styling(latex_options = c("HOLD_position"))



exp_quality_IND %>% 
  filter(foreign_firm_missing == 0) %>% 
  group_by(year) %>%
  summarize(total_firms = n(),
            firms_complete = sum(prop_obs == 1, na.rm = TRUE),
            prop_complete = firms_complete /total_firms) %>% 
  select(Year = year, Exports = prop_complete) %>% 
  left_join(imp_quality_IND %>% 
              filter(foreign_firm_missing == 0) %>% 
              group_by(year) %>%
              summarize(total_firms = n(),
                        firms_complete = sum(prop_obs == 1, na.rm = TRUE),
                        prop_complete = firms_complete /total_firms) %>% 
              select(Year = year, Imports = prop_complete),
            by="Year") %>% 
  pivot_longer(cols = c(Exports, Imports), 
               names_to = "data", 
               values_to = "proportion") %>% 
  ggplot()+
  geom_col(aes(x = Year, y = proportion, fill = as.factor(Year)), alpha = 0.75)+
  facet_wrap(~data)+
  ggtitle("Proportion of Firms with Non-Missing Foreign Partners Data by Year", 
          subtitle = "India")+
  ylab("Proportion")+
  scale_fill_economist()+
  theme_minimal()+
  theme(legend.position  = "None", 
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic"))+
  scale_x_continuous(n.breaks = 7)







# Summary Statistics of the Variable Measuring the Proportion of Observations by Firm-Year with Non-Missing Foreign Partner
imp_quality_IND %>% 
  filter(foreign_firm_missing == 0) %>% 
  group_by(year) %>% 
  summarize(Mean = mean(prop_obs),
            Min = min(prop_obs), 
            p5 = quantile(prop_trade_volume, 0.05),
            p25 = quantile(prop_obs, 0.25), 
            Median = median(prop_obs), 
            p75 = quantile(prop_obs, 0.75), 
            Max = max(prop_obs)) %>% 
  # Summary Statistics of the Variable Measuring the Proportion of Trade Volume by Firm-Year with Non-Missing Foreign Partner
  bind_rows(imp_quality_IND %>% 
              filter(foreign_firm_missing == 0) %>% 
              group_by(year) %>% 
              summarize(Mean = mean(prop_trade_volume),
                        Min = min(prop_trade_volume), 
                        p5 = quantile(prop_trade_volume, 0.05),
                        p25 = quantile(prop_trade_volume, 0.25), 
                        Median = median(prop_trade_volume), 
                        p75 = quantile(prop_trade_volume, 0.75), 
                        Max = max(prop_trade_volume))
  ) %>% 
  # Round values 
  mutate(across(everything(), ~ round(., 4))) %>% 
  # Rename year as Year
  rename(Year = year) %>% 
  # Create Table
  kable(caption = "Indian Foreign Partners Panjiva Imports - Data Quality Summary Statistics", 
        booktabs = T, 
        digits = 3) %>% 
  pack_rows("Proportion of Firm Observations with Non-Missing Foreign Partners by Year", 1, 7) %>%
  pack_rows("Proportion of Firm Total Imports with Non-Missing Foreign Partners by Year", 8, 14) %>% 
  kable_styling(latex_options = c("HOLD_position"))





















exp_quality_IDN %>% 
  filter(foreign_firm_missing == 0) %>% 
  group_by(year) %>% 
  # Summary Statistics of the Variable Measuring the Proportion of Observations by Firm-Year with Non-Missing Foreign Partner
  summarize(Mean = mean(prop_obs),
            Min = min(prop_obs), 
            p5 = quantile(prop_trade_volume, 0.05),
            p25 = quantile(prop_obs, 0.25), 
            Median = median(prop_obs), 
            p75 = quantile(prop_obs, 0.75), 
            Max = max(prop_obs)) %>% 
  # Round values 
  mutate(across(everything(), ~ round(., 3))) %>% 
  column_to_rownames("year") %>% 
  # Create Table
  kable(caption = "Panjiva Exports Indonesia - Summary Statistics of Proportion of Firm Observations with Non-Missing Foreign Partners", 
        booktabs = T, linesep = "", 
        align = c("l",rep("c", 7))) %>% 
  kable_styling(latex_options = c("HOLD_position"))

