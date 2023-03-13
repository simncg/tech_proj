#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  sic_groups_distributions.R                                       #
#                                                                           #                 
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


# Load matched Aberdeen to Panjiva data ----
matched_data <- read_csv("../../Data/India/matched_Aberdeen_to_Panjiva_data_India.csv") %>%
  rename(company_id = our_ID)

# Imports data ----

# Firms that import less than 1000 usd in each one of the years (2019, 2020 and 2021) 
import_less_than_1000usd<-read_csv("../../Data/India/processed_data/import_firms_trade_less_than_1000usd.csv")

import_data <-fread("../../Data/India/import_summaries_by_firm_month_HS_code_complete.csv") %>% 
  select(company_id = domestic_company_id, year, month, date, date_character,hs6, import, log_import) %>% 
  # filter to analysis period
  filter(date >= ymd("2019-02-01"),
         date <= ymd("2021-06-01")) %>% 
  mutate(hs6  = str_pad(as.character(hs6) , 6, "left", "0")) %>% 
  # Drop firms that import less than 1000 usd
  left_join(import_less_than_1000usd, by = c("company_id", "year")) %>% 
  filter(!less_than_1000usd) %>% 
  select(-less_than_1000usd, -sum_import, -n_years) %>% 
  left_join(matched_data %>% 
              select(company_id, SITEID, SICGRP, NAICS6_CODE),
            by = c("company_id")
            ) %>% 
  mutate(SICGRP = ifelse(is.na(SICGRP), "No Data", SICGRP), 
         import_export = "Imports Dataset") %>% 
  select(SICGRP, import_export, company_id)

  
gc()

# Exports data ----

# Firms that export less than 1000 usd in each one of the years (2019, 2020 and 2021) 
export_less_than_1000usd<-read_csv("../../Data/India/processed_data/export_firms_trade_less_than_1000usd.csv")


export_data <-fread("../../Data/India/export_summaries_by_firm_month_HS_code_complete.csv") %>% 
  select(company_id = domestic_company_id, year, month, date, date_character,hs6, export, log_export) %>% 
  # filter to analysis period
  filter(date >= ymd("2019-02-01"),
         date <= ymd("2021-09-01")) %>% 
  mutate(hs6  = str_pad(as.character(hs6) , 6, "left", "0")) %>% 
  # Drop firms that import less than 1000 usd in a particular year
  left_join(export_less_than_1000usd, by = c("company_id", "year")) %>% 
  filter(!less_than_1000usd) %>% 
  select(-less_than_1000usd, -sum_export, -n_years) %>% 
  left_join(matched_data %>% 
              select(company_id, SITEID, SICGRP, NAICS6_CODE),
            by = c("company_id")
  ) %>% 
  mutate(SICGRP = ifelse(is.na(SICGRP), "No Data", SICGRP), 
         import_export = "Exports Dataset") %>% 
  select(SICGRP, import_export, company_id)

gc()



df<-bind_rows(export_data, import_data) %>% 
  group_by(import_export, SICGRP) %>%
  summarize(rows = n()) %>% 
  ungroup() %>% 
  group_by(import_export) %>% 
  mutate(total = sum(rows)) %>% 
  ungroup() %>% 
  mutate(percent = (rows/total)*100)


df2<-bind_rows(export_data, import_data) %>% 
  distinct(company_id, .keep_all = T) %>% 
  group_by(import_export, SICGRP) %>%
  summarize(rows = n()) %>% 
  ungroup() %>% 
  group_by(import_export) %>% 
  mutate(total = sum(rows)) %>% 
  ungroup() %>% 
  mutate(percent = (rows/total)*100)

# In terms of rows
ggplot(df, aes(fill=SICGRP, y=percent, x=import_export)) + 
  geom_bar(position="fill", stat="identity", alpha = 0.9)  +
  labs(fill = "SIC Group")+ 
  theme_minimal()+
  ylab("Share") + 
  xlab("") + 
  coord_flip()+
  scale_fill_brewer(palette = "Paired")



# In terms of firms
ggplot(df2, aes(fill=SICGRP, y=percent, x=import_export)) + 
  geom_bar(position="fill", stat="identity", alpha = 0.9)  +
  labs(fill = "SIC Group")+ 
  theme_minimal()+
  ylab("Share") + 
  xlab("") + 
  coord_flip()+
  scale_fill_brewer(palette = "Paired")



