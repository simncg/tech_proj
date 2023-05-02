#--------------------------------------#
#
#               
#--------------------------------------#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

library(data.table)
library(arrow)
library(dplyr)
library(lubridate)
library(tidyverse)

# India ----

## Technology data ---- 
tech_data <- read_parquet("../../Data/India/processed_data/tech_data_IND.parquet", 
                          col_select = c("company_id", "date", "ecom", "payrobust", "pay_or_ecomnod"))

# Matched Panjiva-Aberdeen-Builtwith dataset (only regression sample) -----
pay_ecom_import_data_IND <- fread("../../Data/India/processed_data/imports_product_model_IND.csv", 
                                  select = "company_id")

pay_ecom_export_data_IND <- fread("../../Data/India/processed_data/exports_product_model_IND.csv", 
                                  select = "company_id")

# Set as data.table
setDT(tech_data)
setDT(pay_ecom_import_data_IND)
setDT(pay_ecom_export_data_IND)

## Create plot of technology adoption for imports dataset ---- 
imp_firms_sample <- unique(pay_ecom_import_data_IND[, company_id])
import_tech <- tech_data[company_id %in% imp_firms_sample] %>% 
  filter(date >= ymd("2018-07-01"), 
         date < ymd("2022-01-01"))

import_tech %>% 
  group_by(date) %>% 
  summarize(`E-commerce` = mean(ecom), 
            `E-payment` = mean(payrobust), 
            `E-commerce or E-payment` = mean(pay_or_ecomnod)) %>% 
  pivot_longer(cols = c(`E-commerce`, `E-payment`, `E-commerce or E-payment`), 
               names_to =  "tech", 
               values_to = "share_of_firms") %>% 
  ggplot()+
  geom_line(aes(x = date, y = share_of_firms, color = tech))+
  ylab("Share of firms")+
  xlab("Date")+
  ggtitle("Share of importing firms that adopted the E-payment or E-commerce technolgies", 
          subtitle = "India - Imports")+
  labs(color = "Technology", 
       caption = "Based on regression sample for model that measures if tech adoption affects trade outcomes")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic"))
  


## Create plot of technology adoption for imports dataset ----
exp_firms_sample <- unique(pay_ecom_export_data_IND[, company_id])
export_tech <- tech_data[company_id %in% exp_firms_sample] %>% 
  filter(date >= ymd("2018-07-01"), 
         date < ymd("2022-01-01"))

export_tech %>% 
  group_by(date) %>% 
  summarize(`E-commerce` = mean(ecom), 
            `E-payment` = mean(payrobust), 
            `E-commerce or E-payment` = mean(pay_or_ecomnod)) %>% 
  pivot_longer(cols = c(`E-commerce`, `E-payment`, `E-commerce or E-payment`), 
               names_to =  "tech", 
               values_to = "share_of_firms") %>% 
  ggplot()+
  geom_line(aes(x = date, y = share_of_firms, color = tech))+
  ylab("Share of firms")+
  xlab("Date")+
  ggtitle("Share of exporting firms that adopted the E-payment or E-commerce technolgies", 
          subtitle = "India - Exports")+
  labs(color = "Technology", 
       caption = "Based on regression sample for model that measures if tech adoption affects trade outcomes")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic"))






## Technology data ---- 
tech_data_IDN <- read_parquet("../../Data/Indonesia/processed_data/tech_data_IDN.parquet", 
                          col_select = c("company_id", "date", "ecom", "payrobust", "pay_or_ecomnod"))

# Matched Panjiva-Aberdeen-Builtwith dataset (only regression sample) -----
pay_ecom_import_data_IDN <- fread("../../Data/Indonesia/processed_data/imports_product_model_IDN.csv", 
                                  select = "company_id")

pay_ecom_export_data_IDN <- fread("../../Data/Indonesia/processed_data/exports_product_model_IDN.csv", 
                                  select = "company_id")

# Set as data.table
setDT(tech_data_IDN)
setDT(pay_ecom_import_data_IDN)
setDT(pay_ecom_export_data_IDN)

## Create plot of technology adoption for imports dataset ---- 
imp_firms_sample <- unique(pay_ecom_import_data_IDN[, company_id])
import_tech <- tech_data_IDN[company_id %in% imp_firms_sample] %>% 
  filter(date >= ymd("2018-07-01"), 
         date < ymd("2022-01-01"))

import_tech %>% 
  group_by(date) %>% 
  summarize(`E-commerce` = mean(ecom), 
            `E-payment` = mean(payrobust), 
            `E-commerce or E-payment` = mean(pay_or_ecomnod)) %>% 
  pivot_longer(cols = c(`E-commerce`, `E-payment`, `E-commerce or E-payment`), 
               names_to =  "tech", 
               values_to = "share_of_firms") %>% 
  ggplot()+
  geom_line(aes(x = date, y = share_of_firms, color = tech))+
  ylab("Share of firms")+
  xlab("Date")+
  ggtitle("Share of importing firms that adopted the E-payment or E-commerce technolgies", 
          subtitle = "Indonesia - Imports")+
  labs(color = "Technology", 
       caption = "Based on regression sample for model that measures if tech adoption affects trade outcomes")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic"))



## Create plot of technology adoption for imports dataset ----
exp_firms_sample <- unique(pay_ecom_export_data_IND[, company_id])
export_tech <- tech_data[company_id %in% exp_firms_sample] %>% 
  filter(date >= ymd("2018-07-01"), 
         date < ymd("2022-01-01"))

export_tech %>% 
  group_by(date) %>% 
  summarize(`E-commerce` = mean(ecom), 
            `E-payment` = mean(payrobust), 
            `E-commerce or E-payment` = mean(pay_or_ecomnod)) %>% 
  pivot_longer(cols = c(`E-commerce`, `E-payment`, `E-commerce or E-payment`), 
               names_to =  "tech", 
               values_to = "share_of_firms") %>% 
  ggplot()+
  geom_line(aes(x = date, y = share_of_firms, color = tech))+
  ylab("Share of firms")+
  xlab("Date")+
  ggtitle("Share of exporting firms that adopted the E-payment or E-commerce technolgies", 
          subtitle = "Indonesia - Exports")+
  labs(color = "Technology", 
       caption = "Based on regression sample for model that measures if tech adoption affects trade outcomes")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic"))


