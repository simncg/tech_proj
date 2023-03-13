# Compare Trade Values in Panjiva with respect to WITS

# Libraries to be used
library(tidyverse)
library(writexl)
library(haven)
library(readr)
library(lubridate)

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Wits ----
wits<-read_dta("../../Data/Extra Data/wits-world-19952021-reshaped.dta") %>% 
  filter(c %in% c("IND", "IDN"), 
         y>=2019 & y<= 2021) %>% 
  select(y, c, t_import_total, t_import_27, t_export_total, t_export_27) %>% 
  mutate(total_imports = (t_import_total - t_import_27)*1000, 
         total_exports = (t_export_total - t_export_27)*1000)

wits_IND<-wits %>% 
  filter(c == "IND") %>% 
  select(c, y, wits_total_imports = total_imports, wits_total_exports = total_exports)

wits_IDN<-wits %>% 
  filter(c == "IDN") %>% 
  select(c, y, wits_total_imports = total_imports, wits_total_exports = total_exports)
  


# Indonesia Panjiva Imports Data ---- 

imports_IDN <-read_dta("../../Data/Indonesia/IDN_Imports_Monthly_domestic_level_dropdup.dta") %>% 
  select(company_id, year, month, hs, import, import_price, import_qty, country)%>% 
  filter(!str_starts(hs, "27")) %>% 
  mutate(hs6 = substr(hs, 1, 6), 
         hs6  = str_pad(hs6 , 6, "left", "0"))


# Indonesia Panjiva Exports Data ---- 

exports_IDN <-read_dta("../../Data/Indonesia/IDN_Exports_Monthly_domestic_level_dropdup.dta") %>% 
  select(company_id, year, month, hs, export, export_price, export_qty, country)%>% 
  filter(!str_starts(hs, "27")) %>% 
  mutate(hs6 = substr(hs, 1, 6)) %>%
  mutate(hs6  = str_pad(hs6 , 6, "left", "0"))

# India Panjiva Imports Data ---- 

imports_IND <-read_dta("../../Data/India/IND_imports_Monthly_domestic_level_dropdup.dta") %>%
  select(company_id = domestic_company_id, year, month, hs6, import, import_price, country, import_qty, company_name = domestic_company_name) %>%
  filter(!str_starts(hs6 , "27")) %>%
  mutate(date = as.Date(paste0(year, "-", month, "-01"))) %>%
  # filter to analysis period
  filter(date >= ymd("2019-01-01"),
         date <= ymd("2021-12-31")) %>%
  mutate(hs6  = str_pad(as.character(hs6), 6, "left", "0"))

  


# India Panjiva Exports Data ---- 
exports_IND <-read_dta("../../Data/India/IND_Exports_Monthly_domestic_level_dropdup.dta") %>%
  select(company_id = domestic_company_id, year, month,hs6, export, export_price, country, export_qty, company_name = domestic_company_name) %>%
  filter(!str_starts(hs6 , "27")) %>%
  # filter to analysis period
  mutate(date = as.Date(paste0(year, "-", month, "-01"))) %>%
  # filter to analysis period
  filter(date >= ymd("2019-01-01"),
         date <= ymd("2021-12-31"))%>%
  mutate(hs6  = str_pad(as.character(hs6) , 6, "left", "0"))


# Comparison WITS and Panjiva Totals

tot_exp_IDN<-exports_IDN %>% 
  group_by(year) %>% 
  summarize(panj_total_exports = sum(export)) %>% 
  left_join(wits_IDN %>% 
              select(y, wits_total_exports), 
            by = c("year"="y"))%>% 
  mutate(ratio_exports = panj_total_exports/wits_total_exports) %>% 
  filter(year == 2020) %>% 
  mutate(across(c(2, 3), scales::dollar))


tot_imp_IDN<-imports_IDN %>% 
  group_by(year) %>% 
  summarize(panj_total_imports = sum(import)) %>% 
  left_join(wits_IDN %>% 
              select(y, wits_total_imports), 
            by = c("year"="y"))%>% 
  mutate(ratio_imports = panj_total_imports/wits_total_imports) %>% 
  filter(year == 2020) %>% 
  mutate(across(c(2, 3), scales::dollar))


tot_exp_IND<-exports_IND %>% 
  group_by(year) %>% 
  summarize(panj_total_exports = sum(export)) %>% 
  left_join(wits_IND %>% 
              select(y, wits_total_exports), 
            by = c("year"="y"))%>% 
  mutate(ratio_exports = panj_total_exports/wits_total_exports) %>% 
  mutate(across(c(2, 3), scales::dollar))


tot_imp_IND<-imports_IND %>% 
  group_by(year) %>% 
  summarize(panj_total_imports = sum(import))%>% 
  left_join(wits_IND %>% 
              select(y, wits_total_imports), 
            by = c("year"="y")) %>% 
  mutate(ratio_imports = panj_total_imports/wits_total_imports) %>% 
  mutate(across(c(2, 3), scales::dollar))



comparison_IDN<-tot_imp_IDN %>% 
  left_join(tot_exp_IDN, by = "year")


colnames(comparison_IDN)<-c("Year", "Panjiva Total Imp", "WITS Total Imp", "Ratio Imp (Panj/WITS)", 
         "Panjiva Total Exp", "WITS Total Exp", "Ratio Exp (Panj/WITS)")

comparison_IND<-tot_imp_IND %>% 
  left_join(tot_exp_IND, by = "year")

colnames(comparison_IND)<-c("Year", "Panjiva Total Imp", "WITS Total Imp", "Ratio Imp (Panj/WITS)", 
              "Panjiva Total Exp", "WITS Total Exp", "Ratio Exp (Panj/WITS)")



saveRDS(comparison_IDN, "../../Data/processed_data/comparison_panjiva_wits_IDN.rds")
saveRDS(comparison_IND, "../../Data/processed_data/comparison_panjiva_wits_IND.rds")
















  
  



