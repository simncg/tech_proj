#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  top_25_exporting_importing_firms.R                               #
#                                                                           #
#                                                                           #                                                                                                                                                       #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")
library(tibble)


# India ----

# Load matched Aberdeen to Panjiva data
aberdeen <- read_csv("../../Data/India/matched_Aberdeen_to_Panjiva_data_India_v2.csv") %>%
  rename(company_id = our_ID) %>% 
  distinct(company_id) %>% 
  mutate(matched_aberdeen = T) 


# Load technology data (only nod variables) 
tech_data   <- read_parquet("../../Data/India/Builtwith_no_drop_long.parquet", 
                            col_select = c("our_ID", "tech")) %>% 
  # For the moment, we are only going to analyze e-payment/e-commerce technologies
  filter(tech %in% c("payrobust_nod", "ecom_nod")) %>% 
  rename(company_id = our_ID) %>% 
  distinct(company_id) %>% 
  mutate(matched_builtwith = T) 

gc()

# Imports data 
import_data <-fread("../../Data/India/import_summaries_by_firm_month_HS_code_complete.csv") %>% 
  select(company_id = domestic_company_id, year, month, date, date_character,hs6, import, log_import) %>% 
  # filter to analysis period
  filter(date >= ymd("2019-02-01"),
         date <= ymd("2021-06-01")) %>% 
  mutate(hs6  = str_pad(as.character(hs6) , 6, "left", "0")) %>% 
  left_join(aberdeen %>% 
              select(company_id, matched_aberdeen),
            by = "company_id") %>% 
  left_join(tech_data %>% 
              select(company_id, matched_builtwith),
            by = "company_id") %>% 
  mutate(matched_aberdeen = ifelse(is.na(matched_aberdeen), F, matched_aberdeen), 
         matched_builtwith = ifelse(is.na(matched_builtwith), F, matched_builtwith), 
         matched_both = as.logical(matched_aberdeen*matched_builtwith))

gc()

# Exports data 
export_data <-fread("../../Data/India/export_summaries_by_firm_month_HS_code_complete.csv") %>% 
  select(company_id = domestic_company_id, year, month, date, date_character,hs6, export, log_export) %>% 
  # filter to analysis period
  filter(date >= ymd("2019-02-01"),
         date <= ymd("2021-09-01")) %>% 
  mutate(hs6  = str_pad(as.character(hs6) , 6, "left", "0")) %>% 
  left_join(aberdeen %>% 
              select(company_id, matched_aberdeen),
            by = "company_id") %>% 
  left_join(tech_data %>% 
              select(company_id, matched_builtwith),
            by = "company_id") %>% 
  mutate(matched_aberdeen = ifelse(is.na(matched_aberdeen), F, matched_aberdeen), 
         matched_builtwith = ifelse(is.na(matched_builtwith), F, matched_builtwith), 
         matched_both = as.logical(matched_aberdeen*matched_builtwith))

gc()

# Top 25 exporting firms
top_25_exporting_firms<-export_data %>% 
  group_by(company_id) %>% 
  summarize(total_export = sum(export), 
            matched_aberdeen  = first(matched_aberdeen), 
            matched_builtwith = first(matched_builtwith), 
            matched_both = first(matched_both)) %>% 
  arrange(desc(total_export)) %>% 
  head(25)

# Top 25 importing firms 
top_25_importing_firms<-import_data %>% 
  group_by(company_id) %>% 
  summarize(total_import = sum(import), 
            matched_aberdeen = first(matched_aberdeen), 
            matched_builtwith = first(matched_builtwith), 
            matched_both = first(matched_both)) %>% 
  arrange(desc(total_import)) %>% 
  head(25)


# Matching shares
matching_shares_IND <- top_25_exporting_firms %>% 
  count(matched_aberdeen) %>% 
  filter(matched_aberdeen) %>% 
  mutate(`Matched Aberdeen` = n/25) %>% 
  bind_cols(top_25_exporting_firms %>% 
              count(matched_builtwith) %>% 
              filter(matched_builtwith) %>% 
              mutate(`Matched Builtwith` = n/25) 
            ) %>% 
  bind_cols(top_25_exporting_firms %>% 
              count(matched_both) %>% 
              filter(matched_both) %>% 
              mutate(`Matched Both` = n/25)
            ) %>% 
  select(`Matched Aberdeen`, 
         `Matched Builtwith`, 
         `Matched Both`) %>% 
  mutate(Data = "Share of top 25 exporting firms") %>% 
  # Imports 
  bind_rows(top_25_importing_firms %>% 
              count(matched_aberdeen) %>% 
              filter(matched_aberdeen) %>% 
              mutate(`Matched Aberdeen` = n/25) %>% 
              bind_cols(top_25_importing_firms %>% 
                          count(matched_builtwith) %>% 
                          filter(matched_builtwith) %>% 
                          mutate(`Matched Builtwith` = n/25) 
              ) %>% 
              bind_cols(top_25_importing_firms %>% 
                          count(matched_both) %>% 
                          filter(matched_both) %>% 
                          mutate(`Matched Both` = n/25)
              ) %>% 
              select(`Matched Aberdeen`, 
                     `Matched Builtwith`, 
                     `Matched Both`) %>% 
              mutate(Data = "Share of top 25 importing firms")) %>% 
  column_to_rownames("Data")

rm(aberdeen, export_data, import_data, tech_data)

# # Get names of the top 25 firms 
# 
# imports_complete_IND <- read_dta("../../Data/India/IND_imports_Monthly_domestic_level_dropdup.dta") %>% 
#   select(company_id = domestic_company_id, name = domestic_company_name) %>% 
#   distinct(company_id, name) 
# 
# top_25_importing_firms <- left_join(top_25_importing_firms, imports_complete_IND, by = 'company_id')
# 
# gc()
# 
# exports_complete_IND <- read_dta("../../Data/India/IND_exports_Monthly_domestic_level_dropdup.dta") %>% 
#   select(company_id = domestic_company_id, name = domestic_company_name) %>% 
#   distinct(company_id, name) 
# 
# 
# top_25_exporting_firms <- left_join(top_25_exporting_firms, exports_complete_IND, by = 'company_id')
# 
# gc()
# 
# 
# write_csv(top_25_importing_firms, "../../Data/India/processed_data/top_25_importing_firms.csv")
# write_csv(top_25_exporting_firms, "../../Data/India/processed_data/top_25_exporting_firms.csv")


rm(top_25_importing_firms, top_25_exporting_firms)

# Indonesia ---- 

# Load matched Aberdeen to Panjiva data 
aberdeen <- read_xlsx("../../Data/Indonesia/matched_data_Indonesia.xlsx") 

# Correspondance 
IDN_Domestic_Ids_Corresp <- read_xlsx("../../Data/Indonesia/IDN_Domestic_Ids_Corresp_update.xlsx")

aberdeen <-IDN_Domestic_Ids_Corresp  %>% 
  select(prev_our_domestic_id, new_our_domestic_id ) %>% 
  distinct() %>% 
  filter(prev_our_domestic_id  %in% aberdeen$our_domestic_id) %>% 
  left_join(aberdeen,  by = c("prev_our_domestic_id" = "our_domestic_id")) %>% 
  rename(company_id = new_our_domestic_id) %>% 
  distinct(company_id) %>% 
  mutate(matched_aberdeen = T) 

# Load technology data
tech_data <- fread("../../Data/Indonesia/Builtwith_IDN_longformat.csv") %>% 
  select(company_id = New_ID_1, tech) %>% 
  filter(tech %in% c("payrobust", "ecom")) %>% 
  distinct(company_id) %>% 
  mutate(matched_builtwith = T) 
  
gc()
  

# Imports data 
import_data <-fread("../../Data/Indonesia/import_summary_by_firm_month_HS_code.csv") %>% 
  select(company_id, date, date_character,hs6,log_import, import, import_dummy, n_countries_import)%>% 
  mutate(hs6  = str_pad(hs6 , 6, "left", "0"), 
         year = year(date))  %>% 
  left_join(aberdeen %>% 
              select(company_id, matched_aberdeen),
            by = "company_id") %>% 
  left_join(tech_data %>% 
              select(company_id, matched_builtwith),
            by = "company_id") %>% 
  mutate(matched_aberdeen = ifelse(is.na(matched_aberdeen), F, matched_aberdeen), 
         matched_builtwith = ifelse(is.na(matched_builtwith), F, matched_builtwith), 
         matched_both = as.logical(matched_aberdeen*matched_builtwith))


# Exports data
export_data <-fread("../../Data/Indonesia/export_summary_by_firm_month_HS_code.csv") %>% 
  select(company_id, date, date_character,hs6,log_export, export, export_dummy, n_countries_export)%>% 
  mutate(hs6  = str_pad(hs6 , 6, "left", "0"), 
         year = year(date))  %>% 
  left_join(aberdeen %>% 
              select(company_id, matched_aberdeen),
            by = "company_id") %>% 
  left_join(tech_data %>% 
              select(company_id, matched_builtwith),
            by = "company_id") %>% 
  mutate(matched_aberdeen = ifelse(is.na(matched_aberdeen), F, matched_aberdeen), 
         matched_builtwith = ifelse(is.na(matched_builtwith), F, matched_builtwith), 
         matched_both = as.logical(matched_aberdeen*matched_builtwith))

gc()

# Top 25 exporting firms 
top_25_exporting_firms<-export_data %>% 
  group_by(company_id) %>% 
  summarize(total_export = sum(export), 
            matched_aberdeen  = first(matched_aberdeen), 
            matched_builtwith = first(matched_builtwith), 
            matched_both = first(matched_both)) %>% 
  arrange(desc(total_export)) %>% 
  head(25)

# Top 25 importing firms 
top_25_importing_firms<-import_data %>% 
  group_by(company_id) %>% 
  summarize(total_import = sum(import), 
            matched_aberdeen = first(matched_aberdeen), 
            matched_builtwith = first(matched_builtwith), 
            matched_both = first(matched_both)) %>% 
  arrange(desc(total_import)) %>% 
  head(25)


# Matching shares 
matching_shares_IDN <- top_25_exporting_firms %>% 
  count(matched_aberdeen) %>% 
  filter(matched_aberdeen) %>% 
  mutate(`Matched Aberdeen` = n/25) %>% 
  bind_cols(top_25_exporting_firms %>% 
              count(matched_builtwith) %>% 
              filter(matched_builtwith) %>% 
              mutate(`Matched Builtwith` = n/25) 
  ) %>% 
  bind_cols(top_25_exporting_firms %>% 
              count(matched_both) %>% 
              filter(matched_both) %>% 
              mutate(`Matched Both` = n/25)
  ) %>% 
  select(`Matched Aberdeen`, 
         `Matched Builtwith`, 
         `Matched Both`) %>% 
  mutate(Data = "Share of top 25 exporting firms") %>% 
  # Imports 
  bind_rows(top_25_importing_firms %>% 
              count(matched_aberdeen) %>% 
              filter(matched_aberdeen) %>% 
              mutate(`Matched Aberdeen` = n/25) %>% 
              bind_cols(top_25_importing_firms %>% 
                          count(matched_builtwith) %>% 
                          filter(matched_builtwith) %>% 
                          mutate(`Matched Builtwith` = n/25) 
              ) %>% 
              bind_cols(top_25_importing_firms %>% 
                          count(matched_both) %>% 
                          filter(matched_both) %>% 
                          mutate(`Matched Both` = n/25)
              ) %>% 
              select(`Matched Aberdeen`, 
                     `Matched Builtwith`, 
                     `Matched Both`) %>% 
              mutate(Data = "Share of top 25 importing firms")) %>% 
  column_to_rownames("Data")



rm(aberdeen, export_data, import_data, tech_data, top_25_exporting_firms, 
   top_25_importing_firms)


# Save matching shares for top 25
saveRDS(matching_shares_IDN, file = "../../Data/Indonesia/processed_data/top25_companies_matching_shares_IDN.rds")
saveRDS(matching_shares_IND, file = "../../Data/India/processed_data/top25_companies_matching_shares_IND_v2.rds")







  