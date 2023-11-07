#=======================================================================================#
# Date:    June 2023                                                        
#                                                                           
# Project: E-commerce and Trade during Crisis Times: Firm-level Evidence from
#          India, Indonesia and Mexico.                                            
#                                                                           
#                                                                           
#                                                                           
# This program generates tables of matching rates between Panjiva-BuiltWith-Aberdeen datasets.  
# Tables A.1, A.2, A.3 (in Appendix).
#                                                                           
#                                                                           
# Inputs:   - Data/raw_data/Indonesia/Builtwith_no_drop_long_v2.parquet
#           - Data/raw_data/Indonesia/matched_data_Indonesia.xlsx
#           - Data/raw_data/Indonesia/IDN_Domestic_Ids_Corresp_update.xlsx
#           - Data/processed_data/Indonesia/import_summary_by_firm_month_HS_code.csv
#           - Data/processed_data/Indonesia/export_summary_by_firm_month_HS_code.csv
#           - Data/raw_data/India/Builtwith_no_drop_long_v2.parquet
#           - Data/raw_data/India/matched_Aberdeen_to_Panjiva_data_India_v2.csv
#           - Data/processed_data/India/import_summaries_by_firm_month_HS_code_complete.csv
#           - Data/processed_data/India/export_summaries_by_firm_month_HS_code_complete.csv
#           - Data/raw_data/Mexico/Builtwith_Mexico_no_drop_long.csv
#           - Data/raw_data/Mexico/matched_Aberdeen_to_Panjiva_data_Mexico.csv
#           - Data/processed_data/Mexico/import_summaries_by_firm_month_HS_code_complete.parquet
#           - Data/processed_data/Mexico/export_summaries_by_firm_month_HS_code_complete.parquet
#                                                                           
# Outputs:  - Outputs/Tables/matching_rates_IDN.tex
#           - Outputs/Tables/matching_rates_MEX.tex
#           - Outputs/Tables/matching_rates_IND.tex
#
#=======================================================================================#

# Set working directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")
library(tibble)
library(ggpubr)

# Indonesia Data ----

# Builtwith Data 
tech_IDN <- read_parquet("../../Data/raw_data/Indonesia/Builtwith_no_drop_long_v2.parquet", 
                         col_select = c("New_ID_1", "tech")) %>% 
  rename(company_id = New_ID_1) %>% 
  filter(tech %in% c("pay_robust", "ecom_nod")) %>% 
  distinct(company_id)

# Load matched Aberdeen to Panjiva data 
aberdeen_IDN <- read_xlsx("../../Data/raw_data/Indonesia/matched_data_Indonesia.xlsx") 

# Correspondance 
IDN_Domestic_Ids_Corresp <- read_xlsx("../../Data/raw_data/Indonesia/IDN_Domestic_Ids_Corresp_update.xlsx")

# many to many expected in some observations here
aberdeen_IDN <-IDN_Domestic_Ids_Corresp  %>% 
  select(prev_our_domestic_id, new_our_domestic_id ) %>% 
  distinct() %>% 
  filter(prev_our_domestic_id  %in% aberdeen_IDN$our_domestic_id) %>% 
  left_join(aberdeen_IDN,  by = c("prev_our_domestic_id" = "our_domestic_id")) %>% 
  rename(company_id = new_our_domestic_id) %>% 
  distinct(company_id)


# Panjiva Imports Data (Firm-Month-HS6)
imports_IDN <-read_csv("../../Data/processed_data/Indonesia/import_summary_by_firm_month_HS_code.csv") %>% 
  select(company_id, date, date_character,hs6,log_import, import, import_dummy)%>% 
  mutate(hs6  = str_pad(hs6 , 6, "left", "0"), 
         year = year(date)) %>% 
  mutate(matched_builtwith = company_id %in% tech_IDN$company_id, 
         matched_aberdeen = company_id %in% aberdeen_IDN$company_id, 
         matched_both = as.logical(matched_builtwith*matched_aberdeen))

# Panjiva Exports Data (Firm-Month-HS6)
exports_IDN <-read_csv("../../Data/processed_data/Indonesia/export_summary_by_firm_month_HS_code.csv") %>% 
  select(company_id, date, date_character,hs6, log_export, export, export_dummy)%>% 
  mutate(hs6  = str_pad(hs6 , 6, "left", "0"), 
         year = year(date)) %>% 
  mutate(matched_builtwith = company_id %in% tech_IDN$company_id, 
         matched_aberdeen = company_id %in% aberdeen_IDN$company_id, 
         matched_both = as.logical(matched_builtwith*matched_aberdeen))


# India Data ----

# Builtwith India 
tech_IND   <- read_parquet("../../Data/raw_data/India/Builtwith_no_drop_long_v2.parquet", 
                           col_select = c("our_ID", "tech")) %>% 
  # For the moment, we are only going to analyze e-payment/e-commerce technologies
  filter(tech %in% c("payrobust_nod", "ecom_nod")) %>% 
  rename(company_id = our_ID) %>% 
  distinct(company_id)

# Firms matched to Aberdeen 
aberdeen_IND <- read_csv("../../Data/raw_data/India/matched_Aberdeen_to_Panjiva_data_India_v2.csv") %>%
  rename(company_id = our_ID) %>% 
  distinct(company_id)


# Panjiva Imports Data 
imports_IND <-fread("../../Data/processed_data/India/import_summaries_by_firm_month_HS_code_complete.csv") %>% 
  select(company_id = domestic_company_id, year, month, date, date_character,hs6, import, log_import) %>%
  # filter to analysis period
  filter(date >= ymd("2018-07-01"),
         date < ymd("2022-01-01")) %>% 
  mutate(hs6  = str_pad(as.character(hs6), 6, "left", "0"), 
         matched_builtwith = company_id %in% tech_IND$company_id, 
         matched_aberdeen = company_id %in% aberdeen_IND$company_id, 
         matched_both = as.logical(matched_aberdeen*matched_builtwith)) %>% 
  as.data.frame()


# Panjiva Exports Data 
exports_IND <-fread("../../Data/processed_data/India/export_summaries_by_firm_month_HS_code_complete.csv") %>% 
  select(company_id = domestic_company_id, year, month, date, date_character,hs6, export, log_export) %>% 
  # filter to analysis period
  filter(date >= ymd("2018-07-01"),
         date < ymd("2022-01-01"))%>% 
  mutate(hs6  = str_pad(as.character(hs6) , 6, "left", "0"), 
         matched_builtwith = company_id %in% tech_IND$company_id, 
         matched_aberdeen = company_id %in% aberdeen_IND$company_id,
         matched_both = as.logical(matched_aberdeen*matched_builtwith)) %>% 
  as.data.frame()

rm(IDN_Domestic_Ids_Corresp)




# Mexico Data ----

# Builtwith India 
tech_MEX   <- fread("../../Data/raw_data/Mexico/Builtwith_Mexico_no_drop_long.csv") %>% 
  select(our_ID, tech) %>% 
  # For the moment, we are only going to analyze e-payment/e-commerce technologies
  filter(tech %in% c("payrobust_nod", "ecom_nod")) %>% 
  rename(company_id = our_ID) %>% 
  distinct(company_id)

# Firms matched to Aberdeen 
aberdeen_MEX <- read_csv("../../Data/raw_data/Mexico/matched_Aberdeen_to_Panjiva_data_Mexico.csv") %>%
  rename(company_id = our_ID) %>% 
  distinct(company_id)


# Panjiva Imports Data 
imports_MEX <-read_parquet("../../Data/processed_data/Mexico/import_summaries_by_firm_month_HS_code_complete.parquet", 
                           col_select = c("domestic_company_id", "year", "month", "date", 
                                          "date_character","hs6", "import", "log_import")) %>% 
  rename(company_id = domestic_company_id) %>%
  # filter to analysis period
  filter(date >= ymd("2018-07-01"),
         date < ymd("2022-01-01")) %>% 
  mutate(hs6  = str_pad(as.character(hs6), 6, "left", "0"), 
         matched_builtwith = company_id %in% tech_MEX$company_id, 
         matched_aberdeen = company_id %in% aberdeen_MEX$company_id, 
         matched_both = as.logical(matched_aberdeen*matched_builtwith)) %>% 
  as.data.frame()


# Panjiva Exports Data 
exports_MEX <-read_parquet("../../Data/processed_data/Mexico/export_summaries_by_firm_month_HS_code_complete.parquet", 
                           col_select = c("domestic_company_id", "year", 'month', "date", 
                                          "date_character", "hs6", "export", "log_export")) %>% 
  rename(company_id = domestic_company_id) %>% 
  # filter to analysis period
  filter(date >= ymd("2018-07-01"),
         date < ymd("2022-01-01"))%>% 
  mutate(hs6  = str_pad(as.character(hs6) , 6, "left", "0"), 
         matched_builtwith = company_id %in% tech_MEX$company_id, 
         matched_aberdeen = company_id %in% aberdeen_MEX$company_id,
         matched_both = as.logical(matched_aberdeen*matched_builtwith)) %>% 
  as.data.frame()


# Matching Rates Tables ----

big_number_format<-function(x){format(x, big.mark = ",")}


## Indonesia ---- 
table_IDN<-imports_IDN %>% 
  select(year, import, matched_builtwith, matched_aberdeen, matched_both) %>% 
  mutate(Data = "Panjiva Imports") %>% 
  group_by(year) %>% 
  mutate(total_imports = sum(import, na.rm = T)) %>%
  ungroup() %>% 
  group_by(year, matched_builtwith) %>% 
  mutate(tot_imp_match_builtwith = sum(import, na.rm = T), 
         share_match_builtwith = (tot_imp_match_builtwith/total_imports)*100) %>%
  ungroup() %>%
  group_by(year, matched_aberdeen) %>% 
  mutate(tot_imp_match_aberdeen = sum(import, na.rm = T), 
         share_match_aberdeen = (tot_imp_match_aberdeen/total_imports)*100) %>% 
  ungroup() %>% 
  group_by(year, matched_both) %>% 
  mutate(tot_imp_match_both = sum(import, na.rm=T), 
         share_match_both = (tot_imp_match_both/total_imports)*100) %>% 
  ungroup() %>% 
  filter(matched_builtwith == T, 
         matched_aberdeen  == T, 
         matched_both == T) %>% 
  distinct(year, .keep_all = T) %>% 
  select(Data,
         Year = year,
         Number = total_imports,
         `% Matched Aberdeen` = share_match_aberdeen,
         `% Matched BuiltWith` = share_match_builtwith, 
         `% Matched Both` = share_match_both
  ) %>% 
  bind_rows(
    exports_IDN %>% 
      select(year, export, matched_builtwith, matched_aberdeen, matched_both) %>% 
      mutate(Data = "Panjiva Exports") %>% 
      group_by(year) %>% 
      mutate(total_exports = sum(export, na.rm = T)) %>% 
      ungroup() %>% 
      group_by(year, matched_builtwith) %>% 
      mutate(tot_exp_match_builtwith = sum(export, na.rm = T), 
             share_match_builtwith = (tot_exp_match_builtwith/total_exports)*100) %>%
      ungroup() %>%
      group_by(year, matched_aberdeen) %>% 
      mutate(tot_exp_match_aberdeen = sum(export, na.rm = T), 
             share_match_aberdeen = (tot_exp_match_aberdeen/total_exports)*100) %>% 
      ungroup() %>% 
      group_by(year, matched_both) %>% 
      mutate(tot_exp_match_both = sum(export, na.rm = T), 
             share_match_both = (tot_exp_match_both/total_exports)*100) %>% 
      ungroup() %>% 
      filter(matched_builtwith == T, 
             matched_aberdeen  == T, 
             matched_both == T) %>% 
      distinct(year, .keep_all = T) %>% 
      select(Data,
             Year = year,
             Number = total_exports,
             `% Matched Aberdeen` = share_match_aberdeen,
             `% Matched BuiltWith` = share_match_builtwith, 
             `% Matched Both` = share_match_both
      )
  ) %>% 
  mutate(across(where(is.numeric), round, 2), 
         across(c(Number), big_number_format)) %>% 
  mutate(Number = paste0("$", Number)) %>% 
  bind_rows(
    imports_IDN %>% 
      select(company_id, year, matched_builtwith, matched_aberdeen, matched_both) %>% 
      distinct(company_id, year, .keep_all = T) %>% 
      mutate(Data = "Panjiva Imports") %>% 
      group_by(year) %>% 
      mutate(n_firms = n()) %>%
      ungroup() %>% 
      group_by(year, matched_builtwith) %>% 
      mutate(n_match_builtwith = sum(matched_builtwith), 
             share_match_builtwith = (n_match_builtwith/n_firms)*100) %>%
      ungroup() %>%
      group_by(year, matched_aberdeen) %>% 
      mutate(n_match_aberdeen = sum(matched_aberdeen), 
             share_match_aberdeen = (n_match_aberdeen/n_firms)*100) %>% 
      ungroup() %>% 
      group_by(year, matched_both) %>% 
      mutate(n_match_both = sum(matched_both), 
             share_match_both = (n_match_both/n_firms)*100) %>% 
      ungroup() %>% 
      filter(share_match_builtwith != 0, 
             share_match_aberdeen != 0, 
             share_match_both != 0) %>% 
      distinct(n_firms, share_match_builtwith, share_match_aberdeen, share_match_both, .keep_all = T) %>% 
      select(Data,
             Year = year,
             `Number` = n_firms,
             `% Matched Aberdeen` = share_match_aberdeen,
             `% Matched BuiltWith` = share_match_builtwith, 
             `% Matched Both` = share_match_both
      ) %>% 
      bind_rows(
        exports_IDN %>% 
          select(company_id, year, matched_builtwith, matched_aberdeen, matched_both) %>% 
          distinct(company_id, year, .keep_all = T) %>% 
          mutate(Data = "Panjiva Exports") %>% 
          group_by(year) %>% 
          mutate(n_firms = n()) %>% 
          group_by(year, matched_builtwith) %>% 
          mutate(n_match_builtwith = sum(matched_builtwith), 
                 share_match_builtwith = (n_match_builtwith/n_firms)*100) %>%
          ungroup() %>%
          group_by(year, matched_aberdeen) %>% 
          mutate(n_match_aberdeen = sum(matched_aberdeen), 
                 share_match_aberdeen = (n_match_aberdeen/n_firms)*100) %>% 
          ungroup() %>% 
          group_by(year, matched_both) %>% 
          mutate(n_match_both = sum(matched_both), 
                 share_match_both = (n_match_both/n_firms)*100) %>% 
          ungroup() %>% 
          filter(share_match_builtwith != 0, 
                 share_match_aberdeen != 0, 
                 share_match_both != 0) %>% 
          distinct(n_firms, share_match_builtwith, share_match_aberdeen, share_match_both, .keep_all = T) %>% 
          select(Data,
                 Year = year,
                 Number = n_firms,
                 `% Matched Aberdeen` = share_match_aberdeen,
                 `% Matched BuiltWith` = share_match_builtwith, 
                 `% Matched Both` = share_match_both
          )
      ) %>% 
      mutate(across(where(is.numeric), round, 2), 
             across(Number, big_number_format))
  ) %>% 
  mutate(Year = paste0(Year, " ", seq_len(12)))%>% 
  tibble::column_to_rownames("Year") %>% 
  select(-Data) %>% 
  kbl(caption = "Indonesia - Panjiva total trade value and number of firms matching rates to BuiltWith and Aberdeen datasets by year", 
      centering = T, booktabs = T, align = "c", format = "latex") %>%
  kable_styling(latex_options = c("HOLD_position", "scale_down")) %>% 
  group_rows("Panel A: Matching Rates in Terms of Total Trade Value", 1, 6) %>%
  group_rows("Import Trade Value", 1, 3, italic = T) %>%
  group_rows("Export Trade Value", 4, 6, italic = T) %>% 
  group_rows("Panel B: Matching Rates in Terms of Number of Firms", 7, 12) %>%
  group_rows("Importers Number", 7, 9, italic = T) %>%
  group_rows("Exporters Number", 10, 12, italic = T) %>% 
  footnote(general = "", threeparttable = T)


capture.output(table_IDN, file = "../../Outputs/Tables/matching_rates_IDN.tex")


# India ---- 

table_IND<-imports_IND %>% 
  select(year, import, matched_builtwith, matched_aberdeen, matched_both) %>% 
  mutate(Data = "Panjiva Imports") %>% 
  group_by(year) %>% 
  mutate(total_imports = sum(import, na.rm = T)) %>%
  ungroup() %>% 
  group_by(year, matched_builtwith) %>% 
  mutate(tot_imp_match_builtwith = sum(import, na.rm = T), 
         share_match_builtwith = (tot_imp_match_builtwith/total_imports)*100) %>%
  ungroup() %>%
  group_by(year, matched_aberdeen) %>% 
  mutate(tot_imp_match_aberdeen = sum(import, na.rm = T), 
         share_match_aberdeen = (tot_imp_match_aberdeen/total_imports)*100) %>% 
  ungroup() %>% 
  group_by(year, matched_both) %>% 
  mutate(tot_imp_match_both = sum(import, na.rm=T), 
         share_match_both = (tot_imp_match_both/total_imports)*100) %>% 
  ungroup() %>% 
  filter(matched_builtwith == T, 
         matched_aberdeen  == T, 
         matched_both == T) %>% 
  distinct(year, .keep_all = T) %>% 
  select(Data,
         Year = year,
         Number = total_imports,
         `% Matched Aberdeen` = share_match_aberdeen,
         `% Matched BuiltWith` = share_match_builtwith, 
         `% Matched Both` = share_match_both
  ) %>% 
  bind_rows(
    exports_IND %>% 
      select(year, export, matched_builtwith, matched_aberdeen, matched_both) %>% 
      mutate(Data = "Panjiva Exports") %>% 
      group_by(year) %>% 
      mutate(total_exports = sum(export, na.rm = T)) %>% 
      ungroup() %>% 
      group_by(year, matched_builtwith) %>% 
      mutate(tot_exp_match_builtwith = sum(export, na.rm = T), 
             share_match_builtwith = (tot_exp_match_builtwith/total_exports)*100) %>%
      ungroup() %>%
      group_by(year, matched_aberdeen) %>% 
      mutate(tot_exp_match_aberdeen = sum(export, na.rm = T), 
             share_match_aberdeen = (tot_exp_match_aberdeen/total_exports)*100) %>% 
      ungroup() %>% 
      group_by(year, matched_both) %>% 
      mutate(tot_exp_match_both = sum(export, na.rm = T), 
             share_match_both = (tot_exp_match_both/total_exports)*100) %>% 
      ungroup() %>% 
      filter(matched_builtwith == T, 
             matched_aberdeen  == T, 
             matched_both == T) %>% 
      distinct(year, .keep_all = T) %>% 
      select(Data,
             Year = year,
             Number = total_exports,
             `% Matched Aberdeen` = share_match_aberdeen,
             `% Matched BuiltWith` = share_match_builtwith, 
             `% Matched Both` = share_match_both
      )
  ) %>% 
  mutate(across(where(is.numeric), round, 2), 
         across(c(Number), big_number_format)) %>% 
  mutate(Number = paste0("$", Number)) %>% 
  bind_rows(
    imports_IND %>% 
      select(company_id, year, matched_builtwith, matched_aberdeen, matched_both) %>% 
      distinct(company_id, year, .keep_all = T) %>% 
      mutate(Data = "Panjiva Imports") %>% 
      group_by(year) %>% 
      mutate(n_firms = n()) %>%
      ungroup() %>% 
      group_by(year, matched_builtwith) %>% 
      mutate(n_match_builtwith = sum(matched_builtwith), 
             share_match_builtwith = (n_match_builtwith/n_firms)*100) %>%
      ungroup() %>%
      group_by(year, matched_aberdeen) %>% 
      mutate(n_match_aberdeen = sum(matched_aberdeen), 
             share_match_aberdeen = (n_match_aberdeen/n_firms)*100) %>% 
      ungroup() %>% 
      group_by(year, matched_both) %>% 
      mutate(n_match_both = sum(matched_both), 
             share_match_both = (n_match_both/n_firms)*100) %>% 
      ungroup() %>% 
      filter(share_match_builtwith != 0, 
             share_match_aberdeen != 0, 
             share_match_both != 0) %>% 
      distinct(n_firms, share_match_builtwith, share_match_aberdeen, share_match_both, .keep_all = T) %>% 
      select(Data,
             Year = year,
             `Number` = n_firms,
             `% Matched Aberdeen` = share_match_aberdeen,
             `% Matched BuiltWith` = share_match_builtwith, 
             `% Matched Both` = share_match_both
      ) %>% 
      bind_rows(
        exports_IND %>% 
          select(company_id, year, matched_builtwith, matched_aberdeen, matched_both) %>% 
          distinct(company_id, year, .keep_all = T) %>% 
          mutate(Data = "Panjiva Exports") %>% 
          group_by(year) %>% 
          mutate(n_firms = n()) %>% 
          group_by(year, matched_builtwith) %>% 
          mutate(n_match_builtwith = sum(matched_builtwith), 
                 share_match_builtwith = (n_match_builtwith/n_firms)*100) %>%
          ungroup() %>%
          group_by(year, matched_aberdeen) %>% 
          mutate(n_match_aberdeen = sum(matched_aberdeen), 
                 share_match_aberdeen = (n_match_aberdeen/n_firms)*100) %>% 
          ungroup() %>% 
          group_by(year, matched_both) %>% 
          mutate(n_match_both = sum(matched_both), 
                 share_match_both = (n_match_both/n_firms)*100) %>% 
          ungroup() %>% 
          filter(share_match_builtwith != 0, 
                 share_match_aberdeen != 0, 
                 share_match_both != 0) %>% 
          distinct(n_firms, share_match_builtwith, share_match_aberdeen, share_match_both, .keep_all = T) %>% 
          select(Data,
                 Year = year,
                 Number = n_firms,
                 `% Matched Aberdeen` = share_match_aberdeen,
                 `% Matched BuiltWith` = share_match_builtwith, 
                 `% Matched Both` = share_match_both
          )
      ) %>% 
      mutate(across(where(is.numeric), round, 2), 
             across(Number, big_number_format))
  )%>% 
  mutate(Year = paste0(Year, " ", seq_len(16)))%>% 
  tibble::column_to_rownames("Year") %>% 
  select(-Data) %>% 
  kbl(caption = "India - Panjiva total trade value and number of firms matching rates to BuiltWith and Aberdeen datasets by year", 
      centering = T, booktabs = T, align = "c", format = "latex") %>%
  kable_styling(latex_options = c("HOLD_position", "scale_down")) %>% 
  group_rows("Panel A: Matching Rates in Terms of Total Trade Value", 1, 8, hline_after = T) %>%
  group_rows("Import Trade Value", 1, 4, italic = T) %>%
  group_rows("Export Trade Value", 5, 8, italic = T) %>% 
  group_rows("Panel B: Matching Rates in Terms of Number of Firms", 9, 16, hline_after = T) %>%
  group_rows("Importers Number", 9, 12, italic = T) %>%
  group_rows("Exporters Number", 13, 16, italic = T) %>% 
  footnote(general = "", threeparttable = T)



capture.output(table_IND, file = "../../Outputs/Tables/matching_rates_IND.tex")




# Mexico ---- 

table_MEX<-imports_MEX %>% 
  select(year, import, matched_builtwith, matched_aberdeen, matched_both) %>% 
  mutate(Data = "Panjiva Imports") %>% 
  group_by(year) %>% 
  mutate(total_imports = sum(import, na.rm = T)) %>%
  ungroup() %>% 
  group_by(year, matched_builtwith) %>% 
  mutate(tot_imp_match_builtwith = sum(import, na.rm = T), 
         share_match_builtwith = (tot_imp_match_builtwith/total_imports)*100) %>%
  ungroup() %>%
  group_by(year, matched_aberdeen) %>% 
  mutate(tot_imp_match_aberdeen = sum(import, na.rm = T), 
         share_match_aberdeen = (tot_imp_match_aberdeen/total_imports)*100) %>% 
  ungroup() %>% 
  group_by(year, matched_both) %>% 
  mutate(tot_imp_match_both = sum(import, na.rm=T), 
         share_match_both = (tot_imp_match_both/total_imports)*100) %>% 
  ungroup() %>% 
  filter(matched_builtwith == T, 
         matched_aberdeen  == T, 
         matched_both == T) %>% 
  distinct(year, .keep_all = T) %>% 
  select(Data,
         Year = year,
         Number = total_imports,
         `% Matched Aberdeen` = share_match_aberdeen,
         `% Matched BuiltWith` = share_match_builtwith, 
         `% Matched Both` = share_match_both
  ) %>% 
  bind_rows(
    exports_MEX %>% 
      select(year, export, matched_builtwith, matched_aberdeen, matched_both) %>% 
      mutate(Data = "Panjiva Exports") %>% 
      group_by(year) %>% 
      mutate(total_exports = sum(export, na.rm = T)) %>% 
      ungroup() %>% 
      group_by(year, matched_builtwith) %>% 
      mutate(tot_exp_match_builtwith = sum(export, na.rm = T), 
             share_match_builtwith = (tot_exp_match_builtwith/total_exports)*100) %>%
      ungroup() %>%
      group_by(year, matched_aberdeen) %>% 
      mutate(tot_exp_match_aberdeen = sum(export, na.rm = T), 
             share_match_aberdeen = (tot_exp_match_aberdeen/total_exports)*100) %>% 
      ungroup() %>% 
      group_by(year, matched_both) %>% 
      mutate(tot_exp_match_both = sum(export, na.rm = T), 
             share_match_both = (tot_exp_match_both/total_exports)*100) %>% 
      ungroup() %>% 
      filter(matched_builtwith == T, 
             matched_aberdeen  == T, 
             matched_both == T) %>% 
      distinct(year, .keep_all = T) %>% 
      select(Data,
             Year = year,
             Number = total_exports,
             `% Matched Aberdeen` = share_match_aberdeen,
             `% Matched BuiltWith` = share_match_builtwith, 
             `% Matched Both` = share_match_both
      )
  ) %>% 
  mutate(across(where(is.numeric), round, 2), 
         across(c(Number), big_number_format)) %>% 
  mutate(Number = paste0("$", Number)) %>% 
  bind_rows(
    imports_MEX %>% 
      select(company_id, year, matched_builtwith, matched_aberdeen, matched_both) %>% 
      distinct(company_id, year, .keep_all = T) %>% 
      mutate(Data = "Panjiva Imports") %>% 
      group_by(year) %>% 
      mutate(n_firms = n()) %>%
      ungroup() %>% 
      group_by(year, matched_builtwith) %>% 
      mutate(n_match_builtwith = sum(matched_builtwith), 
             share_match_builtwith = (n_match_builtwith/n_firms)*100) %>%
      ungroup() %>%
      group_by(year, matched_aberdeen) %>% 
      mutate(n_match_aberdeen = sum(matched_aberdeen), 
             share_match_aberdeen = (n_match_aberdeen/n_firms)*100) %>% 
      ungroup() %>% 
      group_by(year, matched_both) %>% 
      mutate(n_match_both = sum(matched_both), 
             share_match_both = (n_match_both/n_firms)*100) %>% 
      ungroup() %>% 
      filter(share_match_builtwith != 0, 
             share_match_aberdeen != 0, 
             share_match_both != 0) %>% 
      distinct(n_firms, share_match_builtwith, share_match_aberdeen, share_match_both, .keep_all = T) %>% 
      select(Data,
             Year = year,
             `Number` = n_firms,
             `% Matched Aberdeen` = share_match_aberdeen,
             `% Matched BuiltWith` = share_match_builtwith, 
             `% Matched Both` = share_match_both
      ) %>% 
      bind_rows(
        exports_MEX %>% 
          select(company_id, year, matched_builtwith, matched_aberdeen, matched_both) %>% 
          distinct(company_id, year, .keep_all = T) %>% 
          mutate(Data = "Panjiva Exports") %>% 
          group_by(year) %>% 
          mutate(n_firms = n()) %>% 
          group_by(year, matched_builtwith) %>% 
          mutate(n_match_builtwith = sum(matched_builtwith), 
                 share_match_builtwith = (n_match_builtwith/n_firms)*100) %>%
          ungroup() %>%
          group_by(year, matched_aberdeen) %>% 
          mutate(n_match_aberdeen = sum(matched_aberdeen), 
                 share_match_aberdeen = (n_match_aberdeen/n_firms)*100) %>% 
          ungroup() %>% 
          group_by(year, matched_both) %>% 
          mutate(n_match_both = sum(matched_both), 
                 share_match_both = (n_match_both/n_firms)*100) %>% 
          ungroup() %>% 
          filter(share_match_builtwith != 0, 
                 share_match_aberdeen != 0, 
                 share_match_both != 0) %>% 
          distinct(n_firms, share_match_builtwith, share_match_aberdeen, share_match_both, .keep_all = T) %>% 
          select(Data,
                 Year = year,
                 Number = n_firms,
                 `% Matched Aberdeen` = share_match_aberdeen,
                 `% Matched BuiltWith` = share_match_builtwith, 
                 `% Matched Both` = share_match_both
          )
      ) %>% 
      mutate(across(where(is.numeric), round, 2), 
             across(Number, big_number_format))
  )%>% 
  mutate(Year = paste0(Year, " ", seq_len(16)))%>% 
  tibble::column_to_rownames("Year") %>% 
  select(-Data) %>% 
  kbl(caption = "Mexico - Panjiva total trade value and number of firms matching rates to BuiltWith and Aberdeen datasets by year", 
      centering = T, booktabs = T, align = "c", format = "latex") %>%
  kable_styling(latex_options = c("HOLD_position", "scale_down")) %>% 
  group_rows("Panel A: Matching Rates in Terms of Total Trade Value", 1, 8, hline_after = T) %>%
  group_rows("Import Trade Value", 1, 4, italic = T) %>%
  group_rows("Export Trade Value", 5, 8, italic = T) %>% 
  group_rows("Panel B: Matching Rates in Terms of Number of Firms", 9, 16, hline_after = T) %>%
  group_rows("Importers Number", 9, 12, italic = T) %>%
  group_rows("Exporters Number", 13, 16, italic = T) %>% 
  footnote(general = "", threeparttable = T)



capture.output(table_MEX, file = "../../Outputs/Tables/matching_rates_MEX.tex")




