#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo                                                    #
#                                                                           #
# Script:  gen_data_model_sector_exposure_IDN.R                             #
#                                                                           #
#                                                                           
#                                                                                                                                                    
#                                                                           
#                                                                           
#===========================================================================#

# Set Working Directory ----
#fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
#setwd(fileloc)
#rm(fileloc)

# Libraries to be used ----
#source("../src/packages.R")

# Read NAICS classifications ----
NAICS_code_classifications <- read_csv("../../Data/Indonesia/NAICS_code_classifications.csv")

# CDC Essential Industries ----
CDC_Essential_Industries_CISA <- readxl::read_excel("../../Data/Indonesia/CDC_Essential_Industries_CISAv4_1_24.xlsx", 2) %>% 
  janitor::clean_names() %>% 
  select(naics_code_2017 =x2017_naics_code, philadelphia_fed_cisa_v3_0  ) %>% 
  mutate(Essential_Industry = TRUE)

# Read data with stringency index ----
OxCGRT_timeseries_stringency_index <- readxl::read_excel("../../Data/Indonesia/OxCGRT_timeseries_all.xlsx", sheet ="stringency_index")

covid_data <- OxCGRT_timeseries_stringency_index %>%
  filter(country_name == "Indonesia") %>%
  pivot_longer(
    cols = -c(country_code, country_name),
    names_to = "date",
    values_to = "stringency_index"
  ) %>%
  mutate(date = parse_date_time(date, orders = "%d%b%Y")) %>%
  mutate(month_year = ymd(glue(
    "{year(date)}-{month(date)}-01"
  ))) %>%
  group_by(month_year) %>%
  summarise(month_mean_stringency_index  = mean(stringency_index , na.rm = TRUE))

# BuiltWith data ----
matched_built_with <-
  fread("../../Data/Indonesia/Builtwith_IDN_longformat.csv")

# Firms that adopted technology or not ----
firms_that_did_not_adopt_technologies <- matched_built_with %>% 
  filter(type == "dummy") %>% 
  group_by(New_ID_1 ) %>% 
  summarise(number_of_dummies = sum(value)) %>% 
  filter(number_of_dummies == 0)

# Firms that adopted technology before covid ----
firms_with_website_before_covid_and_indexed_after_June_2021<- matched_built_with %>% 
  filter(type == "dummy") %>% 
  group_by(New_ID_1 ) %>% 
  summarise(FI_min = min(FI),
            LI_max = max(LI)) %>%
  filter(FI_min <  ymd("2019-12-01"),
         LI_max >  ymd("2021-06-30"))

rm(matched_built_with)


# SIC Industries Included in Analysis ----
included_SIC_Groups <- c('AG-M-C', # Agricultural, Construction and Mining
                         "MANUF", # Manufacturing
                         "SVCS", #Services
                         "F-I-RE", # Finance Insurance, Real Estate 
                         "TR-UTL", # Transport & Utilities
                         "WHL-RT" #Wholesale-retaol
)

# Read data with trade, COVID and technology info at the firm level ----
full_data <- arrow::read_parquet("../../Data/Indonesia/master_file_builtwith_updated.parquet",
                                 col_select = c("In_aberdeen","company_id","date", "date_character",
                                                "any_imports_less_than_11_USD", "any_exports_less_than_11_USD",
                                                "NAICS3_CODE", "NAICS6_CODE",
                                                "export_total", "import_total","ecomnod","paynod",
                                                "first_adopted_payrobust", "first_adopted_ecom", "first_adopted_pay",
                                                "payrobustnod","tagnod","SIC_group")) %>%
  filter(In_aberdeen)  %>% 
  # filter(!company_id %in% firms_that_did_not_adopt_technologies$New_ID_1) %>% 
  filter(company_id %in% firms_with_website_before_covid_and_indexed_after_June_2021$New_ID_1) %>% 
  filter(SIC_group %in% included_SIC_Groups,
         !is.na(SIC_group)) %>% 
  mutate(manufacturing_dummy = SIC_group ==  "MANUF") %>% 
  mutate(adopted_pay_or_ecom_before_2019 = first_adopted_payrobust == "before 2019" | first_adopted_ecom == "before 2019") 



# Import trade data ----
import_summary_by_firm <- read_csv("../../Data/Indonesia/import_summary_by_firm.csv") %>% 
  select(company_id, date, date_character,log_import, import_dummy, n_countries_import) 

# Export trade data ----
export_summary_by_firm <- read_csv("../../Data/Indonesia/export_summary_by_firm.csv") %>% 
  select(company_id, date, date_character,log_export, export_dummy, n_countries_export)


# Filters for export and import data ----

# Imports filters
paybustorecom_import_firms <- full_data %>%
  filter(!any_imports_less_than_11_USD |
           is.na(any_imports_less_than_11_USD)) %>% 
  filter(date >= ymd("2019-02-01"),
         date <= ymd("2021-06-01")) %>% 
  filter(import_total >0) %>% 
  mutate(pay_or_ecomnod = ifelse((payrobustnod == 1 | ecomnod == 1),1, 0 )) %>%  
  filter(first_adopted_payrobust != "before 2019" | first_adopted_ecom != "before 2019" ) %>% 
  select(company_id) %>% 
  distinct()

# Exports filter
paybustorecom_export_firms <- full_data %>%
  filter(!any_exports_less_than_11_USD |
           is.na(any_exports_less_than_11_USD)) %>% 
  filter(date >= ymd("2019-02-01"),
         date <= ymd("2021-09-01")) %>% 
  filter(export_total >0) %>% 
  mutate(pay_or_ecomnod = ifelse((payrobustnod == 1 | ecomnod == 1),1, 0 )) %>%  
  filter(first_adopted_payrobust != "before 2019" | first_adopted_ecom != "before 2019" ) %>% 
  select(company_id) %>% 
  distinct()


# Imports data for regressions ----
paybustorecomimport_data_NAICS <- import_summary_by_firm %>%
  filter(company_id %in% paybustorecom_import_firms$company_id) %>%
  left_join(
    full_data %>%
      mutate(pay_or_ecomnod = ifelse((
        payrobustnod == 1 | ecomnod == 1
      ), 1, 0)) %>%
      select(company_id, date_character, pay_or_ecomnod, NAICS3_CODE, NAICS6_CODE, adopted_pay_or_ecom_before_2019)
    ,
    by = c("company_id", "date_character")
  ) %>%
  left_join(
    NAICS_code_classifications,
    by = c("NAICS3_CODE" = "NAICS")
  ) %>%
  left_join(
    CDC_Essential_Industries_CISA,
    by = c("NAICS6_CODE" = "naics_code_2017")
  ) %>% 
  mutate(Essential_Industry = ifelse(is.na(Essential_Industry), FALSE, Essential_Industry)) %>% 
  mutate(philadelphia_fed_cisa_v3_0 = ifelse(is.na(philadelphia_fed_cisa_v3_0), 0, philadelphia_fed_cisa_v3_0)) %>% 
  left_join(covid_data, by= c("date" = "month_year")) %>% 
  mutate(adopted_pay_or_ecom_before_2019 = as.numeric(adopted_pay_or_ecom_before_2019)) %>% 
  mutate(Essential_Industry = as.numeric(Essential_Industry)) %>%
  filter(!is.na(adopted_pay_or_ecom_before_2019)) %>% 
  mutate(month_mean_stringency_index = replace_na(month_mean_stringency_index, 0)) %>% 
  mutate(across(c(teamwork_share,customer_share,presence_share,communication_share,affected_share), ~./100))


# Exports data for regressions ----
paybustorecomexport_data_NAICS <- export_summary_by_firm %>%
  filter(company_id %in% paybustorecom_export_firms$company_id) %>%
  left_join(
    full_data %>%
      mutate(pay_or_ecomnod = ifelse((
        payrobustnod == 1 | ecomnod == 1
      ), 1, 0)) %>%
      select(company_id, date_character, pay_or_ecomnod, NAICS3_CODE, NAICS6_CODE, adopted_pay_or_ecom_before_2019)
    ,
    by = c("company_id", "date_character")
  ) %>%
  left_join(
    NAICS_code_classifications,
    by = c("NAICS3_CODE" = "NAICS")
  ) %>%
  left_join(
    CDC_Essential_Industries_CISA,
    by = c("NAICS6_CODE" = "naics_code_2017")
  ) %>% 
  mutate(Essential_Industry = ifelse(is.na(Essential_Industry), FALSE, Essential_Industry)) %>% 
  mutate(philadelphia_fed_cisa_v3_0 = ifelse(is.na(philadelphia_fed_cisa_v3_0), 0, philadelphia_fed_cisa_v3_0)) %>% 
  left_join(covid_data, by= c("date" = "month_year")) %>% 
  mutate(adopted_pay_or_ecom_before_2019 = as.numeric(adopted_pay_or_ecom_before_2019)) %>% 
  mutate(Essential_Industry = as.numeric(Essential_Industry)) %>%
  filter(!is.na(adopted_pay_or_ecom_before_2019)) %>% 
  mutate(month_mean_stringency_index = replace_na(month_mean_stringency_index, 0)) %>% 
  mutate(across(c(teamwork_share,customer_share,presence_share,communication_share,affected_share), ~./100))

gc()


# Remove objects that are not going to be used anymore
rm(CDC_Essential_Industries_CISA, covid_data, 
   firms_that_did_not_adopt_technologies, firms_with_website_before_covid_and_indexed_after_June_2021,
   full_data, import_summary_by_firm, NAICS_code_classifications, 
   OxCGRT_timeseries_stringency_index, paybustorecom_export_firms, 
   paybustorecom_import_firms, export_summary_by_firm, 
   included_SIC_Groups)


#####################################################



# 
# # NO HE REVISADO ESTO DE AQUI PARA ABAJO...
# paybustorecom_export_NAICS_1_summary <- readRDS("C:/Users/wb581369/OneDrive - WBG/analyse_HS6_level_data/done_on_server/paybustorecom_export_NAICS_1_summary.rds")
# paybustorecom_export_NAICS_2_summary <- readRDS("C:/Users/wb581369/OneDrive - WBG/analyse_HS6_level_data/done_on_server/paybustorecom_export_NAICS_2_summary.rds")
# paybustorecom_export_NAICS_3_summary <- readRDS("C:/Users/wb581369/OneDrive - WBG/analyse_HS6_level_data/done_on_server/paybustorecom_export_NAICS_3_summary.rds")
# paybustorecom_export_NAICS_4_summary <- readRDS("C:/Users/wb581369/OneDrive - WBG/analyse_HS6_level_data/done_on_server/paybustorecom_export_NAICS_4_summary.rds")
# paybustorecom_export_NAICS_5_summary <- readRDS("C:/Users/wb581369/OneDrive - WBG/analyse_HS6_level_data/done_on_server/paybustorecom_export_NAICS_5_summary.rds")
# paybustorecom_export_NAICS_6_summary <- readRDS("C:/Users/wb581369/OneDrive - WBG/analyse_HS6_level_data/done_on_server/paybustorecom_export_NAICS_6_summary.rds")
# paybustorecom_export_NAICS_7_summary <- readRDS("C:/Users/wb581369/OneDrive - WBG/analyse_HS6_level_data/done_on_server/paybustorecom_export_NAICS_7_summary.rds")
# paybustorecom_export_NAICS_8_summary <- readRDS("C:/Users/wb581369/OneDrive - WBG/analyse_HS6_level_data/done_on_server/paybustorecom_export_NAICS_8_summary.rds")
# 
# modelsummary(dvnames(list(paybustorecom_export_NAICS_1_summary,
#                           paybustorecom_export_NAICS_2_summary, 
#                           paybustorecom_export_NAICS_3_summary, 
#                           paybustorecom_export_NAICS_4_summary, 
#                           paybustorecom_export_NAICS_5_summary, 
#                           paybustorecom_export_NAICS_6_summary, 
#                           paybustorecom_export_NAICS_7_summary, 
#                           paybustorecom_export_NAICS_8_summary )),
#              estimate  =   "{estimate} ({std.error}){stars}", 
#              stars = TRUE, 
#              fmt = "%.5f",
#              statistic = NULL)
# 
# 
# 
# paybustorecom_import_NAICS_1_summary <- readRDS("C:/Users/wb581369/OneDrive - WBG/analyse_HS6_level_data/done_on_server/paybustorecom_import_NAICS_1_summary.rds")
# paybustorecom_import_NAICS_2_summary <- readRDS("C:/Users/wb581369/OneDrive - WBG/analyse_HS6_level_data/done_on_server/paybustorecom_import_NAICS_2_summary.rds")
# paybustorecom_import_NAICS_3_summary <- readRDS("C:/Users/wb581369/OneDrive - WBG/analyse_HS6_level_data/done_on_server/paybustorecom_import_NAICS_3_summary.rds")
# paybustorecom_import_NAICS_4_summary <- readRDS("C:/Users/wb581369/OneDrive - WBG/analyse_HS6_level_data/done_on_server/paybustorecom_import_NAICS_4_summary.rds")
# paybustorecom_import_NAICS_5_summary <- readRDS("C:/Users/wb581369/OneDrive - WBG/analyse_HS6_level_data/done_on_server/paybustorecom_import_NAICS_5_summary.rds")
# paybustorecom_import_NAICS_6_summary <- readRDS("C:/Users/wb581369/OneDrive - WBG/analyse_HS6_level_data/done_on_server/paybustorecom_import_NAICS_6_summary.rds")
# paybustorecom_import_NAICS_7_summary <- readRDS("C:/Users/wb581369/OneDrive - WBG/analyse_HS6_level_data/done_on_server/paybustorecom_import_NAICS_7_summary.rds")
# paybustorecom_import_NAICS_8_summary <- readRDS("C:/Users/wb581369/OneDrive - WBG/analyse_HS6_level_data/done_on_server/paybustorecom_import_NAICS_8_summary.rds")
# 
# modelsummary(dvnames(list(paybustorecom_import_NAICS_1_summary,
#                           paybustorecom_import_NAICS_2_summary, 
#                           paybustorecom_import_NAICS_3_summary, 
#                           paybustorecom_import_NAICS_4_summary, 
#                           paybustorecom_import_NAICS_5_summary, 
#                           paybustorecom_import_NAICS_6_summary, 
#                           paybustorecom_import_NAICS_7_summary, 
#                           paybustorecom_import_NAICS_8_summary )),
#              estimate  =   "{estimate} ({std.error}){stars}", 
#              stars = TRUE, 
#              fmt = "%.5f",
#              statistic = NULL)
# 
