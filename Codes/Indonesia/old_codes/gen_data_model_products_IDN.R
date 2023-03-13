#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  gen_data_model_products_IDN.R                                    #
#                                                                           #
# This program generates the data necessary to run the regressions of the   #
# model with different products categories specifications (eBay tradable,   #
# China e-commerce, BEC classification, Time-Sensitivity) for Indonesia     #                 
#                                                                           #                                                                         
#                                                                           #
#                                                                           #           -                                                                                                                                                  #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)


# Load libraries ----
source("../src/packages.R")


# Read HS Code Classification data with products information ----
HS_code_classifications <- read_csv("../../Data/Indonesia/HS_code_classifications.csv")%>% 
  mutate(hs_2017   = str_pad(hs_2017  , 6, "left", "0"))

# Read data with intermediate and capital HS products classification ----
int_cap_HS <- read_excel("../../Data/Extra Data/capital_intermediate_HS_code_classification.xlsx", 
                         sheet = "FromHSToISICToEC") %>% 
  # Keep only HS 2012 version
  filter(HS == 4) %>% 
  # Keep only the variable with the HS6 codes and the variable EUC with information of the type of product
  select(hs6 =`HS-6digit`, EUC) %>% 
  # Keep only the HS6 codes of intermediate and capital products
  filter(EUC %in% c("CAP", "INT")) %>% 
  # Join with correspondence tables to convert from HS codes 2012 to HS codes 2017 
  left_join(., read_xlsx("../../Data/Extra Data/HS2017toHS2012ConversionAndCorrelationTables.xlsx") %>% 
              rename( "hs_2017" = "From HS 2017",  "hs_2012"="To HS 2012"), 
            by = c("hs6" = "hs_2012")) %>% 
  select(hs_2017, EUC) %>% 
  # Create an indicator variable 
  mutate(value = 1) %>% 
  # Remove na in hs codes 2017
  na.omit(hs_2017) %>% 
  # Long format to wide format: convert CAP and INT to two separate variables
  pivot_wider(names_from = EUC, values_from = value, values_fill = 0)

# Read BuiltWith Information for Indonesia ----
matched_built_with <-fread("../../Data/Indonesia/Builtwith_IDN_longformat.csv")

# Firms that had website before Covid and last time observed after June 2021
firms_with_website_before_covid_and_indexed_after_June_2021<- matched_built_with %>% 
  filter(type == "dummy") %>% 
  group_by(New_ID_1 ) %>% 
  summarise(FI_min = min(FI),
            LI_max = max(LI)) %>%
  filter(FI_min <  ymd("2019-02-01"),
         LI_max >  ymd("2021-06-30"))

# Indonesia start and end of the indonesia data.  


# India start and end of the India data. 
# %>%
#   filter(FI_min <  ymd("2018-07-01"),
#          LI_max >  ymd("2021-12-31"))

# Remove builtwith information
rm(matched_built_with)

# SIC groups to analyze ----
included_SIC_Groups <- c('AG-M-C', # Agricultural, Construction and Mining
                         "MANUF",  # Manufacturing
                         "SVCS",   #Services
                         "F-I-RE", # Finance Insurance, Real Estate 
                         "TR-UTL", # Transport & Utilities
                         "WHL-RT"  #Wholesale-retaol
                         )

# Read data at the firm-month level ----

# Data with information of trade, Panjiva, Aberdeen and Covid variables  
full_data <- arrow::read_parquet("../../Data/Indonesia/master_file_builtwith_updated.parquet",
                                 col_select = c("In_aberdeen","company_id","date", "date_character",
                                                "any_imports_less_than_11_USD", "any_exports_less_than_11_USD",
                                                "import_dummy", "log_import", "log_import_missings",
                                                "export_dummy", "log_export", "log_export_missings",
                                                "n_countries_imports", "n_countries_export",
                                                "export_total", "import_total","ecomnod","paynod",
                                                "first_adopted_payrobust", "first_adopted_ecom", "first_adopted_pay",
                                                "payrobustnod","tagnod","cloudnod", "google_ookla_d_speed_at_firm",
                                                "month_mean_stringency_index", "multinational","number_of_employees",
                                                "log_number_of_employees", "log_revenue", "log_age","SIC_group")) %>%
  
  filter(In_aberdeen, 
         company_id %in% firms_with_website_before_covid_and_indexed_after_June_2021$New_ID_1, 
         SIC_group %in% included_SIC_Groups,
         !is.na(SIC_group)) %>% 
  mutate(manufacturing_dummy = SIC_group == "MANUF") %>% 
  # Create lagged structure for technology variables
  arrange(company_id, date) %>% 
  group_by(company_id) %>% 
  mutate(across(contains('nod'), list(t_1 = function(x){lag(x, 1)}, 
                                      t_2 = function(x){lag(x, 2)}, 
                                      t_3 = function(x){lag(x, 3)}))) %>% 
  mutate(pay_or_ecomnod = ifelse((payrobustnod == 1 | ecomnod == 1),1, 0 ),
         # Create lagged variables for e-commerce or e-payment
         pay_or_ecomnod_t_1 = ifelse((payrobustnod_t_1 == 1 | ecomnod_t_1 == 1),1, 0),
         pay_or_ecomnod_t_2 = ifelse((payrobustnod_t_2 == 1 | ecomnod_t_2 == 1),1, 0),
         pay_or_ecomnod_t_3 = ifelse((payrobustnod_t_3 == 1 | ecomnod_t_3 == 1),1, 0))
  
  
# Read imports data at the firm-month-HS6 code level ----

# First read data with variable that indicates if a firm import less than 1000 usd in both 2019 
# and 2020. Also includes a variable that indicates if a firm, that is only in 2020, imports
# less than 1000 usd in this year. Also, another variable that does the same but for 2019.  
imports_less_than_1000usd<-read_csv("../../Data/Indonesia/processed_data/import_firms_trade_less_than_1000usd.csv")

# Read imports data at the firm-month-HS6 level
import_summary_by_HS_code <- read_csv("../../Data/Indonesia/import_summary_by_firm_month_HS_code.csv") %>% 
  select(company_id, date, date_character,hs6,log_import, import, import_dummy, n_countries_import)%>% 
  mutate(hs6  = str_pad(hs6 , 6, "left", "0"), 
         year = year(date)) %>% 
  # Drop firms that imported less than 1000 usd in both 2019 and 2020
  # Drop firms that only appears in 2019 and imported less than 1000 usd in this year
  # Drop firms that only appears in 2020 and imported less than 1000 usd in this year
  left_join(imports_less_than_1000usd, 
            by = c("company_id", "year")) %>% 
  filter(!less_than_1000USD_import_19_20, 
         !less_than_1000USD_import_2019, 
         !less_than_1000USD_import_2020) %>% 
  # Select variables to be analyzed 
  select(-year, -less_than_1000USD_import_19_20, -n_years,
         -less_than_1000USD_import_2020, 
         -less_than_1000USD_import_2019)

# Read exports data at the firm-month-HS6 code level ----


# First read data with variable that indicates if a firm export less than 1000 usd in both 2019 
# and 2020. Also includes a variable that indicates if a firm that only appears in 2020 exports
# less than 1000 usd in this year. Also, another variable that does the same but for 2019. 

exports_less_than_1000usd<-read_csv("../../Data/Indonesia/processed_data/export_firms_trade_less_than_1000usd.csv")


# Read exports data at the firm-month-HS6 level
export_summary_by_HS_code <- read_csv("../../Data/Indonesia/export_summary_by_firm_month_HS_code.csv") %>% 
  select(company_id, date, date_character,hs6,log_export, export, export_dummy, n_countries_export ) %>% 
  mutate(hs6  = str_pad(hs6 , 6, "left", "0"), 
         year = year(date)) %>% 
  # Drop firms that in a particular year export less than 1000 usd
  left_join(exports_less_than_1000usd, 
            by = c("company_id", "year")) %>% 
  filter(!less_than_1000USD_export_19_20,
         !less_than_1000USD_export_2019, 
         !less_than_1000USD_export_2020) %>% 
  # Select variables to be analyzed 
  select(-year, -less_than_1000USD_export_19_20)



# Obtain IDs of firms that adopted robust payments and e-commerce after 2019 ----
paybustorecom_import_firms <-full_data %>%
  filter(date >= ymd("2019-02-01"),
         date <= ymd("2021-06-01")) %>% 
  filter(first_adopted_payrobust != "before 2019" &  
         first_adopted_ecom != "before 2019" ) %>% 
  select(company_id, first_adopted_payrobust, first_adopted_ecom) %>% 
  distinct(company_id, .keep_all = T)


# Exports: total exports > 0 and value of exports > 11 USD
paybustorecom_export_firms <-full_data %>%
  filter(date >= ymd("2019-02-01"),
         date <= ymd("2021-09-01")) %>% 
  filter(first_adopted_payrobust != "before 2019" & #|
         first_adopted_ecom != "before 2019") %>% 
  select(company_id, first_adopted_payrobust, first_adopted_ecom) %>% 
  distinct(company_id, .keep_all = T)


# Prepare data to run regressions ----

# Imports data at the firm-month-HS code level with trade, robust payment,
# e-commerce technologies and product classifications variables 
paybustorecomimport_data_IDN <- import_summary_by_HS_code %>%
  filter(company_id %in% paybustorecom_import_firms$company_id) %>%
  left_join(
    full_data %>%
      select(company_id, date_character, pay_or_ecomnod, pay_or_ecomnod_t_1,
             pay_or_ecomnod_t_2,  pay_or_ecomnod_t_3),
    by = c("company_id", "date_character")
  ) %>%
  left_join(
    HS_code_classifications,
    by = c("hs6" = "hs_2017")
  )%>% 
  left_join(
    int_cap_HS, 
    by = c("hs6" = "hs_2017")
  ) %>% 
  mutate_at(c('CAP','INT'), ~replace_na(.,0)) %>% 
  mutate(month = month(date))%>% 
  rename(mean_remote_work_ISIC = mean_IDN)%>% 
  rename(letter_credit_use = LCInt)


# Exports data at the firm-month-HS code level with trade, robust payment,
# e-commerce technologies and product classifications variables 
paybustorecomexport_data_IDN <- export_summary_by_HS_code %>%
  filter(company_id %in% paybustorecom_export_firms$company_id) %>%
  left_join(
    full_data %>%
      select(company_id, date_character, pay_or_ecomnod, pay_or_ecomnod_t_1,
             pay_or_ecomnod_t_2,  pay_or_ecomnod_t_3),
    by = c("company_id", "date_character")
  ) %>%
  left_join(
    HS_code_classifications,
    by = c("hs6" = "hs_2017")
  ) %>% 
  left_join(
    int_cap_HS, 
    by = c("hs6" = "hs_2017")
  ) %>% 
  mutate_at(c('CAP','INT'), ~replace_na(.,0)) %>% 
  mutate(month = month(date)) %>% 
  rename(mean_remote_work_ISIC = mean_IDN)%>% 
  rename(letter_credit_use = LCInt)


# Remove data not to be used onwards
rm(export_summary_by_HS_code, 
   firms_with_website_before_covid_and_indexed_after_June_2021, 
   import_summary_by_HS_code,
   paybustorecom_export_firms,
   paybustorecom_import_firms,
   full_data, 
   HS_code_classifications, 
   included_SIC_Groups, 
   int_cap_HS, 
   imports_less_than_1000usd, 
   exports_less_than_1000usd)


gc()

