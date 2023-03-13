#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  gen_data_tech_mitigation_covid_IDN.R                             #
#                                                                           #
# This program generates the data necessary to run the regressions of the   #
# model that shows whether the existing technology use mitigate covid       #
# impacts.                                                                  #
#                                                                           #                                                                         
#                                                                           #
#                                                                           #           -                                                                                                                                                  #
#===========================================================================#

# Set Working Directory ----
# fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(fileloc)
# rm(fileloc)


# Load libraries ----
#source("../src/packages.R")

# Read HS Code Classification data with products information ----
HS_code_classifications <- read_csv("../../Data/Indonesia/HS_code_classifications.csv")%>% 
  mutate(hs_2017   = str_pad(hs_2017  , 6, "left", "0"))

# Read data with stringency index which measures severity of government restrictions
OxCGRT_timeseries_stringency_index <- readxl::read_excel("../../Data/Indonesia/OxCGRT_timeseries_all.xlsx", sheet ="stringency_index"   )

# Get mean stringency index during Covid period
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

# Read BuiltWith Information for Indonesia ----
matched_built_with <-
  fread("../../Data/Indonesia/Builtwith_IDN_longformat.csv")


# Firms that had website before Covid and last time observed after June 2021
firms_with_website_before_covid_and_indexed_after_June_2021<- matched_built_with %>% 
  filter(type == "dummy") %>% 
  group_by(New_ID_1 ) %>% 
  summarise(FI_min = min(FI),
            LI_max = max(LI)) %>%
  filter(FI_min <  ymd("2019-12-01"),
         LI_max >  ymd("2021-06-30"))

# Remove builtwith information
rm(matched_built_with)

# SIC groups to analyze ----

included_SIC_Groups <- c('AG-M-C', # Agricultural, Construction and Mining
                         "MANUF", # Manufacturing
                         "SVCS", #Services
                         "F-I-RE", # Finance Insurance, Real Estate 
                         "TR-UTL", # Transport & Utilities
                         "WHL-RT" #Wholesale-retaol
)

# Read data at the firm-product level ----

# Data with information  of trade, Panjiva, Aberdeen, stringency index 
# We create dummy variables that identify whether a company had a technology before 2019 or not.
full_data <- arrow::read_parquet("../../Data/Indonesia/master_file_builtwith_updated.parquet",
                                 col_select = c("In_aberdeen","company_id","date", "date_character",
                                                "any_imports_less_than_11_USD", "any_exports_less_than_11_USD",
                                                "NAICS3_CODE", "NAICS6_CODE",
                                                "export_total", "import_total","ecomnod","paynod",
                                                "first_adopted_payrobust", "first_adopted_ecom", "first_adopted_pay",
                                                "payrobustnod","tagnod","SIC_group")) %>%
  filter(In_aberdeen)  %>% 
  filter(company_id %in% firms_with_website_before_covid_and_indexed_after_June_2021$New_ID_1) %>% 
  filter(SIC_group %in% included_SIC_Groups,
         !is.na(SIC_group)) %>% 
  mutate(manufacturing_dummy = SIC_group ==  "MANUF") %>% 
  mutate(adopted_pay_or_ecom_before_2019 = first_adopted_payrobust == "before 2019" | first_adopted_ecom == "before 2019") 


# Data of imports by firm-hs code-month ----
import_summary_by_HS_code <- read_csv("../../Data/Indonesia/import_summary_by_firm_month_HS_code.csv") %>% 
  select(company_id, date, date_character,hs6,log_import, import_dummy, n_countries_import)%>% 
  mutate(hs6  = str_pad(hs6 , 6, "left", "0"))

# Obtain firms that imported more than 11 USD ----
paybustorecom_import_firms <- full_data %>%
  filter(!any_imports_less_than_11_USD |
           is.na(any_imports_less_than_11_USD)) %>% 
  filter(date >= ymd("2019-02-01"),
         date <= ymd("2021-06-01")) %>% 
  filter(import_total >0) %>% 
  select(company_id) %>% 
  distinct()



# Data of exports by firm-hs6 code-month ----
export_summary_by_HS_code <- read_csv("../../Data/Indonesia/export_summary_by_firm_month_HS_code.csv") %>% 
  select(company_id, date, date_character,hs6,log_export, export_dummy, n_countries_export)%>% 
  mutate(hs6  = str_pad(hs6 , 6, "left", "0"))

# Obtain firms that exported more than 11 USD ----
paybustorecom_export_firms <- full_data %>%
  filter(!any_exports_less_than_11_USD |
           is.na(any_exports_less_than_11_USD)) %>% 
  filter(date >= ymd("2019-02-01"),
         date <= ymd("2021-09-01")) %>% 
  filter(export_total >0) %>% 
  select(company_id) %>% 
  distinct()


# Prepare data to run regressions ----

# Import data
paybustorecomimport_data_IDN <- import_summary_by_HS_code %>%
  filter(company_id %in% paybustorecom_import_firms$company_id) %>%
  left_join(
    full_data %>%
      select(company_id, date_character, adopted_pay_or_ecom_before_2019)
    ,
    by = c("company_id", "date_character")
  ) %>%
  left_join(
    HS_code_classifications,
    by = c("hs6" = "hs_2017")
  )  %>% 
  left_join(covid_data, by= c("date" = "month_year"))%>% 
  mutate(month_mean_stringency_index = replace_na(month_mean_stringency_index, 0)) %>% 
  mutate(Ebay_tradable = as.numeric(Ebay_tradable)) %>% 
  mutate(adopted_pay_or_ecom_before_2019 = as.numeric(adopted_pay_or_ecom_before_2019))%>% 
  rename(mean_IDN_remote_work_ISIC = mean_IDN)%>% 
  rename(letter_credit_use = LCInt)

# Export data
paybustorecomexport_data_IDN <- export_summary_by_HS_code %>%
  filter(company_id %in% paybustorecom_export_firms$company_id) %>%
  left_join(
    full_data %>%
      select(company_id, date_character, adopted_pay_or_ecom_before_2019)
    ,
    by = c("company_id", "date_character")
  ) %>%
  left_join(
    HS_code_classifications,
    by = c("hs6" = "hs_2017")
  )  %>% 
  left_join(covid_data, by= c("date" = "month_year"))%>% 
  mutate(month_mean_stringency_index = replace_na(month_mean_stringency_index, 0)) %>% 
  mutate(Ebay_tradable = as.numeric(Ebay_tradable)) %>% 
  mutate(adopted_pay_or_ecom_before_2019 = as.numeric(adopted_pay_or_ecom_before_2019))%>% 
  rename(mean_IDN_remote_work_ISIC = mean_IDN)%>% 
  rename(letter_credit_use = LCInt)

# Remove data not to be used onwards
rm(export_summary_by_HS_code, 
   firms_with_website_before_covid_and_indexed_after_June_2021, 
   import_summary_by_HS_code,
   OxCGRT_timeseries_stringency_index,
   full_data, 
   HS_code_classifications, 
   included_SIC_Groups,
   covid_data, 
   paybustorecom_export_firms, 
   paybustorecom_import_firms)

gc()
