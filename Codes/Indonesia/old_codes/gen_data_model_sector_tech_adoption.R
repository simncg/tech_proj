#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo                                                    #
#                                                                           #
# Script:  gen_data_model_sector_tech_adoption_IDN.R                        #
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


NAICS_code_classifications <- read_csv("../../Data/Indonesia/NAICS_code_classifications.csv")

matched_built_with <-
  fread("../../Data/Indonesia/Builtwith_IDN_longformat.csv")

firms_that_did_not_adopt_technologies <- matched_built_with %>% 
  filter(type == "dummy") %>% 
  group_by(New_ID_1 ) %>% 
  summarise(number_of_dummies = sum(value)) %>% 
  filter(number_of_dummies == 0)


firms_with_website_before_covid_and_indexed_after_June_2021<- matched_built_with %>% 
  filter(type == "dummy") %>% 
  group_by(New_ID_1 ) %>% 
  summarise(FI_min = min(FI),
            LI_max = max(LI)) %>%
  filter(FI_min <  ymd("2019-12-01"),
         LI_max >  ymd("2021-06-30"))

rm(matched_built_with)



included_SIC_Groups <- c('AG-M-C', # Agricultural, Construction and Mining
                         "MANUF", # Manufacturing
                         "SVCS", #Services
                         "F-I-RE", # Finance Insurance, Real Estate 
                         "TR-UTL", # Transport & Utilities
                         "WHL-RT" #Wholesale-retaol
)


full_data <- arrow::read_parquet("../../Data/Indonesia/master_file_builtwith_updated.parquet",
                                 col_select = c("In_aberdeen","company_id","date", "date_character",
                                                "any_imports_less_than_11_USD", "any_exports_less_than_11_USD",
                                                "import_dummy", "log_import", "log_import_missings",
                                                "export_dummy", "log_export", "log_export_missings",
                                                "n_countries_imports", "n_countries_export",
                                                "export_total", "import_total","ecomnod","paynod",
                                                "NAICS3_CODE", "NAICS6_CODE",
                                                "first_adopted_payrobust", "first_adopted_ecom", "first_adopted_pay",
                                                "payrobustnod","tagnod","cloudnod", "google_ookla_d_speed_at_firm",
                                                "month_mean_stringency_index", "multinational","number_of_employees",
                                                "log_number_of_employees", "log_revenue", "log_age","SIC_group" )) %>%
  filter(In_aberdeen)  %>% 
  # filter(!company_id %in% firms_that_did_not_adopt_technologies$New_ID_1) %>% 
  filter(company_id %in% firms_with_website_before_covid_and_indexed_after_June_2021$New_ID_1) %>% 
  filter(SIC_group %in% included_SIC_Groups,
         !is.na(SIC_group)) %>% 
  mutate(manufacturing_dummy = SIC_group ==  "MANUF")


import_summary_by_firm <- read_csv("../../Data/Indonesia/import_summary_by_firm.csv") %>% 
  select(company_id, date, date_character,log_import, import_dummy, n_countries_import) 

export_summary_by_firm <- read_csv("../../Data/Indonesia/export_summary_by_firm.csv") %>% 
  select(company_id, date, date_character,log_export, export_dummy, n_countries_export)



paybustorecom_import_firms <-full_data %>%
  filter(!any_imports_less_than_11_USD |
           is.na(any_imports_less_than_11_USD)) %>% 
  filter(date >= ymd("2019-02-01"),
         date <= ymd("2021-06-01")) %>% 
  filter(import_total >0) %>% 
  mutate(pay_or_ecom_nod = ifelse((payrobustnod == 1 | ecomnod == 1),1, 0 )) %>%  
  filter(first_adopted_payrobust != "before 2019" | first_adopted_ecom != "before 2019" ) %>% 
  select(company_id) %>% 
  distinct()




paybustorecomimport_data_NAICS_2_IDN <- import_summary_by_firm %>%
  filter(company_id %in% paybustorecom_import_firms$company_id) %>%
  left_join(
    full_data %>%
      mutate(pay_or_ecom_nod = ifelse((
        payrobustnod == 1 | ecomnod == 1
      ), 1, 0)) %>%
      select(company_id, date_character, pay_or_ecom_nod, NAICS3_CODE, NAICS6_CODE)
    ,
    by = c("company_id", "date_character")
  ) %>%
  left_join(
    NAICS_code_classifications,
    by = c("NAICS3_CODE" = "NAICS")
  ) %>% 
  mutate(across(c(teamwork_share,customer_share,presence_share,communication_share,affected_share), ~./100))




paybustorecom_export_firms <-full_data %>%
  filter(!any_exports_less_than_11_USD |
           is.na(any_exports_less_than_11_USD)) %>% 
  filter(date >= ymd("2019-02-01"),
         date <= ymd("2021-09-01")) %>% 
  filter(export_total >0) %>% 
  mutate(pay_or_ecom_nod = ifelse((payrobustnod == 1 | ecomnod == 1),1, 0 )) %>%  
  filter(first_adopted_payrobust != "before 2019" | first_adopted_ecom != "before 2019" ) %>% 
  select(company_id) %>% 
  distinct()




paybustorecomexport_data_NAICS_2_IDN <- export_summary_by_firm %>%
  filter(company_id %in% paybustorecom_export_firms$company_id) %>%
  left_join(
    full_data %>%
      mutate(pay_or_ecom_nod = ifelse((
        payrobustnod == 1 | ecomnod == 1
      ), 1, 0)) %>%
      select(company_id, date_character, pay_or_ecom_nod, NAICS3_CODE, NAICS6_CODE)
    ,
    by = c("company_id", "date_character")
  ) %>%
  left_join(
    NAICS_code_classifications,
    by = c("NAICS3_CODE" = "NAICS")
  ) %>% 
  mutate(across(c(teamwork_share,customer_share,presence_share,communication_share,affected_share), ~./100))


gc()
