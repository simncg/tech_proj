#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  gen_data_model_products_IND.R                                    #
#                                                                           #
# This program generates the India data necessary to run the regressions    #
# at the firm-month level.                                                  #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")

# Read data with stringency index which measures severity of government restrictions ----
OxCGRT_timeseries_stringency_index <- readxl::read_excel("../../Data/Extra Data/OxCGRT_timeseries_all.xlsx", sheet ="stringency_index"   )

# Get mean stringency index during Covid period
covid_data <- OxCGRT_timeseries_stringency_index %>%
  filter(country_name == "India") %>%
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


# Firms to be excluded: courier firms that are likely transporting goods for individuals ----
courier_firms <- read_dta("../../Data/India/raw_data/IND_iscourier_domesticid.dta")

# Imports data ----

# Firms IDs to be replaced to the old firm IDS because they are different in the latest version of Panjiva
new_ids<-c("IND164276", "IND37751", "IND171809", "IND12967", "IND253050") # IDs to be replaced
old_ids<-c("IND38927", "IND41278", "IND6043", "IND23040", "IND46989")     # Old IDs (the ones we need)

# Firms that import yearly less than 1000 usd in the years they appear  
import_less_than_1000usd<-read_csv("../../Data/India/processed_data/import_firms_trade_less_than_1000usd.csv")

import_data <-fread("../../Data/India/processed_data/import_summaries_by_firm_month_complete.csv") %>% 
  select(company_id = domestic_company_id, year, month, date, date_character, 
         import, log_import, import_dummy, n_countries_import = n_countries,
         new_source = new_country, n_hs6_products) %>% 
  # filter to analysis period
  filter(date >= ymd("2018-07-01"),
         date <= ymd("2021-12-31")) %>% 
  #Drop firms that imported less than 1000 usd in all years between 2018-2021
  left_join(import_less_than_1000usd, by = c("company_id", "year")) %>% 
  filter(!less_than_1000usd) %>% 
  select(-less_than_1000usd, -sum_import, -n_years) %>% 
  # Replace some id firms that changed with the latest version of Panjiva 
  mutate(company_id = if_else(company_id %in% new_ids, old_ids[match(company_id, new_ids)], company_id))%>% 
  # Drop courier firms that are likely transporting goods for individuals
  filter(!(company_id %in% courier_firms$domestic_id))

gc()

# Exports data ----

# Firms that export yearly less than 1000 usd in the years they appear  
export_less_than_1000usd<-read_csv("../../Data/India/processed_data/export_firms_trade_less_than_1000usd.csv")


export_data <-fread("../../Data/India/processed_data/export_summaries_by_firm_month_complete.csv") %>% 
  select(company_id = domestic_company_id, year, month, date, date_character, export, 
         log_export, export_dummy, n_countries_export = n_countries, 
         new_destination = new_country, n_hs6_products) %>% 
  # filter to analysis period
  filter(date >= ymd("2018-07-01"),
         date <= ymd("2021-12-31")) %>% 
  # Drop firms that exported less than 1000 usd in all years between 2018-2021
  left_join(export_less_than_1000usd, by = c("company_id", "year"))%>% 
  filter(!less_than_1000usd) %>% 
  select(-less_than_1000usd, -sum_export, -n_years) %>% 
  # Replace some id firms that changed with the latest version of Panjiva 
  mutate(company_id = if_else(company_id %in% new_ids, old_ids[match(company_id, new_ids)], company_id))%>% 
  # Drop courier firms that are likely transporting goods for individuals
  filter(!(company_id %in% courier_firms$domestic_id))

gc()


# Load technology data ----
tech_data <- read_parquet("../../Data/India/processed_data/tech_data_IND.parquet")

gc()


# Data for regressions of the model that measures if tech adoption affects trade outcomes ----


# Data for regressions are at the transaction level (firm-month-hs6 code)
# Join data for those observations that are matched to both BuiltWith and Aberdeen data
# We will analyze E-commerce (ecomnod) and E-payment technologies (payrobust)


# Imports data 
pay_ecom_import_data_IND<-import_data %>% 
  # Select just the bare minimum variables
  select(company_id, year, month, date_character, log_import, import, import_dummy, 
         n_countries_import, new_source, n_hs6_products) %>% 
  # Add the builtwith technology data 
  left_join(tech_data %>% 
              select(-LI, -FI),
            by = c("company_id", "date_character")
  ) %>% 
  # Drop firms that adopted e-commerce/e-payments technology before period of analysis
  filter(first_adopted_payrobust != "before july 2018" & 
           first_adopted_ecom != "before july 2018") %>%
  # Order data
  arrange(company_id, date) 



gc()



# Exports data 
pay_ecom_export_data_IND<-export_data %>% 
  # Select just the bare minimum variables
  select(company_id, year, month, date_character, log_export, export, export_dummy,
         new_destination, n_countries_export, n_hs6_products) %>% 
  # Add the builtwith technology data 
  left_join(tech_data %>% 
              select(-LI, -FI),
            by = c("company_id", "date_character")) %>% 
  # Drop firms that adopted e-commerce/e-payments technology before period of analysis
  filter(first_adopted_payrobust != "before july 2018" & 
           first_adopted_ecom != "before july 2018") %>%
  # Order data
  arrange(company_id, date) 

gc()



# Data for regressions of the model that measures if tech adoption mitigates COVID impacts on trade outcomes ----

# Imports data
import_tech_mitig_IND<- import_data %>% 
  # Select just the bare minimum variables
  select(company_id, year, month, date, date_character, log_import, import, import_dummy, 
         n_countries_import, new_source, n_hs6_products) %>% 
  # Add COVID data to have stringency index 
  mutate(date = as.Date(date)) %>% 
  left_join(covid_data, by= c("date" = "month_year")) %>% 
  # Join tech data
  left_join(tech_data %>% 
              select(-LI, -FI, -date), 
            by = c('company_id', "date_character")) %>% 
  # Modify existing variables
  mutate(adopted_pay_or_ecom_before_2020 = as.numeric(adopted_pay_or_ecom_before_2020)) %>% 
  # Fill monthly stringency index with 0s in the pre-covid period 
  mutate(month_mean_stringency_index = ifelse(is.na(month_mean_stringency_index), 0, month_mean_stringency_index))






# Exports data
export_tech_mitig_IND<- export_data %>% 
  # Select just the bare minimum variables
  select(company_id, year, month, date, date_character, log_export, export,
         export_dummy, n_countries_export, new_destination, n_hs6_products) %>% 
  # Add COVID data to have stringency index 
  mutate(date = as.Date(date)) %>% 
  left_join(covid_data, by= c("date" = "month_year")) %>% 
  # Join tech data
  left_join(tech_data %>% 
              select(-LI, -FI, -date), 
            by = c('company_id', "date_character")) %>% 
  # Modify existing variables
  mutate(adopted_pay_or_ecom_before_2020 = as.numeric(adopted_pay_or_ecom_before_2020)) %>% 
  # Fill monthly stringency index with 0s in the pre-covid period 
  mutate(month_mean_stringency_index = ifelse(is.na(month_mean_stringency_index), 0, month_mean_stringency_index))


# Remove no longer needed objects
rm(export_data, import_data, tech_data, export_less_than_1000usd, import_less_than_1000usd, 
   covid_data, OxCGRT_timeseries_stringency_index, new_ids, old_ids, courier_firms)

# Save data ---- 
write_csv(pay_ecom_import_data_IND, "../../Data/India/processed_data/imports_reg_firm_month_IND.csv")
write_csv(pay_ecom_export_data_IND, "../../Data/India/processed_data/exports_reg_firm_month_IND.csv")
write_csv(import_tech_mitig_IND, "../../Data/India/processed_data/imports_tech_mitigation_reg_firm_month_IND.csv")
write_csv(export_tech_mitig_IND, "../../Data/India/processed_data/exports_tech_mitigation_reg_firm_month_IND.csv")


gc()
