#===========================================================================#
# Date:    May 2023                                                         #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  gen_data_model_products_MEX.R                                    #
#                                                                           #
# This program generates the data necessary to run the regressions with     # 
# different products categories specifications (eBay tradable,              #
# China e-commerce, BEC classification, Time-Sensitivity, Capital and       # 
# Intermediate products) for Mexico. The first model measures if technology  #              
# adoption affects trade outcomes. The second model measures if tech adoption#                                                                         
# before 2019 helps to mitigate the impacts of COVID on trade outcomes      #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")

# Load HS Code Classification data with products information ----
hs_data <- read_csv("../../Data/Extra Data/HS_code_classifications.csv")%>% 
  mutate(hs_2017   = str_pad(hs_2017  , 6, "left", "0"))

# Read data with intermediate and capital HS products classification ----
int_cap_HS <- read_excel("../../Data/Extra Data/capital_intermediate_HS_code_classification.xlsx", 
                         sheet = "FromHSToISICToEC") %>% 
  # Keep only HS 2012 version
  filter(HS == 4) %>% 
  # Keep only the variable with the HS6 codes and the variable EUC with information of the type of product
  select(hs6 =`HS-6digit`, EUC) %>% 
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
  pivot_wider(names_from = EUC, values_from = value, values_fill = 0) %>% 
  # Keep only the identifiers variables for intermediate and capital products
  select(hs_2017, CAP, INT) %>% 
  # From dummy to logical 
  mutate(CAP = as.logical(CAP), 
         INT = as.logical(INT))

gc()


# Read data with stringency index which measures severity of government restrictions ----
OxCGRT_timeseries_stringency_index <- readxl::read_excel("../../Data/Extra Data/OxCGRT_timeseries_all.xlsx", sheet ="stringency_index"   )

# Get mean stringency index during Covid period
covid_data <- OxCGRT_timeseries_stringency_index %>%
  filter(country_name == "Mexico") %>%
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


# Firms to be excluded: courier firms that are likely transporting goods for individuals
courier_firms <- read_dta("../../Data/Mexico/raw_data/MEX_iscourier_domesticid.dta")

# Imports data ----

# Firms that import yearly less than 1000 usd in the years they appear  
import_less_than_1000usd<-read_csv("../../Data/Mexico/processed_data/import_firms_trade_less_than_1000usd.csv")

import_data <-read_parquet("../../Data/Mexico/processed_data/import_summaries_by_firm_month_HS_code_complete.parquet", 
                           col_select = c("domestic_company_id", "year", "month", "date", "date_character", "hs6", 
                                          "import", "log_import", "import_dummy", "new_country", "n_countries")) %>% 
  rename(company_id = domestic_company_id, new_source = new_country, n_countries_import = n_countries) %>% 
  # Filter to period of analysis
  filter(date >= ymd("2018-07-01"),
         date <= ymd("2021-12-31")) %>% 
  mutate(hs6  = str_pad(as.character(hs6) , 6, "left", "0")) %>% 
  #Drop firms that imported less than 1000 usd in all years between 2019-2021
  left_join(import_less_than_1000usd, by = c("company_id", "year")) %>% 
  filter(!less_than_1000usd) %>% 
  select(-less_than_1000usd, -sum_import, -n_years) %>% 
  # Drop courier firms that are likely transporting goods for individuals
  filter(!(company_id %in% courier_firms$domestic_firm_id))

gc()

# Exports data ----

# Firms that export yearly less than 1000 usd in the years they appear  
export_less_than_1000usd<-read_csv("../../Data/Mexico/processed_data/export_firms_trade_less_than_1000usd.csv")


export_data <-read_parquet("../../Data/Mexico/processed_data/export_summaries_by_firm_month_HS_code_complete.parquet", 
                           col_select = c("domestic_company_id", "year", "month", "date", "date_character", "hs6", 
                                          "export", "log_export", "export_dummy", "new_country", "n_countries")) %>% 
  rename(company_id = domestic_company_id, new_destination = new_country, n_countries_export = n_countries) %>% 
  # filter to analysis period
  filter(date >= ymd("2018-07-01"),
         date <= ymd("2021-12-31")) %>% 
  mutate(hs6  = str_pad(as.character(hs6) , 6, "left", "0")) %>% 
  # Drop firms that exported less than 1000 usd in all years between 2019-2021
  left_join(export_less_than_1000usd, by = c("company_id", "year"))%>% 
  filter(!less_than_1000usd) %>% 
  select(-less_than_1000usd, -sum_export, -n_years) %>% 
  # Drop courier firms that are likely transporting goods for individuals
  filter(!(company_id %in% courier_firms$domestic_firm_id))

gc()



# Identify GVC firms: We define a GVC firm as a firm that imported and exported during the 2019 ----
GVC_firms<-
  # Use exports data
  export_data %>% 
  # Filter by year 2019
  filter(year(date) == 2019) %>% 
  # Distinct in terms of firms IDs to identify companies that exported during this year
  distinct(company_id) %>% 
  # Join with firms that imported during 2019
  inner_join(
    # Use imports data
    import_data %>% 
      # Filter by year 2010
      filter(year(date) == 2019) %>% 
      # Distinct in terms of firms IDs to identify companies that imported during this year
      distinct(company_id),
    by = "company_id"
  ) %>% 
  mutate(GVC_firm = 1)


# Filter for GVC firms in exports data
export_data<- 
  export_data %>% 
  # Left join GVC firm identifier
  left_join(GVC_firms, by = "company_id") %>% 
  # Keep only observations of GVC firms
  filter(GVC_firm == 1)

# Filter for GVC firms in imports data
import_data<-
  import_data %>% 
  # Left join GVC firm identifier
  left_join(GVC_firms, by = "company_id") %>% 
  # Keep only observations of GVC firms
  filter(GVC_firm == 1)



# Load technology data  ----
tech_data <- read_parquet("../../Data/Mexico/processed_data/tech_data_MEX.parquet")


# Data for regressions of the model that measures if tech adoption affects trade outcomes ----

# Data for regressions are at the transaction level (firm-month-hs6 code)
# Join data for those observations that are matched to both BuiltWith and Aberdeen data
# We will analyze E-commerce (ecomnod) and E-payment technologies (payrobust)


# Imports data 
pay_ecom_import_data_MEX<-import_data %>% 
  # Select just the bare minimum variables
  select(company_id, year, month, date_character, hs6, log_import, import, import_dummy,
         n_countries_import, new_source) %>% 
  # Add the HS code classification data with products information 
  left_join(hs_data, by = c('hs6' = 'hs_2017')) %>% 
  # Add the intermediate - capital products classification information
  left_join(int_cap_HS, by = c('hs6' = 'hs_2017')) %>% 
  # Add the builtwith technology data 
  left_join(tech_data %>% 
              select(-LI, -FI),
            by = c("company_id", "date_character")
  ) %>% 
  # Drop firms that adopted e-commerce/e-payments technology before period of analysis
  filter(first_adopted_payrobust != "before july 2018" & 
           first_adopted_ecom != "before july 2018") %>%
  # Order data
  arrange(company_id, hs6, date) %>% 
  # Rename variable related to remote work ISIC
  rename(#mean_remote_work_ISIC = mean_MEX, 
    letter_credit_use = LCInt)



gc()



# Exports data 
pay_ecom_export_data_MEX<-export_data %>% 
  # Select just the bare minimum variables
  select(company_id, year, month, date_character, hs6, log_export, export, export_dummy, 
         n_countries_export, new_destination) %>% 
  # Add the HS code classification data with products information 
  left_join(hs_data, by = c('hs6' = 'hs_2017')) %>% 
  # Add the intermediate - capital products classification information
  left_join(int_cap_HS, by = c('hs6' = 'hs_2017')) %>% 
  # Add the builtwith technology data 
  left_join(tech_data %>% 
              select(-LI, -FI),
            by = c("company_id", "date_character")) %>% 
  # Drop firms that adopted e-commerce/e-payments technology before period of analysis
  filter(first_adopted_payrobust != "before july 2018" & 
           first_adopted_ecom != "before july 2018") %>%
  # Order data
  arrange(company_id, hs6, date) %>% 
  #rename(mean_remote_work_ISIC = mean_MEX)%>% 
  rename(letter_credit_use = LCInt)

gc()



# Data for regressions of the model that measures if tech adoption mitigates COVID impacts on trade outcomes ----

# Imports data
import_tech_mitig<- import_data %>% 
  # Select just the bare minimum variables
  select(company_id, year, month, date, date_character, hs6, log_import, import, import_dummy,
         new_source, n_countries_import) %>% 
  # Add the HS code classification data with products information 
  left_join(hs_data, by = c('hs6' = 'hs_2017')) %>% 
  # Add the intermediate - capital products classification information
  left_join(int_cap_HS, by = c('hs6' = 'hs_2017')) %>% 
  # Add COVID data to have stringency index 
  mutate(date = as.Date(date)) %>% 
  left_join(covid_data, by= c("date" = "month_year")) %>% 
  # Join tech data
  left_join(tech_data %>% 
              select(-LI, -FI, -date), 
            by = c('company_id', "date_character")) %>% 
  # Modify existing variables
  mutate(Ebay_tradable = as.numeric(Ebay_tradable), 
         adopted_pay_or_ecom_before_2020 = as.numeric(adopted_pay_or_ecom_before_2020)) %>% 
  # Rename some variables
  rename(#mean_remote_work_ISIC = mean_MEX, 
    letter_credit_use = LCInt)%>% 
  # Fill monthly stringency index with 0s in the pre-covid period 
  mutate(month_mean_stringency_index = ifelse(is.na(month_mean_stringency_index), 0, month_mean_stringency_index))






# Exports data
export_tech_mitig<- export_data %>% 
  # Select just the bare minimum variables
  select(company_id, year, month, date, date_character, hs6, log_export, export, export_dummy,
         new_destination, n_countries_export) %>% 
  # Add the HS code classification data with products information 
  left_join(hs_data, by = c('hs6' = 'hs_2017')) %>% 
  # Add the intermediate - capital products classification information
  left_join(int_cap_HS, by = c('hs6' = 'hs_2017')) %>% 
  # Add COVID data to have stringency index 
  mutate(date = as.Date(date)) %>% 
  left_join(covid_data, by= c("date" = "month_year")) %>% 
  # Join tech data
  left_join(tech_data %>% 
              select(-LI, -FI, -date), 
            by = c('company_id', "date_character")) %>% 
  # Modify existing variables
  mutate(Ebay_tradable = as.numeric(Ebay_tradable), 
         adopted_pay_or_ecom_before_2020 = as.numeric(adopted_pay_or_ecom_before_2020)) %>% 
  # Rename some variables
  rename(#mean_remote_work_ISIC = mean_MEX, 
    letter_credit_use = LCInt) %>% 
  # Fill monthly stringency index with 0s in the pre-covid period 
  mutate(month_mean_stringency_index = ifelse(is.na(month_mean_stringency_index), 0, month_mean_stringency_index))


# Remove no longer needed objects
rm(export_data, hs_data, import_data, tech_data, export_less_than_1000usd, 
   int_cap_HS, included_SIC_Groups, import_less_than_1000usd, covid_data, 
   OxCGRT_timeseries_stringency_index, courier_firms)

# Save data ---- 
write_csv(pay_ecom_import_data_MEX, "../../Data/Mexico/processed_data/imports_product_model_MEX.csv")
write_csv(pay_ecom_export_data_MEX, "../../Data/Mexico/processed_data/exports_product_model_MEX.csv")
write_csv(import_tech_mitig, "../../Data/Mexico/processed_data/imports_tech_mitigation_model_MEX.csv")
write_csv(export_tech_mitig, "../../Data/Mexico/processed_data/exports_tech_mitigation_model_MEX.csv")


gc()
