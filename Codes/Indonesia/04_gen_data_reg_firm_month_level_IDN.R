#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
#                                                                           #
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

# Read data with stringency index, which measures severity of government restrictions ----
OxCGRT_timeseries_stringency_index <- readxl::read_excel("../../Data/Extra Data/OxCGRT_timeseries_all.xlsx", sheet ="stringency_index"   )

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


# Read imports data ----

# First read data with variable that indicates if a firm import less than 1000 usd in both 2019 
# and 2020. Also includes a variable that indicates if a firm, that is only in 2020, imports
# less than 1000 usd in this year. Also, another variable that does the same but for 2019.  
imports_less_than_1000usd<-read_csv("../../Data/Indonesia/processed_data/import_firms_trade_less_than_1000usd.csv")

# Read imports data at the firm-month-HS6 level
import_data <- read_csv("../../Data/Indonesia/processed_data/import_summary_by_firm_month.csv") %>% 
  select(company_id, date, date_character, year, month, log_import, import, import_dummy, 
         n_countries_import = n_countries, n_hs6_products)%>% 
  mutate(year = year(date)) %>% 
  # Drop firms that imported less than 1000 usd in both 2019 and 2020
  # Drop firms that only appears in 2019 and imported less than 1000 usd in this year
  # Drop firms that only appears in 2020 and imported less than 1000 usd in this year
  left_join(imports_less_than_1000usd, 
            by = c("company_id", "year")) %>% 
  filter(!less_than_1000USD_import_19_20, 
         !less_than_1000USD_import_2019, 
         !less_than_1000USD_import_2020) %>% 
  # Select variables to be analyzed 
  select(-less_than_1000USD_import_19_20, -n_years,
         -less_than_1000USD_import_2020, 
         -less_than_1000USD_import_2019,
         -sum_import)

# Read exports data ----


# First read data with variable that indicates if a firm export less than 1000 usd in both 2019 
# and 2020. Also includes a variable that indicates if a firm that only appears in 2020 exports
# less than 1000 usd in this year. Also, another variable that does the same but for 2019. 

exports_less_than_1000usd<-read_csv("../../Data/Indonesia/processed_data/export_firms_trade_less_than_1000usd.csv")


# Read exports data at the firm-month-HS6 level
export_data <- read_csv("../../Data/Indonesia/processed_data/export_summary_by_firm_month.csv") %>% 
  select(company_id, date, date_character, year, month, log_export, export, export_dummy, 
         n_countries_export = n_countries, n_hs6_products) %>% 
  mutate(year = year(date)) %>% 
  # Drop firms that in a particular year export less than 1000 usd
  left_join(exports_less_than_1000usd, 
            by = c("company_id", "year")) %>% 
  filter(!less_than_1000USD_export_19_20,
         !less_than_1000USD_export_2019, 
         !less_than_1000USD_export_2020) %>% 
  # Select variables to be analyzed 
  select( -less_than_1000USD_export_19_20, 
         -less_than_1000USD_export_2020, less_than_1000USD_export_2019, 
         -n_years, -sum_export, 
         -less_than_1000USD_export_2019)



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
      # Filter by year 2019
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



# Read technology data for Indonesia ----

tech_data<- read_parquet("../../Data/Indonesia/processed_data/tech_data_IDN.parquet")


# Data for regressions of the model that measures if tech adoption affects trade outcomes ----


# Data for regressions are at the firm-month level
# We will analyze E-commerce (ecomnod) and E-payment technologies (payrobust)


# Imports data 
pay_ecom_import_data_IDN<-import_data %>% 
  # Select just the bare minimum variables
  select(company_id, date_character, year, month, import, log_import, import_dummy, 
         n_countries_import, n_hs6_products) %>% 
  # Add the builtwith technology data 
  left_join(tech_data %>% 
              select(-LI, -FI),
            by = c("company_id", "date_character")
  ) %>% 
  # Drop firms that adopted e-commerce/e-payments technology before period of analysis
  filter(first_adopted_payrobust != "before february 2019" & 
           first_adopted_ecom != "before february 2019") %>%
  # Order data
  arrange(company_id, date)


gc()


# Exports data 
pay_ecom_export_data_IDN<-export_data %>% 
  # Select just the bare minimum variables
  select(company_id,date_character, year, month, export, log_export, export_dummy,
         n_countries_export, n_hs6_products) %>% 
  # Add the builtwith technology data 
  left_join(tech_data %>% 
              select(-LI, -FI),
            by = c("company_id", "date_character")) %>% 
  # Drop firms that adopted e-commerce/e-payments technology before period of analysis
  filter(first_adopted_payrobust != "before february 2019" & 
           first_adopted_ecom != "before february 2019") %>%
  # Order data
  arrange(company_id, date) 

gc()



# Data for regressions of the model that measures if tech adoption mitigates COVID impacts on trade outcomes ----

# Imports data
import_tech_mitig<- import_data %>% 
  # Select just the bare minimum variables
  select(company_id, date, year, month, date_character, import, log_import, import_dummy,
         n_countries_import, n_hs6_products) %>% 
  # Add COVID data to have stringency index 
  mutate(date = as.Date(date)) %>% 
  left_join(covid_data, by= c("date" = "month_year")) %>% 
  # Join tech data
  left_join(tech_data  %>% 
              select(-LI, -FI, -date), 
            by = c('company_id', "date_character")) %>% 
  # Modify existing variables
  mutate(adopted_pay_or_ecom_before_2020 = as.numeric(adopted_pay_or_ecom_before_2020)) %>% 
  # Fill monthly stringency index with 0s in the pre-covid period 
  mutate(month_mean_stringency_index = ifelse(is.na(month_mean_stringency_index), 0, month_mean_stringency_index))






# Exports data
export_tech_mitig<- export_data %>% 
  # Select just the bare minimum variables
  select(company_id, date, year, month, date_character, export, log_export, export_dummy,
         n_countries_export, n_hs6_products) %>% 
  # Add COVID data to have stringency index 
  mutate(date = as.Date(date)) %>% 
  left_join(covid_data, by= c("date" = "month_year")) %>% 
  # Join tech data
  left_join(tech_data %>% 
              select(-LI, -FI,  -date), 
            by = c('company_id', "date_character")) %>% 
  # Modify existing variables
  mutate(adopted_pay_or_ecom_before_2020 = as.numeric(adopted_pay_or_ecom_before_2020)) %>% 
  # Fill monthly stringency index with 0s in the pre-covid period 
  mutate(month_mean_stringency_index = ifelse(is.na(month_mean_stringency_index), 0, month_mean_stringency_index))


# Remove no longer needed objects
rm(export_data, import_data, tech_data, exports_less_than_1000usd,
   imports_less_than_1000usd, covid_data, 
   OxCGRT_timeseries_stringency_index)

# Save data ---- 
write_csv(pay_ecom_import_data_IDN, "../../Data/Indonesia/processed_data/imports_reg_firm_month_IDN.csv")
write_csv(pay_ecom_export_data_IDN, "../../Data/Indonesia/processed_data/exports_reg_firm_month_IDN.csv")
write_csv(import_tech_mitig, "../../Data/Indonesia/processed_data/imports_tech_mitigation_reg_firm_month_IDN.csv")
write_csv(export_tech_mitig, "../../Data/Indonesia/processed_data/exports_tech_mitigation_reg_firm_month_IDN.csv")


gc()

