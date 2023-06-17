#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# 
#                                                                           #
#                                                                           #                                                                                                                                                  #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")


# Load HS Code Classification data with products information ----
hs_data <- read_csv("../../Data/Extra Data/HS_code_classifications.csv")%>% 
  mutate(hs_2017   = str_pad(hs_2017  , 6, "left", "0")) %>% 
  rename(hs6 = hs_2017, letter_credit_use = LCInt, mean_remote_work_ISIC = mean_IND) %>% 
  select(-mean_IDN)


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
         INT = as.logical(INT)) %>% 
  rename(hs6 = hs_2017)

gc()


# Obtain HS6 codes per each HS4 codes  ---- 
hs4_hs6 <- read_csv("../../Data/Extra Data/HS_code_classifications.csv")%>% 
  mutate(hs_2017  = str_pad(hs_2017  , 6, "left", "0")) %>% 
  rename(hs6 = hs_2017) %>% 
  # Add column with the HS4
  mutate(hs4 =  substr(hs6, 1, 4)) %>% 
  # Keep KS4 
  select(hs6, hs4) %>% 
  relocate(hs4, hs6) 




# Read data with stringency index which measures severity of government restrictions -----
OxCGRT_timeseries_stringency_index <- readxl::read_excel("../../Data/Extra Data/OxCGRT_timeseries_all.xlsx", sheet ="stringency_index")

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
  summarise(month_mean_stringency_index  = mean(stringency_index , na.rm = TRUE)) %>% 
  mutate(date_character = as.character(fct_reorder(as.factor(format(month_year, "%Y-%b")), month_year))) %>% 
  select(-month_year)


# Read tech data ---- 
tech_data_IND<-read_parquet("../../Data/India/processed_data/tech_data_IND.parquet", 
                            col_select = c("company_id", "date", "date_character", "pay_or_ecomnod_t_1",
                                           "pay_or_ecomnod_t_2", "pay_or_ecomnod_t_3", "adopted_pay_or_ecom_before_2019", 
                                           "months_since_adoption", "adopter_type", "covid_adopter_type","old_adopter_type"))


# Grids for model that measures if tech adoption affects trade outcomes ---- 

## Exports data for model that measures if tech adoption affects trade outcomes (after processed in gen_data_model_products_IND.R) ----
exports_data_IND<- fread("../../Data/India/processed_data/exports_product_model_IND.csv") %>% 
  arrange(company_id, date, hs6) %>% 
  mutate(hs6  = str_pad(hs6, 6, "left", "0"), 
         hs4 = substr(hs6, 1, 4)) %>% 
  relocate(date, hs6, hs4, company_id) %>% 
  # Convert date variable to date type
  mutate(date = as.Date(date)) 

## Create all possible combinations between firms and FEASIBLE HS6 based on the HS4 code ----
grid_firm_hs6_exp_IND<- exports_data_IND %>% 
  select(company_id, hs4) %>% 
  # When a firm exports an HS6 product that belongs to a specific HS4 code, an observation is created for the corresponding firm-HS4 pair.
  distinct() %>% 
  # Expand the dataset to include all possible combinations between the firm and every HS6 code within the HS4 code that the firm exported.
  left_join(hs4_hs6, by = c("hs4")) %>% 
  mutate(firm_hs6 = paste0(company_id, "-", hs6)) %>% 
  select(firm_hs6) 

## Exports grid ----

# All possible combinations between firms, FEASIBLE HS6 and months (2018-07 - 2021-12).  
# This is the data to be used in the propensity exports regressions of the model that measures if 
# tech adoption affects trade outcomes

exports_grid_IND<- 
  CJ(firm_feas_hs6 = grid_firm_hs6_exp_IND$firm_hs6, 
     date = unique(exports_data_IND$date)) %>% 
  separate(col = firm_feas_hs6, into = c("company_id", "hs6"), sep = "-") %>% 
  # Join export dummy variable by company, month, hs6
  left_join(exports_data_IND %>% 
              select(company_id, date, hs6, export_dummy),
            by = c('company_id', "date", "hs6")) %>% 
  # If missing, then product not imported by the company in that month
  mutate(export_dummy = ifelse(is.na(export_dummy), FALSE, export_dummy)) %>%
  # Join technology data of firms 
  left_join(tech_data_IND, by = c("company_id", "date")) %>% 
  # Join HS products information 
  left_join(hs_data , by = c("hs6")) %>% 
  # Join HS producs information of capital and intermediate goods
  left_join(int_cap_HS, by = c("hs6"))%>% 
  # Order data
  arrange(company_id, date_character, hs6)

# Save exports grid in parquet file
write_parquet(exports_grid_IND, "../../Data/India/processed_data/exports_grid_product_model_IND.parquet")
rm(exports_grid_IND, grid_firm_hs6_exp_IND, exports_data_IND) # Remove objects to free space 
gc()

## Imports data for model that measures if tech adoption affects trade outcomes (after processed in gen_data_model_products_IND.R) ----
imports_data_IND<- fread("../../Data/India/processed_data/imports_product_model_IND.csv") %>% 
  arrange(company_id, date, hs6) %>% 
  mutate(hs6  = str_pad(hs6, 6, "left", "0"), 
         hs4 = substr(hs6, 1, 4)) %>% 
  relocate(date, hs6, hs4, company_id) %>% 
  # Convert date variable to date type
  mutate(date = as.Date(date)) 

## Create all possible combinations between firms and FEASIBLE HS6 based on the HS4 code -----
grid_firm_hs6_imp_IND<- imports_data_IND %>% 
  select(company_id, hs4) %>% 
  # When a firm exports an HS6 product that belongs to a specific HS4 code, an observation is created for the corresponding firm-HS4 pair.
  distinct() %>% 
  # Expand the dataset to include all possible combinations between the firm and every HS6 code within the HS4 code that the firm exported.
  left_join(hs4_hs6, by = c("hs4")) %>% 
  mutate(firm_hs6 = paste0(company_id, "-", hs6)) %>% 
  select(firm_hs6) 

## Imports grid ----

## All possible combinations between firms, FEASIBLE HS6 and months (2018-07 - 2021-12).  
## This is the data to be used in the propensity exports regressions of the model that measures if 
## tech adoption affects trade outcomes

imports_grid_IND<- 
  CJ(firm_feas_hs6 = grid_firm_hs6_imp_IND$firm_hs6, 
     date = unique(imports_data_IND$date)) %>% 
  separate(col = firm_feas_hs6, into = c("company_id", "hs6"), sep = "-") %>% 
  # Join import dummy variable by company, month, hs6
  left_join(imports_data_IND %>% 
              select(company_id, date, hs6, import_dummy),
            by = c('company_id', "date", "hs6")) %>% 
  # If missing, then product not imported by the company in that month
  mutate(import_dummy = ifelse(is.na(import_dummy), FALSE, import_dummy)) %>%
  # Join technology data of firms 
  left_join(tech_data_IND, by = c("company_id", "date")) %>% 
  # Join HS products information 
  left_join(hs_data , by = c("hs6")) %>% 
  # Join HS producs information of capital and intermediate goods
  left_join(int_cap_HS, by = c("hs6"))%>% 
  # Order data
  arrange(company_id, date_character, hs6)

# Save imports grid in parquet file
write_parquet(imports_grid_IND, "../../Data/India/processed_data/imports_grid_product_model_IND.parquet")
rm(imports_grid_IND, grid_firm_hs6_imp_IND, imports_data_IND) # Remove objects to free space 
gc()



# Grids for model that measures if tech adoption mitigated COVID impacts ---- 

## Read exports data for regressions of the model that measures if tech adoption mitigated COVID impacts ----
exports_data_mitig_IND <-fread("../../Data/India/processed_data/exports_tech_mitigation_model_IND.csv") %>%
  arrange(company_id, date, hs6) %>% 
  mutate(hs6  = str_pad(hs6, 6, "left", "0"), 
         hs4 = substr(hs6, 1, 4)) %>% 
  relocate(date, hs6, hs4, company_id) %>% 
  # Convert date variable to date type
  mutate(date = as.Date(date)) 

## Create all possible combinations between firms and FEASIBLE HS6 based on the HS4 code ----
grid_firm_hs6_exp_mitig_IND<- exports_data_mitig_IND %>% 
  select(company_id, hs4) %>% 
  # When a firm exports an HS6 product that belongs to a specific HS4 code, an observation is created for the corresponding firm-HS4 pair.
  distinct() %>% 
  # Expand the dataset to include all possible combinations between the firm and every HS6 code within the HS4 code that the firm exported.
  left_join(hs4_hs6, by = c("hs4")) %>% 
  mutate(firm_hs6 = paste0(company_id, "-", hs6)) %>% 
  select(firm_hs6)

## Exports grid ----

# All possible combinations between firms, FEASIBLE HS6 and months (2018-07 - 2021-12).  
# This is the data to be used in the propensity exports regressions of the model that measures if 
# tech adoption mitigated COVID impacts

exports_grid_mitig_IND<- 
  CJ(firm_feas_hs6 = grid_firm_hs6_exp_mitig_IND$firm_hs6, 
     date = unique(exports_data_mitig_IND$date)) %>% 
  separate(col = firm_feas_hs6, into = c("company_id", "hs6"), sep = "-") %>% 
  # Join export dummy variable by company, month, hs6
  left_join(exports_data_mitig_IND %>% 
              select(company_id, date, hs6, export_dummy),
            by = c('company_id', "date", "hs6")) %>% 
  # If missing, then product not imported by the company in that month
  mutate(export_dummy = ifelse(is.na(export_dummy), FALSE, export_dummy)) %>%
  # Join technology data of firms 
  left_join(tech_data_IND, by = c("company_id", "date")) %>% 
  # Join HS products information 
  left_join(hs_data , by = c("hs6")) %>% 
  # Join HS producs information of capital and intermediate goods
  left_join(int_cap_HS, by = c("hs6")) %>% 
  # Join mean stringency index data
  left_join(covid_data, by = c("date_character")) %>% 
  # Fill missing values in monthly stringency index: pre-covid period fill it with 0s
  mutate(month_mean_stringency_index = ifelse(is.na(month_mean_stringency_index), 0, month_mean_stringency_index)) %>%
  # Order data
  arrange(company_id, date_character, hs6)

  
# Save imports grid in parquet file
write_parquet(exports_grid_mitig_IND, "../../Data/India/processed_data/exports_grid_mitig_model_IND.parquet")
rm(exports_grid_mitig_IND, grid_firm_hs6_exp_mitig_IND, exports_data_mitig_IND) # Remove objects to free space 
gc()



## Read imports data for regressions of the model that measures if tech adoption mitigated COVID impacts ----
imports_data_mitig_IND <-fread("../../Data/India/processed_data/imports_tech_mitigation_model_IND.csv") %>%
  arrange(company_id, date, hs6) %>% 
  mutate(hs6  = str_pad(hs6, 6, "left", "0"), 
         hs4 = substr(hs6, 1, 4)) %>% 
  relocate(date, hs6, hs4, company_id) %>% 
  # Convert date variable to date type
  mutate(date = as.Date(date)) 

## Create all possible combinations between firms and FEASIBLE HS6 based on the HS4 code ----
grid_firm_hs6_imp_mitig_IND<- imports_data_mitig_IND %>% 
  select(company_id, hs4) %>% 
  # When a firm exports an HS6 product that belongs to a specific HS4 code, an observation is created for the corresponding firm-HS4 pair.
  distinct() %>% 
  # Expand the dataset to include all possible combinations between the firm and every HS6 code within the HS4 code that the firm exported.
  left_join(hs4_hs6, by = c("hs4")) %>% 
  mutate(firm_hs6 = paste0(company_id, "-", hs6)) %>% 
  select(firm_hs6) 

## Imports grid ----

# All possible combinations between firms, FEASIBLE HS6 and months (2018-07 - 2021-12).  
# This is the data to be used in the propensity imports regressions of the model that measures if 
# tech adoption mitigated COVID impacts

imports_grid_mitig_IND<- 
  CJ(firm_feas_hs6 = grid_firm_hs6_imp_mitig_IND$firm_hs6, 
     date = unique(imports_data_mitig_IND$date)) %>% 
  separate(col = firm_feas_hs6, into = c("company_id", "hs6"), sep = "-") %>% 
  # Join import dummy variable by company, month, hs6
  left_join(imports_data_mitig_IND %>% 
              select(company_id, date, hs6, import_dummy),
            by = c('company_id', "date", "hs6")) %>% 
  # If missing, then product not imported by the company in that month
  mutate(import_dummy = ifelse(is.na(import_dummy), FALSE, import_dummy)) %>%
  # Join technology data of firms 
  left_join(tech_data_IND, by = c("company_id", "date")) %>% 
  # Join HS products information 
  left_join(hs_data , by = c("hs6")) %>% 
  # Join HS producs information of capital and intermediate goods
  left_join(int_cap_HS, by = c("hs6")) %>% 
  # Join mean stringency index data
  left_join(covid_data, by = c("date_character")) %>% 
  # Fill missing values in monthly stringency index: pre-covid period fill it with 0s
  mutate(month_mean_stringency_index = ifelse(is.na(month_mean_stringency_index), 0, month_mean_stringency_index)) %>%
  # Order data
  arrange(company_id, date_character, hs6)


# Save imports grid in parquet file
write_parquet(imports_grid_mitig_IND, "../../Data/India/processed_data/imports_grid_mitig_model_IND.parquet")
rm(imports_grid_mitig_IND, grid_firm_hs6_imp_mitig_IND, imports_data_mitig_IND) # Remove objects to free space 
gc()



