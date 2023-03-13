#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# This script generates the data necessary to run the extensive margin      #
# analysis                                                                  #
#                                                                           #
#                                                                           #
#                                                                           #           -                                                                                                                                                  #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)


# Libraries to be used ----
source("../src/packages.R")

# HS data ----

# Since we cannot create counts of the products classification variables Letter Credit Use, 
# Mean Remote Work ISIC, Relationship Stickiness and Fraction inputs not sold on exchange, 
# We take the data with just the products and their classifications (hs_data) and we
# identify the products with values of these variables above the median and create
# dummies indicating whether the product is above the median or not. Then, we join 
# this information to the data at the firm-month-product level and count the number of
# products traded above the median and the number of products traded below the median. 
hs_data <- read_csv("../../Data/Extra Data/HS_code_classifications.csv")%>% 
  mutate(hs_2017   = str_pad(hs_2017  , 6, "left", "0")) %>% 
  select(hs6 = hs_2017, letter_credit_use = LCInt, mean_remote_work_ISIC = mean_IND,
         relationship_stickiness, frac_lib_diff) %>% 
  mutate(across(-hs6, ~ median(.,na.rm = T) < ., 
                .names = 'above_median_{col}')) %>% 
  mutate(hs6 = as.integer(hs6)) %>% 
  # Keep only variables indicating whether above the median or not
  select(hs6, contains("above"))


# Get number of country partners by month-firm. ---

# When computing the number of partner countries per firm-month we will do it only
# for firm-month where all rows have the partner country not missing. For firm-month
# where some of the rows is missing it is best to not compute this outcome because 
# it will be incomplete and wrong.

# Imports partner countries
import_partner_country<-read_dta("../../Data/India/raw_data/imports_collapsed_dom_hs6_onlyvalue.dta")%>%
  filter(!str_starts(hs6 , "27"))%>% 
  select(company_id = domestic_id, year, month,  hs6, import, country = foreign_country_cleaned_iso3) %>% 
  mutate(hs6 = substr(hs6, 1, 6), 
         date = as.Date(paste0(year, "-", month, "-01")), 
         country = ifelse(country == "", NA, country)) %>%
  filter(date >= ymd("2018-07-01"), 
         date <= ymd("2021-12-31")) %>% 
  select(-year, -month) %>% 
  group_by(company_id, date) %>%
  # Check if all rows have the country partner by firm-month 
  mutate(non_na_count = sum(!is.na(country)), 
         count_obs = n(), 
         all_non_na = non_na_count == count_obs) %>% 
  # Keep only firm-month groups that all rows have the country partner
  filter(all_non_na) %>% 
  # Group by firm-month to count number of partner countries
  group_by(company_id, date) %>% 
  summarise(n_country_partners = n_distinct(country)) %>%
  mutate(date = as.IDate(date)) %>% 
  ungroup()

gc()

# Exports partner countries
export_partner_country<-read_dta("../../Data/India/raw_data/exports_collapsed_dom_hs6_onlyvalue.dta")%>%
  filter(!str_starts(hs6 , "27"))%>% 
  select(company_id = domestic_id, year, month,  hs6, export, country = foreign_country_cleaned_iso3) %>% 
  mutate(hs6 = substr(hs6, 1, 6), 
         date = as.Date(paste0(year, "-", month, "-01")), 
         country = ifelse(country == "", NA, country)) %>%
  filter(date >= ymd("2018-07-01"), 
         date <= ymd("2021-12-31")) %>% 
  select(-year, -month) %>% 
  group_by(company_id, date) %>%
  # Check if all rows have the country partner by firm-month 
  mutate(non_na_count = sum(!is.na(country)), 
         count_obs = n(), 
         all_non_na = non_na_count == count_obs) %>% 
  # Keep only firm-month groups that all rows have the country partner
  filter(all_non_na) %>% 
  # Group by firm-month to count number of partner countries
  group_by(company_id, date) %>% 
  summarise(n_country_partners = n_distinct(country)) %>%
  mutate(date = as.IDate(date)) %>% 
  ungroup()

gc()

# Data for model that measures if tech adoption affects trade outcomes ---- 

# Imports data for model that measures if tech adoption affects trade outcomes (after processed in gen_data_model_products_IND.R) ----
imports_data_IND<- fread("../../Data/India/processed_data/imports_product_model_IND.csv") %>% 
  arrange(company_id, date, hs6) %>% 
  # Join variables that indicate if value of the variables product for letter_credit_use,
  # mean_remote_work_ISIC, relationship_stickiness and frac_lib_diff is above the median  
  left_join(hs_data, by = "hs6") %>% 
  # Join number of country partners by firm-month
  left_join(import_partner_country, by = c("company_id", "date")) %>% 
  mutate(date = as.Date(date))

# Exports data for model that measures if tech adoption affects trade outcomes  (after processed in gen_data_model_products_IND.R) ----
exports_data_IND<- fread("../../Data/India/processed_data/exports_product_model_IND.csv") %>% 
  arrange(company_id, date, hs6) %>% 
  # Join variables that indicate if value of the variables product for letter_credit_use,
  # mean_remote_work_ISIC, relationship_stickiness and frac_lib_diff is above the median  
  left_join(hs_data, by = "hs6") %>% 
  # Join number of country partners by firm-month
  left_join(export_partner_country, by = c("company_id", "date"))%>% 
  mutate(date = as.Date(date))

# Data for model that measures if tech adoption helps to mitigate covid impacts ---- 

# Imports 
imports_data_mitig_IND<- fread("../../Data/India/processed_data/imports_tech_mitigation_model_IND.csv") %>% 
  # Join variables that indicate if value of the variables product for letter_credit_use,
  # mean_remote_work_ISIC, relationship_stickiness and frac_lib_diff is above the median  
  left_join(hs_data, by = "hs6") %>% 
  # Join number of country partners by firm-month
  left_join(import_partner_country, by = c("company_id", "date")) %>% 
  mutate(date = as.Date(date))


# Exports 
exports_data_mitig_IND<- fread("../../Data/India/processed_data/exports_tech_mitigation_model_IND.csv") %>% 
  # Join variables that indicate if value of the variables product for letter_credit_use,
  # mean_remote_work_ISIC, relationship_stickiness and frac_lib_diff is above the median  
  left_join(hs_data, by = "hs6") %>% 
  # Join number of country partners by firm-month
  left_join(export_partner_country, by = c("company_id", "date")) %>% 
  mutate(date = as.Date(date))



#rm(import_partner_country, export_partner_country)
gc()


# Tech data ----
tech_data <- read_parquet("../../Data/India/processed_data/tech_data_IND.parquet", 
                              col_select = c("company_id", "date", "date_character", "pay_or_ecomnod_t_1",
                                             "pay_or_ecomnod_t_2", "pay_or_ecomnod_t_3", 
                                             "adopted_pay_or_ecom_before_2019"))


gc()

# Create data to capture extensive margin ----


# Function to blow-up dataset and join outcomes 
data_extensive_margin<-function(df, covid_string_index = FALSE){
  
  df_blown_up<-
    # Blown-up dataset to have an observation for each firm in each month, regardless of 
    # whether the firm traded in that month or not.
    CJ(company_id = df$company_id, date = df$date, unique = TRUE)  %>% 
    # Join outcomes 
    left_join(df %>% 
                select(company_id, hs6, date, Ebay_tradable, contains('China'), contains("BEC"), 
                       component, fresh, frozen, time_sens_hs2013_russ_typeofgoods, hummels_timesensitive, 
                       agric_timesensitive, CAP, INT, contains("above")) %>% 
                # Pivot longer variables of product category
                pivot_longer(cols = -c(company_id, hs6, date), 
                             names_to = "hs6_category", 
                             values_to = "hs6_category_value") %>% 
                mutate(hs6_category_value = as.logical(hs6_category_value)) %>% 
                # Count number of traded products by firm-month-product category (#e-commerce products, #non-ecommerce products, #frozen products, etc.)
                group_by(company_id, date, hs6_category, hs6_category_value) %>% 
                summarize(n_products_cat = n()) %>% 
                ungroup() %>% 
                left_join(
                  # Count number of traded products by firm-month
                  df %>% 
                    group_by(company_id, date) %>% 
                    summarize(n_products = n()) %>% 
                    ungroup(), 
                  by = c("date", "company_id")
                ) %>% 
                # Pivot wider to have a dataset at the firm-month level 
                pivot_wider(names_from = c(hs6_category, hs6_category_value), 
                            values_from = n_products_cat, 
                            values_fill = 0,      
                            names_sort = T) %>% 
                # Remove columns counting the number of NA cases 
                select(-ends_with("NA")), 
              by = c("company_id", "date")) %>% 
    # Fill NAs with zero. NAs cases mean that firm did not trade in that month.
    mutate(across(!c(company_id, date), ~replace_na(., 0))) %>% 
    # Create a dummy indicating whether a firm in a particular month traded at least one product
    # with a specific category or not. If the firm trades at least 1 product with the specific category,
    # then the dummy will be TRUE otherwise FALSE. Therefore, when the dummy is FALSE it will include the cases 
    # when the firm does not trade a product with a specific category or when the firm trades nothing.
    # This dummy is created for each one of the products categories. For example, it will be created 
    # a dummy indicating whether the firm traded at least one e-commerce product or not, the name of this
    # variable would be (traded_China_E_commerce). 
    mutate(across(ends_with("TRUE"), ~ . > 0, .names = "traded_{col}")) %>% 
    # Remove in column names "_TRUE" and add at the beginning "traded_" to indicate that is referring to a dummy that indicates whether a product category was traded or not
    rename_at(vars(starts_with("traded_")), ~ sub("_TRUE", "", .x)) %>% 
    # Join number of country partners
    left_join(df %>% 
                select(company_id, date, n_country_partners) %>% 
                distinct(company_id, date, .keep_all = T), 
              by = c("company_id", "date")) %>% 
    # Only fill with 0s those cases where is missing and the firm does not trade (n_products == 0). These cases  
    # imply that the firm does not have partner countries in that month. In cases where n_products > 0 
    # but missing in number of country partners, the missing is because we do not have all the partner countries 
    # in that month, so incomplete information in Panjiva, but it does not mean that the firm does not have country partners. 
    mutate(n_country_partners = ifelse(is.na(n_country_partners) & n_products == 0, 0, n_country_partners)) %>% 
    # Join tech data 
    left_join(tech_data, by = c("company_id", "date"))
  
  # Add strigency index
  if(covid_string_index == TRUE){
    df_blown_up<-df_blown_up %>% 
      left_join(df %>% 
                  select(date, month_mean_stringency_index) %>% 
                  distinct(date, .keep_all = T), 
                by = c("date")
                )
  }
  
  # Return data to run extensive margin 
  return(df_blown_up)
}

# Create extensive margin data ---- 

# Create extensive margin data for products model
exports_ext_marg_IND<-data_extensive_margin(exports_data_IND)
imports_ext_marg_IND<-data_extensive_margin(imports_data_IND)

# Create extensive margin data for tech covid mitigation model 
exports_ext_marg_mitig_IND<-data_extensive_margin(exports_data_mitig_IND, covid_string_index = TRUE)
imports_ext_marg_mitig_IND<-data_extensive_margin(imports_data_mitig_IND, covid_string_index = TRUE)

# Save extensive margin data ----
write_csv(imports_ext_marg_IND, "../../Data/India/processed_data/imports_product_model_extmarg_IND.csv")
write_csv(exports_ext_marg_IND, "../../Data/India/processed_data/exports_product_model_extmarg_IND.csv")
write_csv(imports_ext_marg_mitig_IND, "../../Data/India/processed_data/imports_mitig_model_extmarg_IND.csv")
write_csv(exports_ext_marg_mitig_IND, "../../Data/India/processed_data/exports_mitig_model_extmarg_IND.csv")



