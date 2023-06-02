#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  format_tech_data_IDN.R                                           #
#                                                                           #
# This program formats the Indonesian technology data (BuiltWith) to merge  #
# it with Panjiva and Aberdeen datasets. Also, it creates lags of the       #
# technology variables.                                                     #
#                                                                           #                                                                                                                                                             #
#===========================================================================#


# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")

# Read BuiltWith data for Indonesia ----
tech_data<-read_parquet("../../Data/Indonesia/raw_data/Builtwith_no_drop_long_v2.parquet", 
                        col_select = c("New_ID_1", "year", "month", "tech", "value", "FI", "LI")) %>% 
  mutate(tech = ifelse(tech == "pay_robust", "payrobust_nod", tech)) %>% 
  # For the moment, we are only going to analyze e-payment/e-commerce technologies
  filter(tech %in% c("payrobust_nod", "ecom_nod")) %>% 
  rename(company_id = New_ID_1) %>% 
  mutate(date = as.Date(paste0(year, "-", month, "-01")),
         date_character = as.character(fct_reorder(as.factor(format(date, "%Y-%b")), date))
  )

gc()

# Add adoption date to tech data, date variables and lags for adoption of 
# e-commerce or e-payment technologies
tech_data <- 
  tech_data %>% 
  # Identify technology adoption date (first_adopted variable)
  left_join(
    tech_data %>% 
      select(-LI, -FI) %>% 
      arrange(company_id, tech, year, month) %>% 
      group_by(company_id, tech) %>%
      slice(which(value == 1)[1]) %>% 
      ungroup() %>% 
      mutate(first_adopted = ifelse(date < as.Date("2019-02-01"), "before february 2019", paste0(year, "_", month)),
             first_adopted_2019 = ifelse(date < as.Date("2019-01-01") , "before 2019", paste0(year, "_", month))) %>% 
      select(company_id, tech, first_adopted, first_adopted_2019), 
    by = c("company_id", "tech")
  ) %>% 
  mutate(# If missing value in adopted date, then technology never adopted by the company 
    first_adopted = ifelse(is.na(first_adopted), "never adopted", first_adopted), 
    first_adopted_2019 = ifelse(is.na(first_adopted_2019), "never adopted", first_adopted_2019)
  ) %>% 
  select(-year, -month) %>% 
  # From long to wide to have one row per firm-month and to have tech columns and columns defining their adoption date
  pivot_wider(
    names_from = tech, 
    values_from = c(value, first_adopted, first_adopted_2019)
  ) %>% 
  # Rename resulting columns of the pivot_wider, remove prefix "_value" and "_nod" from column names
  rename_if(str_detect(names(.), "_nod"), ~sub("_nod", "", .)) %>% 
  rename_if(str_detect(names(.), "value_"), ~sub("value_", "", .)) %>% 
  arrange(company_id, date) %>% 
  # Create a variable to identify if firm adopted e-commerce or e-payment before 2019
  mutate(adopted_pay_or_ecom_before_2019 = first_adopted_2019_payrobust == "before 2019" | first_adopted_2019_ecom == "before 2019") %>% 
  # Create lags by company for variables of e-commerce and e-payment technologies
  group_by(company_id) %>% 
  mutate(across(c(ecom, payrobust), list(t_1 = function(x){lag(x, 1)}, 
                                         t_2 = function(x){lag(x, 2)}, 
                                         t_3 = function(x){lag(x, 3)}))) %>% 
  # Create a variable indicating if e-commerce or e-payment technology is adopted.
  mutate(pay_or_ecomnod = ifelse((payrobust == 1 | ecom == 1),1, 0 ),
         # Create lagged variables for adoption of e-commerce or e-payment technologies
         pay_or_ecomnod_t_1 = ifelse((payrobust_t_1 == 1 | ecom_t_1 == 1),1, 0),
         pay_or_ecomnod_t_2 = ifelse((payrobust_t_2 == 1 | ecom_t_2 == 1),1, 0),
         pay_or_ecomnod_t_3 = ifelse((payrobust_t_3 == 1 | ecom_t_3 == 1),1, 0)
  )


gc()



# Save tech data 
write_parquet(tech_data, "../../Data/Indonesia/processed_data/tech_data_IDN.parquet")

