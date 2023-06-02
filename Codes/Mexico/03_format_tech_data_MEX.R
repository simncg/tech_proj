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
# This program formats the Mexican technology data (BuiltWith) to merge     #
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

# Load technology data (only nod variables) ----
tech_data <- fread("../../Data/Mexico/raw_data/Builtwith_Mexico_no_drop_long.csv") %>%  
  select(our_ID, year, month, tech, value, FI, LI) %>% 
  # For the moment, we are only going to analyze e-payment/e-commerce technologies
  filter(tech %in% c("payrobust_nod", "ecom_nod")) %>% 
  rename(company_id = our_ID) %>% 
  mutate(date = as.Date(paste0(year, "-", month, "-01")),
         date_character = as.character(fct_reorder(as.factor(format(date, "%Y-%b")), date))
  )

gc()

# Add adoption date to tech data, date variables and lags for for adoption of 
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
      mutate(first_adopted = ifelse(date < as.Date("2018-07-01") , "before july 2018", paste0(year, "_", month)), 
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
  ) %>% 
  # Create specific date of adoption (as date) 
  mutate(
    date_of_adoption = as.Date(ifelse(max(pay_or_ecomnod) == 1, min(date[pay_or_ecomnod == 1]), NA), origin = "1970-01-01"), 
    # Create number of months since adoption
    months_since_adoption = ifelse(is.na(date_of_adoption), 0,
                                   floor(interval(date_of_adoption, date) / months(1))), 
    # When the firm has not adopted the technology the months are negative until the month of adoption, but we need to replace them by 0s
    months_since_adoption = ifelse(months_since_adoption<0, 0, months_since_adoption)
  ) %>%
  ungroup() %>% 
  # Create adopter type variable. This could be old adopter (pre-covid adopter), 
  # covid adopter (adopter during covid) and never adopter. 
  mutate(
    adopter_type = 
      case_when(
        date_of_adoption < as.Date("2020-01-01") ~ "old_adopter", 
        date_of_adoption >= as.Date("2020-01-01") ~ "covid_adopter", 
        is.na(date_of_adoption)  ~ "never_adopter"
      ), 
    # Create covid adopter type variable. This could be covid early adopter (adopter in 2020), 
    # covid late adopter (adopter after 2020), never adopter and non-covid adopter 
    # (meaning that adopted the technology before 2020). 
    covid_adopter_type = 
      case_when(
        year(date_of_adoption) == 2020 ~ "covid_early_adopter",
        date_of_adoption >= as.Date("2021-01-01") ~ "covid_late_adopter",
        is.na(date_of_adoption) ~ "never_adopter",
        TRUE ~ "non_covid_adopter"
      ), 
    # Create old adopter type variable. This could be adopters before 2016 or in 2016, 
    # 2017 adopter, 2018 adopter and 2019 adopter. 
    old_adopter_type = 
      case_when(
        # Adopters during 2018 and 2019
        year(date_of_adoption) == 2019  ~ "2019_adopter",  
        year(date_of_adoption) == 2018  ~ "2018_adopter", 
        year(date_of_adoption) == 2017  ~ "2017_adopter", 
        # Since the data is available only from 2016 onwards, there could be cases of firms that adopted the technology before 2016 but has as adoption date 2016-01-01 (the earliest date in our data).
        year(date_of_adoption) == 2016  ~ "2016_or_pre_2016_adopter",
        # Never adopters 
        is.na(date_of_adoption) ~ "never_adopter",
        TRUE ~ "non_old_adopter"
      )
    
  ) 


# Save tech data 
write_parquet(tech_data, "../../Data/Mexico/processed_data/tech_data_MEX.parquet")

