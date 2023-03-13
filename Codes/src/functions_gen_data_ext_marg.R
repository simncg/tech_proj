#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  functions_gen_data_ext_marg.R                                    #
#                                                                           #
# This program creates a function necessary to blow-up dataset. This blown-up                 
# dataset is necessary to run extensive margin analysis.                    #                                                                         
#                                                                           #
#                                                                           #           -                                                                                                                                                  #
#===========================================================================#

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
