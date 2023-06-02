#=============================================================================#
# Date:    January 2022                                                       #
#                                                                             #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,     #
#          Indonesia and Mexico.                                              #
#                                                                             #
# Author:  Simon Caicedo - DECTI - The World Bank                             #
#                                                                             #
#                                                                             #
# This script defines functions to conduct t-tests, comparing different type  #
# of adopters across two timeframes: pre-COVID and post-COVID. It utilizes the#
# regression sample from a model assessing  the impact of tech adoption on    #
# mitigating COVID impacts. It is applicable to all files named with the      #
# prefix "analysis_tech_adopters_types"                                       #
#                                                                             #                                                                          
#=============================================================================#

# Function to prepare data for running t-tests -----
prep_data_t_tests<-function(import_data, export_data){
  
  # Create average value of imports trade variables by firm in two periods: before covid (pre-2020) and during covid (Jan 2020 onwards) -----
  imp_vars_summary<-
    import_data %>% 
    mutate(period = 
             ifelse(
               date < as.Date("2020-01-01"), 
               "pre-covid", 
               "covid-period"
             )
    ) %>% 
    group_by(
      company_id,
      period
    ) %>% 
    # Average values by firm and period 
    summarize(
      adopter_type = first(adopter_type), # Keep adopter type
      mean_import = mean(import),
      mean_n_hs6_products_import = mean(n_hs6_products), 
      mean_n_countries_import = mean(n_countries_import), 
      total_import = sum(import)
    ) %>% 
    ungroup() 
  
  
  # Create average value of exports trade variables by adopter type in two periods: before covid (pre-2020) and during covid (Jan 2020 onwards) -----
  exp_vars_summary<-
    export_data %>% 
    mutate(period = 
             ifelse(
               date < as.Date("2020-01-01"), 
               "pre-covid", 
               "covid-period"
             )
    ) %>% 
    group_by(
      company_id,
      period
    ) %>% 
    # Average values by firm and period 
    summarize(
      adopter_type = first(adopter_type), # Keep adopter type
      mean_export = mean(export),
      mean_n_hs6_products_export = mean(n_hs6_products), 
      mean_n_countries_export = mean(n_countries_export), 
      total_export = sum(export)
    ) %>% 
    ungroup() 
  
  
  return(list(imp_vars_summary, exp_vars_summary))
  
}


# Function to run t-test in the pre-covid and covid periods for two different
# type of adopters for a specific variable 
t_test_adopter_types<-function(var, adopter_types, data){
  
  # Pre-Covid period data
  data_pre_covid <- data %>% 
    filter(adopter_type %in% adopter_types, 
           period  == "pre-covid") 
  
  
  # Covid period data
  data_covid<-data %>% 
    filter(adopter_type %in% adopter_types, 
           period  == "covid-period") 
  
  
  
  # Perform t-test for pre-covid period
  t.test.precovid<-t.test(as.formula(paste0(var, "~ as.factor(adopter_type)")), data = data_pre_covid)
  
  
  # Perform t-test for covid period
  t.test.covid<-t.test(as.formula(paste0(var, "~ as.factor(adopter_type)")), data = data_covid)
  
  tests<-list(t.test.precovid, t.test.covid)
  
  names(tests)<-c("Pre-Covid Period", "Covid Period")
  
  return(tests)
  
}


# Run t-tests 
run_t_tests<-function(imp_vars_summary, exp_vars_summary, adopter_types = c("covid_adopter", "old_adopter")){
  
  # Empty list of t-tests...to be filled 
  t_test_list<-list(list(), list())
  # The first of t-tests will be for exports variables and the second set for imports variables
  names(t_test_list)<-c("Exports", "Imports")
  
  j = 0 # Counter for dataset (either exports or imports)
  for(data in list(exp_vars_summary, imp_vars_summary)){
    j = j + 1
    # If exports use exports variables 
    if(j == 1){
      vars<-c("mean_export", "mean_n_countries_export", "mean_n_hs6_products_export", "total_export")
    } 
    else{
      vars<-c("mean_import", "mean_n_countries_import", "mean_n_hs6_products_import", "total_import")
    }
    # If imports use imports variables
    i = 0
    for(var in vars){
      i = i + 1
      
      # Perform t-test for a specific var
      t_test_list[[j]][[i]]<-t_test_adopter_types(var = var, 
                                                 adopter_types = adopter_types, 
                                                 data = data)
    }
    # Add names of variables 
    names(t_test_list[[j]])<-vars
  }
  
  # Return list with list of t-tests for exports and list of t-tests for imports
  return(t_test_list)
  
}




# Create stylish table of t-tests results 
table_t_tests<-function(t_test_list, adopter_types = c("covid_adopter", "old_adopter"), 
                        country = c("India", "Mexico", "Indonesia")){
  
  
  # Create table 
  table<-as.data.frame(matrix(0, nrow = 16, ncol = 6))
  colnames(table)<-c("variable", "period", 
                     paste0("mean_", adopter_types[1]), 
                     paste0("mean_", adopter_types[2]), 
                     "difference", "p_value")
  
  # Counter for test
  i = 0 
  
  # Get the results for each t-test and fill the table table
  
  # Iterate over exports or imports tests
  for(exp_or_imp in t_test_list){
    j = 0  # Counter for tests 
    vars<-names(exp_or_imp) # Get variables names 
    # Iterate over exports or imports variables 
    for(test_vars in exp_or_imp){
      j = j + 1
      periods<-names(test_vars) # Get period names (pre-covid or covid)
      p = 0  # Counter for periods
      # Iterate over pre-covid or covid period 
      for(test_period in test_vars){
        i = i + 1 
        p = p + 1
        table[i, "variable"]<-vars[j] # Fill variable name 
        table[i, "period"]<-periods[p] # Fill period name 
        table[i, paste0("mean_", adopter_types[1])]<-test_period[["estimate"]][[paste0("mean in group ", adopter_types[1])]] # Fill mean for adopters group 1 
        table[i, paste0("mean_", adopter_types[2])]<-test_period[["estimate"]][[paste0("mean in group ", adopter_types[2])]]   # Fill mean for adopters group 2
        table[i, "difference"]<- test_period[["estimate"]][[paste0("mean in group ", adopter_types[1])]] - test_period[["estimate"]][[paste0("mean in group ", adopter_types[2])]] # Means difference 
        table[i, "p_value"]<- test_period[["p.value"]] # p-value
      }
    }
  }
  
  # Define pre-covid period and covid period labels
  if(country %in% c("India", "Mexico")){
    pre_covid_period_label<-"(Jul 2018 - Dec 2019)"
    covid_period_label<-"(Jan 2020 - Dec 2021)"
  } else if(country == "Indonesia"){
    pre_covid_period_label<-"(Feb 2019 - Dec 2019)"
    covid_period_label<-"(Jan 2020 - Sep 2021)"
  }
  
  
  
  # Create final table with the t-tests results 
  final_table<-table %>%
    mutate(across(c(paste0("mean_", adopter_types[1]), paste0("mean_", adopter_types[2]), "difference"), ~ prettyNum(round(., 1), big.mark = ",", scientific = FALSE)), 
           variable = case_when(
             # Adopters during 2018 and 2019
             variable == "mean_export"  ~ "Mean Export Value (USD)",  
             variable == "mean_n_countries_export"  ~ "Mean No. Destinations", 
             variable == "mean_n_hs6_products_export"  ~ "Mean No. Distinct HS6 Exported Products", 
             variable == "mean_import"  ~ "Mean Import Value (USD)",  
             variable == "mean_n_countries_import"  ~ "Mean No. Sources", 
             variable == "mean_n_hs6_products_import"  ~ "Mean No. Distinct HS6 Imported Products" , 
             variable == "total_export" ~ "Total Export Volume (USD)", 
             variable == "total_import" ~ "Total Import Volume (USD)"
             
           ), 
           difference = case_when(
             p_value < 0.1 & p_value >= 0.05 ~ paste0(difference, "*"), 
             p_value < 0.05 & p_value >= 0.01 ~ paste0(difference, "**"), 
             p_value < 0.01 ~ paste0(difference, "***"), 
             TRUE ~ as.character(difference)
           )
    ) %>% 
    arrange(desc(period)) %>% 
    rename(` ` = variable, 
           `Difference` = difference) %>%
    rename_with(~ tools::toTitleCase(str_replace_all(., "_", " ")), 
                contains(paste0("mean_", adopter_types[1]))) %>%
    rename_with(~ tools::toTitleCase(str_replace_all(., "_", " ")), 
                contains(paste0("mean_", adopter_types[2]))) %>%
    select(-period, -p_value) %>% 
    kable(caption = paste0(country," - T-Tests Comparing Average Characteristics of ", 
                           str_to_title(str_replace_all(adopter_types[1], "_", " ")), " and ",
                           str_to_title(str_replace_all(adopter_types[2], "_", " ")), 
                           " During Pre-Covid and Covid Periods"), 
          centering = T, booktabs = T, align = "lccc") %>%
    kable_styling(latex_options = "HOLD_position") %>% 
    pack_rows(paste0("Panel A: Pre-Covid Period ", pre_covid_period_label), 1, 8) %>%
    pack_rows(paste0("Panel B: Covid Period ", covid_period_label), 9, 16) %>% 
    add_footnote(c("Note: Mean variables in the first column denote the average value for the respective variables for each firm within the specific panel period. 'Total Import Volume' and 'Total Export Volume' variables indicate the cumulative trade volume for each firm during the specific panel period."), 
                 threeparttable = T, notation = "none")
  
  
  return(final_table)
  
}



#' conduct_t_tests_and_create_table function
#'
#' This function runs t-tests for a set of variables from import and export datasets, comparing two groups of adopters. 
#' It then generates a summary table of the t-test results.
#' The function makes use of other pre-defined functions: prep_data_t_tests, run_t_tests, and table_t_tests.
#'
#' @param import_data A data frame containing the import data.
#' @param export_data A data.frame containing the export data.
#' @param adopter_types A character vector of length 2 with the names of the adopter types. For example, c("covid_adopter", "old_adopter"). Default is c("covid_adopter", "old_adopter").
#' @param country A character string specifying the country. Countries could be "India", "Mexico", and "Indonesia".
#'
#' @return A summary table with t-test results for the specified adopter types.
#'
#' @examples
#' conduct_t_tests_and_create_table(import_data, export_data, c("covid_adopter", "old_adopter"), "India")
#' 
#' @export


# Function for running t-tests and producing final table with results of t-tests. This function incorporates all functions above
conduct_t_tests_and_create_table<- function(import_data,
                                            export_data, 
                                            adopter_types,
                                            country) {
  
  # Step 1: Prepare data
  list_data = prep_data_t_tests(import_data, export_data)
  
  # Step 2: Run t-tests
  t_test_list = run_t_tests(list_data[[1]], list_data[[2]], adopter_types)
  
  # Step 3: Create table of t-tests results
  final_table = table_t_tests(t_test_list, adopter_types, country)
  
  # Return final table
  return(list(final_table, t_test_list))
}
