#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
#                                                                           #
# This program generates functions to create quality of foreign data        #                                                                             #
#                                                                           #                                                                                                                                                             #
#===========================================================================#


quality_stats<-function(foreign_data, exp_imp_var){
  
  # Quality of data in terms of number of observations
  stats_n_obs <- 
    foreign_data %>% 
    # Dummy variable equal to 1 if foreign partner is missing
    mutate(foreign_firm_missing = ifelse(is.na(foreign_company_id), 1, 0)) %>% 
    # Group by firm-year
    group_by(domestic_company_id, year) %>%
    mutate(# Number of observation by firm-year
      total_obs = n(), 
      # Total trade volume by firm-year
      total_trade_volume = sum({{exp_imp_var}})) %>% 
    ungroup() %>% 
    # Group by firm-year-missing value in foreign trade
    group_by(domestic_company_id, year, foreign_firm_missing) %>% 
    summarize(# Number of observations with missing values and number of observations with non-missing values in the foreign partner variable by firm-year
      obs = n(), 
      # Total number of observations by firm-year
      total_obs = first(total_obs), 
      # Proportion number of observations with missing values or non-missing values 
      prop_obs = obs/total_obs, 
      # Total trade volume by year-firm
      total_trade_volume = first(total_trade_volume),
      # Total trade volume by year-firm and foreign_firm_missing
      trade_volume = sum({{exp_imp_var}}), 
      # Proportion of trade volume by year-firm and foreign_firm_missing
      prop_trade_volume = trade_volume/total_trade_volume)  
  
  return(stats_n_obs)
  
  
}