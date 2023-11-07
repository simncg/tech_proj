#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# This program runs extensive margin regressions using as independent vars  #
# pre-2019 adopter types (pre-2017 adopter, 2017 adopter, 2018 adopter,     #
# 2020 adopter)                                                             #                                                                    
#                                                                           #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ---- 
source("../src/packages.R")

# Import functions for running propensity regressions model with different products categories specifications
source("../src/functions_reg_prop_tech_covid_mitigation.R")

# Coefficient labels to be displayed in the table
coef_labels<-c(
  "pre_2017_adopter:month_mean_stringency_index"="Pre-2017 Adopter × COVID stringency index",
  "month_mean_stringency_index:adopter_2017"="2017-Adopter × COVID stringency index",
  "month_mean_stringency_index:adopter_2018"="2018-Adopter × COVID stringency index",
  "month_mean_stringency_index:adopter_2019"="2019-Adopter × COVID stringency index",
  
  
  "month_mean_stringency_index:cons_BEC" = "COVID stringency index × Consumable",
  "pre_2017_adopter:month_mean_stringency_index:cons_BEC" = "Pre-2017 Adopter × COVID stringency index × Consumable",
  "month_mean_stringency_index:cons_BEC:adopter_2017" = "2017-Adopter × COVID stringency index × Consumable",
  "month_mean_stringency_index:cons_BEC:adopter_2018" = "2018-Adopter × COVID stringency index × Consumable",
  "month_mean_stringency_index:cons_BEC:adopter_2019" = "2019-Adopter × COVID stringency index × Consumable",
  
  
  
  "month_mean_stringency_index:China_E_commerce" = "COVID stringency index × China e-commerce",
  "pre_2017_adopter:month_mean_stringency_index:China_E_commerce" = "Pre-2017 Adopter × COVID stringency index × China e-commerce",
  "month_mean_stringency_index:China_E_commerce:adopter_2017" = "2017-Adopter × COVID stringency index × China e-commerce",
  "month_mean_stringency_index:China_E_commerce:adopter_2018" = "2018-Adopter × COVID stringency index × China e-commerce",
  "month_mean_stringency_index:China_E_commerce:adopter_2019" = "2019-Adopter × COVID stringency index × China e-commerce",
  
  
  
  "month_mean_stringency_index:Ebay_tradable" = "COVID stringency index × eBay-Tradable",
  "pre_2017_adopter:month_mean_stringency_index:Ebay_tradable" = "Pre-2017 Adopter × COVID stringency index × eBay-Tradable",
  "month_mean_stringency_index:Ebay_tradable:adopter_2017" = "2017-Adopter × COVID stringency index × eBay-Tradable",
  "month_mean_stringency_index:Ebay_tradable:adopter_2018" = "2018-Adopter × COVID stringency index × eBay-Tradable",
  "month_mean_stringency_index:Ebay_tradable:adopter_2019" = "2019-Adopter × COVID stringency index × eBay-Tradable",
  
  
  
  "month_mean_stringency_index:durable_BEC" = "COVID stringency index × Durable",
  "pre_2017_adopter:month_mean_stringency_index:durable_BEC" = "Pre-2017 Adopter × COVID stringency index × Durable", 
  "month_mean_stringency_index:durable_BEC:adopter_2017" = "2017-Adopter × COVID stringency index × Durable",
  "month_mean_stringency_index:durable_BEC:adopter_2018" = "2018-Adopter × COVID stringency index × Durable",
  "month_mean_stringency_index:durable_BEC:adopter_2019" = "2019-Adopter × COVID stringency index × Durable"
  
  
)



# Run extensive margin regressions using as independent variables different types of pre-2019 adopter (pre-2017 adopter, 2017 adopter, 2018 adopter, 2019 adopter)
models_prop_pre_2020_adopters_IND<-prop_reg_models_pre_2020_adopters(country_name = "India",
                                                                     coef_labels = coef_labels, 
                                                                     indep_vars = c("pre_2017_adopter", "adopter_2017", "adopter_2018", "adopter_2019"),
                                                                     covid_var = "month_mean_stringency_index",
                                                                     product_vars = c("China_E_commerce", "Ebay_tradable", 
                                                                                      "durable_BEC", "cons_BEC")
)


