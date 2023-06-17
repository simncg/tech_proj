#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# This program runs extensive margin regressions using as independent vars  #
# pre-2019 adopter types (pre-2017 adopter, 2017 adopter, 2018 adopter)     #                                                                    
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
  "pre_2018_adopter:month_mean_stringency_index"="Pre-2018 Adopter × Monthly Avg. Stringency Index",
  "month_mean_stringency_index:adopter_2018"="2018-Adopter × Monthly Avg. Stringency Index",
  
  "month_mean_stringency_index:cons_BEC" = "Monthly Avg. Stringency Index × Consumable",
  "pre_2018_adopter:month_mean_stringency_index:cons_BEC" = "Pre-2018 Adopter × Monthly Avg. Stringency Index × Consumable",
  "month_mean_stringency_index:cons_BEC:adopter_2018" = "2018-Adopter × Monthly Avg. Stringency Index × Consumable",
  
  
  "month_mean_stringency_index:China_E_commerce" = "Monthly Avg. Stringency Index × China e-commerce",
  "pre_2018_adopter:month_mean_stringency_index:China_E_commerce" = "Pre-2018 Adopter × Monthly Avg. Stringency Index × China e-commerce",
  "month_mean_stringency_index:China_E_commerce:adopter_2018" = "2018-Adopter × Monthly Avg. Stringency Index × China e-commerce",
  
  
  "month_mean_stringency_index:Ebay_tradable" = "Monthly Avg. Stringency Index × eBay-Tradable",
  "pre_2018_adopter:month_mean_stringency_index:Ebay_tradable" = "Pre-2018 Adopter × Monthly Avg. Stringency Index × eBay-Tradable",
  "month_mean_stringency_index:Ebay_tradable:adopter_2018" = "2018-Adopter × Monthly Avg. Stringency Index × eBay-Tradable",
  
  
  "month_mean_stringency_index:durable_BEC" = "Monthly Avg. Stringency Index × Durable",
  "pre_2017_adopter:month_mean_stringency_index:durable_BEC" = "Pre-2018 Adopter × Monthly Avg. Stringency Index × Durable", 
  "month_mean_stringency_index:durable_BEC:adopter_2018" = "2018-Adopter × Monthly Avg. Stringency Index × Durable"
  
)



# Run extensive margin regressions using as independent variables different types of pre-2019 adopter (pre-2018 adopter and 2018 adopter)
models_prop_pre_2019_adopters_IDN<-prop_reg_models_pre_2019_adopters(country_name = "Indonesia",
                                                                     coef_labels = coef_labels, 
                                                                     indep_vars = c("pre_2018_adopter", "adopter_2018"),
                                                                     covid_var = "month_mean_stringency_index",
                                                                     product_vars = c("China_E_commerce", "Ebay_tradable", 
                                                                                      "durable_BEC", "cons_BEC")
)


