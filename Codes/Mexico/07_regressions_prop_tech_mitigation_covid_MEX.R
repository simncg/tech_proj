#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  regressions_tech_mitigation_covid_IND.R                          #
#                                                                           #
#                                                                           #                                                                         
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

# Run regressions for definitive products categories to be used in the analysis ----


coef_labels<-c("adopted_pay_or_ecom_before_2019:month_mean_stringency_index"="E-payment or E-commerce 2019 × Monthly Avg. Stringency Index",
               "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:Ebay_tradable" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × eBay-Tradable",
               "month_mean_stringency_index:Ebay_tradable" = "Monthly Avg. Stringency Index × eBay-Tradable",
               "month_mean_stringency_index:China_E_commerce" = "Monthly Avg. Stringency Index × China e-commerce",
               "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × China e-commerce",
               "month_mean_stringency_index:cons_BEC" = "Monthly Avg. Stringency Index × Consumable",
               "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_BEC" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Consumable", 
               "month_mean_stringency_index:durable_BEC" = "Monthly Avg. Stringency Index × Durable",
               "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_BEC" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Durable"
)



models_tech_covid_definitive_MEX<-prop_reg_models_tech_covid_definitive("Mexico", 
                                                                        coef_labels)
