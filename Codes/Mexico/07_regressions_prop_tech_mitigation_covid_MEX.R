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


coef_labels<-c("adopted_pay_or_ecom_before_2020:month_mean_stringency_index"="Firm technology adoption pre-2020 × COVID stringency index",
               "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:Ebay_tradable" = "Firm technology adoption pre-2020 × COVID stringency index × eBay-Tradable",
               "month_mean_stringency_index:Ebay_tradable" = "COVID stringency index × eBay-Tradable",
               "month_mean_stringency_index:China_E_commerce" = "COVID stringency index × China e-commerce",
               "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:China_E_commerce" = "Firm technology adoption pre-2020 × COVID stringency index × China e-commerce",
               "month_mean_stringency_index:cons_BEC" = "COVID stringency index × Consumable",
               "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:cons_BEC" = "Firm technology adoption pre-2020 × COVID stringency index × Consumable", 
               "month_mean_stringency_index:durable_BEC" = "COVID stringency index × Durable",
               "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:durable_BEC" = "Firm technology adoption pre-2020 × COVID stringency index × Durable"
)



models_tech_covid_definitive_MEX<-prop_reg_models_tech_covid_definitive("Mexico", 
                                                                        coef_labels, 
                                                                        indep_var = "adopted_pay_or_ecom_before_2020")
