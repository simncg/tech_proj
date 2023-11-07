#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  This script runs the extensive margin regressions for Mexico     #
#          for the model that measures if tech adoption mitigated COVID     #
#          impacts using as independent variable the number of months since #
#          adopted the technology.                                          #
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

# Run regressions for definitive products to be used in the analysis using months since adoption as indep var ----
coef_labels<-c("months_since_adoption:month_mean_stringency_index"="Months Since Adoption × COVID stringency index",
               "months_since_adoption:month_mean_stringency_index:Ebay_tradable" = "Months Since Adoption × COVID stringency index × eBay-Tradable",
               "month_mean_stringency_index:Ebay_tradable" = "COVID stringency index × eBay-Tradable",
               "month_mean_stringency_index:China_E_commerce" = "COVID stringency index × China e-commerce",
               "months_since_adoption:month_mean_stringency_index:China_E_commerce" = "Months Since Adoption × COVID stringency index × China e-commerce",
               "month_mean_stringency_index:cons_BEC" = "COVID stringency index × Consumable",
               "months_since_adoption:month_mean_stringency_index:cons_BEC" = "Months Since Adoption × COVID stringency index × Consumable", 
               "month_mean_stringency_index:durable_BEC" = "COVID stringency index × Durable",
               "months_since_adoption:month_mean_stringency_index:durable_BEC" = "Months Since Adoption × COVID stringency index × Durable", 
               "month_mean_stringency_index:adopted_pay_or_ecom_before_2020"="Firm technology adoption pre-2020 × COVID stringency index",
               "month_mean_stringency_index:Ebay_tradable:adopted_pay_or_ecom_before_2020" = "Firm technology adoption pre-2020 × COVID stringency index × eBay-Tradable", 
               "month_mean_stringency_index:China_E_commerce:adopted_pay_or_ecom_before_2020" = "Firm technology adoption pre-2020 × COVID stringency index × China e-commerce",
               "month_mean_stringency_index:cons_BEC:adopted_pay_or_ecom_before_2020" = "Firm technology adoption pre-2020 × COVID stringency index × Consumable", 
               "month_mean_stringency_index:durable_BEC:adopted_pay_or_ecom_before_2020" = "Firm technology adoption pre-2020 × COVID stringency index × Durable"
)



models_tech_covid_definitive_months_adopted_MEX<-prop_reg_models_2020_tech_var_months_covid("Mexico", 
                                                                                            coef_labels)
