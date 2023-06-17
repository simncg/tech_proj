#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  This script runs the extensive margin regressions for Indonesia  #
#          for the model that measures if tech adoption mitigated COVID     #
#          impacts using as independent variable the number of months since #
#          adopted the technology and the pre-2019 adoption of technology   #
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
coef_labels<-c("months_since_adoption:month_mean_stringency_index"="Months Since Adoption ?? Monthly Avg. Stringency Index",
               "months_since_adoption:month_mean_stringency_index:Ebay_tradable" = "Months Since Adoption ?? Monthly Avg. Stringency Index ?? eBay-Tradable",
               "month_mean_stringency_index:Ebay_tradable" = "Monthly Avg. Stringency Index ?? eBay-Tradable",
               "month_mean_stringency_index:China_E_commerce" = "Monthly Avg. Stringency Index ?? China e-commerce",
               "months_since_adoption:month_mean_stringency_index:China_E_commerce" = "Months Since Adoption ?? Monthly Avg. Stringency Index ?? China e-commerce",
               "month_mean_stringency_index:cons_BEC" = "Monthly Avg. Stringency Index ?? Consumable",
               "months_since_adoption:month_mean_stringency_index:cons_BEC" = "Months Since Adoption ?? Monthly Avg. Stringency Index ?? Consumable", 
               "month_mean_stringency_index:durable_BEC" = "Monthly Avg. Stringency Index ?? Durable",
               "months_since_adoption:month_mean_stringency_index:durable_BEC" = "Months Since Adoption ?? Monthly Avg. Stringency Index ?? Durable", 
               "month_mean_stringency_index:adopted_pay_or_ecom_before_2019"="E-payment or E-commerce 2019 ?? Monthly Avg. Stringency Index",
               "month_mean_stringency_index:Ebay_tradable:adopted_pay_or_ecom_before_2019" = "E-payment or E-commerce 2019 ?? Monthly Avg. Stringency Index ?? eBay-Tradable", 
               "month_mean_stringency_index:China_E_commerce:adopted_pay_or_ecom_before_2019" = "E-payment or E-commerce 2019 ?? Monthly Avg. Stringency Index ?? China e-commerce",
               "month_mean_stringency_index:cons_BEC:adopted_pay_or_ecom_before_2019" = "E-payment or E-commerce 2019 ?? Monthly Avg. Stringency Index ?? Consumable", 
               "month_mean_stringency_index:durable_BEC:adopted_pay_or_ecom_before_2019" = "E-payment or E-commerce 2019 ?? Monthly Avg. Stringency Index ?? Durable"
)


# Run extensive margin regressions using interactions between covid, products and months since adopted the technology,
# as well as interaction between covid, products and pre-2019 adoption of technology
models_tech_covid_definitive_months_adopted_IDN<-prop_reg_models_2019_tech_var_months_covid("Indonesia", 
                                                                                            coef_labels
                                                                                            )
