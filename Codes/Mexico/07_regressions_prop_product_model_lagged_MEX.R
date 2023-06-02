#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  regressions_prop_products_model_MEX.R                            #
#                                                                           #                                                                      #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ---- 
source("../src/packages.R")

# Import functions for running propensity regressions model with different products categories specifications
source("../src/functions_reg_prop_products_model.R")


# Propensity Regressions for definitive products category to be used in the analysis  -----

# (China E-commerce, E-bay tradable products, Consumable and Durable Goods)
coef_labels<-c(pay_or_ecomnod="E-payment or E-commerce",
               pay_or_ecomnod_t_1="E-payment or E-commerce (t-1)",
               pay_or_ecomnod_t_2="E-payment or E-commerce (t-2)",
               pay_or_ecomnod_t_3="E-payment or E-commerce (t-3)",
               
               "pay_or_ecomnod:cons_BEC" = "E-payment or E-commerce × Consumable",
               "pay_or_ecomnod_t_1:cons_BEC" = "E-payment or E-commerce (t-1) × Consumable",
               "pay_or_ecomnod_t_2:cons_BEC" = "E-payment or E-commerce (t-2) × Consumable",
               "pay_or_ecomnod_t_3:cons_BEC" = "E-payment or E-commerce (t-3) × Consumable",
               
               "pay_or_ecomnod:durable_BEC" = "E-payment or E-commerce × Durable",
               "pay_or_ecomnod_t_1:durable_BEC" = "E-payment or E-commerce (t-1) × Durable",
               "pay_or_ecomnod_t_2:durable_BEC" = "E-payment or E-commerce (t-2) × Durable",
               "pay_or_ecomnod_t_3:durable_BEC" = "E-payment or E-commerce (t-3) × Durable",
               
               "pay_or_ecomnod:Ebay_tradable"="E-payment or E-commerce × eBay-tradable",
               "pay_or_ecomnod_t_1:Ebay_tradable"="E-payment or E-commerce (t-1) × eBay-tradable",
               "pay_or_ecomnod_t_2:Ebay_tradable"="E-payment or E-commerce (t-2) × eBay-tradable",
               "pay_or_ecomnod_t_3:Ebay_tradable"="E-payment or E-commerce (t-3) × eBay-tradable",
               
               "pay_or_ecomnod:China_E_commerce"="E-payment or E-commerce × China e-commerce",
               "pay_or_ecomnod_t_1:China_E_commerce"="E-payment or E-commerce (t-1) × China e-commerce",
               "pay_or_ecomnod_t_2:China_E_commerce"="E-payment or E-commerce (t-2) × China e-commerce",
               "pay_or_ecomnod_t_3:China_E_commerce"="E-payment or E-commerce (t-3) × China e-commerce"
               
)



# Regressions with 2 lags (t-2) in technology variable
models_definitive_MEX_t_2 <- prop_reg_models_definitive("pay_or_ecomnod_t_2", 
                                                        coef_labels = coef_labels, 
                                                        country_name = "Mexico")

gc()
