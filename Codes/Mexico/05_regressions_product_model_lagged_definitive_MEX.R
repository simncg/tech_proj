#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  regressions_products_model_MEX.R                                 #
#                                                                           #
# This program run the regressions of the model with different products     #
# categories specifications (eBay tradable, China e-commerce, BEC           #
# classification, Time-Sensitivity) interacted with ecommerce/e-payment     #  
# technology with a lagged structure.                                       #
# This code is necessary for creating the Rmarkdown                         #
# ".rmd" that contains regression results.                                  #
#                                                                           #                                                                         
#                                                                           #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
#fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
#setwd(fileloc)
#rm(fileloc)

# Read Mexico data for products model ----
#source("gen_data_model_products_MEX.R")
pay_ecom_import_data_MEX<-fread("../../Data/Mexico/processed_data/imports_product_model_MEX.csv")
pay_ecom_export_data_MEX<-fread("../../Data/Mexico/processed_data/exports_product_model_MEX.csv")


# Import functions for running model with different products categories specifications
source("../src/functions_reg_products_model.R")

# Free memory
gc()


# Intensive Margin Regressions for definitive products categories to be used in the analysis ----

# The definitive products are e-Bay tradable, china-ecommerce, consumable and durable products.

# Log imports/Log exports regressions 

# Convert from Boolean to dummy (to avoid _TRUE labels in table)
definitive_vars <- c("Ebay_tradable", "China_E_commerce", 
                     "cons_BEC", "durable_BEC")


pay_ecom_import_data_MEX<-as.data.frame(pay_ecom_import_data_MEX)
pay_ecom_export_data_MEX<-as.data.frame(pay_ecom_export_data_MEX)

pay_ecom_import_data_MEX[definitive_vars] <- lapply(pay_ecom_import_data_MEX[definitive_vars], function(x) 1 * x)
pay_ecom_export_data_MEX[definitive_vars] <- lapply(pay_ecom_export_data_MEX[definitive_vars], function(x) 1 * x)



# Assign variables names to display in tables
coef_labels<-c(pay_or_ecomnod="E-payment or E-commerce", 
               pay_or_ecomnod_t_1 = "E-payment or E-commerce (t-1)",
               pay_or_ecomnod_t_2 = "E-payment or E-commerce (t-2)",
               pay_or_ecomnod_t_3 = "E-payment or E-commerce (t-3)",
               
               "pay_or_ecomnod:Ebay_tradable"="E-payment or E-commerce × eBay-tradable",
               "pay_or_ecomnod_t_1:Ebay_tradable"="E-payment or E-commerce (t-1) × eBay-tradable",
               "pay_or_ecomnod_t_2:Ebay_tradable"="E-payment or E-commerce (t-2) × eBay-tradable",
               "pay_or_ecomnod_t_3:Ebay_tradable"="E-payment or E-commerce (t-3) × eBay-tradable",
               
               "pay_or_ecomnod:China_E_commerce"="E-payment or E-commerce × China e-commerce",
               "pay_or_ecomnod_t_1:China_E_commerce"="E-payment or E-commerce (t-1) × China e-commerce",
               "pay_or_ecomnod_t_2:China_E_commerce"="E-payment or E-commerce (t-2) × China e-commerce",
               "pay_or_ecomnod_t_3:China_E_commerce"="E-payment or E-commerce (t-3) × China e-commerce",
               
               "pay_or_ecomnod:cons_BEC" = "E-payment or E-commerce × Consumable",
               "pay_or_ecomnod_t_1:cons_BEC" = "E-payment or E-commerce (t-1) × Consumable",
               "pay_or_ecomnod_t_2:cons_BEC" = "E-payment or E-commerce (t-2) × Consumable",
               "pay_or_ecomnod_t_3:cons_BEC" = "E-payment or E-commerce (t-3) × Consumable",
               
               "pay_or_ecomnod:durable_BEC" = "E-payment or E-commerce × Durable",
               "pay_or_ecomnod_t_1:durable_BEC" = "E-payment or E-commerce (t-1) × Durable",
               "pay_or_ecomnod_t_2:durable_BEC" = "E-payment or E-commerce (t-2) × Durable",
               "pay_or_ecomnod_t_3:durable_BEC" = "E-payment or E-commerce (t-3) × Durable"
               
               
               
)


# Regressions with 2 lags (t-2) in technology variable for log. exports and log.imports ----
log_imp_exp_MEX <- reg_models_definitive(tech_var  = "pay_or_ecomnod_t_2", 
                                         dep_var_import = "log_import", 
                                         dep_var_export = "log_export",
                                         import_data = pay_ecom_import_data_MEX,
                                         export_data = pay_ecom_export_data_MEX,
                                         country_name = "Mexico",
                                         coef_labels = coef_labels, 
                                         dep_var_labels = c("Log.Import", "Log.Export")
)




# Regressions with 2 lags (t-2) in technology variable for number destinations/number sources ----
no_source_dest_MEX <- reg_models_definitive(tech_var  = "pay_or_ecomnod_t_2", 
                                            dep_var_import = "n_countries_import", 
                                            dep_var_export = "n_countries_export",
                                            import_data = pay_ecom_import_data_MEX,
                                            export_data = pay_ecom_export_data_MEX,
                                            country_name = "Mexico",
                                            coef_labels = coef_labels, 
                                            dep_var_labels = c("No. Sources", "No. Destinations")
)




# Regressions with 2 lags (t-2) in technology variable for new destination/new source ----

new_source_dest_MEX <- reg_models_definitive(tech_var  = "pay_or_ecomnod_t_2", 
                                             dep_var_import = "new_source", 
                                             dep_var_export = "new_destination",
                                             import_data = pay_ecom_import_data_MEX,
                                             export_data = pay_ecom_export_data_MEX,
                                             country_name = "Mexico",
                                             coef_labels = coef_labels, 
                                             dep_var_labels = c("New Source", "New Destination")
)





