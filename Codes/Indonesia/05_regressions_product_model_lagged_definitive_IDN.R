#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  regressions_products_model_IDN.R                                 #
#                                                                           #
# This program run the regressions of the model with different products     #
# categories specifications (eBay tradable, China e-commerce, BEC           #
# classification, Time-Sensitivity) interacted with ecommerce/e-payment     #  
# technology with a lagged structure.                                       #
# This code is necessary for creating the Rmarkdown                         #
# ".rmd" that contains regression results.                    #
#                                                                           #                                                                         
#                                                                           #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
#fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
#setwd(fileloc)
#rm(fileloc)

# Read Indonesia data for products model ----
#source("gen_data_model_products_IDN.R")
pay_ecom_import_data_IDN<-fread("../../Data/Indonesia/processed_data/imports_product_model_IDN.csv")
pay_ecom_export_data_IDN<-fread("../../Data/Indonesia/processed_data/exports_product_model_IDN.csv")


# Import functions for running model with different products categories specifications
source("../src/functions_reg_products_model.R")

# Free memory
gc()


# Regressions with eBay-tradable products/products in China e-commerce tax lists ----

# Log imports/Log exports regressions (t-1)

# Convert from Boolean to dummy (to avoid _TRUE labels in table)
ebay_china_vars <- c("Ebay_tradable", "China_E_commerce", 
                     'cons_BEC', "durable_BEC")


pay_ecom_import_data_IDN<-as.data.frame(pay_ecom_import_data_IDN)
pay_ecom_export_data_IDN<-as.data.frame(pay_ecom_export_data_IDN)

pay_ecom_import_data_IDN[ebay_china_vars] <- lapply(pay_ecom_import_data_IDN[ebay_china_vars], function(x) 1 * x)
pay_ecom_export_data_IDN[ebay_china_vars] <- lapply(pay_ecom_export_data_IDN[ebay_china_vars], function(x) 1 * x)



# Assign variables names to display in tables
coef_labels<-c(pay_or_ecomnod="Firm technology adoption", 
               pay_or_ecomnod_t_1 = "Firm technology adoption (t-1)",
               pay_or_ecomnod_t_2 = "Firm technology adoption (t-2)",
               pay_or_ecomnod_t_3 = "Firm technology adoption (t-3)",
               
               "pay_or_ecomnod:Ebay_tradable"="Firm technology adoption × eBay-tradable",
               "pay_or_ecomnod_t_1:Ebay_tradable"="Firm technology adoption (t-1) × eBay-tradable",
               "pay_or_ecomnod_t_2:Ebay_tradable"="Firm technology adoption (t-2) × eBay-tradable",
               "pay_or_ecomnod_t_3:Ebay_tradable"="Firm technology adoption (t-3) × eBay-tradable",
               
               "pay_or_ecomnod:China_E_commerce"="Firm technology adoption × China e-commerce",
               "pay_or_ecomnod_t_1:China_E_commerce"="Firm technology adoption (t-1) × China e-commerce",
               "pay_or_ecomnod_t_2:China_E_commerce"="Firm technology adoption (t-2) × China e-commerce",
               "pay_or_ecomnod_t_3:China_E_commerce"="Firm technology adoption (t-3) × China e-commerce",
               
               "pay_or_ecomnod:cons_BEC" = "Firm technology adoption × Consumable",
               "pay_or_ecomnod_t_1:cons_BEC" = "Firm technology adoption (t-1) × Consumable",
               "pay_or_ecomnod_t_2:cons_BEC" = "Firm technology adoption (t-2) × Consumable",
               "pay_or_ecomnod_t_3:cons_BEC" = "Firm technology adoption (t-3) × Consumable",
               
               "pay_or_ecomnod:durable_BEC" = "Firm technology adoption × Durable",
               "pay_or_ecomnod_t_1:durable_BEC" = "Firm technology adoption (t-1) × Durable",
               "pay_or_ecomnod_t_2:durable_BEC" = "Firm technology adoption (t-2) × Durable",
               "pay_or_ecomnod_t_3:durable_BEC" = "Firm technology adoption (t-3) × Durable"
               
               
)


# Regressions with 2 lags (t-2) in technology variable for log. exports and log.imports ----
log_imp_exp_IDN <- reg_models_definitive(tech_var  = "pay_or_ecomnod_t_2", 
                                         dep_var_import = "log_import", 
                                         dep_var_export = "log_export",
                                         import_data = pay_ecom_import_data_IDN,
                                         export_data = pay_ecom_export_data_IDN,
                                         country_name = "Indonesia",
                                         coef_labels = coef_labels, 
                                         dep_var_labels = c("Log.Import", "Log.Export")
)




# Regressions with 2 lags (t-2) in technology variable for number destinations/number sources ----
no_source_dest_IDN <- reg_models_definitive(tech_var  = "pay_or_ecomnod_t_2", 
                                            dep_var_import = "n_countries_import", 
                                            dep_var_export = "n_countries_export",
                                            import_data = pay_ecom_import_data_IDN,
                                            export_data = pay_ecom_export_data_IDN,
                                            country_name = "Indonesia",
                                            coef_labels = coef_labels, 
                                            dep_var_labels = c("No. Sources", "No. Destinations")
)

