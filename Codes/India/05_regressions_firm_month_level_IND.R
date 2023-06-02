#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
#                                                                           #
# This program run the regressions of the model that measures if tech       #
# adoption affects trade outcomes                                           #                                                                         
#                                                                           #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Read India data for running regressions that measures if tech adoption affects trade outcomes ----

# Imports data
import_data_IND<-fread("../../Data/India/processed_data/imports_reg_firm_month_IND.csv")

# Exports data 
export_data_IND<-fread("../../Data/India/processed_data/exports_reg_firm_month_IND.csv")


# Run functions 
source("../src/functions_reg_firm_month_model.R")


# Assign variables names to display in tables -----
coef_labels<-c(pay_or_ecomnod="E-payment or E-commerce", 
               pay_or_ecomnod_t_1 = "E-payment or E-commerce (t-1)",
               pay_or_ecomnod_t_2 = "E-payment or E-commerce (t-2)",
               pay_or_ecomnod_t_3 = "E-payment or E-commerce (t-3)"
               )



# Regressions with 2 lags (t-2) in technology variable for log. exports and log.imports ----

reg_firm_month_IND <- reg_firm_month(tech_var  = "pay_or_ecomnod_t_2",
                                     import_data = import_data_IND,
                                     export_data = export_data_IND,
                                     country_name = "India",
                                     coef_labels = coef_labels
                                     )



