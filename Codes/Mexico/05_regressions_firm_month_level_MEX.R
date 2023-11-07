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

# Read Indonesia data for running regressions that measures if tech adoption affects trade outcomes ----

# Imports data
import_data_MEX<-fread("../../Data/Mexico/processed_data/imports_reg_firm_month_MEX.csv")

# Exports data 
export_data_MEX<-fread("../../Data/Mexico/processed_data/exports_reg_firm_month_MEX.csv")


# Run functions 
source("../src/functions_reg_firm_month_model.R")


# Assign variables names to display in tables -----
coef_labels<-c(pay_or_ecomnod="Firm technology adoption", 
               pay_or_ecomnod_t_1 = "Firm technology adoption (t-1)",
               pay_or_ecomnod_t_2 = "Firm technology adoption (t-2)",
               pay_or_ecomnod_t_3 = "Firm technology adoption (t-3)"
)



# Regressions with 2 lags (t-2) in technology variable for log. exports and log.imports ----

reg_firm_month_MEX <- reg_firm_month(tech_var  = "pay_or_ecomnod_t_2",
                                     import_data = import_data_MEX,
                                     export_data = export_data_MEX,
                                     country_name = "Mexico",
                                     coef_labels = coef_labels
)
