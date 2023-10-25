#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
#                                                                           #
#                                                                           #                                                                         
#                                                                           #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Read India data at the firm-month level for regression that measures if tech adoption mitigated covid impacts ----
import_tech_mitig<-fread("../../Data/India/processed_data/imports_tech_mitigation_reg_firm_month_IND.csv")
export_tech_mitig<-fread("../../Data/India/processed_data/exports_tech_mitigation_reg_firm_month_IND.csv")


# Functions for running regressions ----
source("../src/functions_reg_tech_covid_mitigation_firm_month_model.R")



# Additional cleaning steps ----
import_tech_mitig<-as.data.frame(import_tech_mitig)
export_tech_mitig<-as.data.frame(export_tech_mitig)

# Run regressions ebay, china e-commerce, consumable and durable products ----

# Assign variables names to display in tables
coef_labels<-c("adopted_pay_or_ecom_before_2020:month_mean_stringency_index"="Firm technology adoption pre-2020 × COVID stringency index")


# Regressions for log imports and log exports -----
reg_firm_month_tech_covid_mitig_IND<-reg_firm_month_tech_covid_mitig(import_data = import_tech_mitig, 
                                                                     export_data = export_tech_mitig, 
                                                                     country_name = "India", 
                                                                     coef_labels = coef_labels
                                                                     )
