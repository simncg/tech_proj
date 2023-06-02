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


# Read India data for tech mitigation of COVID ----
import_tech_mitig<-fread("../../Data/India/processed_data/imports_tech_mitigation_model_IND.csv")
export_tech_mitig<-fread("../../Data/India/processed_data/exports_tech_mitigation_model_IND.csv")


# Functions for running regressions ----
source("../src/functions_reg_tech_covid_mitigation.R")

# Additional cleaning steps ----
# Convert from Boolean to dummy (to avoid _TRUE labels in table)
covid_tech_vars <- c("China_E_commerce", "Ebay_tradable", 
                     "cons_BEC", "durable_BEC")

import_tech_mitig<-as.data.frame(import_tech_mitig)
export_tech_mitig<-as.data.frame(export_tech_mitig)

import_tech_mitig[covid_tech_vars] <- lapply(import_tech_mitig[covid_tech_vars], function(x) 1 * x)
export_tech_mitig[covid_tech_vars] <- lapply(export_tech_mitig[covid_tech_vars], function(x) 1 * x)

# Run regressions ebay, china e-commerce, consumable and durable products ----

# Assign variables names to display in tables
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


# Regressions for log imports and log exports -----
log_imp_exp_covid_IND<-reg_models_tech_covid_definitive(import_data = import_tech_mitig, 
                                                        export_data = export_tech_mitig, 
                                                        country_name = "India", 
                                                        coef_labels = coef_labels, 
                                                        dep_var_import = "log_import", 
                                                        dep_var_export = "log_export", 
                                                        dep_var_labels = c("Log.Import", "Log.Export")
                                                        )

# Regressions for number of sources and number of destinations -----
no_source_dest_covid_IND<-reg_models_tech_covid_definitive(import_data = import_tech_mitig, 
                                                        export_data = export_tech_mitig, 
                                                        country_name = "India", 
                                                        coef_labels = coef_labels, 
                                                        dep_var_import = "n_countries_import", 
                                                        dep_var_export = "n_countries_export", 
                                                        dep_var_labels = c("No. Sources", "No. Destinations")
                                                        )
# Regressions for new source/new destination -----
new_source_dest_covid_IND<-reg_models_tech_covid_definitive(import_data = import_tech_mitig, 
                                                        export_data = export_tech_mitig, 
                                                        country_name = "India", 
                                                        coef_labels = coef_labels, 
                                                        dep_var_import = "new_source", 
                                                        dep_var_export = "new_destination", 
                                                        dep_var_labels = c("New Source", "New Destination")
                                                        )
