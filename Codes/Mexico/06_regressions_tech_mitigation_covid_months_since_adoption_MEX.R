#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Mexico and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script: This script runs the regressions for Mexico for the model that    #
#         measures if tech adoption mitigated COVID impacts. The way we     #
#         measure tech adoption in these regressions is through a variable  #
#         that counts the number of months since the firm adopted the       #
#         technology.                           
#                                                                           #
#                                                                           #                                                                         
#                                                                           #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

source("../src/packages.R")

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)


# Read Mexico data for tech mitigation of COVID (data at the firm-year-month-HS product level)----
import_tech_mitig<-fread("../../Data/Mexico/processed_data/imports_tech_mitigation_model_MEX.csv")
export_tech_mitig<-fread("../../Data/Mexico/processed_data/exports_tech_mitigation_model_MEX.csv")



# Set months since adoption to 0 for those firms that adopted the technology after 2020. The reason 
# is that we only want that this variable is at some point different from 0 for those firms that adopted the 
# technology before 2020 in order to measure how "existing tech" mitigated COVID impacts. In that way, 
# the control group is those firms that did adopt the technology before 2020 and the never adopters. 
# and the treatment group is only the pre-2020 adopters. 

import_tech_mitig<-import_tech_mitig %>% 
  mutate(months_since_adoption = ifelse(adopted_pay_or_ecom_before_2020 == 0, 0, months_since_adoption))

export_tech_mitig<-export_tech_mitig %>% 
  mutate(months_since_adoption = ifelse(adopted_pay_or_ecom_before_2020 == 0, 0, months_since_adoption))


# Functions for running regressions ----
source("../src/functions_reg_tech_covid_mitigation.R")

# Additional cleaning steps ----
import_tech_mitig<-as.data.frame(import_tech_mitig)
export_tech_mitig<-as.data.frame(export_tech_mitig)

# Convert from Boolean to dummy (to avoid _TRUE labels in table)
covid_tech_vars <- c("China_E_commerce", "Ebay_tradable", 
                     "cons_BEC", "durable_BEC")


import_tech_mitig[covid_tech_vars] <- lapply(import_tech_mitig[covid_tech_vars], function(x) 1 * x)
export_tech_mitig[covid_tech_vars] <- lapply(export_tech_mitig[covid_tech_vars], function(x) 1 * x)

# Run regressions ebay, china e-commerce, consumable and durable products ----

# Assign variables names to display in tables
# Assign variables names to display in tables
coef_labels<-c("months_since_adoption:month_mean_stringency_index"="Months Since Adoption × COVID stringency index",
               "months_since_adoption:month_mean_stringency_index:Ebay_tradable" = "Months Since Adoption × COVID stringency index × eBay-Tradable",
               "month_mean_stringency_index:Ebay_tradable" = "COVID stringency index × eBay-Tradable",
               "month_mean_stringency_index:China_E_commerce" = "COVID stringency index × China e-commerce",
               "months_since_adoption:month_mean_stringency_index:China_E_commerce" = "Months Since Adoption × COVID stringency index × China e-commerce",
               "month_mean_stringency_index:cons_BEC" = "COVID stringency index × Consumable",
               "months_since_adoption:month_mean_stringency_index:cons_BEC" = "Months Since Adoption × COVID stringency index × Consumable",
               "month_mean_stringency_index:durable_BEC" = "COVID stringency index × Durable",
               "months_since_adoption:month_mean_stringency_index:durable_BEC" = "Months Since Adoption × COVID stringency index × Durable", 
               "month_mean_stringency_index:adopted_pay_or_ecom_before_2020"="E-payment or E-commerce 2019 × COVID stringency index",
               "month_mean_stringency_index:Ebay_tradable:adopted_pay_or_ecom_before_2020" = "E-payment or E-commerce 2019 × COVID stringency index × eBay-Tradable",
               "month_mean_stringency_index:China_E_commerce:adopted_pay_or_ecom_before_2020" = "E-payment or E-commerce 2019 × COVID stringency index × China e-commerce",
               "month_mean_stringency_index:cons_BEC:adopted_pay_or_ecom_before_2020" = "E-payment or E-commerce 2019 × COVID stringency index × Consumable",
               "month_mean_stringency_index:durable_BEC:adopted_pay_or_ecom_before_2020" = "E-payment or E-commerce 2019 × COVID stringency index × Durable"
)



# Regressions for log imports and log exports -----
log_imp_exp_covid_months_adop_MEX<-reg_models_tech_covid_two_indep_vars(import_data = import_tech_mitig, 
                                                        export_data = export_tech_mitig, 
                                                        country_name = "Mexico", 
                                                        coef_labels = coef_labels, 
                                                        dep_var_import = "log_import", 
                                                        dep_var_export = "log_export", 
                                                        dep_var_labels = c("Log.Import", "Log.Export"), 
                                                        indep_var1 = "months_since_adoption", 
                                                        indep_var2 = "adopted_pay_or_ecom_before_2020"
                                                        )

# The observations removed from these regressions because of NA are due to some HS products that 
# were not matched to the BEC classification dataset.

# Regressions for number of sources and number of destinations -----
no_source_dest_covid_months_adop_MEX<-reg_models_tech_covid_two_indep_vars(import_data = import_tech_mitig, 
                                                           export_data = export_tech_mitig, 
                                                           country_name = "Mexico", 
                                                           coef_labels = coef_labels, 
                                                           dep_var_import = "n_countries_import", 
                                                           dep_var_export = "n_countries_export", 
                                                           dep_var_labels = c("No. Sources", "No. Destinations"), 
                                                           indep_var1 = "months_since_adoption", 
                                                           indep_var2 = "adopted_pay_or_ecom_before_2020"
                                                           )
# The observations removed from these regressions because of NA are due to some HS products that 
# were not matched to the BEC classification dataset

# Regressions for new source/new destination -----
new_source_dest_covid_months_adop_MEX<-reg_models_tech_covid_two_indep_vars(import_data = import_tech_mitig, 
                                                            export_data = export_tech_mitig, 
                                                            country_name = "Mexico", 
                                                            coef_labels = coef_labels, 
                                                            dep_var_import = "new_source", 
                                                            dep_var_export = "new_destination", 
                                                            dep_var_labels = c("New Source", "New Destination"), 
                                                            indep_var1 = "months_since_adoption", 
                                                            indep_var2 = "adopted_pay_or_ecom_before_2020"
                                                            )


# The observations removed from these regressions because of NA are due to some HS products that 
# were not matched to the BEC classification dataset, but also because of some firms that did not 
# exist in the baseline year used for defining the baseline sources and baseline destinations. 



rm(import_tech_mitig, export_tech_mitig)
gc()
