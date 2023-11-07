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

# Libraries to be used ---- 
source("../src/packages.R")

# Import functions for running propensity regressions model with different products categories specifications
source("../src/functions_reg_prop_tech_covid_mitigation.R")

# Run regressions for definitive products categories to be used in the analysis ----


coef_labels<-c("adopted_pay_or_ecom_before_2020:month_mean_stringency_index"="Firm technology adoption pre-2020 × COVID stringency index",
               "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:Ebay_tradable" = "Firm technology adoption pre-2020 × COVID stringency index × eBay-Tradable",
               "month_mean_stringency_index:Ebay_tradable" = "COVID stringency index × eBay-Tradable",
               "month_mean_stringency_index:China_E_commerce" = "COVID stringency index × China e-commerce",
               "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:China_E_commerce" = "Firm technology adoption pre-2020 × COVID stringency index × China e-commerce",
               "month_mean_stringency_index:cons_BEC" = "COVID stringency index × Consumable",
               "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:cons_BEC" = "Firm technology adoption pre-2020 × COVID stringency index × Consumable", 
               "month_mean_stringency_index:durable_BEC" = "COVID stringency index × Durable",
               "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:durable_BEC" = "Firm technology adoption pre-2020 × COVID stringency index × Durable"
)



models_tech_covid_definitive_IND<-prop_reg_models_tech_covid_definitive("India", 
                                                                        coef_labels, 
                                                                        indep_var = "adopted_pay_or_ecom_before_2020")

  
# 
# THE FOLLOWING CODE IS FOR RUNNING THE REGRESSIONS FOR ALL PRODUCTS CATEGORIES
# 
# # Additional cleaning steps ----
# 
# # Convert from Boolean to dummy (to avoid _TRUE labels in table)
# covid_tech_vars <- c("China_E_commerce", 'China_E_commerce_updated', "diff_new_old_China_ecommerce_list", 
#                      "parts_BEC", "cons_dur_BEC", "cons_semi_BEC",
#                      "cons_BEC", "durable_BEC", "durable_semi_BEC", "transp_BEC", 
#                      "hummels_timesensitive", "agric_timesensitive", 
#                      "time_sens_hs2013_russ_typeofgoods", "adopted_pay_or_ecom_before_2020")
# 
# imports_grid<-as.data.frame(imports_grid)
# exports_grid<-as.data.frame(exports_grid)
# 
# imports_grid[covid_tech_vars] <- lapply(imports_grid[covid_tech_vars], function(x) 1 * x)
# exports_grid[covid_tech_vars] <- lapply(exports_grid[covid_tech_vars], function(x) 1 * x)
# 
# # Run regressions ebay china e-commerce products ----
# 
# # Assign variables names to display in tables
# coef_labels<-c("adopted_pay_or_ecom_before_2020:month_mean_stringency_index"="Firm technology adoption pre-2020 × COVID stringency index",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:Ebay_tradable" = "Firm technology adoption pre-2020 × COVID stringency index × eBay-Tradable",
#                "month_mean_stringency_index:Ebay_tradable" = "COVID stringency index × eBay-Tradable",
#                "month_mean_stringency_index:China_E_commerce" = "COVID stringency index × China e-commerce",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:China_E_commerce" = "Firm technology adoption pre-2020 × COVID stringency index × China e-commerce",
#                "month_mean_stringency_index:China_E_commerce_updated" = "COVID stringency index × China e-commerce upd.",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:China_E_commerce_updated" = "Firm technology adoption pre-2020 × COVID stringency index × China e-commerce upd.", 
#                "month_mean_stringency_index:diff_new_old_China_ecommerce_list" = "COVID stringency index × Diff. China e-commerce",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:diff_new_old_China_ecommerce_list" = "Firm technology adoption pre-2020 × COVID stringency index × Diff. China e-commerce"
# )
# 
# 
# models_tech_covid_ebay_china_IND<-prop_reg_models_tech_covid_ebay_china(imports_grid, 
#                                                                    exports_grid, 
#                                                                    "India", 
#                                                                    coef_labels)
# 
# 
# 
# # Run regressions BEC products ----
# coef_labels<-c("adopted_pay_or_ecom_before_2020:month_mean_stringency_index"="Firm technology adoption pre-2020 × COVID stringency index",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:parts_BEC" = "Firm technology adoption pre-2020 × COVID stringency index × Parts",
#                "month_mean_stringency_index:parts_BEC" = "COVID stringency index × Parts",
#                "month_mean_stringency_index:cons_dur_BEC" = "COVID stringency index × Consumable and Durable",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:cons_dur_BEC" = "Firm technology adoption pre-2020 × COVID stringency index × Consumable and Durable",
#                "month_mean_stringency_index:cons_semi_BEC" = "COVID stringency index × Consumable and Semi-durable",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:cons_semi_BEC" = "Firm technology adoption pre-2020 × COVID stringency index × Consumable and Semi-durable", 
#                "month_mean_stringency_index:cons_BEC" = "COVID stringency index × Consumable",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:cons_BEC" = "Firm technology adoption pre-2020 × COVID stringency index × Consumable",
#                "month_mean_stringency_index:transp_BEC" = "COVID stringency index × Transport",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:transp_BEC" = "Firm technology adoption pre-2020 × COVID stringency index × Transport",
#                "month_mean_stringency_index:durable_BEC" = "COVID stringency index × Durable",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:durable_BEC" = "Firm technology adoption pre-2020 × COVID stringency index × Durable",
#                "month_mean_stringency_index:durable_semi_BEC" = "COVID stringency index × Semi-Durable",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:durable_semi_BEC" = "Firm technology adoption pre-2020 × COVID stringency index × Semi-Durable")
# 
# 
# models_tech_covid_BEC_IND <- prop_reg_models_tech_covid_BEC(imports_grid, 
#                                                             exports_grid, 
#                                                             "India", 
#                                                             coef_labels)
# 
# 
# # Run regressions for time sensitive products ----
# coef_labels<-c("adopted_pay_or_ecom_before_2020:month_mean_stringency_index"="Firm technology adoption pre-2020 × COVID stringency index",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:component" = "Firm technology adoption pre-2020 × COVID stringency index × Component",
#                "month_mean_stringency_index:component" = "COVID stringency index × Component",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:fresh" = "Firm technology adoption pre-2020 × COVID stringency index × Fresh",
#                "month_mean_stringency_index:fresh" = "COVID stringency index × Fresh",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:frozen" = "Firm technology adoption pre-2020 × COVID stringency index × Frozen", 
#                "month_mean_stringency_index:frozen" = "COVID stringency index × Frozen",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:hummels_timesensitive" = "Firm technology adoption pre-2020 × COVID stringency index × Hummels Time-Sensitive",
#                "month_mean_stringency_index:hummels_timesensitive" = "COVID stringency index × Hummels Time-Sensitive",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:agric_timesensitive" = "Firm technology adoption pre-2020 × COVID stringency index × Agricultural Time-Sensitive",
#                "month_mean_stringency_index:agric_timesensitive" = "COVID stringency index × Agricultural Time-Sensitive",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:time_sens_hs2013_russ_typeofgoods" = "Firm technology adoption pre-2020 × COVID stringency index × Hummels and Schaur Time-Sensitive",
#                "month_mean_stringency_index:time_sens_hs2013_russ_typeofgoods" = "COVID stringency index × Hummels and Schaur Time-Sensitive"
# )
# 
# 
# models_covid_time_sensitivity_IND<-prop_reg_models_covid_time_sensitivity(imports_grid, 
#                                                                           exports_grid, 
#                                                                           "India", 
#                                                                           coef_labels)
# 
# 
# # Run regressions for COVID impacted products -----
# coef_labels<-c("adopted_pay_or_ecom_before_2020:month_mean_stringency_index"="Firm technology adoption pre-2020 × COVID stringency index",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:letter_credit_use" = "Firm technology adoption pre-2020 × COVID stringency index × Letter Credit Use",
#                "month_mean_stringency_index:letter_credit_use" = "COVID stringency index × Letter Credit Use",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:mean_remote_work_ISIC" = "Firm technology adoption pre-2020 × COVID stringency index × Feasibility Remote Work",
#                "month_mean_stringency_index:mean_remote_work_ISIC" = "COVID stringency index × Feasibility Remote Work",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:relationship_stickiness" = "Firm technology adoption pre-2020 × COVID stringency index × Relationship Stickiness", 
#                "month_mean_stringency_index:relationship_stickiness" = "COVID stringency index × Relationship Stickiness",
#                "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:frac_lib_diff" = "Firm technology adoption pre-2020 × COVID stringency index × Fraction inputs not sold on exchange and not ref priced",
#                "month_mean_stringency_index:frac_lib_diff" = "COVID stringency index × Fraction inputs not sold on exchange and not ref priced")
# 
# 
# models_covid_prods_impacted_IND<-prop_reg_model_covid_prods_impacted(imports_grid, 
#                                                                      exports_grid, 
#                                                                      "India", 
#                                                                      coef_labels)
# 
# 
# 
# 
# # Regressions for Capital, Intermediate Goods --------
# 
# # Coefficient labels for tables
# coef_labels <- c("adopted_pay_or_ecom_before_2020:month_mean_stringency_index"="Firm technology adoption pre-2020 × COVID stringency index",
#                  "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:CAP" = "Firm technology adoption pre-2020 × COVID stringency index × Capital", 
#                  "month_mean_stringency_index:CAP" = "COVID stringency index × Capital",
#                  "adopted_pay_or_ecom_before_2020:month_mean_stringency_index:INT" = "Firm technology adoption pre-2020 × COVID stringency index × Intermediate",
#                  "month_mean_stringency_index:INT" = "COVID stringency index × Intermediate"
#                  )
# 
# 
# 
# models_covid_prods_cap_int_IND<-prop_reg_model_covid_cap_int(imports_grid, 
#                                                     exports_grid, 
#                                                     "India", 
#                                                     coef_labels)
# 
# 
# 
# 
