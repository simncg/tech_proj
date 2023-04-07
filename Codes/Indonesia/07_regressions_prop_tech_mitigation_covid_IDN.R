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

# Libraries to be used ---- 
source("../src/packages.R")

# Import functions for running propensity regressions model with different products categories specifications
source("../src/functions_reg_prop_tech_covid_mitigation.R")

# Free memory
gc()


# Run propensity regressions for definitive products categories to be used in the analysis ----

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



models_tech_covid_definitive_IDN<-prop_reg_models_tech_covid_definitive("Indonesia", 
                                                                        coef_labels)

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
#                      "time_sens_hs2013_russ_typeofgoods", "adopted_pay_or_ecom_before_2019")
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
# coef_labels<-c("adopted_pay_or_ecom_before_2019:month_mean_stringency_index"="E-payment or E-commerce 2019 × Monthly Avg. Stringency Index",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:Ebay_tradable" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × eBay-Tradable",
#                "month_mean_stringency_index:Ebay_tradable" = "Monthly Avg. Stringency Index × eBay-Tradable",
#                "month_mean_stringency_index:China_E_commerce" = "Monthly Avg. Stringency Index × China e-commerce",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × China e-commerce",
#                "month_mean_stringency_index:China_E_commerce_updated" = "Monthly Avg. Stringency Index × China e-commerce upd.",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce_updated" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × China e-commerce upd.", 
#                "month_mean_stringency_index:diff_new_old_China_ecommerce_list" = "Monthly Avg. Stringency Index × Diff. China e-commerce",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:diff_new_old_China_ecommerce_list" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Diff. China e-commerce"
# )
# 
# 
# models_tech_covid_ebay_china_IDN<-prop_reg_models_tech_covid_ebay_china(imports_grid, 
#                                                                    exports_grid, 
#                                                                    "Indonesia", 
#                                                                    coef_labels)
# 
# 
# 
# # Run regressions BEC products ----
# coef_labels<-c("adopted_pay_or_ecom_before_2019:month_mean_stringency_index"="E-payment or E-commerce 2019 × Monthly Avg. Stringency Index",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:parts_BEC" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Parts",
#                "month_mean_stringency_index:parts_BEC" = "Monthly Avg. Stringency Index × Parts",
#                "month_mean_stringency_index:cons_dur_BEC" = "Monthly Avg. Stringency Index × Consumable and Durable",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_dur_BEC" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Consumable and Durable",
#                "month_mean_stringency_index:cons_semi_BEC" = "Monthly Avg. Stringency Index × Consumable and Semi-durable",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_semi_BEC" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Consumable and Semi-durable", 
#                "month_mean_stringency_index:cons_BEC" = "Monthly Avg. Stringency Index × Consumable",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_BEC" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Consumable",
#                "month_mean_stringency_index:transp_BEC" = "Monthly Avg. Stringency Index × Transport",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:transp_BEC" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Transport",
#                "month_mean_stringency_index:durable_BEC" = "Monthly Avg. Stringency Index × Durable",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_BEC" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Durable",
#                "month_mean_stringency_index:durable_semi_BEC" = "Monthly Avg. Stringency Index × Semi-Durable",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_semi_BEC" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Semi-Durable")
# 
# 
# models_tech_covid_BEC_IDN <- prop_reg_models_tech_covid_BEC(imports_grid, 
#                                                             exports_grid, 
#                                                             "Indonesia", 
#                                                             coef_labels)
# 
# 
# # Run regressions for time sensitive products ----
# coef_labels<-c("adopted_pay_or_ecom_before_2019:month_mean_stringency_index"="E-payment or E-commerce 2019 × Monthly Avg. Stringency Index",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:component" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Component",
#                "month_mean_stringency_index:component" = "Monthly Avg. Stringency Index × Component",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:fresh" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Fresh",
#                "month_mean_stringency_index:fresh" = "Monthly Avg. Stringency Index × Fresh",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:frozen" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Frozen", 
#                "month_mean_stringency_index:frozen" = "Monthly Avg. Stringency Index × Frozen",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:hummels_timesensitive" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Hummels Time-Sensitive",
#                "month_mean_stringency_index:hummels_timesensitive" = "Monthly Avg. Stringency Index × Hummels Time-Sensitive",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:agric_timesensitive" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Agricultural Time-Sensitive",
#                "month_mean_stringency_index:agric_timesensitive" = "Monthly Avg. Stringency Index × Agricultural Time-Sensitive",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:time_sens_hs2013_russ_typeofgoods" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Hummels and Schaur Time-Sensitive",
#                "month_mean_stringency_index:time_sens_hs2013_russ_typeofgoods" = "Monthly Avg. Stringency Index × Hummels and Schaur Time-Sensitive"
# )
# 
# 
# models_covid_time_sensitivity_IDN<-prop_reg_models_covid_time_sensitivity(imports_grid, 
#                                                                           exports_grid, 
#                                                                           "Indonesia", 
#                                                                           coef_labels)
# 
# 
# # Run regressions for COVID impacted products -----
# coef_labels<-c("adopted_pay_or_ecom_before_2019:month_mean_stringency_index"="E-payment or E-commerce 2019 × Monthly Avg. Stringency Index",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:letter_credit_use" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Letter Credit Use",
#                "month_mean_stringency_index:letter_credit_use" = "Monthly Avg. Stringency Index × Letter Credit Use",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:mean_remote_work_ISIC" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Feasibility Remote Work",
#                "month_mean_stringency_index:mean_remote_work_ISIC" = "Monthly Avg. Stringency Index × Feasibility Remote Work",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:relationship_stickiness" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Relationship Stickiness", 
#                "month_mean_stringency_index:relationship_stickiness" = "Monthly Avg. Stringency Index × Relationship Stickiness",
#                "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:frac_lib_diff" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Fraction inputs not sold on exchange and not ref priced",
#                "month_mean_stringency_index:frac_lib_diff" = "Monthly Avg. Stringency Index × Fraction inputs not sold on exchange and not ref priced")
# 
# 
# models_covid_prods_impacted_IDN<-prop_reg_model_covid_prods_impacted(imports_grid, 
#                                                                      exports_grid, 
#                                                                      "Indonesia", 
#                                                                      coef_labels)
# 
# 
# 
# 
# # Regressions for Capital, Intermediate Goods --------
# 
# # Coefficient labels for tables
# coef_labels <- c("adopted_pay_or_ecom_before_2019:month_mean_stringency_index"="E-payment or E-commerce 2019 × Monthly Avg. Stringency Index",
#                  "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:CAP" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Capital", 
#                  "month_mean_stringency_index:CAP" = "Monthly Avg. Stringency Index × Capital",
#                  "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:INT" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Intermediate",
#                  "month_mean_stringency_index:INT" = "Monthly Avg. Stringency Index × Intermediate"
#                  )
# 
# 
# 
# models_covid_prods_cap_int_IDN<-prop_reg_model_covid_cap_int(imports_grid, 
#                                                     exports_grid, 
#                                                     "Indonesia", 
#                                                     coef_labels)
# 
# 
# 
# 
