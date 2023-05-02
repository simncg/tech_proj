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
                     'China_E_commerce_updated', "diff_new_old_China_ecommerce_list")


pay_ecom_import_data_IDN<-as.data.frame(pay_ecom_import_data_IDN)
pay_ecom_export_data_IDN<-as.data.frame(pay_ecom_export_data_IDN)

pay_ecom_import_data_IDN[ebay_china_vars] <- lapply(pay_ecom_import_data_IDN[ebay_china_vars], function(x) 1 * x)
pay_ecom_export_data_IDN[ebay_china_vars] <- lapply(pay_ecom_export_data_IDN[ebay_china_vars], function(x) 1 * x)



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
               
               "pay_or_ecomnod:China_E_commerce_updated"="E-payment or E-commerce × China e-commerce upd.",
               "pay_or_ecomnod_t_1:China_E_commerce_updated"="E-payment or E-commerce (t-1) × China e-commerce upd.",
               "pay_or_ecomnod_t_2:China_E_commerce_updated"="E-payment or E-commerce (t-2) × China e-commerce upd.",
               "pay_or_ecomnod_t_3:China_E_commerce_updated"="E-payment or E-commerce (t-3) × China e-commerce upd.",
               
               "pay_or_ecomnod:diff_new_old_China_ecommerce_list" = "E-payment or E-commerce × Diff. China e-commerce",
               "pay_or_ecomnod_t_1:diff_new_old_China_ecommerce_list" = "E-payment or E-commerce (t-1) × Diff. China e-commerce",
               "pay_or_ecomnod_t_2:diff_new_old_China_ecommerce_list" = "E-payment or E-commerce (t-2) × Diff. China e-commerce",
               "pay_or_ecomnod_t_3:diff_new_old_China_ecommerce_list" = "E-payment or E-commerce (t-3) × Diff. China e-commerce"
)


# Regressions with 1 lag (t-1) in technology variable
models_ebay_china_IDN_t_1 <- reg_models_ebay_china(tech_var = "pay_or_ecomnod_t_1", 
                                                   import_data = pay_ecom_import_data_IDN,
                                                   export_data = pay_ecom_export_data_IDN, 
                                                   coef_labels = coef_labels, 
                                                   country_name = "Indonesia")

# Regressions with 2 lags (t-2) in technology variable
models_ebay_china_IDN_t_2 <- reg_models_ebay_china("pay_or_ecomnod_t_2", 
                                                   import_data = pay_ecom_import_data_IDN,
                                                   export_data = pay_ecom_export_data_IDN, 
                                                   coef_labels = coef_labels, 
                                                   country_name = "Indonesia")

# Regressions with 3 lags (t-3) in technology variable
models_ebay_china_IDN_t_3 <- reg_models_ebay_china("pay_or_ecomnod_t_3", 
                                                   import_data = pay_ecom_import_data_IDN,
                                                   export_data = pay_ecom_export_data_IDN, 
                                                   coef_labels = coef_labels, 
                                                   country_name = "Indonesia")


gc()

# Regressions with BEC products classification ----

# Convert from boolean to dummy
bec_vars<-c("parts_BEC", "cons_dur_BEC", "cons_semi_BEC",
            "cons_BEC", "durable_BEC", "durable_semi_BEC")

pay_ecom_import_data_IDN[bec_vars] <- lapply(pay_ecom_import_data_IDN[bec_vars], function(x) 1 * x)
pay_ecom_export_data_IDN[bec_vars] <- lapply(pay_ecom_export_data_IDN[bec_vars], function(x) 1 * x)


# Coefficient labels for tables
coef_labels<-c(pay_or_ecomnod="E-payment or E-commerce", 
               pay_or_ecomnod_t_1="E-payment or E-commerce (t-1)", 
               pay_or_ecomnod_t_2="E-payment or E-commerce (t-2)", 
               pay_or_ecomnod_t_3="E-payment or E-commerce (t-3)", 
               
               "pay_or_ecomnod:parts_BEC"="E-payment or E-commerce × Parts",
               "pay_or_ecomnod_t_1:parts_BEC"="E-payment or E-commerce (t-1) × Parts",
               "pay_or_ecomnod_t_2:parts_BEC"="E-payment or E-commerce (t-2) × Parts",
               "pay_or_ecomnod_t_3:parts_BEC"="E-payment or E-commerce (t-3) × Parts",
               
               "pay_or_ecomnod:cons_dur_BEC"="E-payment or E-commerce × Consumable and Durable",
               "pay_or_ecomnod_t_1:cons_dur_BEC"="E-payment or E-commerce (t-1) × Consumable and Durable",
               "pay_or_ecomnod_t_2:cons_dur_BEC"="E-payment or E-commerce (t-2) × Consumable and Durable",
               "pay_or_ecomnod_t_3:cons_dur_BEC"="E-payment or E-commerce (t-3) × Consumable and Durable",
               
               "pay_or_ecomnod:cons_semi_BEC"="E-payment or E-commerce × Consumable and Semi-durable",
               "pay_or_ecomnod_t_1:cons_semi_BEC"="E-payment or E-commerce (t-1) × Consumable and Semi-durable",
               "pay_or_ecomnod_t_2:cons_semi_BEC"="E-payment or E-commerce (t-2) × Consumable and Semi-durable",
               "pay_or_ecomnod_t_3:cons_semi_BEC"="E-payment or E-commerce (t-3) × Consumable and Semi-durable",
               
               "pay_or_ecomnod:cons_BEC" = "E-payment or E-commerce × Consumable",
               "pay_or_ecomnod_t_1:cons_BEC" = "E-payment or E-commerce (t-1) × Consumable",
               "pay_or_ecomnod_t_2:cons_BEC" = "E-payment or E-commerce (t-2) × Consumable",
               "pay_or_ecomnod_t_3:cons_BEC" = "E-payment or E-commerce (t-3) × Consumable",
               
               "pay_or_ecomnod:durable_BEC" = "E-payment or E-commerce × Durable",
               "pay_or_ecomnod_t_1:durable_BEC" = "E-payment or E-commerce (t-1) × Durable",
               "pay_or_ecomnod_t_2:durable_BEC" = "E-payment or E-commerce (t-2) × Durable",
               "pay_or_ecomnod_t_3:durable_BEC" = "E-payment or E-commerce (t-3) × Durable",
               
               "pay_or_ecomnod:durable_semi_BEC" = "E-payment or E-commerce × Semi-durable",
               "pay_or_ecomnod_t_1:durable_semi_BEC" = "E-payment or E-commerce (t-1) × Semi-durable", 
               "pay_or_ecomnod_t_2:durable_semi_BEC" = "E-payment or E-commerce (t-2) × Semi-durable", 
               "pay_or_ecomnod_t_3:durable_semi_BEC" = "E-payment or E-commerce (t-3) × Semi-durable", 
               
               "pay_or_ecomnod:transp_BECTRUE" = "E-payment or E-commerce × Transport",
               "pay_or_ecomnod_t_1:transp_BECTRUE" = "E-payment or E-commerce (t-1) × Transport",
               "pay_or_ecomnod_t_2:transp_BECTRUE" = "E-payment or E-commerce (t-2) × Transport",
               "pay_or_ecomnod_t_3:transp_BECTRUE" = "E-payment or E-commerce (t-3) × Transport"
)

# Regressions with 1 lag (t-1) in technology variable
models_BEC_IDN_t_1 <- reg_models_BEC("pay_or_ecomnod_t_1", 
                                     import_data = pay_ecom_import_data_IDN,
                                     export_data = pay_ecom_export_data_IDN, 
                                     coef_labels = coef_labels, 
                                     country_name = "Indonesia")

# Regressions with 2 lags (t-2) in technology variable
models_BEC_IDN_t_2 <- reg_models_BEC("pay_or_ecomnod_t_2", 
                                     import_data = pay_ecom_import_data_IDN,
                                     export_data = pay_ecom_export_data_IDN, 
                                     coef_labels = coef_labels, 
                                     country_name = "Indonesia")

# Regressions with 3 lags (t-3) in technology variable
models_BEC_IDN_t_3 <- reg_models_BEC("pay_or_ecomnod_t_3", 
                                     import_data = pay_ecom_import_data_IDN,
                                     export_data = pay_ecom_export_data_IDN, 
                                     coef_labels = coef_labels, 
                                     country_name = "Indonesia")



# Regression with time-sensitive products variables ----
gc()

# Convert from Boolean to dummy (to avoid label "_TRUE" in tables)
time_sens_vars<-c("component", "fresh", "frozen", "hummels_timesensitive",
                  "agric_timesensitive", "time_sens_hs2013_russ_typeofgoods")

pay_ecom_import_data_IDN[time_sens_vars] <- lapply(pay_ecom_import_data_IDN[time_sens_vars], function(x) 1 * x)
pay_ecom_export_data_IDN[time_sens_vars] <- lapply(pay_ecom_export_data_IDN[time_sens_vars], function(x) 1 * x)


# Assign variables names to display in tables
coef_labels<-c(pay_or_ecomnod="E-payment or E-commerce", 
               pay_or_ecomnod_t_1="E-payment or E-commerce (t-1)", 
               pay_or_ecomnod_t_2="E-payment or E-commerce (t-2)", 
               pay_or_ecomnod_t_3="E-payment or E-commerce (t-3)", 
               
               "pay_or_ecomnod:component"="E-payment or E-commerce × Component ",
               "pay_or_ecomnod_t_1:component"="E-payment or E-commerce (t-1) × Component ",
               "pay_or_ecomnod_t_2:component"="E-payment or E-commerce (t-2) × Component ",
               "pay_or_ecomnod_t_3:component"="E-payment or E-commerce (t-3) × Component ",
               
               "pay_or_ecomnod:fresh"="E-payment or E-commerce × Fresh",
               "pay_or_ecomnod_t_1:fresh"="E-payment or E-commerce (t-1) × Fresh",
               "pay_or_ecomnod_t_2:fresh"="E-payment or E-commerce (t-2) × Fresh",
               "pay_or_ecomnod_t_3:fresh"="E-payment or E-commerce (t-3) × Fresh",
               
               "pay_or_ecomnod:frozen"="E-payment or E-commerce × Frozen",
               "pay_or_ecomnod_t_1:frozen"="E-payment or E-commerce (t-1) × Frozen",
               "pay_or_ecomnod_t_2:frozen"="E-payment or E-commerce (t-2) × Frozen",
               "pay_or_ecomnod_t_3:frozen"="E-payment or E-commerce (t-3) × Frozen",
               
               "pay_or_ecomnod:hummels_timesensitive" = "E-payment or E-commerce × Hummels Time-Sensitive",
               "pay_or_ecomnod_t_1:hummels_timesensitive" = "E-payment or E-commerce (t-1) × Hummels Time-Sensitive",
               "pay_or_ecomnod_t_2:hummels_timesensitive" = "E-payment or E-commerce (t-2) × Hummels Time-Sensitive",
               "pay_or_ecomnod_t_3:hummels_timesensitive" = "E-payment or E-commerce (t-3) × Hummels Time-Sensitive",
               
               "pay_or_ecomnod:time_sens_hs2013_russ_typeofgoods" = "E-payment or E-commerce × Hummels and Schaur Time-Sensitive",
               "pay_or_ecomnod_t_1:time_sens_hs2013_russ_typeofgoods" = "E-payment or E-commerce (t-1) × Hummels and Schaur Time-Sensitive",
               "pay_or_ecomnod_t_2:time_sens_hs2013_russ_typeofgoods" = "E-payment or E-commerce (t-2) × Hummels and Schaur Time-Sensitive",
               "pay_or_ecomnod_t_3:time_sens_hs2013_russ_typeofgoods" = "E-payment or E-commerce (t-3) × Hummels and Schaur Time-Sensitive",
               
               "pay_or_ecomnod:agric_timesensitive"="E-payment or E-commerce × Agricultural Time-Sensitive",
               "pay_or_ecomnod_t_1:agric_timesensitive"="E-payment or E-commerce (t-1) × Agricultural Time-Sensitive",
               "pay_or_ecomnod_t_2:agric_timesensitive"="E-payment or E-commerce (t-2) × Agricultural Time-Sensitive",
               "pay_or_ecomnod_t_3:agric_timesensitive"="E-payment or E-commerce (t-3) × Agricultural Time-Sensitive"
               
)



# Regressions with 1 lag (t-1) in technology variable
models_time_sens_IDN_t_1 <- reg_models_time_sens("pay_or_ecomnod_t_1", 
                                                 import_data = pay_ecom_import_data_IDN,
                                                 export_data = pay_ecom_export_data_IDN, 
                                                 coef_labels = coef_labels, 
                                                 country_name = "Indonesia")

# Regressions with 2 lags (t-2) in technology variable
models_time_sens_IDN_t_2 <- reg_models_time_sens("pay_or_ecomnod_t_2", 
                                                 import_data = pay_ecom_import_data_IDN,
                                                 export_data = pay_ecom_export_data_IDN, 
                                                 coef_labels = coef_labels, 
                                                 country_name = "Indonesia")

# Regressions with 3 lags (t-3) in technology variable
models_time_sens_IDN_t_3 <- reg_models_time_sens("pay_or_ecomnod_t_3", 
                                                 import_data = pay_ecom_import_data_IDN,
                                                 export_data = pay_ecom_export_data_IDN, 
                                                 coef_labels = coef_labels, 
                                                 country_name = "Indonesia")

gc()


# Regressions with other classification for HS Products (Letter credit use, mean remote work, relationship stickiness)---- 

# Assign variables names to display in tables
coef_labels<-c(pay_or_ecomnod="E-payment or E-commerce", 
               pay_or_ecomnod_t_1="E-payment or E-commerce (t-1)", 
               pay_or_ecomnod_t_2="E-payment or E-commerce (t-2)", 
               pay_or_ecomnod_t_3="E-payment or E-commerce (t-3)", 
               
               "pay_or_ecomnod:relationship_stickiness"="E-payment or E-commerce × Relationship Stickiness",
               "pay_or_ecomnod_t_1:relationship_stickiness"="E-payment or E-commerce (t-1) × Relationship Stickiness",
               "pay_or_ecomnod_t_2:relationship_stickiness"="E-payment or E-commerce (t-2) × Relationship Stickiness",
               "pay_or_ecomnod_t_3:relationship_stickiness"="E-payment or E-commerce (t-3) × Relationship Stickiness",
               
               "pay_or_ecomnod:letter_credit_use"="E-payment or E-commerce × Letter Credit Use",
               "pay_or_ecomnod_t_1:letter_credit_use"="E-payment or E-commerce (t-1) × Letter Credit Use",
               "pay_or_ecomnod_t_2:letter_credit_use"="E-payment or E-commerce (t-2) × Letter Credit Use",
               "pay_or_ecomnod_t_3:letter_credit_use"="E-payment or E-commerce (t-3) × Letter Credit Use",
               
               "pay_or_ecomnod:mean_remote_work_ISIC"="E-payment or E-commerce × Mean Remote Work ISIC",
               "pay_or_ecomnod_t_1:mean_remote_work_ISIC"="E-payment or E-commerce (t-1) × Mean Remote Work ISIC",
               "pay_or_ecomnod_t_2:mean_remote_work_ISIC"="E-payment or E-commerce (t-2) × Mean Remote Work ISIC",
               "pay_or_ecomnod_t_3:mean_remote_work_ISIC"="E-payment or E-commerce (t-3) × Mean Remote Work ISIC",
               
               "pay_or_ecomnod:frac_lib_diff" = "E-payment or E-commerce × Fraction inputs not sold on exchange and not ref priced",
               "pay_or_ecomnod_t_1:frac_lib_diff" = "E-payment or E-commerce (t-1) × Fraction inputs not sold on exchange and not ref priced",
               "pay_or_ecomnod_t_2:frac_lib_diff" = "E-payment or E-commerce (t-2) × Fraction inputs not sold on exchange and not ref priced",
               "pay_or_ecomnod_t_3:frac_lib_diff" = "E-payment or E-commerce (t-3) × Fraction inputs not sold on exchange and not ref priced")



# Regressions with 1 lag (t-1) in technology variable
models_other_HS_IDN_t_1 <- reg_models_other_HS("pay_or_ecomnod_t_1", 
                                               import_data = pay_ecom_import_data_IDN,
                                               export_data = pay_ecom_export_data_IDN, 
                                               coef_labels = coef_labels, 
                                               country_name = "Indonesia")

# Regressions with 2 lags (t-2) in technology variable
models_other_HS_IDN_t_2 <- reg_models_other_HS("pay_or_ecomnod_t_2", 
                                               import_data = pay_ecom_import_data_IDN,
                                               export_data = pay_ecom_export_data_IDN, 
                                               coef_labels = coef_labels, 
                                               country_name = "Indonesia")

# Regressions with 3 lags (t-3) in technology variable
models_other_HS_IDN_t_3 <- reg_models_other_HS("pay_or_ecomnod_t_3", 
                                               import_data = pay_ecom_import_data_IDN,
                                               export_data = pay_ecom_export_data_IDN, 
                                               coef_labels = coef_labels, 
                                               country_name = "Indonesia")

gc()




# Regressions for Capital, Intermediate Goods --------

# From boolean to dummy 
cap_int_vars<-c("CAP", "INT")

pay_ecom_import_data_IDN[cap_int_vars] <- lapply(pay_ecom_import_data_IDN[cap_int_vars], function(x) 1 * x)
pay_ecom_export_data_IDN[cap_int_vars] <- lapply(pay_ecom_export_data_IDN[cap_int_vars], function(x) 1 * x)


# Coefficient labels for tables
coef_labels<-c(pay_or_ecomnod="E-payment or E-commerce", 
               pay_or_ecomnod_t_1="E-payment or E-commerce (t-1)", 
               pay_or_ecomnod_t_2="E-payment or E-commerce (t-2)", 
               pay_or_ecomnod_t_3="E-payment or E-commerce (t-3)", 
               
               "pay_or_ecomnod:CAP"="E-payment or E-commerce × Capital",
               "pay_or_ecomnod_t_1:CAP"="E-payment or E-commerce (t-1) × Capital",
               "pay_or_ecomnod_t_2:CAP"="E-payment or E-commerce (t-2) × Capital",
               "pay_or_ecomnod_t_3:CAP"="E-payment or E-commerce (t-3) × Capital",
               
               "pay_or_ecomnod:INT"="E-payment or E-commerce × Intermediate",
               "pay_or_ecomnod_t_1:INT"="E-payment or E-commerce (t-1) × Intermediate",
               "pay_or_ecomnod_t_2:INT"="E-payment or E-commerce (t-2) × Intermediate",
               "pay_or_ecomnod_t_3:INT"="E-payment or E-commerce (t-3) × Intermediate"
)




# Regressions with 1 lag (t-1) in technology variable
models_cap_int_IDN_t_1 <- reg_models_cap_int("pay_or_ecomnod_t_1", 
                                             import_data = pay_ecom_import_data_IDN,
                                             export_data = pay_ecom_export_data_IDN, 
                                             coef_labels = coef_labels, 
                                             country_name = "Indonesia")

# Regressions with 2 lags (t-2) in technology variable
models_cap_int_IDN_t_2 <- reg_models_cap_int("pay_or_ecomnod_t_2", 
                                             import_data = pay_ecom_import_data_IDN,
                                             export_data = pay_ecom_export_data_IDN, 
                                             coef_labels = coef_labels, 
                                             country_name = "Indonesia")

# Regressions with 3 lags (t-3) in technology variable
models_cap_int_IDN_t_3 <- reg_models_cap_int("pay_or_ecomnod_t_3", 
                                             import_data = pay_ecom_import_data_IDN,
                                             export_data = pay_ecom_export_data_IDN, 
                                             coef_labels = coef_labels, 
                                             country_name = "Indonesia")

gc()