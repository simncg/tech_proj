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
# This code is necessar for creating the Rmarkdown                          #
# "" that contains regression results.                                      #
#                                                                           #                                                                         
#                                                                           #
#                                                                           #           -                                                                                                                                                  #
#===========================================================================#

# Set Working Directory ----
#fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
#setwd(fileloc)
#rm(fileloc)

# Read Indonesia data for products model ----
source("gen_data_model_products_IDN.R")

gc()


# Regressions with eBay-tradable products/products in China e-commerce tax lists ----


# Propensity regressions (import dummy, export dummy): run on the server
models_files<-c("paybustorecom_import_0_summary.rds", 
                "paybustorecom_export_0_summary.rds",
                "paybustorecom_import_1_summary.rds", 
                "paybustorecom_export_1_summary.rds", 
                "paybustorecom_import_2_summary.rds", 
                "paybustorecom_export_2_summary.rds", 
                "paybustorecom_import_3_summary.rds", 
                "paybustorecom_export_3_summary.rds", 
                "paybustorecom_import_4_summary.rds",
                "paybustorecom_export_4_summary.rds"
                )

# Read models results
models_ebay_china_prop_IDN<-lapply(models_files, 
                                   function(x){readRDS(paste0("../../Outputs/Server/Indonesia/products_model/", x))})

# Log imports/Log exports regressions 

# Convert from Boolean to dummy (to avoid _TRUE labels in table)
ebay_china_vars <- c("Ebay_tradable", "China_E_commerce", 
                     'China_E_commerce_updated', "diff_new_old_China_ecommerce_list")

paybustorecomimport_data_IDN[ebay_china_vars] <- lapply(paybustorecomimport_data_IDN[ebay_china_vars], function(x) 1 * x)
paybustorecomexport_data_IDN[ebay_china_vars] <- lapply(paybustorecomexport_data_IDN[ebay_china_vars], function(x) 1 * x)


# Dependent variable: Log of imports
paybustorecom_import_0 <- feols(log_import ~ pay_or_ecomnod | 
                                  company_id + hs6+ date_character,
                                cluster = c("company_id", "hs6"),
                                data = paybustorecomimport_data_IDN)

paybustorecom_import_1 <- feols(log_import ~ pay_or_ecomnod + Ebay_tradable:pay_or_ecomnod|
                                  company_id + hs6+ date_character, 
                                cluster = c("company_id", "hs6"),
                                data = paybustorecomimport_data_IDN )

paybustorecom_import_2 <- feols(log_import ~ pay_or_ecomnod + China_E_commerce:pay_or_ecomnod |
                                  company_id + hs6+ date_character, 
                                cluster = c("company_id", "hs6"),
                                data = paybustorecomimport_data_IDN )

paybustorecom_import_3 <- feols(log_import ~ pay_or_ecomnod + China_E_commerce_updated:pay_or_ecomnod |
                                  company_id + hs6+ date_character, 
                                cluster = c("company_id", "hs6"),
                                data = paybustorecomimport_data_IDN )

paybustorecom_import_4 <- feols(log_import ~ pay_or_ecomnod + diff_new_old_China_ecommerce_list:pay_or_ecomnod |
                                  company_id + hs6+ date_character, 
                                cluster = c("company_id", "hs6"),
                                data = paybustorecomimport_data_IDN )


# Dependent variable: Log of Exports
paybustorecom_export_0 <- feols(log_export ~ pay_or_ecomnod | 
                                  company_id + hs6+ date_character, 
                                cluster = c("company_id", "hs6"),
                                data = paybustorecomexport_data_IDN)

paybustorecom_export_1 <- feols(log_export ~ pay_or_ecomnod + Ebay_tradable:pay_or_ecomnod | 
                                  company_id + hs6+ date_character,
                                cluster = c("company_id", "hs6"),
                                data = paybustorecomexport_data_IDN )

paybustorecom_export_2 <- feols(log_export ~ pay_or_ecomnod + China_E_commerce:pay_or_ecomnod | 
                                  company_id + hs6+ date_character,
                                cluster = c("company_id", "hs6"),
                                data = paybustorecomexport_data_IDN )

paybustorecom_export_3 <- feols(log_export ~ pay_or_ecomnod + China_E_commerce_updated:pay_or_ecomnod |
                                  company_id + hs6+ date_character, 
                                cluster = c("company_id", "hs6"),
                                data = paybustorecomexport_data_IDN )

paybustorecom_export_4 <- feols(log_export ~ pay_or_ecomnod + diff_new_old_China_ecommerce_list:pay_or_ecomnod | 
                                  company_id + hs6+ date_character, 
                                cluster = c("company_id", "hs6"),
                                data = paybustorecomexport_data_IDN )


# List with models (Log exports and log imports)
models_ebay_china_IDN<-list(paybustorecom_import_0, paybustorecom_export_0,
                        paybustorecom_import_1, paybustorecom_export_1,
                        paybustorecom_import_2, paybustorecom_export_2,
                        paybustorecom_import_3, paybustorecom_export_3,
                        paybustorecom_import_4, paybustorecom_export_4)


# Regressions with BEC products classification ----

# Propensity regressions (import dummy, export dummy): run on the server
models_files<-c("paybustorecom_import_5_summary.rds", "paybustorecom_export_5_summary.rds",
                "paybustorecom_import_6_summary.rds", "paybustorecom_export_6_summary.rds",
                "paybustorecom_import_7_summary.rds", "paybustorecom_export_7_summary.rds",
                "paybustorecom_import_8_summary.rds", "paybustorecom_export_8_summary.rds",
                "paybustorecom_import_9_summary.rds", "paybustorecom_export_9_summary.rds",
                "paybustorecom_import_10_summary.rds", "paybustorecom_export_10_summary.rds",
                "paybustorecom_import_11_summary.rds", "paybustorecom_export_11_summary.rds")

# Read models results
models_BEC_prop_IDN<-lapply(models_files, 
                            function(x){readRDS(paste0("../../Outputs/Server/Indonesia/products_model/", x))})


gc()

# Convert from boolean to dummy
bec_vars<-c("parts_BEC", "cons_dur_BEC", "cons_semi_BEC",
            "cons_BEC", "durable_BEC", "durable_semi_BEC")

paybustorecomimport_data_IDN[bec_vars] <- lapply(paybustorecomimport_data_IDN[bec_vars], function(x) 1 * x)
paybustorecomexport_data_IDN[bec_vars] <- lapply(paybustorecomexport_data_IDN[bec_vars], function(x) 1 * x)


# Dependent variable: Log. Import
paybustorecom_import_5 <- feols(log_import ~ pay_or_ecomnod + parts_BEC:pay_or_ecomnod |
                                  company_id + hs6+ date_character, 
                                cluster = c("company_id", "hs6"),
                                data = paybustorecomimport_data_IDN )

paybustorecom_import_6<- feols(log_import ~ pay_or_ecomnod + cons_dur_BEC:pay_or_ecomnod |
                                 company_id + hs6+ date_character, 
                               cluster = c("company_id", "hs6"),
                               data = paybustorecomimport_data_IDN )

paybustorecom_import_7 <- feols(log_import ~ pay_or_ecomnod + cons_semi_BEC:pay_or_ecomnod | 
                                  company_id + hs6+ date_character, 
                                cluster = c("company_id", "hs6"),
                                data = paybustorecomimport_data_IDN )

paybustorecom_import_8 <- feols(log_import ~ pay_or_ecomnod + cons_BEC:pay_or_ecomnod | 
                                  company_id + hs6+ date_character, 
                                cluster = c("company_id", "hs6"),
                                data = paybustorecomimport_data_IDN )

paybustorecom_import_9 <- feols(log_import ~ pay_or_ecomnod + transp_BEC:pay_or_ecomnod | 
                                  company_id + hs6+ date_character, 
                                cluster = c("company_id", "hs6"),
                                data = paybustorecomimport_data_IDN)


paybustorecom_import_10 <- feols(log_import ~ pay_or_ecomnod + durable_BEC:pay_or_ecomnod |
                                   company_id + hs6+ date_character,
                                 data = paybustorecomimport_data_IDN )

paybustorecom_import_11 <- feols(log_import ~ pay_or_ecomnod + durable_semi_BEC:pay_or_ecomnod |
                                   company_id + hs6+ date_character, 
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomimport_data_IDN )

# Dependent variable: Log. Exports
paybustorecom_export_5 <- feols(log_export ~ pay_or_ecomnod + parts_BEC:pay_or_ecomnod | 
                                  company_id + hs6+ date_character,
                                cluster = c("company_id", "hs6"),
                                data = paybustorecomexport_data_IDN )

paybustorecom_export_6<- feols(log_export ~ pay_or_ecomnod + cons_dur_BEC:pay_or_ecomnod |
                                 company_id + hs6+ date_character,
                               cluster = c("company_id", "hs6"),
                               data = paybustorecomexport_data_IDN )

paybustorecom_export_7 <- feols(log_export ~ pay_or_ecomnod + cons_semi_BEC:pay_or_ecomnod | 
                                  company_id + hs6+ date_character,
                                cluster = c("company_id", "hs6"),
                                data = paybustorecomexport_data_IDN )

paybustorecom_export_8 <- feols(log_export ~ pay_or_ecomnod + cons_BEC:pay_or_ecomnod | 
                                  company_id + hs6+ date_character, 
                                cluster = c("company_id", "hs6"),
                                data = paybustorecomexport_data_IDN )

paybustorecom_export_9 <- feols(log_export ~ pay_or_ecomnod + transp_BEC:pay_or_ecomnod |
                                  company_id + hs6+ date_character, 
                                cluster = c("company_id", "hs6"),
                                data = paybustorecomexport_data_IDN )

paybustorecom_export_10 <- feols(log_export ~ pay_or_ecomnod + durable_BEC:pay_or_ecomnod |
                                   company_id + hs6+ date_character,
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomexport_data_IDN )

paybustorecom_export_11 <- feols(log_export ~ pay_or_ecomnod + durable_semi_BEC:pay_or_ecomnod | 
                                   company_id + hs6+ date_character,
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomexport_data_IDN )


# List with models
models_BEC_IDN<-list(paybustorecom_import_5, paybustorecom_export_5,
                     paybustorecom_import_6, paybustorecom_export_6,
                     paybustorecom_import_7, paybustorecom_export_7,
                     paybustorecom_import_8, paybustorecom_export_8,
                     paybustorecom_import_10, paybustorecom_export_10,
                     paybustorecom_import_11, paybustorecom_export_11)



# Regression with time-sensitive products variables ----

# Propensity regressions (import dummy, export dummy): run on the server
models_files<-c("paybustorecom_import_12_summary.rds", "paybustorecom_export_12_summary.rds",
                "paybustorecom_import_13_summary.rds", "paybustorecom_export_13_summary.rds",
                "paybustorecom_import_14_summary.rds", "paybustorecom_export_14_summary.rds",
                "paybustorecom_import_15_summary.rds", "paybustorecom_export_15_summary.rds",
                "paybustorecom_import_16_summary.rds", "paybustorecom_export_16_summary.rds",
                "paybustorecom_import_17_summary.rds", "paybustorecom_export_17_summary.rds")


# Read models results
models_time_sens_prop_IDN<-lapply(models_files, 
                            function(x){readRDS(paste0("../../Outputs/Server/Indonesia/products_model/", x))})


gc()

# From Boolean to dummy
time_sens_vars<-c("component", "fresh", "frozen", "hummels_timesensitive",
                  "agric_timesensitive", "time_sens_hs2013_russ_typeofgoods")

paybustorecomimport_data_IDN[time_sens_vars] <- lapply(paybustorecomimport_data_IDN[time_sens_vars], function(x) 1 * x)
paybustorecomexport_data_IDN[time_sens_vars] <- lapply(paybustorecomexport_data_IDN[time_sens_vars], function(x) 1 * x)


# Dependent variable: Log of imports
paybustorecom_import_12 <- feols(log_import ~ pay_or_ecomnod + component:pay_or_ecomnod |
                                   company_id + hs6+ date_character,
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomimport_data_IDN )


paybustorecom_import_13 <- feols(log_import ~ pay_or_ecomnod + fresh:pay_or_ecomnod |
                                   company_id + hs6+ date_character,
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomimport_data_IDN )

paybustorecom_import_14 <- feols(log_import ~ pay_or_ecomnod + frozen:pay_or_ecomnod |
                                   company_id + hs6+ date_character, 
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomimport_data_IDN )

paybustorecom_import_15 <- feols(log_import ~ pay_or_ecomnod + hummels_timesensitive:pay_or_ecomnod |
                                   company_id + hs6+ date_character,
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomimport_data_IDN )

paybustorecom_import_16 <- feols(log_import ~ pay_or_ecomnod + agric_timesensitive:pay_or_ecomnod |
                                   company_id + hs6+ date_character, 
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomimport_data_IDN )

paybustorecom_import_17 <- feols(log_import ~ pay_or_ecomnod + time_sens_hs2013_russ_typeofgoods:pay_or_ecomnod |
                                   company_id + hs6+ date_character, 
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomimport_data_IDN )

# Dependent variable: Log of exports
paybustorecom_export_12 <- feols(log_export ~ pay_or_ecomnod + component:pay_or_ecomnod |
                                   company_id + hs6+ date_character,
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomexport_data_IDN )

paybustorecom_export_13 <- feols(log_export ~ pay_or_ecomnod + fresh:pay_or_ecomnod |
                                   company_id + hs6+ date_character,
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomexport_data_IDN )

paybustorecom_export_14 <- feols(log_export ~ pay_or_ecomnod + frozen:pay_or_ecomnod | 
                                   company_id + hs6+ date_character, 
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomexport_data_IDN )

paybustorecom_export_15 <- feols(log_export ~ pay_or_ecomnod + hummels_timesensitive:pay_or_ecomnod |
                                   company_id + hs6+ date_character, 
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomexport_data_IDN )

paybustorecom_export_16 <- feols(log_export ~ pay_or_ecomnod + agric_timesensitive:pay_or_ecomnod | 
                                   company_id + hs6+ date_character,
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomexport_data_IDN )

paybustorecom_export_17 <- feols(log_export ~ pay_or_ecomnod + time_sens_hs2013_russ_typeofgoods:pay_or_ecomnod |
                                   company_id + hs6+ date_character,
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomexport_data_IDN )



# List with models
models_time_sens_IDN <- list(paybustorecom_import_12, paybustorecom_export_12,
                         paybustorecom_import_13, paybustorecom_export_13,
                         paybustorecom_import_14, paybustorecom_export_14,
                         paybustorecom_import_15, paybustorecom_export_15,
                         paybustorecom_import_16, paybustorecom_export_16,
                         paybustorecom_import_17, paybustorecom_export_17)


# Remove objects that are not going to be used onwards
rm(paybustorecom_import_0, paybustorecom_export_0,
   paybustorecom_import_1, paybustorecom_export_1,
   paybustorecom_import_2, paybustorecom_export_2,
   paybustorecom_import_3, paybustorecom_export_3,
   paybustorecom_import_4, paybustorecom_export_4, 
   paybustorecom_import_5, paybustorecom_export_5,
   paybustorecom_import_6, paybustorecom_export_6,
   paybustorecom_import_7, paybustorecom_export_7,
   paybustorecom_import_8, paybustorecom_export_8,
   paybustorecom_import_10, paybustorecom_export_10,
   paybustorecom_import_11, paybustorecom_export_11,
   paybustorecom_import_12, paybustorecom_export_12,
   paybustorecom_import_13, paybustorecom_export_13,
   paybustorecom_import_14, paybustorecom_export_14,
   paybustorecom_import_15, paybustorecom_export_15,
   paybustorecom_import_16, paybustorecom_export_16,
   paybustorecom_import_17, paybustorecom_export_17)


gc()


# Regressions with other classification for HS Products (Letter credit use, mean IDN remote work, relationship stickiness)---- 

# Propensity regressions (import dummy, export dummy): run on the server
models_files<-c("paybustorecom_import_20_summary.rds", "paybustorecom_export_20_summary.rds",
                "paybustorecom_import_21_summary.rds", "paybustorecom_export_21_summary.rds")

# Read models results
models_other_HS_prop_IDN<-lapply(models_files, 
                            function(x){readRDS(paste0("../../Outputs/Server/Indonesia/products_model/", x))})

gc()


# Dependent variable: Log imports 
paybustorecom_import_18 <- feols(log_import ~ pay_or_ecomnod + letter_credit_use:pay_or_ecomnod |
                                 company_id + hs6+ date_character, 
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomimport_data_IDN )
paybustorecom_import_19 <- feols(log_import ~ pay_or_ecomnod + mean_IDN_remote_work_ISIC:pay_or_ecomnod |
                                 company_id + hs6+ date_character, 
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomimport_data_IDN )

paybustorecom_import_20 <- feols(log_import ~ pay_or_ecomnod + relationship_stickiness:pay_or_ecomnod | 
                                 company_id + hs6+ date_character,
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomimport_data_IDN)

paybustorecom_import_21 <- feols(log_import ~ pay_or_ecomnod + frac_lib_diff:pay_or_ecomnod | 
                                 company_id + hs6+ date_character, 
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomimport_data_IDN )


paybustorecom_export_18 <- feols(log_export ~ pay_or_ecomnod + letter_credit_use:pay_or_ecomnod |
                                 company_id + hs6+ date_character,
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomexport_data_IDN )

paybustorecom_export_19 <- feols(log_export ~ pay_or_ecomnod + mean_IDN_remote_work_ISIC:pay_or_ecomnod |
                                 company_id + hs6+ date_character, 
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomexport_data_IDN )

paybustorecom_export_20 <- feols(log_export ~ pay_or_ecomnod + relationship_stickiness:pay_or_ecomnod |
                                 company_id + hs6+ date_character, 
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomexport_data_IDN )

paybustorecom_export_21 <- feols(log_export ~ pay_or_ecomnod + frac_lib_diff:pay_or_ecomnod | 
                                 company_id + hs6+ date_character,
                                 cluster = c("company_id", "hs6"),
                                 data = paybustorecomexport_data_IDN )



models_other_HS_IDN<-list(paybustorecom_import_18, paybustorecom_export_18,
                          paybustorecom_import_19, paybustorecom_export_19,
                          paybustorecom_import_20, paybustorecom_export_20,
                          paybustorecom_import_21, paybustorecom_export_21)


rm(paybustorecom_import_18, paybustorecom_export_18,
   paybustorecom_import_19, paybustorecom_export_19,
   paybustorecom_import_20, paybustorecom_export_20,
   paybustorecom_import_21, paybustorecom_export_21)
