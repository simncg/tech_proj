#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  regressions_tech_mitigation_covid_IDN.R                          #
#                                                                           #
# This program run the regressions of the model the regressions of the      #
# model that shows whether the existing technology use mitigate covid       #
# impacts. This code is necessary for creating the Rmarkdown                #
# "" that contains regression results.                                      #
#                                                                           #                                                                         
#                                                                           #
#                                                                           #           -                                                                                                                                                  #
#===========================================================================#

# Set Working Directory ----
#fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
#setwd(fileloc)
#rm(fileloc)

# Read Indonesia data for tech mitigation of COVID ----
source("gen_data_model_tech_mitigation_covid_IDN.R")

# Convert from Boolean to dummy (to avoid _TRUE labels in table)
covid_tech_vars <- c("China_E_commerce", 'China_E_commerce_updated', "diff_new_old_China_ecommerce_list", 
                     "parts_BEC", "cons_dur_BEC", "cons_semi_BEC",
                     "cons_BEC", "durable_BEC", "durable_semi_BEC", "transp_BEC", 
                     "hummels_timesensitive", "agric_timesensitive", 
                     "time_sens_hs2013_russ_typeofgoods")

paybustorecomimport_data_IDN[covid_tech_vars] <- lapply(paybustorecomimport_data_IDN[covid_tech_vars], function(x) 1 * x)
paybustorecomexport_data_IDN[covid_tech_vars] <- lapply(paybustorecomexport_data_IDN[covid_tech_vars], function(x) 1 * x)


# Regressions tech mitigation of COVID on ebay-China e-commerce product ----

model_files<-c("paybustorecom_import_preresearch_covid_1", "paybustorecom_export_preresearch_covid_1",
               "paybustorecom_import_preresearch_covid_2", "paybustorecom_export_preresearch_covid_2",
               "paybustorecom_import_preresearch_covid_3", "paybustorecom_export_preresearch_covid_3",
               "paybustorecom_import_preresearch_covid_4", "paybustorecom_export_preresearch_covid_4")


models_tech_covid_ebay_china_prop_IDN<-lapply(model_files, 
                                              function(x){readRDS(paste0("../../Outputs/Server/Indonesia/tech_mitigation_covid_model/",
                                                                         x, "_summary.rds"))})


gc()
# Dependent variable: Log imports
paybustorecom_import_preresearch_covid_1 <- feols(log_import ~ 
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index:Ebay_tradable +
                                                  month_mean_stringency_index:Ebay_tradable|
                                                  company_id + hs6 + date_character, 
                                                  cluster = c("company_id", "hs6"),
                                                  data = paybustorecomimport_data_IDN )

paybustorecom_import_preresearch_covid_2 <- feols(log_import ~ 
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce +
                                                  month_mean_stringency_index:China_E_commerce|
                                                  company_id + hs6 + date_character, 
                                                  cluster = c("company_id", "hs6"),
                                                  data = paybustorecomimport_data_IDN )

paybustorecom_import_preresearch_covid_3 <- feols(log_import ~ 
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce_updated +
                                                  month_mean_stringency_index:China_E_commerce_updated|
                                                  company_id + hs6 + date_character, 
                                                  cluster = c("company_id", "hs6"),
                                                  data = paybustorecomimport_data_IDN )



paybustorecom_import_preresearch_covid_4 <- feols(log_import ~ 
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index:diff_new_old_China_ecommerce_list +
                                                  month_mean_stringency_index:diff_new_old_China_ecommerce_list|
                                                  company_id + hs6 + date_character, 
                                                  cluster = c("company_id", "hs6"),
                                                  data = paybustorecomimport_data_IDN )




paybustorecom_export_preresearch_covid_1 <- feols(log_export ~ 
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index:Ebay_tradable +
                                                  month_mean_stringency_index:Ebay_tradable|
                                                  company_id + hs6 + date_character, 
                                                  cluster = c("company_id", "hs6"),
                                                  data = paybustorecomexport_data_IDN )

paybustorecom_export_preresearch_covid_2 <- feols(log_export ~ 
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce +
                                                  month_mean_stringency_index:China_E_commerce|
                                                  company_id + hs6 + date_character, 
                                                  cluster = c("company_id", "hs6"),
                                                  data = paybustorecomexport_data_IDN )

paybustorecom_export_preresearch_covid_3 <- feols(log_export ~ 
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce_updated +
                                                  month_mean_stringency_index:China_E_commerce_updated|
                                                  company_id + hs6 + date_character, 
                                                  cluster = c("company_id", "hs6"),
                                                  data = paybustorecomexport_data_IDN )



paybustorecom_export_preresearch_covid_4 <- feols(log_export ~ 
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index:diff_new_old_China_ecommerce_list +
                                                  month_mean_stringency_index:diff_new_old_China_ecommerce_list|
                                                  company_id + hs6 + date_character, 
                                                  cluster = c("company_id", "hs6"),
                                                  data = paybustorecomexport_data_IDN )


#How Existing Tech Insulates from COVID impacts ecommerce
models_tech_covid_ebay_china_IDN<-list(paybustorecom_import_preresearch_covid_1, paybustorecom_export_preresearch_covid_1,
                                      paybustorecom_import_preresearch_covid_2, paybustorecom_export_preresearch_covid_2,
                                      paybustorecom_import_preresearch_covid_3, paybustorecom_export_preresearch_covid_3,
                                      paybustorecom_import_preresearch_covid_4, paybustorecom_export_preresearch_covid_4)

rm(paybustorecom_import_preresearch_covid_1, paybustorecom_export_preresearch_covid_1,
   paybustorecom_import_preresearch_covid_2, paybustorecom_export_preresearch_covid_2,
   paybustorecom_import_preresearch_covid_3, paybustorecom_export_preresearch_covid_3,
   paybustorecom_import_preresearch_covid_4, paybustorecom_export_preresearch_covid_4)


gc()


# Regressions tech mitigation of COVID on BEC products ----
model_files<-c("paybustorecom_import_preresearch_covid_5", "paybustorecom_export_preresearch_covid_5",
               "paybustorecom_import_preresearch_covid_6", "paybustorecom_export_preresearch_covid_6",
               "paybustorecom_import_preresearch_covid_7", "paybustorecom_export_preresearch_covid_7",
               "paybustorecom_import_preresearch_covid_8", "paybustorecom_export_preresearch_covid_8", 
               "paybustorecom_import_preresearch_covid_9", "paybustorecom_export_preresearch_covid_9",
               "paybustorecom_import_preresearch_covid_10", "paybustorecom_export_preresearch_covid_10",
               "paybustorecom_import_preresearch_covid_11", "paybustorecom_export_preresearch_covid_11")


models_tech_covid_BEC_prop_IDN<-lapply(model_files, 
                                  function(x){readRDS(paste0("../../Outputs/Server/Indonesia/tech_mitigation_covid_model/",
                                                             x, "_summary.rds"))})


gc()

# Dependent variable: Log imports
paybustorecom_import_preresearch_covid_5 <- feols(log_import ~ 
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index:parts_BEC +
                                                  month_mean_stringency_index:parts_BEC|
                                                  company_id + hs6 + date_character, 
                                                  cluster = c("company_id", "hs6"),
                                                  data = paybustorecomimport_data_IDN )

paybustorecom_import_preresearch_covid_6 <- feols(log_import ~ 
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_dur_BEC +
                                                  month_mean_stringency_index:cons_dur_BEC|
                                                  company_id + hs6 + date_character, 
                                                  cluster = c("company_id", "hs6"),
                                                  data = paybustorecomimport_data_IDN )

paybustorecom_import_preresearch_covid_7 <- feols(log_import ~ 
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_semi_BEC +
                                                  month_mean_stringency_index:cons_semi_BEC|
                                                  company_id + hs6 + date_character, 
                                                  cluster = c("company_id", "hs6"),
                                                  data = paybustorecomimport_data_IDN )

paybustorecom_import_preresearch_covid_8 <- feols(log_import ~ 
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_BEC +
                                                  month_mean_stringency_index:cons_BEC|
                                                  company_id + hs6 + date_character, 
                                                  cluster = c("company_id", "hs6"),
                                                  data = paybustorecomimport_data_IDN )


paybustorecom_import_preresearch_covid_9 <- feols(log_import ~ 
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index:transp_BEC +
                                                  month_mean_stringency_index:transp_BEC|
                                                  company_id + hs6 + date_character, 
                                                  cluster = c("company_id", "hs6"),
                                                  data = paybustorecomimport_data_IDN )

paybustorecom_import_preresearch_covid_10 <- feols(log_import ~ 
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_BEC +
                                                   month_mean_stringency_index:durable_BEC|
                                                   company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomimport_data_IDN )

paybustorecom_import_preresearch_covid_11 <- feols(log_import ~ 
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_semi_BEC +
                                                   month_mean_stringency_index:durable_semi_BEC|
                                                   company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomimport_data_IDN )


paybustorecom_export_preresearch_covid_5 <- feols(log_export ~ 
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index:parts_BEC +
                                                  month_mean_stringency_index:parts_BEC|
                                                  company_id + hs6 + date_character, 
                                                  cluster = c("company_id", "hs6"),
                                                  data = paybustorecomexport_data_IDN )

paybustorecom_export_preresearch_covid_6 <- feols(log_export ~ 
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_dur_BEC +
                                                  month_mean_stringency_index:cons_dur_BEC|
                                                  company_id + hs6 + date_character, 
                                                  cluster = c("company_id", "hs6"),
                                                  data = paybustorecomexport_data_IDN )

paybustorecom_export_preresearch_covid_7 <- feols(log_export ~ 
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_semi_BEC +
                                                  month_mean_stringency_index:cons_semi_BEC|
                                                  company_id + hs6 + date_character, 
                                                  cluster = c("company_id", "hs6"),
                                                  data = paybustorecomexport_data_IDN )

paybustorecom_export_preresearch_covid_8 <- feols(log_export ~ 
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_BEC +
                                                  month_mean_stringency_index:cons_BEC|
                                                  company_id + hs6 + date_character, 
                                                  cluster = c("company_id", "hs6"),
                                                  data = paybustorecomexport_data_IDN )


paybustorecom_export_preresearch_covid_9 <- feols(log_export ~ 
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                  adopted_pay_or_ecom_before_2019:month_mean_stringency_index:transp_BEC +
                                                  month_mean_stringency_index:transp_BEC|
                                                  company_id + hs6 + date_character, 
                                                  cluster = c("company_id", "hs6"),
                                                  data = paybustorecomexport_data_IDN )

paybustorecom_export_preresearch_covid_10 <- feols(log_export ~ 
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_BEC +
                                                   month_mean_stringency_index:durable_BEC|
                                                   company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomexport_data_IDN )

paybustorecom_export_preresearch_covid_11 <- feols(log_export ~ 
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_semi_BEC +
                                                   month_mean_stringency_index:durable_semi_BEC|
                                                   company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomexport_data_IDN)

models_tech_covid_BEC_IDN<-list(paybustorecom_import_preresearch_covid_5, paybustorecom_export_preresearch_covid_5,
                            paybustorecom_import_preresearch_covid_6, paybustorecom_export_preresearch_covid_6,
                            paybustorecom_import_preresearch_covid_7, paybustorecom_export_preresearch_covid_7,
                            paybustorecom_import_preresearch_covid_8, paybustorecom_export_preresearch_covid_8,
                            paybustorecom_import_preresearch_covid_9, paybustorecom_export_preresearch_covid_9,
                            paybustorecom_import_preresearch_covid_10, paybustorecom_export_preresearch_covid_10,
                            paybustorecom_import_preresearch_covid_11, paybustorecom_export_preresearch_covid_11)

rm(paybustorecom_import_preresearch_covid_5, paybustorecom_export_preresearch_covid_5,
   paybustorecom_import_preresearch_covid_6, paybustorecom_export_preresearch_covid_6,
   paybustorecom_import_preresearch_covid_7, paybustorecom_export_preresearch_covid_7,
   paybustorecom_import_preresearch_covid_8, paybustorecom_export_preresearch_covid_8,
   paybustorecom_import_preresearch_covid_9, paybustorecom_export_preresearch_covid_9,
   paybustorecom_import_preresearch_covid_10, paybustorecom_export_preresearch_covid_10,
   paybustorecom_import_preresearch_covid_11, paybustorecom_export_preresearch_covid_11)

gc()


# Regressions tech mitigation of COVID on time-sensitive products ----

model_files<-c("paybustorecom_import_preresearch_covid_12", "paybustorecom_export_preresearch_covid_12",
               "paybustorecom_import_preresearch_covid_13", "paybustorecom_export_preresearch_covid_13",
               "paybustorecom_import_preresearch_covid_14", "paybustorecom_export_preresearch_covid_14",
               "paybustorecom_import_preresearch_covid_15", "paybustorecom_export_preresearch_covid_15", 
               "paybustorecom_import_preresearch_covid_16", "paybustorecom_export_preresearch_covid_16",
               "paybustorecom_import_preresearch_covid_17", "paybustorecom_export_preresearch_covid_17")


models_covid_time_sensitivity_prop_IDN<-lapply(model_files, 
                                       function(x){readRDS(paste0("../../Outputs/Server/Indonesia/tech_mitigation_covid_model/",
                                                                  x, "_summary.rds"))})


gc()


# Dependent variable: Log Imports
paybustorecom_import_preresearch_covid_12 <- feols(log_import ~ 
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index:component +
                                                   month_mean_stringency_index:component|
                                                   company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomimport_data_IDN )

paybustorecom_import_preresearch_covid_13 <- feols(log_import ~ 
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index:fresh +
                                                   month_mean_stringency_index:fresh|
                                                   company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomimport_data_IDN )


paybustorecom_import_preresearch_covid_14 <- feols(log_import ~ 
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index:frozen +
                                                   month_mean_stringency_index:frozen|
                                                   company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomimport_data_IDN )

paybustorecom_import_preresearch_covid_15 <- feols(log_import ~ 
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index:hummels_timesensitive +
                                                   month_mean_stringency_index:hummels_timesensitive|
                                                   company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomimport_data_IDN )


paybustorecom_import_preresearch_covid_16 <- feols(log_import ~ 
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index:agric_timesensitive +
                                                   month_mean_stringency_index:agric_timesensitive|
                                                   company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomimport_data_IDN )


paybustorecom_import_preresearch_covid_17 <- feols(log_import ~ 
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index:time_sens_hs2013_russ_typeofgoods +
                                                   month_mean_stringency_index:time_sens_hs2013_russ_typeofgoods|
                                                   company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomimport_data_IDN )



paybustorecom_export_preresearch_covid_12 <- feols(log_export ~ 
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index:component +
                                                   month_mean_stringency_index:component|
                                                   company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomexport_data_IDN )

paybustorecom_export_preresearch_covid_13 <- feols(log_export ~ 
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index:fresh +
                                                   month_mean_stringency_index:fresh|
                                                   company_id + hs6 + date_character,
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomexport_data_IDN )


paybustorecom_export_preresearch_covid_14 <- feols(log_export ~ 
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index:frozen +
                                                   month_mean_stringency_index:frozen|
                                                   company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomexport_data_IDN )

paybustorecom_export_preresearch_covid_15 <- feols(log_export ~ 
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index:hummels_timesensitive +
                                                   month_mean_stringency_index:hummels_timesensitive|
                                                   company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomexport_data_IDN )


paybustorecom_export_preresearch_covid_16 <- feols(log_export ~ 
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index:agric_timesensitive +
                                                   month_mean_stringency_index:agric_timesensitive|
                                                   company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomexport_data_IDN )


paybustorecom_export_preresearch_covid_17 <- feols(log_export ~ 
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                   adopted_pay_or_ecom_before_2019:month_mean_stringency_index:time_sens_hs2013_russ_typeofgoods +
                                                   month_mean_stringency_index:time_sens_hs2013_russ_typeofgoods|
                                                   company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomexport_data_IDN )


models_covid_time_sensitivity_IDN<-list(paybustorecom_import_preresearch_covid_12, paybustorecom_export_preresearch_covid_12,
                                    paybustorecom_import_preresearch_covid_13, paybustorecom_export_preresearch_covid_13,
                                    paybustorecom_import_preresearch_covid_14, paybustorecom_export_preresearch_covid_14,
                                    paybustorecom_import_preresearch_covid_15, paybustorecom_export_preresearch_covid_15,
                                    paybustorecom_import_preresearch_covid_16, paybustorecom_export_preresearch_covid_16,
                                    paybustorecom_import_preresearch_covid_17, paybustorecom_export_preresearch_covid_17)
  

rm(paybustorecom_import_preresearch_covid_12, paybustorecom_export_preresearch_covid_12,
   paybustorecom_import_preresearch_covid_13, paybustorecom_export_preresearch_covid_13,
   paybustorecom_import_preresearch_covid_14, paybustorecom_export_preresearch_covid_14,
   paybustorecom_import_preresearch_covid_15, paybustorecom_export_preresearch_covid_15,
   paybustorecom_import_preresearch_covid_16, paybustorecom_export_preresearch_covid_16,
   paybustorecom_import_preresearch_covid_17, paybustorecom_export_preresearch_covid_17)


#  Regressions tech mitigation of COVID on COVID impacted products ----

model_files<-c("paybustorecom_import_preresearch_covid_20", "paybustorecom_export_preresearch_covid_20",
               "paybustorecom_import_preresearch_covid_21", "paybustorecom_export_preresearch_covid_21")


model_covid_prods_impacted_prop_IDN<-lapply(model_files, 
                                               function(x){readRDS(paste0("../../Outputs/Server/Indonesia/tech_mitigation_covid_model/",
                                                                          x, "_summary.rds"))})


gc()


paybustorecom_import_preresearch_covid_18 <- feols(log_import ~ 
                                                     adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                     adopted_pay_or_ecom_before_2019:month_mean_stringency_index:letter_credit_use +
                                                     month_mean_stringency_index:letter_credit_use|
                                                     company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomimport_data_IDN )
paybustorecom_import_preresearch_covid_19 <- feols(log_import ~ 
                                                     adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                     adopted_pay_or_ecom_before_2019:month_mean_stringency_index:mean_IDN_remote_work_ISIC +
                                                     month_mean_stringency_index:mean_IDN_remote_work_ISIC|
                                                     company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomimport_data_IDN )

paybustorecom_import_preresearch_covid_20 <- feols(log_import ~ 
                                                     adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                     adopted_pay_or_ecom_before_2019:month_mean_stringency_index:relationship_stickiness +
                                                     month_mean_stringency_index:relationship_stickiness|
                                                     company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomimport_data_IDN )
paybustorecom_import_preresearch_covid_21 <- feols(log_import ~ 
                                                     adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                     adopted_pay_or_ecom_before_2019:month_mean_stringency_index:frac_lib_diff +
                                                     month_mean_stringency_index:frac_lib_diff|
                                                     company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomimport_data_IDN )


paybustorecom_export_preresearch_covid_18 <- feols(log_export ~ 
                                                     adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                     adopted_pay_or_ecom_before_2019:month_mean_stringency_index:letter_credit_use +
                                                     month_mean_stringency_index:letter_credit_use|
                                                     company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomexport_data_IDN )
paybustorecom_export_preresearch_covid_19 <- feols(log_export ~ 
                                                     adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                     adopted_pay_or_ecom_before_2019:month_mean_stringency_index:mean_IDN_remote_work_ISIC +
                                                     month_mean_stringency_index:mean_IDN_remote_work_ISIC|
                                                     company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomexport_data_IDN )

paybustorecom_export_preresearch_covid_20 <- feols(log_export ~ 
                                                     adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                     adopted_pay_or_ecom_before_2019:month_mean_stringency_index:relationship_stickiness +
                                                     month_mean_stringency_index:relationship_stickiness|
                                                     company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomexport_data_IDN )
paybustorecom_export_preresearch_covid_21 <- feols(log_export ~ 
                                                     adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                     adopted_pay_or_ecom_before_2019:month_mean_stringency_index:frac_lib_diff +
                                                     month_mean_stringency_index:frac_lib_diff|
                                                     company_id + hs6 + date_character, 
                                                   cluster = c("company_id", "hs6"),
                                                   data = paybustorecomexport_data_IDN )

model_covid_prods_impacted_IDN<- list(paybustorecom_import_preresearch_covid_18, paybustorecom_export_preresearch_covid_18,
                                  paybustorecom_import_preresearch_covid_19, paybustorecom_export_preresearch_covid_19,
                                  paybustorecom_import_preresearch_covid_20, paybustorecom_export_preresearch_covid_20,
                                  paybustorecom_import_preresearch_covid_21, paybustorecom_export_preresearch_covid_21)

rm(paybustorecom_import_preresearch_covid_18, paybustorecom_export_preresearch_covid_18,
   paybustorecom_import_preresearch_covid_19, paybustorecom_export_preresearch_covid_19,
   paybustorecom_import_preresearch_covid_20, paybustorecom_export_preresearch_covid_20,
   paybustorecom_import_preresearch_covid_21, paybustorecom_export_preresearch_covid_21)

gc()
