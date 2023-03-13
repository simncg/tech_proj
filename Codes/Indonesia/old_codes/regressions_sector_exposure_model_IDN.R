#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo                                                    #
#                                                                           #
# Script:  regressions_sector_exposure_model_IDN.R                          #
#                                                                           #
# This script runs regressions to explore the effects of sectorial          #
# exposures to COVID on trade outcomes for those sectors with firms that    #
# adopted e-payments/e-commerce technologies before 2019.                   #
#                                                                           #
#                                                                           #
#===========================================================================#

source("gen_data_model_sector_exposure_IDN.R")

# Regressions for Log.exports ----
paybustorecom_export_NAICS_1 <- feols(log_export ~ 
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:philadelphia_fed_cisa_v3_0 +
                                        month_mean_stringency_index:philadelphia_fed_cisa_v3_0
                                      | company_id + date_character, 
                                      data = paybustorecomexport_data_NAICS )


paybustorecom_export_NAICS_2 <- feols(log_export ~ 
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:teleworkable_emp +
                                        month_mean_stringency_index:teleworkable_emp
                                      | company_id + date_character, 
                                      data = paybustorecomexport_data_NAICS )



paybustorecom_export_NAICS_3 <- feols(log_export ~ 
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:teleworkable_wage +
                                        month_mean_stringency_index:teleworkable_wage
                                      | company_id + date_character, 
                                      data = paybustorecomexport_data_NAICS )

paybustorecom_export_NAICS_4 <- feols(log_export ~ 
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:teamwork_share +
                                        month_mean_stringency_index:teamwork_share
                                      | company_id + date_character, 
                                      data = paybustorecomexport_data_NAICS )

paybustorecom_export_NAICS_5 <- feols(log_export ~ 
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:customer_share +
                                        month_mean_stringency_index:customer_share
                                      | company_id + date_character, 
                                      data = paybustorecomexport_data_NAICS )


paybustorecom_export_NAICS_6 <- feols(log_export ~ 
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:presence_share +
                                        month_mean_stringency_index:presence_share
                                      | company_id + date_character, 
                                      data = paybustorecomexport_data_NAICS )


paybustorecom_export_NAICS_7 <- feols(log_export ~ 
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:communication_share +
                                        month_mean_stringency_index:communication_share
                                      | company_id + date_character, 
                                      data = paybustorecomexport_data_NAICS )

paybustorecom_export_NAICS_8 <- feols(log_export ~ 
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:affected_share +
                                        month_mean_stringency_index:affected_share
                                      | company_id + date_character, 
                                      data = paybustorecomexport_data_NAICS )


# Save models of sectorial exposure for exports in a list
models_sect_exports_IDN<-list(paybustorecom_export_NAICS_1,
                          paybustorecom_export_NAICS_2,
                          paybustorecom_export_NAICS_3,
                          paybustorecom_export_NAICS_4,
                          paybustorecom_export_NAICS_5,
                          paybustorecom_export_NAICS_6,
                          paybustorecom_export_NAICS_7,
                          paybustorecom_export_NAICS_8)



# Regressions for Log. Imports ----
paybustorecom_import_NAICS_1 <- feols(log_import ~ 
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:philadelphia_fed_cisa_v3_0 +
                                        month_mean_stringency_index:philadelphia_fed_cisa_v3_0
                                      | company_id + date_character,
                                      data = paybustorecomimport_data_NAICS)

paybustorecom_import_NAICS_2 <- feols(log_import ~ 
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:teleworkable_emp +
                                        month_mean_stringency_index:teleworkable_emp
                                      | company_id + date_character, 
                                      data = paybustorecomimport_data_NAICS)

paybustorecom_import_NAICS_3 <- feols(log_import ~ 
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:teleworkable_wage +
                                        month_mean_stringency_index:teleworkable_wage
                                      | company_id + date_character, 
                                      data = paybustorecomimport_data_NAICS)

paybustorecom_import_NAICS_4 <- feols(log_import ~ 
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:teamwork_share +
                                        month_mean_stringency_index:teamwork_share
                                      | company_id + date_character, 
                                      data = paybustorecomimport_data_NAICS )

paybustorecom_import_NAICS_5 <- feols(log_import ~ 
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:customer_share +
                                        month_mean_stringency_index:customer_share
                                      | company_id + date_character, 
                                      data = paybustorecomimport_data_NAICS )


paybustorecom_import_NAICS_6 <- feols(log_import ~ 
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:presence_share +
                                        month_mean_stringency_index:presence_share
                                      | company_id + date_character, 
                                      data = paybustorecomimport_data_NAICS)


paybustorecom_import_NAICS_7 <- feols(log_import ~ 
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:communication_share +
                                        month_mean_stringency_index:communication_share
                                      | company_id + date_character, 
                                      data = paybustorecomimport_data_NAICS)

paybustorecom_import_NAICS_8 <- feols(log_import ~ 
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:affected_share +
                                        month_mean_stringency_index:affected_share
                                      | company_id + date_character, 
                                      data = paybustorecomimport_data_NAICS)


models_sect_imports_IDN<-list(paybustorecom_import_NAICS_1,
                          paybustorecom_import_NAICS_2,
                          paybustorecom_import_NAICS_3,
                          paybustorecom_import_NAICS_4,
                          paybustorecom_import_NAICS_5,
                          paybustorecom_import_NAICS_6,
                          paybustorecom_import_NAICS_7,
                          paybustorecom_import_NAICS_8)


# Remove objects that are not going to be used anymore
rm(paybustorecom_import_NAICS_1,
   paybustorecom_import_NAICS_2,
   paybustorecom_import_NAICS_3,
   paybustorecom_import_NAICS_4,
   paybustorecom_import_NAICS_5,
   paybustorecom_import_NAICS_6,
   paybustorecom_import_NAICS_7,
   paybustorecom_import_NAICS_8,
   paybustorecom_export_NAICS_1,
   paybustorecom_export_NAICS_2,
   paybustorecom_export_NAICS_3,
   paybustorecom_export_NAICS_4,
   paybustorecom_export_NAICS_5,
   paybustorecom_export_NAICS_6,
   paybustorecom_export_NAICS_7,
   paybustorecom_export_NAICS_8)



# Regressions for export propensity ----

model_files<-c("paybustorecom_export_NAICS_1",
               "paybustorecom_export_NAICS_2",
               "paybustorecom_export_NAICS_3",
               "paybustorecom_export_NAICS_4",
               "paybustorecom_export_NAICS_5",
               "paybustorecom_export_NAICS_6",
               "paybustorecom_export_NAICS_7",
               "paybustorecom_export_NAICS_8")

models_sect_exports_prop_IDN<-lapply(model_files, 
                                     function(x){readRDS(paste0("../../Outputs/Server/Indonesia/sectorial_exposure_model/",
                                                                x, "_summary.rds"))})



# Regressions for import propensity ----
model_files<-c("paybustorecom_import_NAICS_1",
               "paybustorecom_import_NAICS_2",
               "paybustorecom_import_NAICS_3",
               "paybustorecom_import_NAICS_4",
               "paybustorecom_import_NAICS_5",
               "paybustorecom_import_NAICS_6",
               "paybustorecom_import_NAICS_7",
               "paybustorecom_import_NAICS_8")

models_sect_imports_prop_IDN<-lapply(model_files, 
                                     function(x){readRDS(paste0("../../Outputs/Server/Indonesia/sectorial_exposure_model/",
                                                                x, "_summary.rds"))})


