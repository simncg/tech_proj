#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo                                                    #
#                                                                           #
# Script:  regressions_model_sector_tech_adoption_IDN.R                     #
#                                                                           #
#                                                                           
#                                                                                                                                                    
#                                                                           
#                                                                           
#===========================================================================#

source("gen_data_model_sector_tech_adoption.R")


paybustorecom_import_NAICS_1 <- feols(log_import ~ pay_or_ecom_nod + pay_or_ecom_nod:ecom_sh_2digit
                                      | company_id + date_character, 
                                      data = paybustorecomimport_data_NAICS_2_IDN)



paybustorecom_export_NAICS_1 <- feols(log_export ~ pay_or_ecom_nod + pay_or_ecom_nod:ecom_sh_2digit
                                      | company_id + date_character, 
                                      data = paybustorecomexport_data_NAICS_2_IDN)


models_sector_tech_adop_IDN<-list(paybustorecom_import_NAICS_1,
                                  paybustorecom_export_NAICS_1)

rm(paybustorecom_import_NAICS_1,
   paybustorecom_export_NAICS_1)


# Propensity regressions ---- 
model_files<-c("paybustorecom_import_NAICS_1_onlinetrade_summary",
               "paybustorecom_export_NAICS_1_onlinetrade_summary")


models_sector_tech_adop_prop_IDN<-lapply(model_files, 
                                     function(x){readRDS(paste0("../../Outputs/Server/Indonesia/sector_tech_adoption_model/",
                                                                x, ".rds"))})