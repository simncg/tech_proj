#===========================================================================#
# Date:    January 2022                                                     #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  functions_reg_prop_tech_covid_mitigation                         #
#                                                                           #
# This program generates functions for running regressions of the model that#
# shows whether the existing technology use mitigate covid  impacts. This   #
# code is necessary for creating the Rmarkdown "" 
# that contains regression results.                                         #
#                                                                           #                                                                         
#                                                                           #
#                                                                           #           -                                                                                                                                                  #
#===========================================================================#

# Set Working Directory ----
#fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
#setwd(fileloc)
#rm(fileloc)

# Functions for customizing results latex tables ----
options("modelsummary_format_numeric_latex" = "plain")


# Format N, R2, Adj.R2 in tables
f1 <- function(x) format(round(x, 3), big.mark=",")
f2 <- function(x) format(round(x, 0), big.mark=",")

gm <- list(
  list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = f2),
  list("raw" = "r.squared", "clean" = "R-squared", "fmt" = f1),
  list("raw" = "adj.r.squared", "clean" = "Adj.R-squared", "fmt"=f1)
)


format_se_coef <- function(x) format(round(x, 6), nsmall = 2, scientific = FALSE)

# E-bay Tradable and China e-commerce -----
prop_reg_models_tech_covid_ebay_china<-function(import_data, export_data, country_name, coef_labels){
  
  
  # Dependent variable: Dummy imports
  paybustorecom_import_preresearch_covid_1 <- summary(feols(import_dummy ~ 
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index:Ebay_tradable +
                                                      month_mean_stringency_index:Ebay_tradable|
                                                      company_id + hs6 + date_character, 
                                                    cluster = c("company_id", "hs6"),
                                                    data = import_data), lean = T)
  
  paybustorecom_import_preresearch_covid_2 <- summary(feols(import_dummy ~ 
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce +
                                                      month_mean_stringency_index:China_E_commerce|
                                                      company_id + hs6 + date_character, 
                                                    cluster = c("company_id", "hs6"),
                                                    data = import_data), lean = T)
  
  paybustorecom_import_preresearch_covid_3 <- summary(feols(import_dummy ~ 
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce_updated +
                                                      month_mean_stringency_index:China_E_commerce_updated|
                                                      company_id + hs6 + date_character, 
                                                    cluster = c("company_id", "hs6"),
                                                    data = import_data), lean = T)
  
  
  
  paybustorecom_import_preresearch_covid_4 <- summary(feols(import_dummy ~ 
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index:diff_new_old_China_ecommerce_list +
                                                      month_mean_stringency_index:diff_new_old_China_ecommerce_list|
                                                      company_id + hs6 + date_character, 
                                                    cluster = c("company_id", "hs6"),
                                                    data = import_data), lean = T)
  
  
  
  # Dependent variable: exports dummy
  paybustorecom_export_preresearch_covid_1 <- summary(feols(export_dummy ~ 
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index:Ebay_tradable +
                                                      month_mean_stringency_index:Ebay_tradable|
                                                      company_id + hs6 + date_character, 
                                                    cluster = c("company_id", "hs6"),
                                                    data = export_data), lean = T)
  
  paybustorecom_export_preresearch_covid_2 <- summary(feols(export_dummy ~ 
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce +
                                                      month_mean_stringency_index:China_E_commerce|
                                                      company_id + hs6 + date_character, 
                                                    cluster = c("company_id", "hs6"),
                                                    data = export_data), lean = T)
  
  paybustorecom_export_preresearch_covid_3 <- summary(feols(export_dummy ~ 
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce_updated +
                                                      month_mean_stringency_index:China_E_commerce_updated|
                                                      company_id + hs6 + date_character, 
                                                    cluster = c("company_id", "hs6"),
                                                    data = export_data), lean = T)
  
  
  paybustorecom_export_preresearch_covid_4 <- summary(feols(export_dummy ~ 
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index:diff_new_old_China_ecommerce_list +
                                                      month_mean_stringency_index:diff_new_old_China_ecommerce_list|
                                                      company_id + hs6 + date_character, 
                                                    cluster = c("company_id", "hs6"),
                                                    data = export_data), lean = T)
  
  #How Existing Tech Insulates from COVID impacts e-commerce
  models_tech_covid_ebay_china<-list(paybustorecom_import_preresearch_covid_1, paybustorecom_export_preresearch_covid_1,
                                     paybustorecom_import_preresearch_covid_2, paybustorecom_export_preresearch_covid_2,
                                     paybustorecom_import_preresearch_covid_3, paybustorecom_export_preresearch_covid_3,
                                     paybustorecom_import_preresearch_covid_4, paybustorecom_export_preresearch_covid_4)
  
  
  
  # Assign dependent variable name to models names
  names(models_tech_covid_ebay_china)<-rep(c("Import Propensity", "Export Propensity"), length(models_tech_covid_ebay_china)/2)
  
  # Add fixed-effects indicators to tables
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_tech_covid_ebay_china)), 
                             "Product FE", rep("Yes", length(models_tech_covid_ebay_china)),
                             "Month FE", rep("Yes", length(models_tech_covid_ebay_china))), 
                           nrow = 3, byrow=T))
  
  
  # Table summary models
  table<-modelsummary(models_tech_covid_ebay_china,
                      coef_rename = coef_labels, 
                      gof_map = gm, 
                      stars = c('*' = .1, '**' = .05, '***'= 0.01), 
                      add_rows = FE, 
                      output = "latex", 
                      align = paste(c("l", rep("c", length(models_tech_covid_ebay_china))), sep="", collapse=""), 
                      notes = list("The variable E-payment or E-commerce 2019 means that the company adopted the E-payment or E-commerce technology before 2019.", "Clustered-standard errors at the firm-product level."),
                      fmt = format_se_coef, 
                      title = paste0(country_name,' - Regression Results for Export Propensity and Import Propensity: e-Bay tradable and China e-commerce products')) %>% 
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_tech_covid_ebay_china))) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  # Save results
  saveRDS(list(models_tech_covid_ebay_china, table), paste0("../../Outputs/", country_name, "/regressions_results/tech_mitigation_covid_model/models_tech_covid_ebay_china_summary.rds"))
  
  
  return(list(models_tech_covid_ebay_china, table))
  
}


# BEC Products ------
prop_reg_models_tech_covid_BEC<-function(import_data, export_data, country_name, coef_labels){
  
  
  
  # Dependent variable: Dummy imports
  paybustorecom_import_preresearch_covid_5 <- summary(feols(import_dummy ~ 
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index:parts_BEC +
                                                      month_mean_stringency_index:parts_BEC|
                                                      company_id + hs6 + date_character, 
                                                    cluster = c("company_id", "hs6"),
                                                    data = import_data), lean = T)
  
  paybustorecom_import_preresearch_covid_6 <- summary(feols(import_dummy ~ 
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_dur_BEC +
                                                      month_mean_stringency_index:cons_dur_BEC|
                                                      company_id + hs6 + date_character, 
                                                    cluster = c("company_id", "hs6"),
                                                    data = import_data), lean = T)
  
  paybustorecom_import_preresearch_covid_7 <- summary(feols(import_dummy ~ 
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_semi_BEC +
                                                      month_mean_stringency_index:cons_semi_BEC|
                                                      company_id + hs6 + date_character, 
                                                    cluster = c("company_id", "hs6"),
                                                    data = import_data), lean = T)
  
  paybustorecom_import_preresearch_covid_8 <- summary(feols(import_dummy ~ 
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_BEC +
                                                      month_mean_stringency_index:cons_BEC|
                                                      company_id + hs6 + date_character, 
                                                    cluster = c("company_id", "hs6"),
                                                    data = import_data), lean = T)
  
  
  paybustorecom_import_preresearch_covid_9 <- summary(feols(import_dummy ~ 
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index:transp_BEC +
                                                      month_mean_stringency_index:transp_BEC|
                                                      company_id + hs6 + date_character, 
                                                    cluster = c("company_id", "hs6"),
                                                    data = import_data), lean = T)
  
  paybustorecom_import_preresearch_covid_10 <- summary(feols(import_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_BEC +
                                                       month_mean_stringency_index:durable_BEC|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = import_data), lean = T)
  
  paybustorecom_import_preresearch_covid_11 <- summary(feols(import_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_semi_BEC +
                                                       month_mean_stringency_index:durable_semi_BEC|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = import_data), lean = T)
  
  # Dependent variable: Dummy exports 
  paybustorecom_export_preresearch_covid_5 <- summary(feols(export_dummy ~ 
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index:parts_BEC +
                                                      month_mean_stringency_index:parts_BEC|
                                                      company_id + hs6 + date_character, 
                                                    cluster = c("company_id", "hs6"),
                                                    data = export_data), lean = T)
  
  paybustorecom_export_preresearch_covid_6 <- summary(feols(export_dummy ~ 
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_dur_BEC +
                                                      month_mean_stringency_index:cons_dur_BEC|
                                                      company_id + hs6 + date_character, 
                                                    cluster = c("company_id", "hs6"),
                                                    data = export_data), lean = T)
  
  paybustorecom_export_preresearch_covid_7 <- summary(feols(export_dummy ~ 
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_semi_BEC +
                                                      month_mean_stringency_index:cons_semi_BEC|
                                                      company_id + hs6 + date_character, 
                                                    cluster = c("company_id", "hs6"),
                                                    data = export_data), lean = T)
  
  paybustorecom_export_preresearch_covid_8 <- summary(feols(export_dummy ~ 
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_BEC +
                                                      month_mean_stringency_index:cons_BEC|
                                                      company_id + hs6 + date_character, 
                                                    cluster = c("company_id", "hs6"),
                                                    data = export_data), lean = T)
  
  
  paybustorecom_export_preresearch_covid_9 <- summary(feols(export_dummy ~ 
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                      adopted_pay_or_ecom_before_2019:month_mean_stringency_index:transp_BEC +
                                                      month_mean_stringency_index:transp_BEC|
                                                      company_id + hs6 + date_character, 
                                                    cluster = c("company_id", "hs6"),
                                                    data = export_data), lean = T)
  
  paybustorecom_export_preresearch_covid_10 <- summary(feols(export_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_BEC +
                                                       month_mean_stringency_index:durable_BEC|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = export_data), lean = T)
  
  paybustorecom_export_preresearch_covid_11 <- summary(feols(export_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_semi_BEC +
                                                       month_mean_stringency_index:durable_semi_BEC|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = export_data), lean = T)
  
  
  
  
  models_tech_covid_BEC<-list(paybustorecom_import_preresearch_covid_5, paybustorecom_export_preresearch_covid_5,
                              paybustorecom_import_preresearch_covid_6, paybustorecom_export_preresearch_covid_6,
                              paybustorecom_import_preresearch_covid_7, paybustorecom_export_preresearch_covid_7,
                              paybustorecom_import_preresearch_covid_8, paybustorecom_export_preresearch_covid_8,
                              paybustorecom_import_preresearch_covid_9, paybustorecom_export_preresearch_covid_9,
                              paybustorecom_import_preresearch_covid_10, paybustorecom_export_preresearch_covid_10,
                              paybustorecom_import_preresearch_covid_11, paybustorecom_export_preresearch_covid_11)
  
  
  # Assign dependent variable name to models names
  names(models_tech_covid_BEC)<-rep(c("Import Propensity", "Export Propensity"), length(models_tech_covid_BEC)/2)
  
  # Add fixed-effects indicators to tables
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_tech_covid_BEC)), 
                             "Product FE", rep("Yes", length(models_tech_covid_BEC)),
                             "Month FE", rep("Yes", length(models_tech_covid_BEC))), 
                           nrow = 3, byrow=T))
  
  
  # Table summary models
  table <- modelsummary(models_tech_covid_BEC,
                        coef_rename = coef_labels, 
                        gof_map = gm, 
                        stars = c('*' = .1, '**' = .05, '***'= 0.01), 
                        add_rows = FE, 
                        output = "latex", 
                        align = paste(c("l", rep("c", length(models_tech_covid_BEC))), sep="", collapse=""), 
                        notes = list("The variable E-payment or E-commerce 2019 means that the company adopted the E-payment or E-commerce technology before 2019.", "Clustered-standard errors at the firm-product level."), 
                        fmt = format_se_coef, 
                        title = paste0(country_name, ' - Regression Results for Export Propensity and Import Propensity: BEC products classification')) %>% 
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_tech_covid_BEC))) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  
  # Save results
  saveRDS(list(models_tech_covid_BEC, table), paste0("../../Outputs/", country_name, "/regressions_results/tech_mitigation_covid_model/models_tech_covid_BEC_summary.rds"))
  
  
  
  return(list(models_tech_covid_BEC, table))
  
  
}




# Regressions tech mitigation of COVID on time-sensitive products ----

prop_reg_models_covid_time_sensitivity<-function(import_data, export_data,  country_name, coef_labels){
  
  
  # Dependent variable: Dummy Imports
  paybustorecom_import_preresearch_covid_12 <- summary(feols(import_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:component +
                                                       month_mean_stringency_index:component|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = import_data), lean = T)
  
  paybustorecom_import_preresearch_covid_13 <- summary(feols(import_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:fresh +
                                                       month_mean_stringency_index:fresh|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = import_data), lean = T)
  
  
  paybustorecom_import_preresearch_covid_14 <- summary(feols(import_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:frozen +
                                                       month_mean_stringency_index:frozen|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = import_data), lean = T)
  
  paybustorecom_import_preresearch_covid_15 <- summary(feols(import_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:hummels_timesensitive +
                                                       month_mean_stringency_index:hummels_timesensitive|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = import_data), lean = T)
  
  
  paybustorecom_import_preresearch_covid_16 <- summary(feols(import_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:agric_timesensitive +
                                                       month_mean_stringency_index:agric_timesensitive|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = import_data), lean = T)
  
  
  paybustorecom_import_preresearch_covid_17 <- summary(feols(import_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:time_sens_hs2013_russ_typeofgoods +
                                                       month_mean_stringency_index:time_sens_hs2013_russ_typeofgoods|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = import_data), lean = T)
  
  
  
  paybustorecom_export_preresearch_covid_12 <- summary(feols(export_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:component +
                                                       month_mean_stringency_index:component|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = export_data), lean = T)
  
  paybustorecom_export_preresearch_covid_13 <- summary(feols(export_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:fresh +
                                                       month_mean_stringency_index:fresh|
                                                       company_id + hs6 + date_character,
                                                     cluster = c("company_id", "hs6"),
                                                     data = export_data), lean = T)
  
  
  paybustorecom_export_preresearch_covid_14 <- summary(feols(export_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:frozen +
                                                       month_mean_stringency_index:frozen|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = export_data), lean = T)
  
  paybustorecom_export_preresearch_covid_15 <- summary(feols(export_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:hummels_timesensitive +
                                                       month_mean_stringency_index:hummels_timesensitive|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = export_data), lean = T)
  
  
  paybustorecom_export_preresearch_covid_16 <- summary(feols(export_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:agric_timesensitive +
                                                       month_mean_stringency_index:agric_timesensitive|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = export_data), lean = T)
  
  
  paybustorecom_export_preresearch_covid_17 <- summary(feols(export_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:time_sens_hs2013_russ_typeofgoods +
                                                       month_mean_stringency_index:time_sens_hs2013_russ_typeofgoods|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = export_data), lean = T)
  
  
  models_covid_time_sensitivity<-list(paybustorecom_import_preresearch_covid_12, paybustorecom_export_preresearch_covid_12,
                                      paybustorecom_import_preresearch_covid_13, paybustorecom_export_preresearch_covid_13,
                                      paybustorecom_import_preresearch_covid_14, paybustorecom_export_preresearch_covid_14,
                                      paybustorecom_import_preresearch_covid_15, paybustorecom_export_preresearch_covid_15,
                                      paybustorecom_import_preresearch_covid_16, paybustorecom_export_preresearch_covid_16,
                                      paybustorecom_import_preresearch_covid_17, paybustorecom_export_preresearch_covid_17)
  
  
  
  # Assign dependent variable name to models names
  names(models_covid_time_sensitivity)<-rep(c("Import Propensity", "Export Propensity"), length(models_covid_time_sensitivity)/2)
  
  # Add fixed-effects indicators to tables
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_covid_time_sensitivity)), 
                             "Product FE", rep("Yes", length(models_covid_time_sensitivity)),
                             "Month FE", rep("Yes", length(models_covid_time_sensitivity))), 
                           nrow = 3, byrow=T))
  
  
  
  # Table summary models
  table<-modelsummary(models_covid_time_sensitivity,
                      coef_rename = coef_labels, 
                      gof_map = gm, 
                      stars = c('*' = .1, '**' = .05, '***'= 0.01), 
                      add_rows = FE, 
                      output = "latex", 
                      align = paste(c("l", rep("c", length(models_covid_time_sensitivity))), sep="", collapse=""), 
                      notes = list("The variable E-payment or E-commerce 2019 means that the company adopted the E-payment or E-commerce technology before 2019.", "Clustered-standard errors at the firm-product level."), 
                      fmt = format_se_coef, 
                      title = paste0(country_name, " - Regression Results for Import Propensity and Export Propensity: Time-sensitive Products")) %>% 
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_covid_time_sensitivity))) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  # Save results
  saveRDS(list(models_covid_time_sensitivity, table), paste0("../../Outputs/", country_name, "/regressions_results/tech_mitigation_covid_model/models_covid_time_sensitivity_summary.rds"))
  
  
  
  return(list(models_covid_time_sensitivity, table))
  
  
}


#  Regressions tech mitigation of COVID on COVID impacted products ----

prop_reg_model_covid_prods_impacted<-function(import_data, export_data,  country_name, coef_labels){
  
  
  paybustorecom_import_preresearch_covid_18 <- summary(feols(import_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:letter_credit_use +
                                                       month_mean_stringency_index:letter_credit_use|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = import_data), lean = T)
  paybustorecom_import_preresearch_covid_19 <- summary(feols(import_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:mean_remote_work_ISIC +
                                                       month_mean_stringency_index:mean_remote_work_ISIC|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = import_data), lean = T)
  
  paybustorecom_import_preresearch_covid_20 <- summary(feols(import_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:relationship_stickiness +
                                                       month_mean_stringency_index:relationship_stickiness|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = import_data), lean = T)
  paybustorecom_import_preresearch_covid_21 <- summary(feols(import_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:frac_lib_diff +
                                                       month_mean_stringency_index:frac_lib_diff|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = import_data), lean = T)
  
  
  paybustorecom_export_preresearch_covid_18 <- summary(feols(export_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:letter_credit_use +
                                                       month_mean_stringency_index:letter_credit_use|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = export_data), lean = T)
  paybustorecom_export_preresearch_covid_19 <- summary(feols(export_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:mean_remote_work_ISIC +
                                                       month_mean_stringency_index:mean_remote_work_ISIC|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = export_data), lean = T)
  
  paybustorecom_export_preresearch_covid_20 <- summary(feols(export_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:relationship_stickiness +
                                                       month_mean_stringency_index:relationship_stickiness|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = export_data), lean = T)
  paybustorecom_export_preresearch_covid_21 <- summary(feols(export_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:frac_lib_diff +
                                                       month_mean_stringency_index:frac_lib_diff|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = export_data), lean = T)
  
  model_covid_prods_impacted<- list(paybustorecom_import_preresearch_covid_18, paybustorecom_export_preresearch_covid_18,
                                    paybustorecom_import_preresearch_covid_19, paybustorecom_export_preresearch_covid_19,
                                    paybustorecom_import_preresearch_covid_20, paybustorecom_export_preresearch_covid_20,
                                    paybustorecom_import_preresearch_covid_21, paybustorecom_export_preresearch_covid_21)
  
  
  
  
  # Assign dependent variable name to models names
  names(model_covid_prods_impacted)<-rep(c("Import Propensity", "Export Propensity"), length(model_covid_prods_impacted)/2)
  
  # Add fixed-effects indicators to tables
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(model_covid_prods_impacted)), 
                             "Product FE", rep("Yes", length(model_covid_prods_impacted)),
                             "Month FE", rep("Yes", length(model_covid_prods_impacted))), 
                           nrow = 3, byrow=T))
  
  
  
  # Table summary models
  table<-modelsummary(model_covid_prods_impacted,
                      coef_rename = coef_labels, 
                      gof_map = gm, 
                      stars = c('*' = .1, '**' = .05, '***'= 0.01), 
                      add_rows = FE, 
                      output = "latex", 
                      align = paste(c("l", rep("c", length(model_covid_prods_impacted))), sep="", collapse=""), 
                      notes = list("The variable E-payment or E-commerce 2019 means that the company adopted the E-payment or E-commerce technology before 2019.", "Clustered-standard errors at the firm-product level."), 
                      fmt = format_se_coef, 
                      title =  paste0(country_name, " - Regression Results for Import Propensity Export Propensity: COVID impacted Products")) %>% 
    add_header_above(c(" " = 1, "Dependent Variables" = length(model_covid_prods_impacted))) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  
  # Save results
  saveRDS(list(model_covid_prods_impacted, table), paste0("../../Outputs/", country_name, "/regressions_results/tech_mitigation_covid_model/model_covid_prods_impacted_summary.rds"))
  
  
  return(list(model_covid_prods_impacted, table))
  
  
}


#  Regressions tech mitigation of COVID on capital - intermediate products ----

prop_reg_model_covid_cap_int<-function(import_data, export_data,  country_name, coef_labels){
  
  
  paybustorecom_import_preresearch_covid_22 <- summary(feols(import_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:CAP +
                                                       month_mean_stringency_index:CAP|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = import_data), lean = T)
  paybustorecom_import_preresearch_covid_23 <- summary(feols(import_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:INT +
                                                       month_mean_stringency_index:INT|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = import_data), lean = T)
  
  
  paybustorecom_export_preresearch_covid_22 <- summary(feols(export_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:CAP +
                                                       month_mean_stringency_index:CAP|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = export_data), lean = T)
  paybustorecom_export_preresearch_covid_23 <- summary(feols(export_dummy ~ 
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:INT +
                                                       month_mean_stringency_index:INT|
                                                       company_id + hs6 + date_character, 
                                                     cluster = c("company_id", "hs6"),
                                                     data = export_data), lean = T)
  
  model_covid_prods_cap_int<- list(paybustorecom_import_preresearch_covid_22, paybustorecom_export_preresearch_covid_22,
                                   paybustorecom_import_preresearch_covid_23, paybustorecom_export_preresearch_covid_23)
  
  
  
  
  # Assign dependent variable name to models names
  names(model_covid_prods_cap_int)<-rep(c("Import Propensity", "Export Propensity"), length(model_covid_prods_cap_int)/2)
  
  # Add fixed-effects indicators to tables
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(model_covid_prods_cap_int)), 
                             "Product FE", rep("Yes", length(model_covid_prods_cap_int)),
                             "Month FE", rep("Yes", length(model_covid_prods_cap_int))), 
                           nrow = 3, byrow=T))
  
  
  
  # Table summary models
  table<-modelsummary(model_covid_prods_cap_int,
                      coef_rename = coef_labels, 
                      gof_map = gm, 
                      stars = c('*' = .1, '**' = .05, '***'= 0.01), 
                      add_rows = FE, 
                      output = "latex", 
                      align = paste(c("l", rep("c", length(model_covid_prods_cap_int))), sep="", collapse=""), 
                      notes = list("The variable E-payment or E-commerce 2019 means that the company adopted the E-payment or E-commerce technology before 2019.", "Clustered-standard errors at the firm-product level."), 
                      fmt = format_se_coef, 
                      title =  paste0(country_name, " - Regression Results for Import Propensity Export Propensity: COVID impacted Products")) %>% 
    add_header_above(c(" " = 1, "Dependent Variables" = length(model_covid_prods_cap_int))) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  
  
  saveRDS(list(model_covid_prods_cap_int, table), paste0("../../Outputs/", country_name, "/regressions_results/tech_mitigation_covid_model/model_covid_prods_cap_int_summary.rds"))
  
  
  return(list(model_covid_prods_cap_int, table))
  
}





# E-bay Tradable and China e-commerce -----
prop_reg_models_tech_covid_definitive<-function(country_name, coef_labels){
  
  # Abbreviation
  if(country_name == "India"){
    abbr<-"IND"
  } else if(country_name == "Indonesia"){
    abbr<-"IDN"
  } else if(country_name == "Mexico"){
    abbr<-"MEX"
  }
  
  vars <- c("Ebay_tradable", "China_E_commerce", "cons_BEC", "durable_BEC", "adopted_pay_or_ecom_before_2019")
  
  import_data <-read_parquet(paste0("../../Data/", country_name, "/processed_data/imports_grid_mitig_model_", abbr, ".parquet"), 
                             col_select = c("company_id", "hs6", "date_character","import_dummy", "adopted_pay_or_ecom_before_2019", 
                                            "month_mean_stringency_index", "Ebay_tradable", "China_E_commerce", "cons_BEC", "durable_BEC"))
  
  import_data<-as.data.table(import_data)
  
  
  import_data[, (vars) := lapply(.SD, function(x) 1 * x), .SDcols = vars]
  
  
  # Dependent variable: Import dummy
  
  # E-bay tradable
  paybustorecom_import_preresearch_covid_1 <- summary(feols(import_dummy ~ 
                                                              adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                              adopted_pay_or_ecom_before_2019:month_mean_stringency_index:Ebay_tradable +
                                                              month_mean_stringency_index:Ebay_tradable|
                                                              company_id + hs6 + date_character, 
                                                            cluster = c("company_id", "hs6"),
                                                            data = import_data), lean = T)
  
  # China E-commerce
  paybustorecom_import_preresearch_covid_2 <- summary(feols(import_dummy ~ 
                                                              adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                              adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce +
                                                              month_mean_stringency_index:China_E_commerce|
                                                              company_id + hs6 + date_character, 
                                                            cluster = c("company_id", "hs6"),
                                                            data = import_data), lean = T)
  
  # Consumable goods
  paybustorecom_import_preresearch_covid_3 <- summary(feols(import_dummy ~ 
                                                              adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                              adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_BEC +
                                                              month_mean_stringency_index:cons_BEC|
                                                              company_id + hs6 + date_character, 
                                                            cluster = c("company_id", "hs6"),
                                                            data = import_data), lean = T)
  
  
  # Durable goods
  paybustorecom_import_preresearch_covid_4 <- summary(feols(import_dummy ~ 
                                                              adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                              adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_BEC +
                                                              month_mean_stringency_index:durable_BEC|
                                                              company_id + hs6 + date_character, 
                                                            cluster = c("company_id", "hs6"),
                                                            data = import_data), lean = T)
  
  
  rm(import_data)
  gc()
  
  export_data <-read_parquet(paste0("../../Data/", country_name, "/processed_data/exports_grid_mitig_model_", abbr, ".parquet"), 
                             col_select = c("company_id", "hs6", "date_character","export_dummy", "adopted_pay_or_ecom_before_2019", 
                                            "month_mean_stringency_index", "Ebay_tradable", "China_E_commerce", "cons_BEC", "durable_BEC"))
  
  export_data<-as.data.table(export_data)
  
  
  export_data[, (vars) := lapply(.SD, function(x) 1 * x), .SDcols = vars]
  
  
  # Dependent variable: Exports dummy
  paybustorecom_export_preresearch_covid_1 <- summary(feols(export_dummy ~ 
                                                              adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                              adopted_pay_or_ecom_before_2019:month_mean_stringency_index:Ebay_tradable +
                                                              month_mean_stringency_index:Ebay_tradable|
                                                              company_id + hs6 + date_character, 
                                                            cluster = c("company_id", "hs6"),
                                                            data = export_data), lean = T)
  
  paybustorecom_export_preresearch_covid_2 <- summary(feols(export_dummy ~ 
                                                              adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                              adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce +
                                                              month_mean_stringency_index:China_E_commerce|
                                                              company_id + hs6 + date_character, 
                                                            cluster = c("company_id", "hs6"),
                                                            data = export_data), lean = T)
  
  paybustorecom_export_preresearch_covid_3 <- summary(feols(export_dummy ~ 
                                                              adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                              adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_BEC +
                                                              month_mean_stringency_index:cons_BEC|
                                                              company_id + hs6 + date_character, 
                                                            cluster = c("company_id", "hs6"),
                                                            data = export_data), lean = T)
  
  
  paybustorecom_export_preresearch_covid_4 <- summary(feols(export_dummy ~ 
                                                              adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
                                                              adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_BEC +
                                                              month_mean_stringency_index:durable_BEC|
                                                              company_id + hs6 + date_character, 
                                                            cluster = c("company_id", "hs6"),
                                                            data = export_data), lean = T)
  
  rm(export_data)
  gc()
  
  #How Existing Tech Insulates from COVID impacts e-commerce
  models_tech_covid_definitive<-list(paybustorecom_import_preresearch_covid_1, paybustorecom_export_preresearch_covid_1,
                                     paybustorecom_import_preresearch_covid_2, paybustorecom_export_preresearch_covid_2,
                                     paybustorecom_import_preresearch_covid_3, paybustorecom_export_preresearch_covid_3,
                                     paybustorecom_import_preresearch_covid_4, paybustorecom_export_preresearch_covid_4)
  
  
  
  # Assign dependent variable name to models names
  names(models_tech_covid_definitive)<-rep(c("Import Propensity", "Export Propensity"), length(models_tech_covid_definitive)/2)
  
  # Add fixed-effects indicators to tables
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_tech_covid_definitive)), 
                             "Product FE", rep("Yes", length(models_tech_covid_definitive)),
                             "Month FE", rep("Yes", length(models_tech_covid_definitive))), 
                           nrow = 3, byrow=T))
  
  
  # Table summary models
  table<-modelsummary(models_tech_covid_definitive,
                      coef_rename = coef_labels, 
                      gof_map = gm, 
                      stars = c('*' = .1, '**' = .05, '***'= 0.01), 
                      add_rows = FE, 
                      output = "latex", 
                      align = paste(c("l", rep("c", length(models_tech_covid_definitive))), sep="", collapse=""), 
                      notes = list("The variable E-payment or E-commerce 2019 means that the company adopted the E-payment or E-commerce technology before 2019.", "Clustered-standard errors at the firm-product level."),
                      fmt = format_se_coef, 
                      title = paste0(country_name,' - Regression Results for Export Propensity and Import Propensity: e-Bay tradable, China e-commerce, Consumable and Durable products')) %>% 
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_tech_covid_definitive))) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  # Save results
  saveRDS(list(models_tech_covid_definitive, table), paste0("../../Outputs/", country_name, "/regressions_results/tech_mitigation_covid_model/models_tech_covid_definitive_summary.rds"))
  
  
  return(list(models_tech_covid_definitive, table))
  
}





