#=============================================================================#
# Date:    February 2022                                                      #
#                                                                             #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,     #
#          Indonesia and Mexico.                                              #
#                                                                             #
# Author:  Simon Caicedo - DECTI - The World Bank                             #
#                                                                             #
# Script:  regressions_products_model_IDN.R                                   #
#                                                                             #
# This program creates functions for running regressions for the model        #
# with different products categories specifications (eBay tradable, China     #
# e-commerce, BEC  classification, Time-Sensitivity, capital-intermediate     #
# goods classification) interacted with a var technology (ecommerce/e-payment)#
# with a lagged structure. This code is necessary for creating the Rmarkdown  #
# "" that contains regression results.                                        #
#                                                                             #                                                                          
#                                                                             #
#                                                                             #                                                                                                                                                  #
#=============================================================================#

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

format_se_coef <- function(x) format(round(x, 5), nsmall = 2, scientific = FALSE)


# Function for running propensity regressions technology with ebay and china e-commerce with a specific technology ----
prop_reg_models_ebay_china<-function(tech_var, import_data, export_data, country_name, coef_labels){
  
  n_lags <- parse_number(tech_var)
  
  
  # Dependent variable: Import dummy
  paybustorecom_import_0 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "| 
                                    company_id + hs6+ date_character")),
                                  cluster = c("company_id", "hs6"),
                                  data = import_data), lean = T)
  
  paybustorecom_import_1 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ Ebay_tradable:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data), lean = T)
  
  paybustorecom_import_2 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ China_E_commerce:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data), lean = T)
  
  paybustorecom_import_3 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ China_E_commerce_updated:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data), lean = T)
  
  paybustorecom_import_4 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ diff_new_old_China_ecommerce_list:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data), lean = T)
  
  
  
  # Dependent variable: Export Dummy
  paybustorecom_export_0 <- summary(feols(as.formula(paste("export_dummy ~", tech_var, "| 
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = export_data), lean = T)
  
  paybustorecom_export_1 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ Ebay_tradable:", tech_var, "| 
                                    company_id + hs6+ date_character")),
                                  cluster = c("company_id", "hs6"),
                                  data = export_data), lean = T)
  
  paybustorecom_export_2 <- summary(feols(as.formula(paste("export_dummy ~", tech_var, "+ China_E_commerce:", tech_var, "| 
                                    company_id + hs6+ date_character")),
                                  cluster = c("company_id", "hs6"),
                                  data = export_data), lean = T)
  
  paybustorecom_export_3 <- summary(feols(as.formula(paste("export_dummy ~", tech_var, "+ China_E_commerce_updated:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = export_data), lean = T)
  
  paybustorecom_export_4 <- summary(feols(as.formula(paste("export_dummy ~", tech_var, "+ diff_new_old_China_ecommerce_list:", tech_var, "| 
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = export_data), lean = T)
  
  
  # List with models (Import propensity and export propensity)
  models_ebay_china<-list(paybustorecom_import_0, paybustorecom_export_0,
                          paybustorecom_import_1, paybustorecom_export_1,
                          paybustorecom_import_2, paybustorecom_export_2,
                          paybustorecom_import_3, paybustorecom_export_3,
                          paybustorecom_import_4, paybustorecom_export_4)
  
  
  # Assign dependent variable name to models names
  names(models_ebay_china)<-rep(c("Import Propensity", "Export Propensity"), length(models_ebay_china)/2)
  
  # Create Fixed Efects data frame to add them to tables
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_ebay_china)), 
                             "Product FE", rep("Yes", length(models_ebay_china)),
                             "Year-Month FE", rep("Yes", length(models_ebay_china))), 
                           nrow = 3, byrow=T)) 
  
  
  # Produce table with models summary  
  table<-modelsummary(models_ebay_china,
                      coef_rename = coef_labels, 
                      gof_map = gm, 
                      stars = c('*' = .1, '**' = .05, '***'= 0.01), 
                      add_rows = FE, 
                      output = "latex", 
                      align = paste(c("l", rep("c", length(models_ebay_china))), sep="", collapse=""), 
                      notes = "Clustered-standard errors at the firm-product level.", 
                      fmt = format_se_coef, 
                      title = paste0(country_name," - Regression Results for Import Propensity and Export Propensity: e-Bay tradable and China e-commerce products. ", 
                                     n_lags, "-Lag in technology variable")) %>% 
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_ebay_china))) %>%
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  
  
  # Save results
  saveRDS(list(models_ebay_china, table), paste0("../../Outputs/", country_name, "/regressions_results/products_model/models_ebay_china_", n_lags, "_lags_summary.rds"))
  
  return(list(models_ebay_china, table))
  
}

# Function for running regressions BEC products classification ----
prop_reg_models_BEC<-function(tech_var, import_data, export_data, country_name, coef_labels){
  
  n_lags <- parse_number(tech_var)
  
  # Dependent variable: Import Dummy
  paybustorecom_import_5 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ parts_BEC:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data), lean = T)
  
  paybustorecom_import_6<- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ cons_dur_BEC:", tech_var, "|
                                   company_id + hs6+ date_character")), 
                                 cluster = c("company_id", "hs6"),
                                 data = import_data), lean = T)
  
  paybustorecom_import_7 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ cons_semi_BEC:", tech_var, "| 
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data), lean = T)
  
  paybustorecom_import_8 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ cons_BEC:", tech_var, "| 
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data), lean = T)
  
  paybustorecom_import_9 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ transp_BEC:", tech_var, "| 
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data), lean = T)
  
  
  paybustorecom_import_10 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ durable_BEC:", tech_var, "|
                                     company_id + hs6+ date_character")),
                                   data = import_data), lean = T)
  
  paybustorecom_import_11 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ durable_semi_BEC:", tech_var, "|
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = import_data), lean = T)
  
  # All interactions
  paybustorecom_import_inter <- summary(feols(as.formula(paste0("import_dummy ~", tech_var,
                                                        "+ parts_BEC:", tech_var,
                                                        #    "+ cons_dur_BEC:", tech_var,
                                                        #    "+ cons_semi_BEC:", tech_var, 
                                                        "+ cons_BEC:", tech_var, 
                                                        "+ transp_BEC:", tech_var, 
                                                        "+ durable_BEC:", tech_var, 
                                                        #    "+ durable_semi_BEC:", tech_var, 
                                                        "| company_id + hs6+ date_character")),
                                      cluster = c("company_id", "hs6"),
                                      data = import_data ), lean = T)
  
  
  # Dependent variable: Export Propensity
  paybustorecom_export_5 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ parts_BEC:", tech_var,
                                                    "| company_id + hs6+ date_character")),
                                  cluster = c("company_id", "hs6"),
                                  data = export_data), lean = T)
  
  paybustorecom_export_6<- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ cons_dur_BEC:", tech_var, "|
                                   company_id + hs6+ date_character")),
                                 cluster = c("company_id", "hs6"),
                                 data = export_data), lean = T)
  
  paybustorecom_export_7 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ cons_semi_BEC:", tech_var,  "| 
                                    company_id + hs6+ date_character")),
                                  cluster = c("company_id", "hs6"),
                                  data = export_data), lean = T)
  
  paybustorecom_export_8 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ cons_BEC:", tech_var, "| 
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = export_data), lean = T)
  
  paybustorecom_export_9 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ transp_BEC:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = export_data), lean = T)
  
  paybustorecom_export_10 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ durable_BEC:", tech_var, "|
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = export_data), lean = T)
  
  paybustorecom_export_11 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ durable_semi_BEC:", tech_var, "| 
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = export_data), lean = T)
  
  # All interactions
  paybustorecom_export_inter <- summary(feols(as.formula(paste0("export_dummy ~", tech_var,
                                                        "+ parts_BEC:", tech_var,
                                                        #    "+ cons_dur_BEC:", tech_var,
                                                        #    "+ cons_semi_BEC:", tech_var, 
                                                        "+ cons_BEC:", tech_var, 
                                                        "+ transp_BEC:", tech_var, 
                                                        "+ durable_BEC:", tech_var, 
                                                        #    "+ durable_semi_BEC:", tech_var, 
                                                        "| company_id + hs6+ date_character")),
                                      cluster = c("company_id", "hs6"),
                                      data = export_data ), lean = T)
  
  
  # List with models
  models_BEC<-list(paybustorecom_import_5, paybustorecom_export_5,
                   paybustorecom_import_6, paybustorecom_export_6,
                   paybustorecom_import_7, paybustorecom_export_7,
                   paybustorecom_import_8, paybustorecom_export_8,
                   paybustorecom_import_10, paybustorecom_export_10,
                   paybustorecom_import_11, paybustorecom_export_11, 
                   paybustorecom_import_inter, paybustorecom_export_inter
  )
  
  
  # Assign dependent variable names to models 
  names(models_BEC)<-rep(c("Import Propensity", "Export Propensity"), length(models_BEC)/2)
  
  # Add fixed-effects indicators to tables
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_BEC)), 
                             "Product FE", rep("Yes", length(models_BEC)),
                             "Month FE", rep("Yes", length(models_BEC))), 
                           nrow = 3, byrow=T)) 
  
  # Create Latex Table with models summary
  table<-modelsummary(models_BEC,
                      coef_rename = coef_labels, 
                      gof_map = gm, 
                      stars = c('*' = .1, '**' = .05, '***'= 0.01), 
                      add_rows = FE, 
                      output = "latex", 
                      align = paste(c("l", rep("c", length(models_BEC))), sep="", collapse=""), 
                      notes = "Clustered-standard errors at the firm-product level.", 
                      fmt = format_se_coef, 
                      title = paste0(country_name, " - Regression Results for Import Propensity and Export Propensity: BEC products classification. ", 
                                     n_lags, "-Lag in technology variable")) %>% 
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_BEC))) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  # Save results
  saveRDS(list(models_BEC, table), paste0("../../Outputs/", country_name, "/regressions_results/products_model/models_BEC_", n_lags, "_lags_summary.rds"))
  
  return(list(models_BEC, table))
  
}

# Funtion for regressions with time-sensitive products variables ----

prop_reg_models_time_sens<-function(tech_var, import_data, export_data, country_name, coef_labels){
  
  n_lags <- parse_number(tech_var)
  
  
  # Dependent variable: Import Dummy
  paybustorecom_import_12 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ component:", tech_var,  "|
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = import_data), lean = T)
  
  
  paybustorecom_import_13 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ fresh:", tech_var, "|
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = import_data), lean = T)
  
  paybustorecom_import_14 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ frozen:", tech_var, "|
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = import_data), lean = T)
  
  paybustorecom_import_15 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ hummels_timesensitive:", tech_var, "|
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = import_data), lean = T)
  
  paybustorecom_import_16 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ agric_timesensitive:", tech_var, "|
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = import_data), lean = T)
  
  paybustorecom_import_17 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ time_sens_hs2013_russ_typeofgoods:", tech_var, "|
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = import_data), lean = T)
  
  # All interactions
  paybustorecom_import_inter <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, 
                                                        "+ component:", tech_var,
                                                        "+ fresh:", tech_var,
                                                        #"+ frozen:", tech_var,
                                                        "|company_id + hs6+ date_character")),
                                      cluster = c("company_id", "hs6"),
                                      data = import_data), lean = T)
  
  
  # Dependent variable: Export Dummy
  paybustorecom_export_12 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ component:", tech_var, "|
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = export_data), lean = T)
  
  paybustorecom_export_13 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ fresh:", tech_var, "|
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = export_data), lean = T)
  
  paybustorecom_export_14 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ frozen:", tech_var, "| 
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = export_data), lean = T)
  
  paybustorecom_export_15 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ hummels_timesensitive:", tech_var, "|
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = export_data), lean = T)
  
  paybustorecom_export_16 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ agric_timesensitive:", tech_var, "| 
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = export_data), lean = T)
  
  paybustorecom_export_17 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ time_sens_hs2013_russ_typeofgoods:", tech_var, "|
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = export_data), lean = T)
  
  # All interactions
  paybustorecom_export_inter <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, 
                                                        "+ component:", tech_var,
                                                        "+ fresh:", tech_var,
                                                        #"+ frozen:", tech_var,
                                                        "|company_id + hs6+ date_character")),
                                      cluster = c("company_id", "hs6"),
                                      data = export_data), lean = T)
  
  # List with models
  models_time_sens <- list(paybustorecom_import_12, paybustorecom_export_12,
                           paybustorecom_import_13, paybustorecom_export_13,
                           paybustorecom_import_14, paybustorecom_export_14,
                           paybustorecom_import_15, paybustorecom_export_15,
                           paybustorecom_import_16, paybustorecom_export_16,
                           paybustorecom_import_17, paybustorecom_export_17, 
                           paybustorecom_import_inter, paybustorecom_export_inter)
  
  # Assign dependent variable names to models 
  names(models_time_sens)<-rep(c("Import Propensity", "Export Propensity"), length(models_time_sens)/2)
  
  
  # Add fixed-effects indicators to tables
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_time_sens)), 
                             "Product FE", rep("Yes", length(models_time_sens)),
                             "Month FE", rep("Yes", length(models_time_sens))), 
                           nrow = 3, byrow=T))
  
  
  # Latex Tables with models summary
  table<-modelsummary(models_time_sens,
                      coef_rename = coef_labels, 
                      gof_map = gm, 
                      stars = c('*' = .1, '**' = .05, '***'= 0.01), 
                      add_rows = FE, 
                      output = "latex", 
                      align = paste(c("l", rep("c", length(models_time_sens))), sep="", collapse=""), 
                      notes = "Clustered-standard errors at the firm-product level.", 
                      fmt = format_se_coef, 
                      title = paste0(country_name," - Regression Results for Import Propensity and Export Propensity: Time-sensitive Products. ", 
                                     n_lags, "-Lag in technology variable")) %>% 
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_time_sens))) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down"))
  
  
  # Save results
  saveRDS(list(models_time_sens, table), paste0("../../Outputs/", country_name, "/regressions_results/products_model/models_time_sens_", n_lags, "_lags_summary.rds"))
  
  
  return(list(models_time_sens, table))
  
}


# Function for regressions with other classification for HS Products (Letter credit use, mean IDN remote work, relationship stickiness)---- 

prop_reg_models_other_HS<-function(tech_var, import_data, export_data, country_name, coef_labels){
  
  n_lags <- parse_number(tech_var)
  
  
  # Dependent variable: Import Dummy 
  paybustorecom_import_18 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ letter_credit_use:", tech_var, "|
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = import_data), lean = T)
  paybustorecom_import_19 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ mean_remote_work_ISIC:", tech_var, "|
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = import_data), lean = T)
  
  paybustorecom_import_20 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ relationship_stickiness:", tech_var, "| 
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = import_data), lean = T)
  
  paybustorecom_import_21 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ frac_lib_diff:", tech_var, "| 
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = import_data), lean = T)
  
  
  paybustorecom_import_inter <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, 
                                                        "+ letter_credit_use:", tech_var, 
                                                        "+ mean_remote_work_ISIC:", tech_var,
                                                        "+ relationship_stickiness:", tech_var,
                                                        "+ frac_lib_diff:", tech_var, 
                                                        "| company_id + hs6+ date_character")), 
                                        cluster = c("company_id", "hs6"),
                                        data = import_data), lean = T)
  
  
  
  paybustorecom_export_18 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ letter_credit_use:", tech_var, "|
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = export_data), lean = T)
  
  paybustorecom_export_19 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ mean_remote_work_ISIC:", tech_var, "|
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = export_data), lean = T)
  
  paybustorecom_export_20 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ relationship_stickiness:", tech_var, "|
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = export_data), lean = T)
  
  paybustorecom_export_21 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ frac_lib_diff:", tech_var, "| 
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = export_data), lean = T)
  
  paybustorecom_export_inter <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, 
                                                        "+ letter_credit_use:", tech_var, 
                                                        "+ mean_remote_work_ISIC:", tech_var,
                                                        "+ relationship_stickiness:", tech_var,
                                                        "+ frac_lib_diff:", tech_var, 
                                                        "| company_id + hs6+ date_character")), 
                                        cluster = c("company_id", "hs6"),
                                        data = export_data), lean = T)
  
  
  models_other_HS<-list(paybustorecom_import_18, paybustorecom_export_18,
                        paybustorecom_import_19, paybustorecom_export_19,
                        paybustorecom_import_20, paybustorecom_export_20,
                        paybustorecom_import_21, paybustorecom_export_21, 
                        paybustorecom_import_inter, paybustorecom_export_inter)
  
  # Add dependent variables names to models
  names(models_other_HS)<-rep(c("Import Propensity", "Export Propensity"), length(models_other_HS)/2)
  
  # Fixed effects
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_other_HS)), 
                             "Month FE", rep("Yes", length(models_other_HS)), 
                             "Produce FE", rep("Yes", length(models_other_HS))), 
                           nrow = 3, byrow=T))
  
  
  # Produce table with models summary  
  table<-modelsummary(models_other_HS,
                      coef_rename = coef_labels, 
                      gof_map = gm, 
                      stars = c('*' = .1, '**' = .05, '***'= 0.01), 
                      add_rows = FE, 
                      output = "latex", 
                      align = paste(c("l", rep("c", length(models_other_HS))), sep="", collapse=""), 
                      notes = "Clustered-standard errors at the firm-product level.", 
                      fmt = format_se_coef, 
                      title = paste0(country_name," - Regression Results for Export Propensity and Import Propensity: COVID impacted Products. ", 
                                     n_lags, "-Lag in technology variable")) %>% 
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_other_HS)))%>%
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  
  
  # Save results
  saveRDS(list(models_other_HS, table), paste0("../../Outputs/", country_name, "/regressions_results/products_model/models_other_HS_", n_lags, "_lags_summary.rds"))
  
  
  return(list(models_other_HS, table))
  
}


# Regressions for Capital, Intermediate Goods --------

# Function for running regressions intermediate-capital products classficiation
prop_reg_models_cap_int<-function(tech_var, import_data, export_data, country_name, coef_labels){
  
  n_lags <- parse_number(tech_var)
  
  # Dependent variable: Import Dummy
  paybustorecom_import_22 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ INT:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = import_data), lean = T)
  
  paybustorecom_import_23<- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ CAP:", tech_var, "|
                                   company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data), lean = T)
  
  paybustorecom_import_inter<- summary(feols(as.formula(paste0("import_dummy ~", tech_var, 
                                                       "+ CAP:", tech_var,
                                                       "+ INT:", tech_var,
                                                       "|company_id + hs6+ date_character")), 
                                     cluster = c("company_id", "hs6"),
                                     data = import_data), lean = T)
  
  
  # Dependent variable: Export Propensity
  paybustorecom_export_22 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ INT:", tech_var, "| 
                                    company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = export_data), lean = T)
  
  paybustorecom_export_23<- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ CAP:", tech_var, "|
                                   company_id + hs6+ date_character")),
                                  cluster = c("company_id", "hs6"),
                                  data = export_data), lean = T)
  
  
  paybustorecom_export_inter<- summary(feols(as.formula(paste0("export_dummy ~", tech_var, 
                                                       "+ CAP:", tech_var,
                                                       "+ INT:", tech_var,
                                                       "|company_id + hs6+ date_character")), 
                                     cluster = c("company_id", "hs6"),
                                     data = export_data), lean = T)
  
  # List with models
  models_cap_int<-list(paybustorecom_import_22, paybustorecom_export_22,
                       paybustorecom_import_23, paybustorecom_export_23, 
                       paybustorecom_import_inter, paybustorecom_export_inter)
  
  # Assign dependent variable names to models 
  names(models_cap_int)<-rep(c("Import Propensity", "Export Propensity"), length(models_cap_int)/2)
  
  # Add fixed-effects indicators to tables
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_cap_int)), 
                             "Product FE", rep("Yes", length(models_cap_int)),
                             "Month FE", rep("Yes", length(models_cap_int))), 
                           nrow = 3, byrow=T)) 
  
  # Create Latex Table with models summary
  table<-modelsummary(models_cap_int,
                      coef_rename = coef_labels, 
                      gof_map = gm, 
                      stars = c('*' = .1, '**' = .05, '***'= 0.01), 
                      add_rows = FE, 
                      output = "latex", 
                      align = paste(c("l", rep("c", length(models_cap_int))), sep="", collapse=""), 
                      notes = "Clustered-standard errors at the firm-product level.", 
                      fmt = format_se_coef, 
                      title = paste0(country_name," - Regression Results for Import Propensity and Export Propensity: Capital - Intermediate products. ", 
                                     n_lags, "-Lag in technology variable")) %>% 
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_cap_int))) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  
  # Save results
  saveRDS(list(models_cap_int, table), paste0("../../Outputs/", country_name, "/regressions_results/products_model/models_cap_int_", n_lags, "_lags_summary.rds"))
  
  
  return(list(models_cap_int, table))
  
}



# Function for running propensity regressions for the definitive products to be used in the analysis (China E-commerce, E-bay tradable, Consumable and Durable Goods):
prop_reg_models_definitive<-function(tech_var, country_name, coef_labels){
  
  n_lags <- parse_number(tech_var)
  
  
  # Abbreviation 
  if(country_name == "India"){
    abbr<-"IND"
  } else if(country_name == "Indonesia"){
    abbr<-"IDN"
  } else if(country_name == "Mexico"){
    abbr<-"MEX"
  }
  
  vars <- c("Ebay_tradable", "China_E_commerce", "cons_BEC", "durable_BEC")
  
  
  # Read imports data for regressions
  import_data<-read_parquet(paste0("../../Data/", country_name, "/processed_data/imports_grid_product_model_", abbr, ".parquet"),
                            col_select = c("Ebay_tradable", "China_E_commerce", "cons_BEC", "durable_BEC",
                                           "company_id", "hs6", "date_character", "import_dummy", tech_var))
  
  import_data[, (vars) := lapply(.SD, function(x) 1 * x), .SDcols = vars]
  
  
  # Dependent variable: Import dummy
  paybustorecom_import_0 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "| 
                                    company_id + hs6+ date_character")),
                                          cluster = c("company_id", "hs6"),
                                          data = import_data), lean = T)
  
  gc()
  
  # E-bay tradable
  paybustorecom_import_1 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ Ebay_tradable:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                          cluster = c("company_id", "hs6"),
                                          data = import_data), lean = T)
  
  gc()
  
  # China E-commerce
  paybustorecom_import_2 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ China_E_commerce:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                          cluster = c("company_id", "hs6"),
                                          data = import_data), lean = T)
  
  gc()
  
  # Consumable Goods
  paybustorecom_import_3 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ cons_BEC:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                          cluster = c("company_id", "hs6"),
                                          data = import_data), lean = T)
  
  gc()
  
  
  # Durable goods 
  paybustorecom_import_4 <- summary(feols(as.formula(paste0("import_dummy ~", tech_var, "+ durable_BEC:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                          cluster = c("company_id", "hs6"),
                                          data = import_data), lean = T)
  
  gc()
  
  
  
  
  rm(import_data) # Free space
  gc()
  
  
  # Read exports data for regressions
  export_data<-read_parquet(paste0("../../Data/", country_name, "/processed_data/exports_grid_product_model_", abbr, ".parquet"), 
                            col_select = c("Ebay_tradable", "China_E_commerce", "cons_BEC", "durable_BEC",
                                            "company_id", "hs6", "date_character", "export_dummy", tech_var))
  
  export_data[, (vars) := lapply(.SD, function(x) 1 * x), .SDcols = vars]
  
  gc()
  
  # Dependent variable: Export Dummy
  paybustorecom_export_0 <- summary(feols(as.formula(paste("export_dummy ~", tech_var, "| 
                                    company_id + hs6+ date_character")), 
                                          cluster = c("company_id", "hs6"),
                                          data = export_data), lean = T)
  
  gc()
  
  paybustorecom_export_1 <- summary(feols(as.formula(paste0("export_dummy ~", tech_var, "+ Ebay_tradable:", tech_var, "| 
                                    company_id + hs6+ date_character")),
                                          cluster = c("company_id", "hs6"),
                                          data = export_data), lean = T)
  
  gc()
  
  paybustorecom_export_2 <- summary(feols(as.formula(paste("export_dummy ~", tech_var, "+ China_E_commerce:", tech_var, "| 
                                    company_id + hs6+ date_character")),
                                          cluster = c("company_id", "hs6"),
                                          data = export_data), lean = T)
  
  gc()
  
  paybustorecom_export_3 <- summary(feols(as.formula(paste("export_dummy ~", tech_var, "+ cons_BEC:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                          cluster = c("company_id", "hs6"),
                                          data = export_data), lean = T)
  
  gc()
  
  paybustorecom_export_4 <- summary(feols(as.formula(paste("export_dummy ~", tech_var, "+ durable_BEC:", tech_var, "| 
                                    company_id + hs6+ date_character")), 
                                          cluster = c("company_id", "hs6"),
                                          data = export_data), lean = T)
  
  gc()
  
  rm(export_data) # Free space
  gc()
  
  
  # List with models (import propensity and export propensity)
  models_definitive<-list(paybustorecom_import_0, paybustorecom_export_0,
                          paybustorecom_import_1, paybustorecom_export_1,
                          paybustorecom_import_2, paybustorecom_export_2,
                          paybustorecom_import_3, paybustorecom_export_3,
                          paybustorecom_import_4, paybustorecom_export_4)
  
  
  # Assign dependent variable name to models names
  names(models_definitive)<-rep(c("Import Propensity", "Export Propensity"), length(models_definitive)/2)
  
  # Create Fixed Efects data frame to add them to tables
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_definitive)), 
                             "Product FE", rep("Yes", length(models_definitive)),
                             "Year-Month FE", rep("Yes", length(models_definitive))), 
                           nrow = 3, byrow=T)) 
  
  
  # Produce table with models summary  
  table<-modelsummary(models_definitive,
                      coef_rename = coef_labels, 
                      gof_map = gm, 
                      stars = c('*' = .1, '**' = .05, '***'= 0.01), 
                      add_rows = FE, 
                      output = "latex", 
                      align = paste(c("l", rep("c", length(models_definitive))), sep="", collapse=""), 
                      notes = "Clustered-standard errors at the firm-product level.", 
                      fmt = format_se_coef, 
                      title = paste0(country_name," - Regression Results for Import Propensity and Export Propensity: e-Bay tradable, China e-commerce, Consumable and Durable products. ", 
                                     n_lags, "-Lag in technology variable")) %>% 
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_definitive))) %>%
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  
  
  # Save results
  saveRDS(list(models_definitive, table), paste0("../../Outputs/", country_name, "/regressions_results/products_model/models_definitive_", n_lags, "_lags_summary.rds"))
  
  return(list(models_definitive, table))
  
}


