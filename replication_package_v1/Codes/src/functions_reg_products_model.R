#=============================================================================#
# Date:    January 2022                                                       #
#                                                                             #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,     #
#          Indonesia and Mexico.                                              #
#                                                                             #
# Author:  Simon Caicedo - DECTI - The World Bank                             #
#                                                                             #
#                                                                             #
# This program creates functions for running regressions for the model        #
# with different products categories specifications (eBay tradable, China     #
# e-commerce, BEC  classification, Time-Sensitivity, capital-intermediate     #
# goods classification) interacted with a var technology (ecommerce/e-payment)#
# with a lagged structure. This code is necessary for creating the Rmarkdown  #
# that contains regression results.                                           #
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



# Function for running regressions technology for definitive products to be used: e-bay tradable, China e-commerce, durable and consumable products ----
reg_models_definitive<-function(tech_var, dep_var_import, dep_var_export,
                                import_data, export_data, country_name, coef_labels, 
                                dep_var_labels){
  
  n_lags <- parse_number(tech_var)
  
  
  paybustorecom_import_0 <- feols(as.formula(paste0(dep_var_import, "~", tech_var, "| 
                                    company_id + hs6+ date_character")),
                                  cluster = c("company_id", "hs6"),
                                  data = import_data)
  
  paybustorecom_import_1 <- feols(as.formula(paste0(dep_var_import, "~", tech_var, "+ Ebay_tradable:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data )
  
  paybustorecom_import_2 <- feols(as.formula(paste0(dep_var_import, "~", tech_var, "+ China_E_commerce:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data)
  
  paybustorecom_import_3 <- feols(as.formula(paste0(dep_var_import, "~", tech_var, "+ cons_BEC:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data )
  
  paybustorecom_import_4 <- feols(as.formula(paste0(dep_var_import, "~", tech_var, "+ durable_BEC:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data )
  
  
  
  paybustorecom_export_0 <- feols(as.formula(paste(dep_var_export, "~", tech_var, "| 
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = export_data)
  
  paybustorecom_export_1 <- feols(as.formula(paste0(dep_var_export, "~", tech_var, "+ Ebay_tradable:", tech_var, "| 
                                    company_id + hs6+ date_character")),
                                  cluster = c("company_id", "hs6"),
                                  data = export_data )
  
  paybustorecom_export_2 <- feols(as.formula(paste(dep_var_export, "~", tech_var, "+ China_E_commerce:", tech_var, "| 
                                    company_id + hs6+ date_character")),
                                  cluster = c("company_id", "hs6"),
                                  data = export_data )
  
  paybustorecom_export_3 <- feols(as.formula(paste(dep_var_export, "~", tech_var, "+ cons_BEC:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = export_data )
  
  paybustorecom_export_4 <- feols(as.formula(paste(dep_var_export, "~", tech_var, "+ durable_BEC:", tech_var, "| 
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = export_data )
  
  # List with models (Log exports and log imports)
  models_definitive<-list(paybustorecom_import_0, paybustorecom_export_0,
                          paybustorecom_import_1, paybustorecom_export_1,
                          paybustorecom_import_2, paybustorecom_export_2,
                          paybustorecom_import_3, paybustorecom_export_3,
                          paybustorecom_import_4, paybustorecom_export_4)
  
  # Assign dependent variable name to models names
  names(models_definitive)<-rep(dep_var_labels, length(models_definitive)/2)
  
  # Create Fixed Efects data frame to add them to tables
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_definitive)), 
                             "Product FE", rep("Yes", length(models_definitive)),
                             "Month FE", rep("Yes", length(models_definitive))), 
                           nrow = 3, byrow=T)) 
  
  
  if(dep_var_export == "new_destination" & dep_var_import == "new_source"){
    list_notes<-list("Clustered-standard errors at the firm-product level", 
                     "A new source/new destination is defined with respect to baseline year 2017", 
                     "The regressions for new source/destination are estimated using a subset of firms that had transactions in 2017 as well.")
    
    format_se_coef <- function(x) format(round(x, 6), nsmall = 2, scientific = FALSE)
    
    
  } else{
    list_notes<-list("Clustered-standard errors at the firm-product level")
    
    format_se_coef <- f1
    
  }
  
  
  
  # Produce table with models summary  
  table<-modelsummary(models_definitive,
                      coef_rename = coef_labels, 
                      gof_map = gm, 
                      stars = c('*' = .1, '**' = .05, '***'= 0.01), 
                      add_rows = FE, 
                      output = "latex", 
                      align = paste(c("l", rep("c", length(models_definitive))), sep="", collapse=""), 
                      notes = list_notes, 
                      fmt = format_se_coef, 
                      title = paste0(country_name," - Regression Results for ",  dep_var_labels[1] , " and ", dep_var_labels[2], ": e-Bay tradable, China e-commerce, Consumable and Durable products. ", 
                                     n_lags, "-Lag in technology variable")) |>
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_definitive))) %>%
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  
  return(list(models_definitive, table))
  
}




# reg_models_definitive<-function(tech_var, import_data, export_data, country_name, coef_labels){
#   
#   n_lags <- parse_number(tech_var)
#   
#   
#   # Dependent variable: Log of imports 
#   paybustorecom_import_0 <- feols(as.formula(paste0("log_import ~", tech_var, "| 
#                                     company_id + hs6+ date_character")),
#                                   cluster = c("company_id", "hs6"),
#                                   data = import_data)
#   
#   paybustorecom_import_1 <- feols(as.formula(paste0("log_import ~", tech_var, "+ Ebay_tradable:", tech_var, "|
#                                     company_id + hs6+ date_character")), 
#                                   cluster = c("company_id", "hs6"),
#                                   data = import_data )
#   
#   paybustorecom_import_2 <- feols(as.formula(paste0("log_import ~", tech_var, "+ China_E_commerce:", tech_var, "|
#                                     company_id + hs6+ date_character")), 
#                                   cluster = c("company_id", "hs6"),
#                                   data = import_data)
#   
#   paybustorecom_import_3 <- feols(as.formula(paste0("log_import ~", tech_var, "+ cons_BEC:", tech_var, "|
#                                     company_id + hs6+ date_character")), 
#                                   cluster = c("company_id", "hs6"),
#                                   data = import_data )
#   
#   paybustorecom_import_4 <- feols(as.formula(paste0("log_import ~", tech_var, "+ durable_BEC:", tech_var, "|
#                                     company_id + hs6+ date_character")), 
#                                   cluster = c("company_id", "hs6"),
#                                   data = import_data )
#   
#   
#   
#   # Dependent variable: Log of Exports 
#   paybustorecom_export_0 <- feols(as.formula(paste("log_export ~", tech_var, "| 
#                                     company_id + hs6+ date_character")), 
#                                   cluster = c("company_id", "hs6"),
#                                   data = export_data)
#   
#   paybustorecom_export_1 <- feols(as.formula(paste0("log_export ~", tech_var, "+ Ebay_tradable:", tech_var, "| 
#                                     company_id + hs6+ date_character")),
#                                   cluster = c("company_id", "hs6"),
#                                   data = export_data )
#   
#   paybustorecom_export_2 <- feols(as.formula(paste("log_export ~", tech_var, "+ China_E_commerce:", tech_var, "| 
#                                     company_id + hs6+ date_character")),
#                                   cluster = c("company_id", "hs6"),
#                                   data = export_data )
#   
#   paybustorecom_export_3 <- feols(as.formula(paste("log_export ~", tech_var, "+ cons_BEC:", tech_var, "|
#                                     company_id + hs6+ date_character")), 
#                                   cluster = c("company_id", "hs6"),
#                                   data = export_data )
#   
#   paybustorecom_export_4 <- feols(as.formula(paste("log_export ~", tech_var, "+ durable_BEC:", tech_var, "| 
#                                     company_id + hs6+ date_character")), 
#                                   cluster = c("company_id", "hs6"),
#                                   data = export_data )
#   
#   # List with models (Log exports and log imports)
#   models_definitive<-list(paybustorecom_import_0, paybustorecom_export_0,
#                           paybustorecom_import_1, paybustorecom_export_1,
#                           paybustorecom_import_2, paybustorecom_export_2,
#                           paybustorecom_import_3, paybustorecom_export_3,
#                           paybustorecom_import_4, paybustorecom_export_4)
#   
#   # Assign dependent variable name to models names
#   names(models_definitive)<-rep(c("Log.Imports", "Log.Exports"), length(models_definitive)/2)
#   
#   # Create Fixed Efects data frame to add them to tables
#   FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_definitive)), 
#                              "Product FE", rep("Yes", length(models_definitive)),
#                              "Month FE", rep("Yes", length(models_definitive))), 
#                            nrow = 3, byrow=T)) 
#   
#   
#   # Produce table with models summary  
#   table<-modelsummary(models_definitive,
#                       coef_rename = coef_labels, 
#                       gof_map = gm, 
#                       stars = c('*' = .1, '**' = .05, '***'= 0.01), 
#                       add_rows = FE, 
#                       output = "latex", 
#                       align = paste(c("l", rep("c", length(models_definitive))), sep="", collapse=""), 
#                       notes = "Clustered-standard errors at the firm-product level.", 
#                       fmt = f1, 
#                       title = paste0(country_name," - Regression Results for Log. Imports and Log. Exports: e-Bay tradable, China e-commerce, Consumable and Durable products. ", 
#                                      n_lags, "-Lag in technology variable")) |>
#     add_header_above(c(" " = 1, "Dependent Variables" = length(models_definitive))) %>%
#     kable_styling(latex_options = c("HOLD_position", "scale_down")) 
#   
#   
#   return(list(models_definitive, table))
#   
# }
# 




# Function for running regressions technology with ebay and china e-commerce with a specific technology ----
reg_models_ebay_china<-function(tech_var, import_data, export_data, country_name, coef_labels){
  
  n_lags <- parse_number(tech_var)
  
  
  # Dependent variable: Log of imports 
  paybustorecom_import_0 <- feols(as.formula(paste0("log_import ~", tech_var, "| 
                                    company_id + hs6+ date_character")),
                                  cluster = c("company_id", "hs6"),
                                  data = import_data)
  
  paybustorecom_import_1 <- feols(as.formula(paste0("log_import ~", tech_var, "+ Ebay_tradable:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data )
  
  paybustorecom_import_2 <- feols(as.formula(paste0("log_import ~", tech_var, "+ China_E_commerce:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data)
  
  paybustorecom_import_3 <- feols(as.formula(paste0("log_import ~", tech_var, "+ China_E_commerce_updated:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data )
  
  paybustorecom_import_4 <- feols(as.formula(paste0("log_import ~", tech_var, "+ diff_new_old_China_ecommerce_list:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data )

  
  
  # Dependent variable: Log of Exports 
  paybustorecom_export_0 <- feols(as.formula(paste("log_export ~", tech_var, "| 
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = export_data)
  
  paybustorecom_export_1 <- feols(as.formula(paste0("log_export ~", tech_var, "+ Ebay_tradable:", tech_var, "| 
                                    company_id + hs6+ date_character")),
                                  cluster = c("company_id", "hs6"),
                                  data = export_data )
  
  paybustorecom_export_2 <- feols(as.formula(paste("log_export ~", tech_var, "+ China_E_commerce:", tech_var, "| 
                                    company_id + hs6+ date_character")),
                                  cluster = c("company_id", "hs6"),
                                  data = export_data )
  
  paybustorecom_export_3 <- feols(as.formula(paste("log_export ~", tech_var, "+ China_E_commerce_updated:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = export_data )
  
  paybustorecom_export_4 <- feols(as.formula(paste("log_export ~", tech_var, "+ diff_new_old_China_ecommerce_list:", tech_var, "| 
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = export_data )
  
  # List with models (Log exports and log imports)
  models_ebay_china<-list(paybustorecom_import_0, paybustorecom_export_0,
                          paybustorecom_import_1, paybustorecom_export_1,
                          paybustorecom_import_2, paybustorecom_export_2,
                          paybustorecom_import_3, paybustorecom_export_3,
                          paybustorecom_import_4, paybustorecom_export_4)
  
  # Assign dependent variable name to models names
  names(models_ebay_china)<-rep(c("Log.Imports", "Log.Exports"), length(models_ebay_china)/2)
  
  # Create Fixed Efects data frame to add them to tables
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_ebay_china)), 
                             "Product FE", rep("Yes", length(models_ebay_china)),
                             "Month FE", rep("Yes", length(models_ebay_china))), 
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
                      fmt = f1, 
                      title = paste0(country_name," - Regression Results for Log. Imports and Log. Exports: e-Bay tradable and China e-commerce products. ", 
                                     n_lags, "-Lag in technology variable")) |>
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_ebay_china))) %>%
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  
  return(list(models_ebay_china, table))
  
}

# Function for running regressions BEC products classification ----
reg_models_BEC<-function(tech_var, import_data, export_data, country_name, coef_labels){
  
  n_lags <- parse_number(tech_var)
  
  # Dependent variable: Log. Import
  paybustorecom_import_5 <- feols(as.formula(paste0("log_import ~", tech_var, "+ parts_BEC:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data )
  
  paybustorecom_import_6<- feols(as.formula(paste0("log_import ~", tech_var, "+ cons_dur_BEC:", tech_var, "|
                                   company_id + hs6+ date_character")), 
                                 cluster = c("company_id", "hs6"),
                                 data = import_data )
  
  paybustorecom_import_7 <- feols(as.formula(paste0("log_import ~", tech_var, "+ cons_semi_BEC:", tech_var, "| 
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data )
  
  paybustorecom_import_8 <- feols(as.formula(paste0("log_import ~", tech_var, "+ cons_BEC:", tech_var, "| 
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data )
  
  paybustorecom_import_9 <- feols(as.formula(paste0("log_import ~", tech_var, "+ transp_BEC:", tech_var, "| 
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data)
  
  
  paybustorecom_import_10 <- feols(as.formula(paste0("log_import ~", tech_var, "+ durable_BEC:", tech_var, "|
                                     company_id + hs6+ date_character")),
                                   data = import_data )
  
  paybustorecom_import_11 <- feols(as.formula(paste0("log_import ~", tech_var, "+ durable_semi_BEC:", tech_var, "|
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = import_data )
  
  # All interactions
  paybustorecom_import_inter <- feols(as.formula(paste0("log_import ~", tech_var,
                                                        "+ parts_BEC:", tech_var,
                                                    #    "+ cons_dur_BEC:", tech_var,
                                                    #    "+ cons_semi_BEC:", tech_var, 
                                                        "+ cons_BEC:", tech_var, 
                                                        "+ transp_BEC:", tech_var, 
                                                        "+ durable_BEC:", tech_var, 
                                                    #    "+ durable_semi_BEC:", tech_var, 
                                                        "| company_id + hs6+ date_character")),
                                      cluster = c("company_id", "hs6"),
                                      data = import_data 
  )
  
  
  # Dependent variable: Log. Exports
  paybustorecom_export_5 <- feols(as.formula(paste0("log_export ~", tech_var, "+ parts_BEC:", tech_var,
                                                    "| company_id + hs6+ date_character")),
                                  cluster = c("company_id", "hs6"),
                                  data = export_data )
  
  paybustorecom_export_6<- feols(as.formula(paste0("log_export ~", tech_var, "+ cons_dur_BEC:", tech_var, "|
                                   company_id + hs6+ date_character")),
                                 cluster = c("company_id", "hs6"),
                                 data = export_data)
  
  paybustorecom_export_7 <- feols(as.formula(paste0("log_export ~", tech_var, "+ cons_semi_BEC:", tech_var,  "| 
                                    company_id + hs6+ date_character")),
                                  cluster = c("company_id", "hs6"),
                                  data = export_data )
  
  paybustorecom_export_8 <- feols(as.formula(paste0("log_export ~", tech_var, "+ cons_BEC:", tech_var, "| 
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = export_data )
  
  paybustorecom_export_9 <- feols(as.formula(paste0("log_export ~", tech_var, "+ transp_BEC:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = export_data )
  
  paybustorecom_export_10 <- feols(as.formula(paste0("log_export ~", tech_var, "+ durable_BEC:", tech_var, "|
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = export_data )
  
  paybustorecom_export_11 <- feols(as.formula(paste0("log_export ~", tech_var, "+ durable_semi_BEC:", tech_var, "| 
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = export_data)
  
  # All interactions
  paybustorecom_export_inter <- feols(as.formula(paste0("log_export ~", tech_var,
                                                        "+ parts_BEC:", tech_var,
                                                    #    "+ cons_dur_BEC:", tech_var,
                                                    #    "+ cons_semi_BEC:", tech_var, 
                                                        "+ cons_BEC:", tech_var, 
                                                        "+ transp_BEC:", tech_var, 
                                                        "+ durable_BEC:", tech_var, 
                                                    #    "+ durable_semi_BEC:", tech_var, 
                                                        "| company_id + hs6+ date_character")),
                                        cluster = c("company_id", "hs6"),
                                        data = export_data 
                                      )
  
  
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
  names(models_BEC)<-rep(c("Log.Imports", "Log.Exports"), length(models_BEC)/2)
  
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
                      fmt = f1, 
                      title = paste0(country_name, " - Regression Results for Log. Imports and Log. Exports: BEC products classification. ", 
                                     n_lags, "-Lag in technology variable")) |>
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_BEC))) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  
  return(list(models_BEC, table))
  
}

# Funtion for regressions with time-sensitive products variables ----

reg_models_time_sens<-function(tech_var, import_data, export_data, country_name, coef_labels){
  
  n_lags <- parse_number(tech_var)
  
  
  # Dependent variable: Log of imports
  paybustorecom_import_12 <- feols(as.formula(paste0("log_import ~", tech_var, "+ component:", tech_var,  "|
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = import_data)
  
  
  paybustorecom_import_13 <- feols(as.formula(paste0("log_import ~", tech_var, "+ fresh:", tech_var, "|
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = import_data )
  
  paybustorecom_import_14 <- feols(as.formula(paste0("log_import ~", tech_var, "+ frozen:", tech_var, "|
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = import_data )
  
  paybustorecom_import_15 <- feols(as.formula(paste0("log_import ~", tech_var, "+ hummels_timesensitive:", tech_var, "|
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = import_data )
  
  paybustorecom_import_16 <- feols(as.formula(paste0("log_import ~", tech_var, "+ agric_timesensitive:", tech_var, "|
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = import_data )
  
  paybustorecom_import_17 <- feols(as.formula(paste0("log_import ~", tech_var, "+ time_sens_hs2013_russ_typeofgoods:", tech_var, "|
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = import_data )
  
  # All interactions
  paybustorecom_import_inter <- feols(as.formula(paste0("log_import ~", tech_var, 
                                                        "+ component:", tech_var,
                                                        "+ fresh:", tech_var,
                                                        #"+ frozen:", tech_var,
                                                        "|company_id + hs6+ date_character")),
                                      cluster = c("company_id", "hs6"),
                                      data = import_data)
  
  
  # Dependent variable: Log of exports
  paybustorecom_export_12 <- feols(as.formula(paste0("log_export ~", tech_var, "+ component:", tech_var, "|
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = export_data )
  
  paybustorecom_export_13 <- feols(as.formula(paste0("log_export ~", tech_var, "+ fresh:", tech_var, "|
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = export_data)
  
  paybustorecom_export_14 <- feols(as.formula(paste0("log_export ~", tech_var, "+ frozen:", tech_var, "| 
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = export_data )
  
  paybustorecom_export_15 <- feols(as.formula(paste0("log_export ~", tech_var, "+ hummels_timesensitive:", tech_var, "|
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = export_data )
  
  paybustorecom_export_16 <- feols(as.formula(paste0("log_export ~", tech_var, "+ agric_timesensitive:", tech_var, "| 
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = export_data )
  
  paybustorecom_export_17 <- feols(as.formula(paste0("log_export ~", tech_var, "+ time_sens_hs2013_russ_typeofgoods:", tech_var, "|
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = export_data)
  
  # All interactions
  paybustorecom_export_inter <- feols(as.formula(paste0("log_export ~", tech_var, 
                                                        "+ component:", tech_var,
                                                        "+ fresh:", tech_var,
                                                        #"+ frozen:", tech_var,
                                                        "|company_id + hs6+ date_character")),
                                      cluster = c("company_id", "hs6"),
                                      data = export_data)
  
  # List with models
  models_time_sens <- list(paybustorecom_import_12, paybustorecom_export_12,
                           paybustorecom_import_13, paybustorecom_export_13,
                           paybustorecom_import_14, paybustorecom_export_14,
                           paybustorecom_import_15, paybustorecom_export_15,
                           paybustorecom_import_16, paybustorecom_export_16,
                           paybustorecom_import_17, paybustorecom_export_17, 
                           paybustorecom_import_inter, paybustorecom_export_inter)
  
  # Assign dependent variable names to models 
  names(models_time_sens)<-rep(c("Log.Imports", "Log.Exports"), length(models_time_sens)/2)
  
  
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
                      fmt = f1, 
                      title = paste0(country_name," - Regression Results for Log.Imports and Log.Exports: Time-sensitive Products. ", 
                                     n_lags, "-Lag in technology variable")) |>
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_time_sens))) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down"))
  
  
  return(list(models_time_sens, table))
  
}


# Function for regressions with other classification for HS Products (Letter credit use, mean IDN remote work, relationship stickiness)---- 

reg_models_other_HS<-function(tech_var, import_data, export_data, country_name, coef_labels){
  
  n_lags <- parse_number(tech_var)
  
  
  # Dependent variable: Log imports 
  paybustorecom_import_18 <- feols(as.formula(paste0("log_import ~", tech_var, "+ letter_credit_use:", tech_var, "|
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = import_data )
  paybustorecom_import_19 <- feols(as.formula(paste0("log_import ~", tech_var, "+ mean_remote_work_ISIC:", tech_var, "|
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = import_data )
  
  paybustorecom_import_20 <- feols(as.formula(paste0("log_import ~", tech_var, "+ relationship_stickiness:", tech_var, "| 
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = import_data)
  
  paybustorecom_import_21 <- feols(as.formula(paste0("log_import ~", tech_var, "+ frac_lib_diff:", tech_var, "| 
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = import_data )
  
  
  paybustorecom_import_inter <- feols(as.formula(paste0("log_import ~", tech_var, 
                                                        "+ letter_credit_use:", tech_var, 
                                                        "+ mean_remote_work_ISIC:", tech_var,
                                                        "+ relationship_stickiness:", tech_var,
                                                        "+ frac_lib_diff:", tech_var, 
                                                        "| company_id + hs6+ date_character")), 
                                      cluster = c("company_id", "hs6"),
                                      data = import_data )
  
  
  
  paybustorecom_export_18 <- feols(as.formula(paste0("log_export ~", tech_var, "+ letter_credit_use:", tech_var, "|
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = export_data )
  
  paybustorecom_export_19 <- feols(as.formula(paste0("log_export ~", tech_var, "+ mean_remote_work_ISIC:", tech_var, "|
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = export_data )
  
  paybustorecom_export_20 <- feols(as.formula(paste0("log_export ~", tech_var, "+ relationship_stickiness:", tech_var, "|
                                     company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = export_data )
  
  paybustorecom_export_21 <- feols(as.formula(paste0("log_export ~", tech_var, "+ frac_lib_diff:", tech_var, "| 
                                     company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = export_data )
  
  paybustorecom_export_inter <- feols(as.formula(paste0("log_export ~", tech_var, 
                                                        "+ letter_credit_use:", tech_var, 
                                                        "+ mean_remote_work_ISIC:", tech_var,
                                                        "+ relationship_stickiness:", tech_var,
                                                        "+ frac_lib_diff:", tech_var, 
                                                        "| company_id + hs6+ date_character")), 
                                      cluster = c("company_id", "hs6"),
                                      data = export_data )
  
  
  models_other_HS<-list(paybustorecom_import_18, paybustorecom_export_18,
                        paybustorecom_import_19, paybustorecom_export_19,
                        paybustorecom_import_20, paybustorecom_export_20,
                        paybustorecom_import_21, paybustorecom_export_21, 
                        paybustorecom_import_inter, paybustorecom_export_inter)
  
  # Add dependent variables names to models
  names(models_other_HS)<-rep(c("Log.Imports", "Log.Exports"), length(models_other_HS)/2)
  
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
                      fmt = f1, 
                      title = paste0(country_name," - Regression Results for Log.Exports and Log.Imports: COVID impacted Products. ", 
                                     n_lags, "-Lag in technology variable")) |>
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_other_HS)))%>%
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  return(list(models_other_HS, table))
  
}


# Regressions for Capital, Intermediate Goods --------

# Function for running regressions BEC products classficiation
reg_models_cap_int<-function(tech_var, import_data, export_data, country_name, coef_labels){
  
  n_lags <- parse_number(tech_var)
  
  # Dependent variable: Log. Import
  paybustorecom_import_22 <- feols(as.formula(paste0("log_import ~", tech_var, "+ INT:", tech_var, "|
                                    company_id + hs6+ date_character")), 
                                   cluster = c("company_id", "hs6"),
                                   data = import_data )
  
  paybustorecom_import_23<- feols(as.formula(paste0("log_import ~", tech_var, "+ CAP:", tech_var, "|
                                   company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data )
  
  paybustorecom_import_inter<- feols(as.formula(paste0("log_import ~", tech_var, 
                                                "+ CAP:", tech_var,
                                                "+ INT:", tech_var,
                                                "|company_id + hs6+ date_character")), 
                                  cluster = c("company_id", "hs6"),
                                  data = import_data )
  
  
  # Dependent variable: Log. Exports
  paybustorecom_export_22 <- feols(as.formula(paste0("log_export ~", tech_var, "+ INT:", tech_var, "| 
                                    company_id + hs6+ date_character")),
                                   cluster = c("company_id", "hs6"),
                                   data = export_data )
  
  paybustorecom_export_23<- feols(as.formula(paste0("log_export ~", tech_var, "+ CAP:", tech_var, "|
                                   company_id + hs6+ date_character")),
                                  cluster = c("company_id", "hs6"),
                                  data = export_data)
  
  
  paybustorecom_export_inter<- feols(as.formula(paste0("log_export ~", tech_var, 
                                                       "+ CAP:", tech_var,
                                                       "+ INT:", tech_var,
                                                       "|company_id + hs6+ date_character")), 
                                     cluster = c("company_id", "hs6"),
                                     data = export_data )
  
  
  # List with models
  models_cap_int<-list(paybustorecom_import_22, paybustorecom_export_22,
                       paybustorecom_import_23, paybustorecom_export_23, 
                       paybustorecom_import_inter, paybustorecom_export_inter)
  
  # Assign dependent variable names to models 
  names(models_cap_int)<-rep(c("Log.Imports", "Log.Exports"), length(models_cap_int)/2)
  
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
                      fmt = f1, 
                      title = paste0(country_name," - Regression Results for Log. Imports and Log. Exports: Capital - Intermediate products. ", 
                                     n_lags, "-Lag in technology variable")) |>
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_cap_int))) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  
  return(list(models_cap_int, table))
  
}







