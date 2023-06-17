#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
#                                                                           #
# This program generates functions for running regressions of the model that#
# shows whether the existing technology use mitigate covid  impacts. This   #
# code is necessary for creating the Rmarkdown
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




# Regressions for definitive products categories to be used in the analysis: E-bay Tradable, China e-commerce, Consumable and Durable products -----


# Regressions for definitive products categories to be used in the analysis: E-bay Tradable, China e-commerce, Consumable and Durable products -----
reg_models_tech_covid_definitive<-function(import_data, export_data, country_name, coef_labels, 
                                           dep_var_import, dep_var_export, 
                                           dep_var_labels, indep_var){
  
  
  paybustorecom_import_preresearch_covid_1 <- feols(as.formula(paste0(dep_var_import,  "~ ",
                                                                      indep_var, ":month_mean_stringency_index +",
                                                                      indep_var,":month_mean_stringency_index:Ebay_tradable +
                                                      month_mean_stringency_index:Ebay_tradable|
                                                      company_id + hs6 + date_character")), 
                                                    cluster = c("company_id", "hs6"),
                                                    data = import_data )
  
  paybustorecom_import_preresearch_covid_2 <- feols(as.formula(paste0(dep_var_import,  "~ ", 
                                                                      indep_var, ":month_mean_stringency_index +",
                                                                      indep_var, ":month_mean_stringency_index:China_E_commerce +
                                                      month_mean_stringency_index:China_E_commerce|
                                                      company_id + hs6 + date_character")), 
                                                    cluster = c("company_id", "hs6"),
                                                    data = import_data )
  
  paybustorecom_import_preresearch_covid_3 <- feols(as.formula(paste0(dep_var_import,  "~ ",
                                                                      indep_var, ":month_mean_stringency_index +",
                                                                      indep_var, ":month_mean_stringency_index:cons_BEC +
                                                      month_mean_stringency_index:cons_BEC|
                                                      company_id + hs6 + date_character")), 
                                                    cluster = c("company_id", "hs6"),
                                                    data = import_data )
  
  
  
  paybustorecom_import_preresearch_covid_4 <- feols(as.formula(paste0(dep_var_import,  "~ ", 
                                                                      indep_var, ":month_mean_stringency_index +",
                                                                      indep_var, ":month_mean_stringency_index:durable_BEC +
                                                      month_mean_stringency_index:durable_BEC|
                                                      company_id + hs6 + date_character")), 
                                                    cluster = c("company_id", "hs6"),
                                                    data = import_data )
  
  
  
  paybustorecom_export_preresearch_covid_1 <- feols(as.formula(paste0(dep_var_export,  "~ ", 
                                                                      indep_var,":month_mean_stringency_index + ",
                                                                      indep_var, ":month_mean_stringency_index:Ebay_tradable +
                                                      month_mean_stringency_index:Ebay_tradable|
                                                      company_id + hs6 + date_character")), 
                                                    cluster = c("company_id", "hs6"),
                                                    data = export_data )
  
  paybustorecom_export_preresearch_covid_2 <- feols(as.formula(paste0(dep_var_export,  " ~ ",
                                                                      indep_var, ":month_mean_stringency_index + ",
                                                                      indep_var, ":month_mean_stringency_index:China_E_commerce +
                                                      month_mean_stringency_index:China_E_commerce|
                                                      company_id + hs6 + date_character")), 
                                                    cluster = c("company_id", "hs6"),
                                                    data = export_data )
  
  paybustorecom_export_preresearch_covid_3 <- feols(as.formula(paste0(dep_var_export,  " ~", 
                                                                      indep_var, ":month_mean_stringency_index +",
                                                                      indep_var, ":month_mean_stringency_index:cons_BEC +
                                                      month_mean_stringency_index:cons_BEC|
                                                      company_id + hs6 + date_character")), 
                                                    cluster = c("company_id", "hs6"),
                                                    data = export_data )
  
  
  paybustorecom_export_preresearch_covid_4 <- feols(as.formula(paste0(dep_var_export,  " ~ ",
                                                                      indep_var, ":month_mean_stringency_index +",
                                                                      indep_var, ":month_mean_stringency_index:durable_BEC +
                                                      month_mean_stringency_index:durable_BEC|
                                                      company_id + hs6 + date_character")), 
                                                    cluster = c("company_id", "hs6"),
                                                    data = export_data)
  
  #How Existing Tech Insulates from COVID impacts e-commerce
  models_tech_covid_definitive<-list(paybustorecom_import_preresearch_covid_1, paybustorecom_export_preresearch_covid_1,
                                     paybustorecom_import_preresearch_covid_2, paybustorecom_export_preresearch_covid_2,
                                     paybustorecom_import_preresearch_covid_3, paybustorecom_export_preresearch_covid_3,
                                     paybustorecom_import_preresearch_covid_4, paybustorecom_export_preresearch_covid_4)
  
  
  
  # Assign dependent variable name to models names
  names(models_tech_covid_definitive)<-rep(dep_var_labels, length(models_tech_covid_definitive)/2)
  
  # Add fixed-effects indicators to tables
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_tech_covid_definitive)), 
                             "Product FE", rep("Yes", length(models_tech_covid_definitive)),
                             "Month FE", rep("Yes", length(models_tech_covid_definitive))), 
                           nrow = 3, byrow=T))
  
  if(indep_var == "adopted_pay_or_ecom_before_2019"){
    indep_var_note<-"The variable E-payment or E-commerce 2019 means that the company adopted the E-payment or E-commerce technology before 2019."
  } else if(indep_var == "months_since_adoption"){
    indep_var_note<-"The variable Months Since Adoption represents the number of months since the firm adopted the E-payment or E-commerce technology."
  }
  
  
  
  if(dep_var_export == "new_destination" & dep_var_import == "new_source"){
    list_notes<-list("Clustered-standard errors at the firm-product level", 
                     indep_var_note,
                     "A new source/new destination is defined with respect to baseline year 2017", 
                     "The regressions for new source/destination are estimated using a subset of firms that had transactions in 2017 as well.")
    
    #format_se_coef <- function(x) format(round(x, 6), nsmall = 2, scientific = FALSE)
    
  } else{
    list_notes<-list("Clustered-standard errors at the firm-product level", 
                     indep_var_note)
    
    #format_se_coef <- f1
    
  }
  
  format_se_coef <- function(x) format(round(x, 6), nsmall = 2, scientific = FALSE)
  
  # Table summary models
  table<-modelsummary(models_tech_covid_definitive,
                      coef_rename = coef_labels, 
                      gof_map = gm, 
                      stars = c('*' = .1, '**' = .05, '***'= 0.01), 
                      add_rows = FE, 
                      output = "latex", 
                      align = paste(c("l", rep("c", length(models_tech_covid_definitive))), sep="", collapse=""), 
                      notes = list_notes,
                      fmt = format_se_coef,  
                      title = paste0(country_name," - Regression Results for ", dep_var_labels[1], " and ", dep_var_labels[2], ": e-Bay tradable, China e-commerce, Consumable and Durable products")) |>
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_tech_covid_definitive))) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  
  
  return(list(models_tech_covid_definitive, table))
  
}







# This function allows to estimate the existing tech regressions using two independent variable measuring the existing tech adoption. For example, we estimate the 
# regressions using technology adoption before 2019 dummy and months since adoption. 

reg_models_tech_covid_two_indep_vars<-function(import_data, export_data, country_name, coef_labels, 
                                               dep_var_import, dep_var_export, 
                                               dep_var_labels, indep_var1, indep_var2){
  
  
  paybustorecom_import_preresearch_covid_1 <- feols(as.formula(paste0(dep_var_import,  "~ ",
                                                                      indep_var1, ":month_mean_stringency_index +",
                                                                      indep_var1,":month_mean_stringency_index:Ebay_tradable +",
                                                                      indep_var2, ":month_mean_stringency_index +",
                                                                      indep_var2,":month_mean_stringency_index:Ebay_tradable +",
                                                                      "month_mean_stringency_index:Ebay_tradable", 
                                                                      "|company_id + hs6 + date_character")), 
                                                    cluster = c("company_id", "hs6"),
                                                    data = import_data )
  
  paybustorecom_import_preresearch_covid_2 <- feols(as.formula(paste0(dep_var_import,  "~ ", 
                                                                      indep_var1, ":month_mean_stringency_index +",
                                                                      indep_var1, ":month_mean_stringency_index:China_E_commerce +", 
                                                                      indep_var2, ":month_mean_stringency_index +",
                                                                      indep_var2, ":month_mean_stringency_index:China_E_commerce +", 
                                                                      "month_mean_stringency_index:China_E_commerce",
                                                                      "|company_id + hs6 + date_character")), 
                                                    cluster = c("company_id", "hs6"),
                                                    data = import_data )
  
  paybustorecom_import_preresearch_covid_3 <- feols(as.formula(paste0(dep_var_import,  "~ ",
                                                                      indep_var1, ":month_mean_stringency_index +",
                                                                      indep_var1, ":month_mean_stringency_index:cons_BEC +", 
                                                                      indep_var2, ":month_mean_stringency_index +",
                                                                      indep_var2, ":month_mean_stringency_index:cons_BEC +",
                                                                      "month_mean_stringency_index:cons_BEC", 
                                                                      "|company_id + hs6 + date_character")), 
                                                    cluster = c("company_id", "hs6"),
                                                    data = import_data )
  
  
  
  paybustorecom_import_preresearch_covid_4 <- feols(as.formula(paste0(dep_var_import,  "~ ", 
                                                                      indep_var1, ":month_mean_stringency_index +",
                                                                      indep_var1, ":month_mean_stringency_index:durable_BEC +", 
                                                                      indep_var2, ":month_mean_stringency_index +",
                                                                      indep_var2, ":month_mean_stringency_index:durable_BEC +",
                                                                      "month_mean_stringency_index:durable_BEC", 
                                                                      "|company_id + hs6 + date_character")), 
                                                    cluster = c("company_id", "hs6"),
                                                    data = import_data )
  
  
  
  paybustorecom_export_preresearch_covid_1 <- feols(as.formula(paste0(dep_var_export,  "~ ", 
                                                                      indep_var1,":month_mean_stringency_index + ",
                                                                      indep_var1, ":month_mean_stringency_index:Ebay_tradable +",
                                                                      indep_var2,":month_mean_stringency_index + ",
                                                                      indep_var2, ":month_mean_stringency_index:Ebay_tradable +",
                                                                      "month_mean_stringency_index:Ebay_tradable",
                                                                      "|company_id + hs6 + date_character")), 
                                                    cluster = c("company_id", "hs6"),
                                                    data = export_data )
  
  paybustorecom_export_preresearch_covid_2 <- feols(as.formula(paste0(dep_var_export,  " ~",
                                                                      indep_var1, ":month_mean_stringency_index + ",
                                                                      indep_var1, ":month_mean_stringency_index:China_E_commerce +", 
                                                                      indep_var2, ":month_mean_stringency_index + ",
                                                                      indep_var2, ":month_mean_stringency_index:China_E_commerce +", 
                                                                      "month_mean_stringency_index:China_E_commerce", 
                                                                      "|company_id + hs6 + date_character")), 
                                                    cluster = c("company_id", "hs6"),
                                                    data = export_data )
  
  paybustorecom_export_preresearch_covid_3 <- feols(as.formula(paste0(dep_var_export,  " ~", 
                                                                      indep_var1, ":month_mean_stringency_index +",
                                                                      indep_var1, ":month_mean_stringency_index:cons_BEC +", 
                                                                      indep_var2, ":month_mean_stringency_index +",
                                                                      indep_var2, ":month_mean_stringency_index:cons_BEC +", 
                                                                      "month_mean_stringency_index:cons_BEC", 
                                                                      "|company_id + hs6 + date_character")), 
                                                    cluster = c("company_id", "hs6"),
                                                    data = export_data )
  
  
  paybustorecom_export_preresearch_covid_4 <- feols(as.formula(paste0(dep_var_export,  " ~ ",
                                                                      indep_var1, ":month_mean_stringency_index +",
                                                                      indep_var1, ":month_mean_stringency_index:durable_BEC +",
                                                                      indep_var2, ":month_mean_stringency_index +",
                                                                      indep_var2, ":month_mean_stringency_index:durable_BEC +",
                                                                      "month_mean_stringency_index:durable_BEC", 
                                                                      "|company_id + hs6 + date_character")), 
                                                    cluster = c("company_id", "hs6"),
                                                    data = export_data)
  
  #How Existing Tech Insulates from COVID impacts e-commerce
  models_tech_covid_definitive<-list(paybustorecom_import_preresearch_covid_1, paybustorecom_export_preresearch_covid_1,
                                     paybustorecom_import_preresearch_covid_2, paybustorecom_export_preresearch_covid_2,
                                     paybustorecom_import_preresearch_covid_3, paybustorecom_export_preresearch_covid_3,
                                     paybustorecom_import_preresearch_covid_4, paybustorecom_export_preresearch_covid_4)
  
  
  
  # Assign dependent variable name to models names
  names(models_tech_covid_definitive)<-rep(dep_var_labels, length(models_tech_covid_definitive)/2)
  
  # Add fixed-effects indicators to tables
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_tech_covid_definitive)), 
                             "Product FE", rep("Yes", length(models_tech_covid_definitive)),
                             "Month FE", rep("Yes", length(models_tech_covid_definitive))), 
                           nrow = 3, byrow=T))
  
  
  if(dep_var_export == "new_destination" & dep_var_import == "new_source"){
    list_notes<-"A new source/new destination is defined with respect to baseline year 2017. The regressions for new source/destination are estimated using a subset of firms that had transactions in 2017 as well."
  } else{
    list_notes<-""
  }
  
  format_se_coef <- function(x) format(round(x, 6), nsmall = 2, scientific = FALSE)
  
  # Table summary models
  table<-modelsummary(models_tech_covid_definitive,
                      coef_rename = coef_labels, 
                      gof_map = gm, 
                      stars = c('*' = .1, '**' = .05, '***'= 0.01), 
                      add_rows = FE, 
                      output = "latex", 
                      align = paste(c("l", rep("c", length(models_tech_covid_definitive))), sep="", collapse=""), 
                      fmt = format_se_coef,  
                      title = paste0(country_name," - Regression Results for ", dep_var_labels[1], " and ", dep_var_labels[2], ": e-Bay tradable, China e-commerce, Consumable and Durable products")) |>
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_tech_covid_definitive))) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) %>% 
    footnote(paste0("Clustered-standard errors at the firm-product level. The variable E-payment or E-commerce 2019 is a dummy equal to 1 if the company adopted the E-payment or E-commerce technology before 2019 and 0 if not. The variable Months Since Adoption is equal to the number of months since the firm adopted the E-payment or E-commerce technology. This is applicable only for firms that adopted the technology before 2019. For firms that did not adopt the technology prior to 2019, the 'Months Since Adoption' variable is set to 0. ",
                    list_notes), 
             threeparttable = TRUE) 
  
  return(list(models_tech_covid_definitive, table))
  
}




#This function will create a regression of the form $y_{ipt} = \alpha_0 + \alpha_1 indep_var1 \cdot covid + \alpha_1 indep_var1 \cdot covid \cdot product_p + same last two terms for all indep_vars  + covid \cdot product_p $
reg_models_pre_2019_adopters<-function(import_data, export_data, country_name, 
                                       product_vars, coef_labels, dep_var_import, 
                                       dep_var_export, dep_var_import_label, 
                                       dep_var_export_label, 
                                       covid_var, indep_vars){
  
    
  # The following list  will contain all formulas of regressions.
  all_formulas <- vector("list", length(product_vars)*2)  # Create an empty list with length of number of models to be estimated
  
  # Create formulas of regressions
  model_count = 0
  for(p in product_vars){
    for(y in c(dep_var_import, dep_var_export)){
      formula <- paste0(y, " ~ ")
      model_count = model_count + 1
      for(x in indep_vars){
        formula<-paste0(formula ,x, ":", covid_var, " + ", x, ":", covid_var, ":", p, " + ")
      }
      formula<-paste0(formula, p, ":", covid_var)
      formula<-paste0(formula, " |company_id + hs6 + date_character")
      all_formulas[[model_count]]<-formula
    }
  }
    
    
  # Function created to run feols 
  fe_reg<-function(formula){
    if(startsWith(formula, dep_var_import)){ # If imports dep var use imports dataset
      feols(as.formula(formula), cluster = c("company_id", "hs6"), data = import_data)
    } else if(startsWith(formula, dep_var_export)){ # If exports dep var use exports dataset 
      feols(as.formula(formula), cluster = c("company_id", "hs6"), data = export_data)
    }
  }
    
  # Apply fe_reg function to all formulas in the list all_formulas 
  models<-lapply(all_formulas, fe_reg)
  
  # Vector with labels of different dependent variables
  dep_var_labels<-c(dep_var_import_label, dep_var_export_label)
    
  # Assign dependent variable names to model models 
  names(models)<-rep(dep_var_labels, length(product_vars))
    
  # Add fixed-effects indicators to tables
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models)), 
                             "Product FE", rep("Yes", length(models)),
                             "Month FE", rep("Yes", length(models))), 
                           nrow = 3, byrow=T))
    
    
  if(dep_var_export == "new_destination" & dep_var_import == "new_source"){
    list_notes<-"A new source/new destination is defined with respect to baseline year 2017. The regressions for new source/destination are estimated using a subset of firms that had transactions in 2017 as well."
  } else{
    list_notes<-""
  }
    
  format_se_coef <- function(x) format(round(x, 6), nsmall = 2, scientific = FALSE)
    
    
  # Table summary models
  table<-modelsummary(models,
                      coef_map = coef_labels, 
                      gof_map = gm, 
                      stars = c('*' = .1, '**' = .05, '***'= 0.01), 
                      add_rows = FE, 
                      output = "latex", 
                      align = paste(c("l", rep("c", length(models))), sep="", collapse=""), 
                      fmt = format_se_coef,  
                      title = paste0(country_name," - Regression Results for ", dep_var_labels[1], " and ", dep_var_labels[2], ": e-Bay tradable, China e-commerce, Consumable and Durable products")) |>
    add_header_above(c(" " = 1, "Dependent Variables" = length(models))) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) %>% 
    footnote(paste0("Clustered-standard errors at the firm-product level.",
                    list_notes), 
             threeparttable = TRUE) 
    
    
  return(list(models, table))
    
}







# BELOW YOU CAN FIND FUNCTIONS THAT WE ARE NOT LONGER USING

# reg_models_tech_covid_definitive<-function(import_data, export_data, country_name, coef_labels, 
#                                            dep_var_import, dep_var_export,
#                                            dep_var_labels){
#   
#   
#   paybustorecom_import_preresearch_covid_1 <- feols(as.formula(paste0(dep_var_import,  "~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:Ebay_tradable +
#                                                       month_mean_stringency_index:Ebay_tradable|
#                                                       company_id + hs6 + date_character")), 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = import_data )
#   
#   paybustorecom_import_preresearch_covid_2 <- feols(as.formula(paste0(dep_var_import,  "~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce +
#                                                       month_mean_stringency_index:China_E_commerce|
#                                                       company_id + hs6 + date_character")), 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = import_data )
#   
#   paybustorecom_import_preresearch_covid_3 <- feols(as.formula(paste0(dep_var_import,  "~
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_BEC +
#                                                       month_mean_stringency_index:cons_BEC|
#                                                       company_id + hs6 + date_character")), 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = import_data )
#   
#   
#   
#   paybustorecom_import_preresearch_covid_4 <- feols(as.formula(paste0(dep_var_import,  " ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_BEC +
#                                                       month_mean_stringency_index:durable_BEC|
#                                                       company_id + hs6 + date_character")), 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = import_data )
#   
#   
#   
#   paybustorecom_export_preresearch_covid_1 <- feols(as.formula(paste0(dep_var_export,  "~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:Ebay_tradable +
#                                                       month_mean_stringency_index:Ebay_tradable|
#                                                       company_id + hs6 + date_character")), 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = export_data )
#   
#   paybustorecom_export_preresearch_covid_2 <- feols(as.formula(paste0(dep_var_export,  " ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce +
#                                                       month_mean_stringency_index:China_E_commerce|
#                                                       company_id + hs6 + date_character")), 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = export_data )
#   
#   paybustorecom_export_preresearch_covid_3 <- feols(as.formula(paste0(dep_var_export,  " ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_BEC +
#                                                       month_mean_stringency_index:cons_BEC|
#                                                       company_id + hs6 + date_character")), 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = export_data )
#   
#   
#   paybustorecom_export_preresearch_covid_4 <- feols(as.formula(paste0(dep_var_export,  " ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_BEC +
#                                                       month_mean_stringency_index:durable_BEC|
#                                                       company_id + hs6 + date_character")), 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = export_data)
#   
#   #How Existing Tech Insulates from COVID impacts e-commerce
#   models_tech_covid_definitive<-list(paybustorecom_import_preresearch_covid_1, paybustorecom_export_preresearch_covid_1,
#                                      paybustorecom_import_preresearch_covid_2, paybustorecom_export_preresearch_covid_2,
#                                      paybustorecom_import_preresearch_covid_3, paybustorecom_export_preresearch_covid_3,
#                                      paybustorecom_import_preresearch_covid_4, paybustorecom_export_preresearch_covid_4)
#   
#   
#   
#   # Assign dependent variable name to models names
#   names(models_tech_covid_definitive)<-rep(dep_var_labels, length(models_tech_covid_definitive)/2)
#   
#   # Add fixed-effects indicators to tables
#   FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_tech_covid_definitive)), 
#                              "Product FE", rep("Yes", length(models_tech_covid_definitive)),
#                              "Month FE", rep("Yes", length(models_tech_covid_definitive))), 
#                            nrow = 3, byrow=T))
#   
#   
#   if(dep_var_export == "new_destination" & dep_var_import == "new_source"){
#     list_notes<-list("Clustered-standard errors at the firm-product level", 
#                      "The variable E-payment or E-commerce 2019 means that the company adopted the E-payment or E-commerce technology before 2019.",
#                      "A new source/new destination is defined with respect to baseline year 2017", 
#                      "The regressions for new source/destination are estimated using a subset of firms that had transactions in 2017 as well.")
#     
#     format_se_coef <- function(x) format(round(x, 6), nsmall = 2, scientific = FALSE)
#     
#   } else{
#     list_notes<-list("Clustered-standard errors at the firm-product level", 
#                      "The variable E-payment or E-commerce 2019 means that the company adopted the E-payment or E-commerce technology before 2019.")
#     
#     format_se_coef <- f1
#     
#   }
#   
#   
#   # Table summary models
#   table<-modelsummary(models_tech_covid_definitive,
#                       coef_rename = coef_labels, 
#                       gof_map = gm, 
#                       stars = c('*' = .1, '**' = .05, '***'= 0.01), 
#                       add_rows = FE, 
#                       output = "latex", 
#                       align = paste(c("l", rep("c", length(models_tech_covid_definitive))), sep="", collapse=""), 
#                       notes = list_notes,
#                       fmt = format_se_coef,  
#                       title = paste0(country_name," - Regression Results for ", dep_var_labels[1], " and ", dep_var_labels[2], ": e-Bay tradable, China e-commerce, Consumable and Durable products")) |>
#     add_header_above(c(" " = 1, "Dependent Variables" = length(models_tech_covid_definitive))) %>% 
#     kable_styling(latex_options = c("HOLD_position", "scale_down")) 
#   
#   
#   
#   return(list(models_tech_covid_definitive, table))
#   
# }



# reg_models_tech_covid_definitive<-function(import_data, export_data, country_name, coef_labels){
#   
#   
#   # Dependent variable: Log imports
#   paybustorecom_import_preresearch_covid_1 <- feols(log_import ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:Ebay_tradable +
#                                                       month_mean_stringency_index:Ebay_tradable|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = import_data )
#   
#   paybustorecom_import_preresearch_covid_2 <- feols(log_import ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce +
#                                                       month_mean_stringency_index:China_E_commerce|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = import_data )
#   
#   paybustorecom_import_preresearch_covid_3 <- feols(log_import ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_BEC +
#                                                       month_mean_stringency_index:cons_BEC|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = import_data )
#   
#   
#   
#   paybustorecom_import_preresearch_covid_4 <- feols(log_import ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_BEC +
#                                                       month_mean_stringency_index:durable_BEC|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = import_data )
#   
#   
#   
#   # Dependent variable: Log exports
#   paybustorecom_export_preresearch_covid_1 <- feols(log_export ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:Ebay_tradable +
#                                                       month_mean_stringency_index:Ebay_tradable|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = export_data )
#   
#   paybustorecom_export_preresearch_covid_2 <- feols(log_export ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce +
#                                                       month_mean_stringency_index:China_E_commerce|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = export_data )
#   
#   paybustorecom_export_preresearch_covid_3 <- feols(log_export ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_BEC +
#                                                       month_mean_stringency_index:cons_BEC|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = export_data )
#   
#   
#   paybustorecom_export_preresearch_covid_4 <- feols(log_export ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_BEC +
#                                                       month_mean_stringency_index:durable_BEC|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = export_data)
#   
#   #How Existing Tech Insulates from COVID impacts e-commerce
#   models_tech_covid_definitive<-list(paybustorecom_import_preresearch_covid_1, paybustorecom_export_preresearch_covid_1,
#                                      paybustorecom_import_preresearch_covid_2, paybustorecom_export_preresearch_covid_2,
#                                      paybustorecom_import_preresearch_covid_3, paybustorecom_export_preresearch_covid_3,
#                                      paybustorecom_import_preresearch_covid_4, paybustorecom_export_preresearch_covid_4)
#   
#   
#   
#   # Assign dependent variable name to models names
#   names(models_tech_covid_definitive)<-rep(c("Log.Imports", "Log.Exports"), length(models_tech_covid_definitive)/2)
#   
#   # Add fixed-effects indicators to tables
#   FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_tech_covid_definitive)), 
#                              "Product FE", rep("Yes", length(models_tech_covid_definitive)),
#                              "Month FE", rep("Yes", length(models_tech_covid_definitive))), 
#                            nrow = 3, byrow=T))
#   
#   
#   # Table summary models
#   table<-modelsummary(models_tech_covid_definitive,
#                       coef_rename = coef_labels, 
#                       gof_map = gm, 
#                       stars = c('*' = .1, '**' = .05, '***'= 0.01), 
#                       add_rows = FE, 
#                       output = "latex", 
#                       align = paste(c("l", rep("c", length(models_tech_covid_definitive))), sep="", collapse=""), 
#                       notes = list("The variable E-payment or E-commerce 2019 means that the company adopted the E-payment or E-commerce technology before 2019.", "Clustered-standard errors at the firm-product level."),
#                       fmt = f1, 
#                       title = paste0(country_name,' - Regression Results for Log. Exports and Log.Imports: e-Bay tradable, China e-commerce, Consumable and Durable products')) |>
#     add_header_above(c(" " = 1, "Dependent Variables" = length(models_tech_covid_definitive))) %>% 
#     kable_styling(latex_options = c("HOLD_position", "scale_down")) 
#   
#   
#   
#   return(list(models_tech_covid_definitive, table))
#   
# }
# 


# 
# # E-bay Tradable and China e-commerce -----
# reg_models_tech_covid_ebay_china<-function(import_data, export_data, country_name, coef_labels){
#   
#   
#   # Dependent variable: Log imports
#   paybustorecom_import_preresearch_covid_1 <- feols(log_import ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:Ebay_tradable +
#                                                       month_mean_stringency_index:Ebay_tradable|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = import_data )
#   
#   paybustorecom_import_preresearch_covid_2 <- feols(log_import ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce +
#                                                       month_mean_stringency_index:China_E_commerce|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = import_data )
#   
#   paybustorecom_import_preresearch_covid_3 <- feols(log_import ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce_updated +
#                                                       month_mean_stringency_index:China_E_commerce_updated|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = import_data )
#   
#   
#   
#   paybustorecom_import_preresearch_covid_4 <- feols(log_import ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:diff_new_old_China_ecommerce_list +
#                                                       month_mean_stringency_index:diff_new_old_China_ecommerce_list|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = import_data )
#   
#   
#   
#   # Dependent variable: Log exports
#   paybustorecom_export_preresearch_covid_1 <- feols(log_export ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:Ebay_tradable +
#                                                       month_mean_stringency_index:Ebay_tradable|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = export_data )
#   
#   paybustorecom_export_preresearch_covid_2 <- feols(log_export ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce +
#                                                       month_mean_stringency_index:China_E_commerce|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = export_data )
#   
#   paybustorecom_export_preresearch_covid_3 <- feols(log_export ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce_updated +
#                                                       month_mean_stringency_index:China_E_commerce_updated|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = export_data )
#   
#   
#   paybustorecom_export_preresearch_covid_4 <- feols(log_export ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:diff_new_old_China_ecommerce_list +
#                                                       month_mean_stringency_index:diff_new_old_China_ecommerce_list|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = export_data)
#   
#   #How Existing Tech Insulates from COVID impacts e-commerce
#   models_tech_covid_ebay_china<-list(paybustorecom_import_preresearch_covid_1, paybustorecom_export_preresearch_covid_1,
#                                      paybustorecom_import_preresearch_covid_2, paybustorecom_export_preresearch_covid_2,
#                                      paybustorecom_import_preresearch_covid_3, paybustorecom_export_preresearch_covid_3,
#                                      paybustorecom_import_preresearch_covid_4, paybustorecom_export_preresearch_covid_4)
#   
#   
#   
#   # Assign dependent variable name to models names
#   names(models_tech_covid_ebay_china)<-rep(c("Log.Imports", "Log.Exports"), length(models_tech_covid_ebay_china)/2)
#   
#   # Add fixed-effects indicators to tables
#   FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_tech_covid_ebay_china)), 
#                              "Product FE", rep("Yes", length(models_tech_covid_ebay_china)),
#                              "Month FE", rep("Yes", length(models_tech_covid_ebay_china))), 
#                            nrow = 3, byrow=T))
#   
#   
#   # Table summary models
#   table<-modelsummary(models_tech_covid_ebay_china,
#                coef_rename = coef_labels, 
#                gof_map = gm, 
#                stars = c('*' = .1, '**' = .05, '***'= 0.01), 
#                add_rows = FE, 
#                output = "latex", 
#                align = paste(c("l", rep("c", length(models_tech_covid_ebay_china))), sep="", collapse=""), 
#                notes = list("The variable E-payment or E-commerce 2019 means that the company adopted the E-payment or E-commerce technology before 2019.", "Clustered-standard errors at the firm-product level."),
#                fmt = f1, 
#                title = paste0(country_name,' - Regression Results for Log. Exports and Log.Imports: e-Bay tradable and China e-commerce products')) |>
#     add_header_above(c(" " = 1, "Dependent Variables" = length(models_tech_covid_ebay_china))) %>% 
#     kable_styling(latex_options = c("HOLD_position", "scale_down")) 
#   
#   
#   
#   return(list(models_tech_covid_ebay_china, table))
#   
# }
# 
# 
# # BEC Products ------
# reg_models_tech_covid_BEC<-function(import_data, export_data, country_name, coef_labels){
#   
#   
#   
#   # Dependent variable: Log imports
#   paybustorecom_import_preresearch_covid_5 <- feols(log_import ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:parts_BEC +
#                                                       month_mean_stringency_index:parts_BEC|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = import_data )
#   
#   paybustorecom_import_preresearch_covid_6 <- feols(log_import ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_dur_BEC +
#                                                       month_mean_stringency_index:cons_dur_BEC|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = import_data )
#   
#   paybustorecom_import_preresearch_covid_7 <- feols(log_import ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_semi_BEC +
#                                                       month_mean_stringency_index:cons_semi_BEC|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = import_data )
#   
#   paybustorecom_import_preresearch_covid_8 <- feols(log_import ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_BEC +
#                                                       month_mean_stringency_index:cons_BEC|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = import_data )
#   
#   
#   paybustorecom_import_preresearch_covid_9 <- feols(log_import ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:transp_BEC +
#                                                       month_mean_stringency_index:transp_BEC|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = import_data )
#   
#   paybustorecom_import_preresearch_covid_10 <- feols(log_import ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_BEC +
#                                                        month_mean_stringency_index:durable_BEC|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = import_data )
#   
#   paybustorecom_import_preresearch_covid_11 <- feols(log_import ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_semi_BEC +
#                                                        month_mean_stringency_index:durable_semi_BEC|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = import_data )
#   
#   # Dependent variable: Log exports 
#   paybustorecom_export_preresearch_covid_5 <- feols(log_export ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:parts_BEC +
#                                                       month_mean_stringency_index:parts_BEC|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = export_data )
#   
#   paybustorecom_export_preresearch_covid_6 <- feols(log_export ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_dur_BEC +
#                                                       month_mean_stringency_index:cons_dur_BEC|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = export_data )
#   
#   paybustorecom_export_preresearch_covid_7 <- feols(log_export ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_semi_BEC +
#                                                       month_mean_stringency_index:cons_semi_BEC|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = export_data )
#   
#   paybustorecom_export_preresearch_covid_8 <- feols(log_export ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_BEC +
#                                                       month_mean_stringency_index:cons_BEC|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = export_data )
#   
#   
#   paybustorecom_export_preresearch_covid_9 <- feols(log_export ~ 
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                       adopted_pay_or_ecom_before_2019:month_mean_stringency_index:transp_BEC +
#                                                       month_mean_stringency_index:transp_BEC|
#                                                       company_id + hs6 + date_character, 
#                                                     cluster = c("company_id", "hs6"),
#                                                     data = export_data )
#   
#   paybustorecom_export_preresearch_covid_10 <- feols(log_export ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_BEC +
#                                                        month_mean_stringency_index:durable_BEC|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = export_data )
#   
#   paybustorecom_export_preresearch_covid_11 <- feols(log_export ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:durable_semi_BEC +
#                                                        month_mean_stringency_index:durable_semi_BEC|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = export_data)
#   
# 
#   
#   
#   models_tech_covid_BEC<-list(paybustorecom_import_preresearch_covid_5, paybustorecom_export_preresearch_covid_5,
#                               paybustorecom_import_preresearch_covid_6, paybustorecom_export_preresearch_covid_6,
#                               paybustorecom_import_preresearch_covid_7, paybustorecom_export_preresearch_covid_7,
#                               paybustorecom_import_preresearch_covid_8, paybustorecom_export_preresearch_covid_8,
#                               paybustorecom_import_preresearch_covid_9, paybustorecom_export_preresearch_covid_9,
#                               paybustorecom_import_preresearch_covid_10, paybustorecom_export_preresearch_covid_10,
#                               paybustorecom_import_preresearch_covid_11, paybustorecom_export_preresearch_covid_11)
#   
#   
#   # Assign dependent variable name to models names
#   names(models_tech_covid_BEC)<-rep(c("Log.Imports", "Log.Exports"), length(models_tech_covid_BEC)/2)
#   
#   # Add fixed-effects indicators to tables
#   FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_tech_covid_BEC)), 
#                              "Product FE", rep("Yes", length(models_tech_covid_BEC)),
#                              "Month FE", rep("Yes", length(models_tech_covid_BEC))), 
#                            nrow = 3, byrow=T))
#   
#   
#   # Table summary models
#   table <- modelsummary(models_tech_covid_BEC,
#                  coef_rename = coef_labels, 
#                  gof_map = gm, 
#                  stars = c('*' = .1, '**' = .05, '***'= 0.01), 
#                  add_rows = FE, 
#                  output = "latex", 
#                  align = paste(c("l", rep("c", length(models_tech_covid_BEC))), sep="", collapse=""), 
#                  notes = list("The variable E-payment or E-commerce 2019 means that the company adopted the E-payment or E-commerce technology before 2019.", "Clustered-standard errors at the firm-product level."), 
#                  fmt = f1, 
#                  title = paste0(country_name, ' - Regression Results for Log. Exports and Log.Imports: BEC products classification')) |>
#     add_header_above(c(" " = 1, "Dependent Variables" = length(models_tech_covid_BEC))) %>% 
#     kable_styling(latex_options = c("HOLD_position", "scale_down")) 
#   
#   
#   return(list(models_tech_covid_BEC, table))
#   
#   
# }
# 
# 
# 
# 
# # Regressions tech mitigation of COVID on time-sensitive products ----
# 
# reg_models_covid_time_sensitivity<-function(import_data, export_data,  country_name, coef_labels){
#   
#   
#   # Dependent variable: Log Imports
#   paybustorecom_import_preresearch_covid_12 <- feols(log_import ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:component +
#                                                        month_mean_stringency_index:component|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = import_data )
#   
#   paybustorecom_import_preresearch_covid_13 <- feols(log_import ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:fresh +
#                                                        month_mean_stringency_index:fresh|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = import_data )
#   
#   
#   paybustorecom_import_preresearch_covid_14 <- feols(log_import ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:frozen +
#                                                        month_mean_stringency_index:frozen|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = import_data )
#   
#   paybustorecom_import_preresearch_covid_15 <- feols(log_import ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:hummels_timesensitive +
#                                                        month_mean_stringency_index:hummels_timesensitive|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = import_data )
#   
#   
#   paybustorecom_import_preresearch_covid_16 <- feols(log_import ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:agric_timesensitive +
#                                                        month_mean_stringency_index:agric_timesensitive|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = import_data )
#   
#   
#   paybustorecom_import_preresearch_covid_17 <- feols(log_import ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:time_sens_hs2013_russ_typeofgoods +
#                                                        month_mean_stringency_index:time_sens_hs2013_russ_typeofgoods|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = import_data )
#   
#   
#   
#   paybustorecom_export_preresearch_covid_12 <- feols(log_export ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:component +
#                                                        month_mean_stringency_index:component|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = export_data )
#   
#   paybustorecom_export_preresearch_covid_13 <- feols(log_export ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:fresh +
#                                                        month_mean_stringency_index:fresh|
#                                                        company_id + hs6 + date_character,
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = export_data )
#   
#   
#   paybustorecom_export_preresearch_covid_14 <- feols(log_export ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:frozen +
#                                                        month_mean_stringency_index:frozen|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = export_data )
#   
#   paybustorecom_export_preresearch_covid_15 <- feols(log_export ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:hummels_timesensitive +
#                                                        month_mean_stringency_index:hummels_timesensitive|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = export_data )
#   
#   
#   paybustorecom_export_preresearch_covid_16 <- feols(log_export ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:agric_timesensitive +
#                                                        month_mean_stringency_index:agric_timesensitive|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = export_data )
#   
#   
#   paybustorecom_export_preresearch_covid_17 <- feols(log_export ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:time_sens_hs2013_russ_typeofgoods +
#                                                        month_mean_stringency_index:time_sens_hs2013_russ_typeofgoods|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = export_data )
#   
#   
#   models_covid_time_sensitivity<-list(paybustorecom_import_preresearch_covid_12, paybustorecom_export_preresearch_covid_12,
#                                         paybustorecom_import_preresearch_covid_13, paybustorecom_export_preresearch_covid_13,
#                                         paybustorecom_import_preresearch_covid_14, paybustorecom_export_preresearch_covid_14,
#                                         paybustorecom_import_preresearch_covid_15, paybustorecom_export_preresearch_covid_15,
#                                         paybustorecom_import_preresearch_covid_16, paybustorecom_export_preresearch_covid_16,
#                                         paybustorecom_import_preresearch_covid_17, paybustorecom_export_preresearch_covid_17)
#   
#   
#   
#   # Assign dependent variable name to models names
#   names(models_covid_time_sensitivity)<-rep(c("Log.Imports", "Log.Exports"), length(models_covid_time_sensitivity)/2)
#   
#   # Add fixed-effects indicators to tables
#   FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_covid_time_sensitivity)), 
#                              "Product FE", rep("Yes", length(models_covid_time_sensitivity)),
#                              "Month FE", rep("Yes", length(models_covid_time_sensitivity))), 
#                            nrow = 3, byrow=T))
#   
#   
#   
#   # Table summary models
#   table<-modelsummary(models_covid_time_sensitivity,
#                coef_rename = coef_labels, 
#                gof_map = gm, 
#                stars = c('*' = .1, '**' = .05, '***'= 0.01), 
#                add_rows = FE, 
#                output = "latex", 
#                align = paste(c("l", rep("c", length(models_covid_time_sensitivity))), sep="", collapse=""), 
#                notes = list("The variable E-payment or E-commerce 2019 means that the company adopted the E-payment or E-commerce technology before 2019.", "Clustered-standard errors at the firm-product level."), 
#                fmt = f1, 
#                title = paste0(country_name, " - Regression Results for Log. Imports and Log.Exports: Time-sensitive Products")) |>
#     add_header_above(c(" " = 1, "Dependent Variables" = length(models_covid_time_sensitivity))) %>% 
#     kable_styling(latex_options = c("HOLD_position", "scale_down")) 
#   
#   
#   return(list(models_covid_time_sensitivity, table))
#   
#   
# }
# 
# 
# #  Regressions tech mitigation of COVID on COVID impacted products ----
# 
# reg_model_covid_prods_impacted<-function(import_data, export_data,  country_name, coef_labels){
#   
#   
#   paybustorecom_import_preresearch_covid_18 <- feols(log_import ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:letter_credit_use +
#                                                        month_mean_stringency_index:letter_credit_use|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = import_data )
#   paybustorecom_import_preresearch_covid_19 <- feols(log_import ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:mean_remote_work_ISIC +
#                                                        month_mean_stringency_index:mean_remote_work_ISIC|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = import_data )
#   
#   paybustorecom_import_preresearch_covid_20 <- feols(log_import ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:relationship_stickiness +
#                                                        month_mean_stringency_index:relationship_stickiness|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = import_data )
#   paybustorecom_import_preresearch_covid_21 <- feols(log_import ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:frac_lib_diff +
#                                                        month_mean_stringency_index:frac_lib_diff|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = import_data )
#   
#   
#   paybustorecom_export_preresearch_covid_18 <- feols(log_export ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:letter_credit_use +
#                                                        month_mean_stringency_index:letter_credit_use|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = export_data )
#   paybustorecom_export_preresearch_covid_19 <- feols(log_export ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:mean_remote_work_ISIC +
#                                                        month_mean_stringency_index:mean_remote_work_ISIC|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = export_data )
#   
#   paybustorecom_export_preresearch_covid_20 <- feols(log_export ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:relationship_stickiness +
#                                                        month_mean_stringency_index:relationship_stickiness|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = export_data )
#   paybustorecom_export_preresearch_covid_21 <- feols(log_export ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:frac_lib_diff +
#                                                        month_mean_stringency_index:frac_lib_diff|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = export_data )
#   
#   model_covid_prods_impacted<- list(paybustorecom_import_preresearch_covid_18, paybustorecom_export_preresearch_covid_18,
#                                     paybustorecom_import_preresearch_covid_19, paybustorecom_export_preresearch_covid_19,
#                                     paybustorecom_import_preresearch_covid_20, paybustorecom_export_preresearch_covid_20,
#                                     paybustorecom_import_preresearch_covid_21, paybustorecom_export_preresearch_covid_21)
#   
#   
#   
#   
#   # Assign dependent variable name to models names
#   names(model_covid_prods_impacted)<-rep(c("Log.Imports", "Log.Exports"), length(model_covid_prods_impacted)/2)
#   
#   # Add fixed-effects indicators to tables
#   FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(model_covid_prods_impacted)), 
#                              "Product FE", rep("Yes", length(model_covid_prods_impacted)),
#                              "Month FE", rep("Yes", length(model_covid_prods_impacted))), 
#                            nrow = 3, byrow=T))
#   
#   
#   
#   # Table summary models
#   table<-modelsummary(model_covid_prods_impacted,
#                coef_rename = coef_labels, 
#                gof_map = gm, 
#                stars = c('*' = .1, '**' = .05, '***'= 0.01), 
#                add_rows = FE, 
#                output = "latex", 
#                align = paste(c("l", rep("c", length(model_covid_prods_impacted))), sep="", collapse=""), 
#                notes = list("The variable E-payment or E-commerce 2019 means that the company adopted the E-payment or E-commerce technology before 2019.", "Clustered-standard errors at the firm-product level."), 
#                fmt = f1, 
#                title =  paste0(country_name, " - Regression Results for Log. Imports Log. Exports: COVID impacted Products")) |>
#     add_header_above(c(" " = 1, "Dependent Variables" = length(model_covid_prods_impacted))) %>% 
#     kable_styling(latex_options = c("HOLD_position", "scale_down")) 
#   
#   
#   
#   return(list(model_covid_prods_impacted, table))
#   
#   
# }
# 
# 
# #  Regressions tech mitigation of COVID on capital - intermediate products ----
# 
# reg_model_covid_cap_int<-function(import_data, export_data,  country_name, coef_labels){
#   
#   
#   paybustorecom_import_preresearch_covid_22 <- feols(log_import ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:CAP +
#                                                        month_mean_stringency_index:CAP|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = import_data )
#   paybustorecom_import_preresearch_covid_23 <- feols(log_import ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:INT +
#                                                        month_mean_stringency_index:INT|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = import_data )
#   
#   
#   paybustorecom_export_preresearch_covid_22 <- feols(log_export ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:CAP +
#                                                        month_mean_stringency_index:CAP|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = export_data )
#   paybustorecom_export_preresearch_covid_23 <- feols(log_export ~ 
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index +
#                                                        adopted_pay_or_ecom_before_2019:month_mean_stringency_index:INT +
#                                                        month_mean_stringency_index:INT|
#                                                        company_id + hs6 + date_character, 
#                                                      cluster = c("company_id", "hs6"),
#                                                      data = export_data )
#   
#   model_covid_prods_cap_int<- list(paybustorecom_import_preresearch_covid_22, paybustorecom_export_preresearch_covid_22,
#                                     paybustorecom_import_preresearch_covid_23, paybustorecom_export_preresearch_covid_23)
#   
#   
#   
#   
#   # Assign dependent variable name to models names
#   names(model_covid_prods_cap_int)<-rep(c("Log.Imports", "Log.Exports"), length(model_covid_prods_cap_int)/2)
#   
#   # Add fixed-effects indicators to tables
#   FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(model_covid_prods_cap_int)), 
#                              "Product FE", rep("Yes", length(model_covid_prods_cap_int)),
#                              "Month FE", rep("Yes", length(model_covid_prods_cap_int))), 
#                            nrow = 3, byrow=T))
#   
#   
#   
#   # Table summary models
#   table<-modelsummary(model_covid_prods_cap_int,
#                       coef_rename = coef_labels, 
#                       gof_map = gm, 
#                       stars = c('*' = .1, '**' = .05, '***'= 0.01), 
#                       add_rows = FE, 
#                       output = "latex", 
#                       align = paste(c("l", rep("c", length(model_covid_prods_cap_int))), sep="", collapse=""), 
#                       notes = list("The variable E-payment or E-commerce 2019 means that the company adopted the E-payment or E-commerce technology before 2019.", "Clustered-standard errors at the firm-product level."), 
#                       fmt = f1, 
#                       title =  paste0(country_name, " - Regression Results for Log. Imports Log. Exports: COVID impacted Products")) |>
#     add_header_above(c(" " = 1, "Dependent Variables" = length(model_covid_prods_cap_int))) %>% 
#     kable_styling(latex_options = c("HOLD_position", "scale_down")) 
#   
#   
#   
#   return(list(model_covid_prods_cap_int, table))
#   
# }
# 
# 


