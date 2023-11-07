#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
#                                                                           #
# This program generates functions for running regressions at the firm-month#
# level of the model that shows whether the existing technology use mitigate#
# covid  impacts                                                            #
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




# Regressions at the firm-month level for model that measures if tech adoption mitigated covid impacts -----
reg_firm_month_tech_covid_mitig<-function(import_data, export_data, country_name, coef_labels){
  
  
  log_import_reg <- feols(as.formula("log_import ~ adopted_pay_or_ecom_before_2020:month_mean_stringency_index | 
                                    company_id + date_character"),
                          cluster = c("company_id"),
                          data = import_data)
  
  
  log_export_reg <- feols(as.formula("log_export ~ adopted_pay_or_ecom_before_2020:month_mean_stringency_index | 
                                    company_id + date_character"), 
                          cluster = c("company_id"),
                          data = export_data)
  
  n_countries_imp_reg <- feols(as.formula("n_countries_import ~ adopted_pay_or_ecom_before_2020:month_mean_stringency_index | 
                                    company_id + date_character"),
                               cluster = c("company_id"),
                               data = import_data)
  
  
  n_countries_exp_reg <- feols(as.formula("n_countries_export ~ adopted_pay_or_ecom_before_2020:month_mean_stringency_index | 
                                    company_id + date_character"), 
                               cluster = c("company_id"),
                               data = export_data)
  
  # Since for Indonesia we cannot define a baseline year due to a short period we do not use 
  # new source/new destination in the regressions of this country 
  
  if(country_name!="Indonesia"){
    
    new_source_reg <- feols(as.formula("new_source ~ adopted_pay_or_ecom_before_2020:month_mean_stringency_index | 
                                    company_id + date_character"),
                            cluster = c("company_id"),
                            data = import_data)
    
    
    new_destination_reg <- feols(as.formula("new_destination ~ adopted_pay_or_ecom_before_2020:month_mean_stringency_index | 
                                    company_id + date_character"), 
                                 cluster = c("company_id"),
                                 data = export_data)
    
    
    
    # List with models
    models_tech_covid_definitive<-list(log_import_reg, log_export_reg,
                                        n_countries_imp_reg, n_countries_exp_reg, 
                                        new_source_reg, new_destination_reg)
                
    
    # Assign dependent variable name to models names
    names(models_tech_covid_definitive)<-c("Log. Import", "Log. Export", 
                                "No. Sources", "No. Destinations", 
                                "New Source", "New Destination")
    
    
    list_notes<-list("Firm technology adoption pre-2020 variable is a dummy indicating pre-2020 adoption of E-payment or E-commerce technology", 
                     "Clustered-standard errors at the firm level.", 
                     "A new source/new destination is defined with respect to baseline year 2017", 
                     "The regressions for new source/destination are estimated using a subset of firms that had transactions in 2017 as well.")
    
  } 
  else{
    
    models_tech_covid_definitive<-list(log_import_reg, log_export_reg,
                                       n_countries_imp_reg, n_countries_exp_reg)
    
    # Assign dependent variable name to models names
    names(models_tech_covid_definitive)<-c("Log. Import", "Log. Export", 
                                "No. Sources", "No. Destinations")
    
    
    list_notes<-list("Firm technology adoption pre-2020 variable is a dummy indicating pre-2020 adoption of E-payment or E-commerce technology", 
                     "Clustered-standard errors at the firm level.")
    
  }
  
  
  # Create Fixed Efects data frame to add them to tables
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models_tech_covid_definitive)), 
                             "Month FE", rep("Yes", length(models_tech_covid_definitive))), 
                           nrow = 2, byrow=T)) 

  
  # Table summary models
  table<-modelsummary(models_tech_covid_definitive,
                      coef_rename = coef_labels, 
                      gof_map = gm, 
                      stars = c('*' = .1, '**' = .05, '***'= 0.01), 
                      add_rows = FE, 
                      output = "latex", 
                      align = paste(c("l", rep("c", length(models_tech_covid_definitive))), sep="", collapse=""), 
                      notes = list_notes,
                      fmt = function(x) format(round(x, 6), nsmall = 2, scientific = FALSE),  
                      title = paste0(country_name," - Regression Results at the firm-month level")) |>
    add_header_above(c(" " = 1, "Dependent Variables" = length(models_tech_covid_definitive))) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  
  
  return(list(models_tech_covid_definitive, table))
  
}



