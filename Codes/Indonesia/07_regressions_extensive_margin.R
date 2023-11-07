#===========================================================================#
# Date:    March 2023                                                       #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# This program runs regressions and creates tables for the extensive margin # 
# analysis for Indoneia                                                     #
#                                                                           #
#                                                                           #
#                                                                           #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

options(modelsummary_format_numeric_latex = "plain")

# Libraries to be used ----
source("../src/packages.R")

# Function created to produce regressions and tables of results ----
source("../src/functions_extensive_margin.R")

# Data for model that measures if tech adoption affects trade outcomes ----- 
imports_ext_marg<-fread("../../Data/Indonesia/processed_data/imports_product_model_extmarg_IDN.csv")
exports_ext_marg<-fread("../../Data/Indonesia/processed_data/exports_product_model_extmarg_IDN.csv")

# Data for model that measures if tech adoption helps to mitigate covid impacts ----
imports_ext_marg_mitig<-fread("../../Data/Indonesia/processed_data/imports_mitig_model_extmarg_IDN.csv")
exports_ext_marg_mitig<-fread("../../Data/Indonesia/processed_data/exports_mitig_model_extmarg_IDN.csv")

# From logical to dummy
imports_ext_marg_mitig$adopted_pay_or_ecom_before_2020<-as.numeric(imports_ext_marg_mitig$adopted_pay_or_ecom_before_2020)
exports_ext_marg_mitig$adopted_pay_or_ecom_before_2020<-as.numeric(exports_ext_marg_mitig$adopted_pay_or_ecom_before_2020)


# Read data with information of variables to be displayed in tables of regression results
dictionary<-read_excel("../../Data/Extra Data/dictionary_for_tables.xlsx")

# Run regression for Model 1 (Does tech adoption affect trade outcomes?) and produce tables of results.
results_mod1<-list()

for(i in 1:nrow(dictionary)){
  print(i)
  
  if(i == 16){  # agric time sensitive cannot be estimated for Indonesia exports with PPML. After removing NAs (or perfect fit fixed-effects), not a single explanatory variable is different from 0.
    results_mod1[[i]]<-list()
  } 
  else{
    results_mod1[[i]]<-panel_table_mod1(
      imports_data = imports_ext_marg, 
      exports_data = exports_ext_marg, 
      product_var = dictionary$product_var[i], 
      tech_var = "pay_or_ecomnod", 
      name_tech_var = "E-payment or E-commerce", 
      table_notes = c(dictionary$table_notes[i]), 
      country_name = "Indonesia", 
      name_product = dictionary$name_product[i], 
      short_name_prod = dictionary$prod[i], 
      short_name_non_prod = dictionary$non_prod[i]
    )
  }
}


names(results_mod1)<-dictionary$product_var


saveRDS(results_mod1, "../../Outputs/Indonesia/regressions_results/products_model/extensive_margin_results_mod1_IDN.rds")

# Regressions with number of partner countries as outcome
results_n_countries_mod1<-panel_table_mod1_n_countries(imports_data = imports_ext_marg, 
                                                       exports_data = exports_ext_marg,
                                                       n_countries_var = "n_country_partners", 
                                                       tech_var = "pay_or_ecomnod", 
                                                       name_tech_var = "E-payment or E-commerce",
                                                       country_name = "Indonesia")


saveRDS(results_n_countries_mod1, "../../Outputs/Indonesia/regressions_results/products_model/extensive_margin_results_mod1_n_countries_IDN.rds")





# Run regressions for Model 2 (Does tech adoption help to mitigate COVID effects on trade outcomes?) and produce tables of results
list_tables_mod2<-list()

for(i in 1:nrow(dictionary)){
  print(i)
  if(i == 16){  # agric time sensitive cannot be estimated for Indonesia exports with PPML. After removing NAs (or perfect fit fixed-effects), not a single explanatory variable is different from 0.
    results_mod1[[i]]<-list()
  } 
  else{
    list_tables_mod2[[i]]<-table_mod2(
      imports_data = imports_ext_marg_mitig, 
      exports_data = exports_ext_marg_mitig, 
      product_var = dictionary$product_var[i], 
      tech_var = "adopted_pay_or_ecom_before_2020", 
      covid_var = "month_mean_stringency_index",
      table_notes = c(dictionary$table_notes[i]), 
      country_name = "Indonesia", 
      name_product = dictionary$name_product[i], 
      short_name_prod = dictionary$prod[i], 
      short_name_non_prod = dictionary$non_prod[i]
    )
  }
}


names(list_tables_mod2)<-dictionary$product_var

saveRDS(list_tables_mod2, "../../Outputs/Indonesia/regressions_results/tech_mitigation_covid_model/extensive_margin_results_mod2_IDN.rds")


# Regressions with number of partner countries as outcome
list_tables_mod2_n_countries<-table_mod2_n_countries(imports_data = imports_ext_marg_mitig, 
                                                     exports_data = exports_ext_marg_mitig,
                                                     n_countries_var = "n_country_partners",
                                                     tech_var = "adopted_pay_or_ecom_before_2020", 
                                                     covid_var = "month_mean_stringency_index",
                                                     country_name = "Indonesia")


saveRDS(list_tables_mod2_n_countries, "../../Outputs/Indonesia/regressions_results/tech_mitigation_covid_model/extensive_margin_results_mod2_n_countries_IDN.rds")


