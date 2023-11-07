#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:               
#                                                                           #
#                                                                           #                                                                         
#                                                                           #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# 
# # Set Working Directory ----
# fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(fileloc)
# rm(fileloc)


# Libraries to be used ----
source("../src/packages.R")

# Read functions needed to run regressions
source("../src/functions_reg_tech_covid_mitigation.R")


# Read Indonesia data for tech mitigation of COVID (data at the firm-year-month-HS product level)----
import_tech_mitig<-fread("../../Data/Indonesia/processed_data/imports_tech_mitigation_model_IDN.csv")
export_tech_mitig<-fread("../../Data/Indonesia/processed_data/exports_tech_mitigation_model_IDN.csv")

import_tech_mitig<-as.data.frame(import_tech_mitig)
export_tech_mitig<-as.data.frame(export_tech_mitig)


# Convert from Boolean to dummy (to avoid _TRUE labels in table)
covid_tech_vars <- c("China_E_commerce", "Ebay_tradable", 
                     "cons_BEC", "durable_BEC")

import_tech_mitig[covid_tech_vars] <- lapply(import_tech_mitig[covid_tech_vars], function(x) 1 * x)
export_tech_mitig[covid_tech_vars] <- lapply(export_tech_mitig[covid_tech_vars], function(x) 1 * x)


# Generate an additional adopter type variable, which will be equal to non-pre-2020-adopter if the firm 
# is never adopter/2019-adopter/covid-adopter and if it is pre-2019 adopter the variable will be equal 
# to 2018-adopter, 2017-adopter or pre-2017 adopter depending on the year when the firm adopted the technology

import_tech_mitig<-import_tech_mitig %>% 
  mutate(
    pre_2020_adopter_type = 
      case_when(
        year(date_of_adoption) >= 2020 | is.na(date_of_adoption) ~ "non_pre_2020_adopter",
        year(date_of_adoption) < 2020  ~ old_adopter_type,
        TRUE ~ NA # Although this case never happens
      )
  )





export_tech_mitig<-export_tech_mitig %>% 
  mutate(
    pre_2020_adopter_type = 
      case_when(
        year(date_of_adoption) >= 2020 | is.na(date_of_adoption) ~ "non_pre_2020_adopter",
        year(date_of_adoption) < 2020  ~ old_adopter_type,
        TRUE ~ NA # Although this case never happens
      )
  )


# Create dummies for 2017-adopter, 2018-adopter, pre-2017 adopter, we use as baseline (ref) the non-pre-2020 adopter
export_tech_mitig<-data.frame(export_tech_mitig, i(export_tech_mitig$pre_2020_adopter_type, ref = "non_pre_2020_adopter")) %>% 
  rename(`pre_2018_adopter` = `X2017_or_pre_2017_adopter`, 
         `adopter_2018` = `X2018_adopter`, 
         `adopter_2019` = `X2019_adopter`
  )


import_tech_mitig<-data.frame(import_tech_mitig, i(import_tech_mitig$pre_2019_adopter_type, ref = "non_pre_2020_adopter")) %>% 
  rename(`pre_2018_adopter` = `X2017_or_pre_2017_adopter`, 
         `adopter_2018` = `X2018_adopter`, 
         `adopter_2019` = `X2019_adopter`
  )




# Labels to be displayed in table of results 
coef_labels<-c(
  "pre_2018_adopter:month_mean_stringency_index"="Pre-2018 Adopter × COVID stringency index",
  "month_mean_stringency_index:adopter_2018"="2018-Adopter × COVID stringency index",
  "month_mean_stringency_index:adopter_2019"="2019-Adopter × COVID stringency index",
  
  "month_mean_stringency_index:cons_BEC" = "COVID stringency index × Consumable",
  "pre_2018_adopter:month_mean_stringency_index:cons_BEC" = "Pre-2018 Adopter × COVID stringency index × Consumable",
  "month_mean_stringency_index:cons_BEC:adopter_2018" = "2018-Adopter × COVID stringency index × Consumable",
  "month_mean_stringency_index:cons_BEC:adopter_2019" = "2019-Adopter × COVID stringency index × Consumable",
  
  
  "month_mean_stringency_index:China_E_commerce" = "COVID stringency index × China e-commerce",
  "pre_2018_adopter:month_mean_stringency_index:China_E_commerce" = "Pre-2018 Adopter × COVID stringency index × China e-commerce",
  "month_mean_stringency_index:China_E_commerce:adopter_2018" = "2018-Adopter × COVID stringency index × China e-commerce",
  "month_mean_stringency_index:China_E_commerce:adopter_2019" = "2019-Adopter × COVID stringency index × China e-commerce",
  
  
  
  "month_mean_stringency_index:Ebay_tradable" = "COVID stringency index × eBay-Tradable",
  "pre_2018_adopter:month_mean_stringency_index:Ebay_tradable" = "Pre-2018 Adopter × COVID stringency index × eBay-Tradable",
  "month_mean_stringency_index:Ebay_tradable:adopter_2018" = "2018-Adopter × COVID stringency index × eBay-Tradable",
  "month_mean_stringency_index:Ebay_tradable:adopter_2019" = "2019-Adopter × COVID stringency index × eBay-Tradable",
  
  
  
  "month_mean_stringency_index:durable_BEC" = "COVID stringency index × Durable",
  "pre_2018_adopter:month_mean_stringency_index:durable_BEC" = "Pre-2018 Adopter × COVID stringency index × Durable", 
  "month_mean_stringency_index:durable_BEC:adopter_2018" = "2018-Adopter × COVID stringency index × Durable", 
  "month_mean_stringency_index:durable_BEC:adopter_2019" = "2019-Adopter × COVID stringency index × Durable"
  
  
)



# Run log.import and log. export regressions using as independent variables pre-2017 adopter, 2017-adopter, 2018-adopter, 2019-adopter
pre_2020_adopters_reg_imp_exp_IDN<- reg_models_pre_2020_adopters(import_data = import_tech_mitig, 
                                                                 export_data = export_tech_mitig, 
                                                                 country_name = "Indonesia", 
                                                                 product_vars = c("cons_BEC", 'China_E_commerce', "Ebay_tradable", "durable_BEC"), 
                                                                 coef_labels = coef_labels, 
                                                                 dep_var_import = "log_import", 
                                                                 dep_var_expor = "log_export", 
                                                                 dep_var_import_label = "Log. Import", 
                                                                 dep_var_export_label = "Log. Export", 
                                                                 covid_var = "month_mean_stringency_index", 
                                                                 indep_vars = c("pre_2018_adopter", "adopter_2018", "adopter_2019")
                                                                 )



# Run no. destinations/sources regressions using as independent variables pre-2017 adopter, 2017-adopter, 2018-adopter, 2019-adopter
pre_2020_adopters_reg_n_countries_IDN<- reg_models_pre_2020_adopters(import_data = import_tech_mitig, 
                                                                     export_data = export_tech_mitig, 
                                                                     country_name = "Indonesia", 
                                                                     product_vars = c("cons_BEC", 'China_E_commerce', "Ebay_tradable", "durable_BEC"), 
                                                                     coef_labels = coef_labels, 
                                                                     dep_var_import = "n_countries_import", 
                                                                     dep_var_expor = "n_countries_export", 
                                                                     dep_var_import_label = "No. Sources", 
                                                                     dep_var_export_label = "No. Destinations" , 
                                                                     covid_var = "month_mean_stringency_index", 
                                                                     indep_vars = c("pre_2018_adopter", "adopter_2018", "adopter_2019")
                                                                     )

rm(import_tech_mitig, export_tech_mitig)
gc()
