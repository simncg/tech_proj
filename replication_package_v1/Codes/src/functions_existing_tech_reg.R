#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
#                                                                           #
# This program generates functions for running intesive and extensive margin#
# existing tech regressions for a specific type of product (Section 4.1).'  #
# For example, for China-E-commerce or Consumables.                         #
#                                                                           #                                                                                                                                                                                                                       #
#===========================================================================#


# Function for running existing tech intensive margin regressions for a specific type of product 
exist_tech_reg<-function(import_data, export_data, 
                         dep_var_import, dep_var_export, 
                         covid_var, product_var,
                         tech_var, company_var, 
                         hs_var, date_var){
  
  
  # Regression for imports variables
  imp_reg<-feols(as.formula(paste0(dep_var_import,  "~ ", 
                                   tech_var, ":", covid_var, " + ",
                                   tech_var, ":", covid_var, ":", product_var, " + ",
                                   covid_var, ":", product_var, "|",
                                   company_var, " + ", hs_var, " + ", date_var)), 
                 cluster = c(company_var, hs_var),
                 data = import_data)
  
  # Regression for exports variables
  exp_reg<-feols(as.formula(paste0(dep_var_export,  "~ ", 
                                   tech_var, ":", covid_var, " + ",
                                   tech_var, ":", covid_var, ":", product_var, " + ",
                                   covid_var, ":", product_var, "|",
                                   company_var, " + ", hs_var, " + ", date_var)), 
                 cluster = c(company_var, hs_var),
                 data = export_data)
  
  # List with models
  models<-list(imp_reg, exp_reg)
  

  return(models)
  
}





# Function for running existing tech extensive margin regressions for a specific type of product ----

# For extensive margin regressions, since the datasets are too large, we can just have once at a time in memory.
# Therefore, we define a function just to run the regressions for either the export or import dataset

exist_tech_reg_ext_marg<-function(data, dep_var, 
                         covid_var, product_var,
                         tech_var, company_var, 
                         hs_var, date_var){
  
  
  # Run regressions
  reg<-summary(feols(as.formula(paste0(dep_var,  "~ ", 
                               tech_var, ":", covid_var, " + ",
                               tech_var, ":", covid_var, ":", product_var, " + ",
                               covid_var, ":", product_var, "|",
                               company_var, " + ", hs_var, " + ", date_var)), 
             cluster = c(company_var, hs_var),
             data = data), lean = T)
  
  return(list(reg))
  
}




