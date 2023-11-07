#===========================================================================#
# Date:    November 2023                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
#                                                                           #
# This program generates functions for running intensive and extensive margin#
# for firms that adopted the technology during the period of analysis and   #
# certain type of products.                                                 #
#                                                                           #                                                                                                                                                                                                                       #
#===========================================================================#


# Function for running regressions for sample with firms that adopted the technology during period of analysis
# for a specific type of product
tech_product_reg<-function(import_data, export_data, 
                   dep_var_import, dep_var_export, 
                   product_var,
                   tech_var, company_var, 
                   hs_var, date_var){
  
  # Regression for imports variables interacted with products 
  imp_product<-feols(as.formula(paste0(dep_var_import,  "~ ", 
                                       tech_var, " + ", 
                                       tech_var, ":", product_var,
                                       "|", company_var, " + ", hs_var, " + ", date_var)), 
                 cluster = c(company_var, hs_var),
                 data = import_data)
  
  # Regression for exports variables interacted with products 
  exp_product<-feols(as.formula(paste0(dep_var_export,  "~ ", 
                                       tech_var, " + ", 
                                       tech_var, ":", product_var,
                                       "|", company_var, " + ", hs_var, " + ", date_var)), 
                     cluster = c(company_var, hs_var),
                     data = export_data)
  
  
  models<-list(imp_product, exp_product)
  
  
  return(models)
  
}





# Function for running existing tech extensive margin regressions ----


# Function for running regressions for sample with firms that adopted the technology during period of analysis
# for a specific type of product
tech_product_reg_ext<-function(data,  dep_var,
                               product_var,
                               tech_var, company_var, 
                               hs_var, date_var){
  
  # Regression interacted with products 
  reg_product<-summary(feols(as.formula(paste0(dep_var,  "~ ", 
                                       tech_var, " + ", 
                                       tech_var, ":", product_var,
                                       "|", company_var, " + ", hs_var, " + ", date_var)), 
                     cluster = c(company_var, hs_var),
                     data = data), lean = T)
  
  model<-list(reg_product)
  
  
  return(model)
  
}



