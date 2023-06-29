#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
#                                                                           #
# This program generates functions for running regressions regressions      #
# with different dummies referring to pre-2019 tech adopters                #
#                                                                           #                                                                         
#                                                                           #
#                                                                           #           -                                                                                                                                                  #
#===========================================================================#

source("../src/packages.R")
library(fastDummies)

#This function will create a regression of the form $y_{ipt} = \alpha_0 + \alpha_1 indep_var1 \cdot covid + \alpha_1 indep_var1 \cdot covid \cdot product_p + same last two terms for all indep_vars  + covid \cdot product_p $
reg_models_pre_2019_adopters<-function(import_data, export_data, country_name, 
                                       product_vars, dep_var_import, 
                                       dep_var_export, covid_var, indep_vars){
  
  
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
  
  return(models)
  
}





# Function for running extensive margin regressions using different type of pre-2019 adopters as independent variables (pre-2017 adopter, 2017-adopter, 2018-adopter)
ext_reg_models_pre_2019_adopters<-function(country_name, indep_vars, covid_var, product_vars){
  
  # Abbreviation
  if(country_name == "India"){
    abbr<-"IND"
  } else if(country_name == "Indonesia"){
    abbr<-"IDN"
  } else if(country_name == "Mexico"){
    abbr<-"MEX"
  }
  
  # The following list  will contain all formulas of regressions.
  all_formulas <- vector("list", length(product_vars)*2)  # Create an empty list with length of number of models to be estimated
  
  # Create formulas for regressions 
  model_count = 0
  for(y in c("import_dummy", "export_dummy")){
    for(p in product_vars){
      model_count = model_count + 1
      # Add dependent variable to the formula
      formula <- paste0(y, " ~ ")
      for(x in indep_vars){
        # Add independent variables interacted with covid var plus independent variables interacted with covid and product
        formula<-paste0(formula ,x, ":", covid_var, " + ", x, ":", covid_var, ":", p, " + ")
      }
      # Add to the formula product interacted with covid
      formula<-paste0(formula, p, ":", covid_var)
      # Add fixed effects
      formula<-paste0(formula, " |company_id + hs6 + date_character")
      # Add formula to list with all formulas
      all_formulas[[model_count]]<-formula
    }
  }
  
  
  # Read imports data for extensive margin 
  import_data <-read_parquet(paste0("../../Data/", country_name, "/processed_data/imports_grid_mitig_model_", abbr, ".parquet"), 
                             col_select = c("company_id", "hs6", "date_character", 
                                            "date_of_adoption", "old_adopter_type",
                                            "import_dummy", covid_var, product_vars))
  
  import_data<-as.data.table(import_data)
  
  gc()
  
  import_data[, (product_vars) := lapply(.SD, function(x) 1 * x), .SDcols = product_vars]
  
  
  # Create pre_2019_adopter_type variable
  import_data[ , pre_2019_adopter_type := 
                 fifelse(
                   year(date_of_adoption) >= 2019 | is.na(date_of_adoption), 
                   "non_pre_2019_adopter", 
                   old_adopter_type
                 )
  ]
  
  
  # Create dummies of categories within pre_2019_adopter type (2017-adopter, pre-2017 adopter, 2018 adopter)
  import_data<-dummy_cols(import_data, select_columns = "pre_2019_adopter_type")
  
  # Rename columns 
  if(country_name != "Indonesia"){
    setnames(import_data, 
             old = c("pre_2019_adopter_type_2016_or_pre_2016_adopter", 
                     "pre_2019_adopter_type_2017_adopter", 
                     "pre_2019_adopter_type_2018_adopter"), 
             new = c("pre_2017_adopter", "adopter_2017", "adopter_2018")
    )
    
  } else if(country_name == "Indonesia"){
    setnames(import_data, 
             old = c("pre_2019_adopter_type_2017_or_pre_2017_adopter", 
                     "pre_2019_adopter_type_2018_adopter"), 
             new = c("pre_2018_adopter", 
                     "adopter_2018")
    )
  }
  
  
  
  
  # Function to run feols and obtain summary of results
  fe_reg_import<-function(formula){
    summary(feols(as.formula(formula), cluster = c("company_id", "hs6"), data = import_data), lean = T)
  }
  
  
  models_imports<-lapply(all_formulas[1:length(product_vars)], fe_reg_import)
  
  
  rm(import_data)
  gc()
  
  
  
  # Read imports data for extensive margin 
  export_data <-read_parquet(paste0("../../Data/", country_name, "/processed_data/exports_grid_mitig_model_", abbr, ".parquet"), 
                             col_select = c("company_id", "hs6", "date_character", 
                                            "date_of_adoption", "old_adopter_type",
                                            "export_dummy", covid_var, product_vars))
  
  export_data<-as.data.table(export_data)
  
  gc()
  
  export_data[, (product_vars) := lapply(.SD, function(x) 1 * x), .SDcols = product_vars]
  
  
  # Create pre_2019_adopter_type variable
  export_data[ , pre_2019_adopter_type := 
                 fifelse(
                   year(date_of_adoption) >= 2019 | is.na(date_of_adoption), 
                   "non_pre_2019_adopter", 
                   old_adopter_type
                 )
  ]
  
  
  # Create dummies of categories within pre_2019_adopter type (2017-adopter, pre-2017 adopter, 2018 adopter)
  export_data<-dummy_cols(export_data, select_columns = "pre_2019_adopter_type")
  
  # Rename columns 
  if(country_name != "Indonesia"){
    setnames(export_data, 
             old = c("pre_2019_adopter_type_2016_or_pre_2016_adopter", 
                     "pre_2019_adopter_type_2017_adopter", 
                     "pre_2019_adopter_type_2018_adopter"), 
             new = c("pre_2017_adopter", "adopter_2017", "adopter_2018")
             ) 
  } else if(country_name == "Indonesia"){
    setnames(export_data, 
             old = c("pre_2019_adopter_type_2017_or_pre_2017_adopter", 
                     "pre_2019_adopter_type_2018_adopter"), 
             new = c("pre_2018_adopter", "adopter_2018")
    )
  }
  
  
  
  # Function to run feols and obtain summary of results
  fe_reg_export<-function(formula){
    summary(feols(as.formula(formula), cluster = c("company_id", "hs6"), data = export_data), lean = T)
  }
  
  # Run export propensity regressions
  models_exports<-lapply(all_formulas[(length(all_formulas) - length(product_vars) + 1):length(all_formulas)], fe_reg_export)
  
  
  rm(export_data)
  gc()
  
  
  
  # Create a new list where first model of imports is followed by first model of exports
  # second model of imports is followed by second model of exports, and so on so forth. 
  # This is just for organizing purposes of the tab;e. 
  models <- vector("list", length = length(product_vars) * 2)
  
  # Iterate over the elements of the lists and populate the new list
  for (i in 1:length(product_vars)) {
    models[(2 * i) - 1] <- models_imports[i]
    models[2 * i] <- models_exports[i]
  }
  
  return(models)
  
}


