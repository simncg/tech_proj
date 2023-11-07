#===========================================================================#
# Date:    November 2023                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
#                                                                           #
# This program generates the Intensive and Extensive Margin Regression      # 
# Results (Non-Existing Tech, i.e, adoption during period of analysis of    #
# technolgy) for the three countries and two types of products:             #
# China-E-commerce and Consumables                                          #
#                                                                           #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")

# Import function to run existing tech regression (intensive margin) for a specific type of product
source("../src/functions_period_tech_reg.R")


# ---- INTENSIVE MARGIN REGRESSIONS ----

# Initialize vector to be filled with models with e-commerce interacted with lag of tech
e_comm_mod<-list()

# Initialize vector to be filled with models with consumable interacted with lag of tech
cons_mod<-list()


# Iterate over countries
for(country in c("India", "Mexico", "Indonesia")){
  
  
  # Abbreviation
  if(country == "India"){
    abbr<-"IND"
  } else if(country == "Indonesia"){
    abbr<-"IDN"
  } else if(country == "Mexico"){
    abbr<-"MEX"
  }
  
  
  
  # Imports data
  imports_data<-fread(paste0("../../Data/processed_data/", country , "/imports_product_model_", abbr, ".csv")) %>% 
    select(company_id, hs6, date_character, log_import, pay_or_ecomnod_t_2, China_E_commerce, cons_BEC) %>% 
    # From Boolean to dummy (for regression purposes)
    mutate(China_E_commerce = China_E_commerce*1, 
           cons_BEC = cons_BEC*1)
    
  
  # Exports data 
  exports_data<-fread(paste0("../../Data/processed_data/", country, "/exports_product_model_", abbr, ".csv")) %>% 
    select(company_id, hs6, date_character, log_export, pay_or_ecomnod_t_2, China_E_commerce, cons_BEC) %>% 
    # From Boolean to dummy (for regression purposes)
    mutate(China_E_commerce = China_E_commerce*1, 
           cons_BEC = cons_BEC*1)
  
  
  
  ## Run existing tech regression
  for(product_var in c("China_E_commerce", "cons_BEC")){
    
    # Regressions for China E-commerce products
    reg_result<-tech_product_reg(import_data = imports_data, export_data = exports_data, 
                                 dep_var_import = "log_import", dep_var_export = "log_export",
                                 product_var = product_var, tech_var = "pay_or_ecomnod_t_2",
                                 company_var = "company_id", 
                                 hs_var = "hs6", date_var = "date_character")
    
    # Add results to final lists
    if(product_var == "China_E_commerce"){
      # Add results to list of China-E-commerce models
      e_comm_mod<-c(e_comm_mod, reg_result)
    } else if(product_var == "cons_BEC"){
      # Add results to list of consumable models
      cons_mod<-c(cons_mod, reg_result)
    }
    
  }
}

# Remove datasets 
rm(exports_data, imports_data)



# ---- EXTENSIVE MARGIN REGRESSIONS ----
# Better to run this part on the server

# Initialize vector to be filled with results of extensive margin regressions with existing tech e-commerce users
e_comm_mod_ext<-list()

# Initialize vector to be filled with results of extensive margin regressions with existing tech consumable users
cons_mod_ext<-list()


# Iterate over countries
for(country in c("India", "Mexico", "Indonesia")){
  
  # Abbreviation
  if(country == "India"){
    abbr<-"IND"
  } else if(country == "Indonesia"){
    abbr<-"IDN"
  } else if(country == "Mexico"){
    abbr<-"MEX"
  }
  
  # Iterate over imports and exports
  for(trade_var in c("imports", "exports")){
    # Product vars and tech var
    vars <- c("China_E_commerce", "cons_BEC", "pay_or_ecomnod_t_2")
    
    if(trade_var == "imports"){
      dep_var = "import_dummy"
    } else if(trade_var == "exports"){
      dep_var = "export_dummy"
    }
    
    
    data <-read_parquet(paste0("../../Data/processed_data/", country, "/", trade_var, "_grid_product_model_", abbr, ".parquet"), 
                        col_select = c("company_id", "hs6", "date_character", dep_var , "pay_or_ecomnod_t_2", 
                                       "China_E_commerce", "cons_BEC"))
    
    data<-as.data.table(data)
    
    
    data[, (vars) := lapply(.SD, function(x) 1 * x), .SDcols = vars]
    
    
    
    ## Run extensive margin regression 
    for(product_var in c("China_E_commerce", "cons_BEC")){
      
      # Regressions for China E-commerce products
      reg_result<-tech_product_reg_ext(data = data, dep_var = dep_var,
                                       product_var = product_var,
                                       tech_var = "pay_or_ecomnod_t_2",
                                       company_var = "company_id", 
                                       hs_var = "hs6", date_var = "date_character")
      
      # Add results to final lists
      if(product_var == "China_E_commerce"){
        # Add results to list of China-E-commerce models
        e_comm_mod_ext<-c(e_comm_mod_ext, reg_result)
        
      } else if(product_var == "cons_BEC"){
        # Add results to list of consumable models
        cons_mod_ext<-c(cons_mod_ext, reg_result)
      }
      
    }
    
    # Remove data to free memory
    rm(data)
    gc()
    
  }
  
}



# Tables  ------

# Add fixed-effects indicators to tables
FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(e_comm_mod)), 
                           "Product FE", rep("Yes", length(e_comm_mod)),
                           "Month FE", rep("Yes", length(e_comm_mod))), 
                         nrow = 3, byrow=T))


# Labels for variables and interaction of variables in tables 

# Assign variables names to display in tables -----
coef_labels<-c(pay_or_ecomnod_t_2 = "Firm technology adoption (t-2)",
               "pay_or_ecomnod_t_2:China_E_commerce"="Firm technology adoption (t-2) × China e-commerce",
               "pay_or_ecomnod_t_2:cons_BEC" = "Firm technology adoption (t-2) × Consumable"
               )



# Customize latex tables
options("modelsummary_format_numeric_latex" = "plain")

# Format N, R2, Adj.R2 in tables
f1 <- function(x) format(round(x, 3), big.mark=",")
f2 <- function(x) format(round(x, 0), big.mark=",")

gm <- list(
  list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = f2),
  list("raw" = "r.squared", "clean" = "R-squared", "fmt" = f1)
)


format_se_coef <- function(x) format(round(x, 3), nsmall = 3, scientific = FALSE)


## Table for China E-commerce (Intensive Margin) ----

# Assign dependent variable name to models names
names(e_comm_mod)<-rep(c("Log. Imports", "Log. Exports"), length(e_comm_mod)/2)


# Produce table 
table_china_e_com_int<-
  modelsummary(e_comm_mod,
               coef_rename = coef_labels, 
               gof_map = gm, 
               estimate = "{estimate}{stars}",
               stars = c('*' = .1, '**' = .05, '***'= 0.01), 
               add_rows = FE, 
               align = paste(c("l", rep("c", length(e_comm_mod))), sep="", collapse=""), 
               fmt = format_se_coef, 
               notes = NULL, 
               output = "latex", 
               title = "Intensive Margin - China E-commerce"
  ) %>% 
  add_header_above(c(" " = 1, "India" = 2, "Mexico" = 2, "Indonesia" = 2)) %>% 
  kable_styling(latex_options = c("scale_down", "HOLD_position")) %>% 
  footnote("", threeparttable = TRUE)



capture.output(table_china_e_com_int, file = "../../Outputs/Tables/tech_china_e_commerce_intensive.tex")

## Table for Consumable products (Intensive Margin) ----

# Assign dependent variable name to models names
names(cons_mod)<-rep(c("Log. Imports", "Log. Exports"), length(cons_mod)/2)


# Produce table
table_consumable_int<-
  modelsummary(cons_mod,
               coef_rename = coef_labels, 
               gof_map = gm, 
               estimate = "{estimate}{stars}",
               stars = c('*' = .1, '**' = .05, '***'= 0.01), 
               add_rows = FE, 
               align = paste(c("l", rep("c", length(cons_mod))), sep="", collapse=""), 
               fmt = format_se_coef, 
               notes = NULL, 
               title = "Intensive Margin - Consumable Products",
               output = "latex"
  ) %>% 
  add_header_above(c(" " = 1, "India" = 2, "Mexico" = 2, "Indonesia" = 2)) %>% 
  kable_styling(latex_options = c("scale_down", "HOLD_position"))  %>% 
  footnote("", threeparttable = TRUE)

capture.output(table_consumable_int, file = "../../Outputs/Tables/tech_consumable_intensive.tex")


## Table for China E-commerce (Extensive Margin) ----

format_se_coef <- function(x) format(round(x, 5), nsmall = 3, scientific = FALSE)


# Assign dependent variable name to models names
names(e_comm_mod_ext)<-rep(c("Import Propensity", "Export Propensity"), length(e_comm_mod_ext)/2)


# Produce table 
table_china_e_com_ext<-
  modelsummary(e_comm_mod_ext,
               coef_rename = coef_labels, 
               gof_map = gm, 
               estimate = "{estimate}{stars}",
               stars = c('*' = .1, '**' = .05, '***'= 0.01), 
               add_rows = FE, 
               align = paste(c("l", rep("c", length(e_comm_mod_ext))), sep="", collapse=""), 
               fmt = format_se_coef, 
               notes = NULL, 
               output = "latex", 
               title = "Extensive Margin - China E-commerce Products"
  ) %>% 
  add_header_above(c(" " = 1, "India" = 2, "Mexico" = 2, "Indonesia" = 2)) %>% 
  kable_styling(latex_options = c("scale_down", "HOLD_position"))  %>% 
  footnote("Clustered-standard errors at the firm-product level. ", threeparttable = TRUE)


capture.output(table_china_e_com_ext, file = "../../Outputs/Tables/tech_china_e_commerce_extensive.tex")



## Table for Consumable products (Extensive Margin) ----

# Assign dependent variable name to models names
names(cons_mod_ext)<-rep(c("Import Propensity", "Export Propensity"), length(cons_mod_ext)/2)


# Produce table 
table_consumable_ext<-
  modelsummary(cons_mod_ext,
               coef_rename = coef_labels, 
               gof_map = gm, 
               estimate = "{estimate}{stars}",
               stars = c('*' = .1, '**' = .05, '***'= 0.01), 
               add_rows = FE, 
               align = paste(c("l", rep("c", length(cons_mod_ext))), sep="", collapse=""), 
               fmt = format_se_coef, 
               notes = NULL, 
               title = "Extensive Margin - Consumable Products",
               output = "latex") %>% 
  add_header_above(c(" " = 1, "India" = 2, "Mexico" = 2, "Indonesia" = 2)) %>% 
  kable_styling(latex_options = c("scale_down", "HOLD_position")) %>% 
  footnote("Clustered-standard errors at the firm-product level. ", threeparttable = TRUE) 


capture.output(table_consumable_ext, file = "../../Outputs/Tables/tech_consumable_extensive.tex")

