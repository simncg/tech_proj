#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: E-commerce and Trade during Crisis Times: Firm-level             #
#          Evidence from India, Indonesia and Mexico                        #                  #
#                                                                           #
#                                                                           #
#                                                                           #
# This program generates Tables 3, 4, A.6 and A.7.                          #
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
source("../src/functions_existing_tech_reg.R")


# ---- INTENSIVE MARGIN REGRESSIONS ----

# Initialize vector to be filled with models with existing tech e-commerce users
exist_e_comm_mod<-list()

# Initialize vector to be filled with models with existing tech consumable users
exist_cons_mod<-list()


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
  
  
  ## Read imports dataset for intensive margin regressions 
  imports_data<-fread(paste0("../../Data/processed_data/", country, "/imports_tech_mitigation_model_", abbr, ".csv")) %>% 
    # Select relevant variables
    select(company_id, hs6, date, date_character, log_import, adopted_pay_or_ecom_before_2019, 
           month_mean_stringency_index, China_E_commerce, cons_BEC) %>% 
    # From Boolean to dummy (for regression purposes)
    mutate(China_E_commerce = China_E_commerce*1, 
           cons_BEC = cons_BEC*1)
  
  ## Read exports dataset for extensive margin regressions
  exports_data<-fread(paste0("../../Data/processed_data/", country, "/exports_tech_mitigation_model_", abbr, ".csv")) %>% 
    # Select relevant variables
    select(company_id, hs6, date, date_character, log_export, adopted_pay_or_ecom_before_2019, 
           month_mean_stringency_index, China_E_commerce, cons_BEC) %>% 
    # From boolean to dummy (for regression purposes)
    mutate(China_E_commerce = China_E_commerce*1, 
           cons_BEC = cons_BEC*1)
  
  ## Run existing tech regression
  for(product_var in c("China_E_commerce", "cons_BEC")){

    # Regressions for China E-commerce products
    reg_results<-exist_tech_reg(import_data = imports_data, export_data = exports_data, 
                               dep_var_import = "log_import", dep_var_export = "log_export",
                               covid_var = "month_mean_stringency_index", product_var = product_var,
                               tech_var = "adopted_pay_or_ecom_before_2019", company_var = "company_id", 
                               hs_var = "hs6", date_var = "date_character")
    
    # Add results to final lists
    if(product_var == "China_E_commerce"){
      # Add results to list of China-E-commerce models
      exist_e_comm_mod<-c(exist_e_comm_mod, reg_results)
    } else if(product_var == "cons_BEC"){
      # Add results to list of consumable models
      exist_cons_mod<-c(exist_cons_mod, reg_results)
    }
    
  }
}

# Remove datasets 
rm(exports_data, imports_data)



# ---- EXTENSIVE MARGIN REGRESSIONS ----



# Initialize vector to be filled with results of extensive margin regressions with existing tech e-commerce users
exist_e_comm_mod_ext<-list()

# Initialize vector to be filled with results of extensive margin regressions with existing tech consumable users
exist_cons_mod_ext<-list()


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
    vars <- c("China_E_commerce", "cons_BEC", "adopted_pay_or_ecom_before_2019")
    
    if(trade_var == "imports"){
      dep_var = "import_dummy"
    } else if(trade_var == "exports"){
      dep_var = "export_dummy"
    }
    
    
    data <-read_parquet(paste0("../../Data/processed_data/", country, "/", trade_var, "_grid_mitig_model_", abbr, ".parquet"), 
                               col_select = c("company_id", "hs6", "date_character", dep_var , "adopted_pay_or_ecom_before_2019", 
                                              "month_mean_stringency_index", "China_E_commerce", "cons_BEC"))
    
    data<-as.data.table(data)
    
    
    data[, (vars) := lapply(.SD, function(x) 1 * x), .SDcols = vars]
    
    
    
    ## Run extensive margin regression 
    for(product_var in c("China_E_commerce", "cons_BEC")){
      
      # Regressions for China E-commerce products
      reg_result<-exist_tech_reg_ext_marg(data = data, dep_var = dep_var,
                                          covid_var = "month_mean_stringency_index", 
                                          product_var = product_var,
                                          tech_var = "adopted_pay_or_ecom_before_2019", company_var = "company_id", 
                                          hs_var = "hs6", date_var = "date_character")
      
      # Add results to final lists
      if(product_var == "China_E_commerce"){
        # Add results to list of China-E-commerce models
        exist_e_comm_mod_ext<-c(exist_e_comm_mod_ext, reg_result)
        
      } else if(product_var == "cons_BEC"){
        # Add results to list of consumable models
        exist_cons_mod_ext<-c(exist_cons_mod_ext, reg_result)
      }
      
    }
    
    # Remove data to free memory
    rm(data)
    gc()
    
  }
    
}



# Tables  ------

# Add fixed-effects indicators to tables
FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(exist_e_comm_mod)), 
                           "Product FE", rep("Yes", length(exist_e_comm_mod)),
                           "Month FE", rep("Yes", length(exist_e_comm_mod))), 
                         nrow = 3, byrow=T))


# Labels for variables and interaction of variables in tables 
coef_labels<-c("adopted_pay_or_ecom_before_2019:month_mean_stringency_index"="E-payment or E-commerce 2019 × Monthly Avg. Stringency Index",
               "month_mean_stringency_index:China_E_commerce" = "Monthly Avg. Stringency Index × China e-commerce",
               "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:China_E_commerce" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × China e-commerce",
               "month_mean_stringency_index:cons_BEC" = "Monthly Avg. Stringency Index × Consumable",
               "adopted_pay_or_ecom_before_2019:month_mean_stringency_index:cons_BEC" = "E-payment or E-commerce 2019 × Monthly Avg. Stringency Index × Consumable")

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
names(exist_e_comm_mod)<-rep(c("Log. Imports", "Log. Exports"), length(exist_e_comm_mod)/2)


# Produce table 
table_china_e_com_int<-
  modelsummary(exist_e_comm_mod,
             coef_rename = coef_labels, 
             gof_map = gm, 
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***'= 0.01), 
             add_rows = FE, 
             align = paste(c("l", rep("c", length(exist_e_comm_mod))), sep="", collapse=""), 
             fmt = format_se_coef, 
             notes = NULL, 
             output = "latex", 
             title = ""
) %>% 
  add_header_above(c(" " = 1, "India" = 2, "Mexico" = 2, "Indonesia" = 2)) %>% 
  kable_styling(latex_options = c("scale_down", "HOLD_position")) %>% 
  footnote("", threeparttable = TRUE)



capture.output(table_china_e_com_int, file = "../../Outputs/Tables/existing_tech_china_e_commerce_intensive.tex")

## Table for Consumable products (Intensive Margin) ----

# Assign dependent variable name to models names
names(exist_cons_mod)<-rep(c("Log. Imports", "Log. Exports"), length(exist_cons_mod)/2)


# Produce table
table_consumable_int<-
  modelsummary(exist_cons_mod,
             coef_rename = coef_labels, 
             gof_map = gm, 
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***'= 0.01), 
             add_rows = FE, 
             align = paste(c("l", rep("c", length(exist_cons_mod))), sep="", collapse=""), 
             fmt = format_se_coef, 
             notes = NULL, 
             title = "",
             output = "latex"
             ) %>% 
  add_header_above(c(" " = 1, "India" = 2, "Mexico" = 2, "Indonesia" = 2)) %>% 
  kable_styling(latex_options = c("scale_down", "HOLD_position"))  %>% 
  footnote("", threeparttable = TRUE)

capture.output(table_consumable_int, file = "../../Outputs/Tables/existing_tech_consumable_intensive.tex")


## Table for China E-commerce (Extensive Margin) ----

format_se_coef <- function(x) format(round(x, 5), nsmall = 3, scientific = FALSE)


# Assign dependent variable name to models names
names(exist_e_comm_mod_ext)<-rep(c("Import Propensity", "Export Propensity"), length(exist_e_comm_mod_ext)/2)


# Produce table 
table_china_e_com_ext<-
  modelsummary(exist_e_comm_mod_ext,
             coef_rename = coef_labels, 
             gof_map = gm, 
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***'= 0.01), 
             add_rows = FE, 
             align = paste(c("l", rep("c", length(exist_e_comm_mod_ext))), sep="", collapse=""), 
             fmt = format_se_coef, 
             notes = NULL, 
             output = "latex"
) %>% 
  add_header_above(c(" " = 1, "India" = 2, "Mexico" = 2, "Indonesia" = 2)) %>% 
  kable_styling(latex_options = c("scale_down", "HOLD_position"))  %>% 
  footnote("Clustered-standard errors at the firm-product level. ", threeparttable = TRUE)


capture.output(table_china_e_com_ext, file = "../../Outputs/Tables/existing_tech_china_e_commerce_extensive.tex")



## Table for Consumable products (Extensive Margin) ----

# Assign dependent variable name to models names
names(exist_cons_mod_ext)<-rep(c("Import Propensity", "Export Propensity"), length(exist_cons_mod_ext)/2)


# Produce table 
table_consumable_ext<-
  modelsummary(exist_cons_mod_ext,
             coef_rename = coef_labels, 
             gof_map = gm, 
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***'= 0.01), 
             add_rows = FE, 
             align = paste(c("l", rep("c", length(exist_cons_mod_ext))), sep="", collapse=""), 
             fmt = format_se_coef, 
             notes = NULL, 
             output = "latex") %>% 
  add_header_above(c(" " = 1, "India" = 2, "Mexico" = 2, "Indonesia" = 2)) %>% 
  kable_styling(latex_options = c("scale_down")) %>% 
  footnote("Clustered-standard errors at the firm-product level. ", threeparttable = TRUE) 


capture.output(table_consumable_ext, file = "../../Outputs/Tables/existing_tech_consumable_extensive.tex")

