#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  functions_extensive_margin.R                                     #
#                                                                           #
# This program generates functions to be used in the extensive margin       # 
# analysis                                                                  #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Functions to format tables ----

# Not \num{} in latex tables
options(modelsummary_format_numeric_latex = "plain")


# Extract squared correlation from PPML model to present in table with modelsummary function
modelsummary_ppml<-function(model){
  ti<-broom::tidy(model)
  gl<-broom::glance(model)["nobs"]
  #gl$sq.cor<-model$sq.cor
  gl$r.squared<-model$sq.cor
  
  model <- list(
    tidy = ti,
    glance = gl)
  class(model) <- "modelsummary_list"
  
  return(model)
}


# Format N, R2, Adj.R2 in tables
f1 <- function(x) format(round(x, 3), big.mark=",")
f2 <- function(x) format(round(x, 0), big.mark=",")

# Format SE and coefficients
format_se_coef <- function(x) format(round(x, 5), nsmall = 2, scientific = FALSE)

gm <- list(
  list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = f2),
  list("raw" = "r.squared", "clean" = "R-squared", "fmt" = f1)#,
  #list("raw" = "sq.cor", "clean" = "Squared Corr.", "fmt" = f1)
)

# Add fixed-effects indicators to tables -----
FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", 10), 
                           "Month FE", rep("Yes", 10)), 
                         nrow = 2, byrow=T)) 


FE_n_countries<-as.data.frame(matrix(c("Firm FE", rep("Yes", 4), 
                                       "Month FE", rep("Yes", 4)), 
                                     nrow = 2, byrow=T)) 



# Functions for running the Extensive margin regressions for model 1 (Does tech adoption affect trade outcomes)? ----

# Function for number of products and propensity (probability)
ext_marg_reg<-function(imports_data, exports_data, product_var, tech_var, lag){
  
  # OLS model Log(1 + #products) ---
  
  # Imports ===
  
  # Number of traded products of the category (e.g., #e-commerce products)
  imp_ols<-feols(as.formula(paste0("log(1+", product_var, "_TRUE) ~", tech_var, "_t_", lag ,"| company_id + date")), 
                 cluster = c("company_id"), 
                 data = imports_data)
  
  # Number of traded products not belonging to the category (e.g., #non-e-commerce products)
  imp_non_ols<-feols(as.formula(paste0("log(1+", product_var, "_FALSE) ~", tech_var, "_t_", lag ,"| company_id + date")), 
                     cluster = c("company_id"), 
                     data = imports_data)
  
  
  # Exports ===
  exp_ols<-feols(as.formula(paste0("log(1+", product_var, "_TRUE) ~", tech_var, "_t_", lag ,"| company_id+date")), 
                 cluster = c("company_id"), 
                 data = exports_data)
  
  exp_non_ols<-feols(as.formula(paste0("log(1+", product_var, "_FALSE) ~", tech_var, "_t_", lag ,"| company_id + date")), 
                     cluster = c("company_id"), 
                     data = exports_data)
  
  
  # PPML ---
  
  # Imports ===
  
  # Number of traded products of the category (e.g., #e-commerce products)
  imp_ppml<-feglm(as.formula(paste0(product_var, "_TRUE ~", tech_var, "_t_", lag,"| company_id + date")), 
                  cluster = c("company_id"), 
                  family = "quasipoisson",
                  data = imports_data)
  
  imp_ppml<-modelsummary_ppml(imp_ppml)
  
  # Number of traded products not belonging to the category (e.g., #non-e-commerce products)
  imp_non_ppml<-feglm(as.formula(paste0(product_var, "_FALSE ~", tech_var, "_t_", lag,"| company_id + date")), 
                      cluster = c("company_id"), 
                      family = "quasipoisson",
                      data = imports_data)
  
  imp_non_ppml<-modelsummary_ppml(imp_non_ppml)
  
  # Exports ===
  
  # Number of traded products of the category (e.g., #e-commerce products)
  exp_ppml<-feglm(as.formula(paste0(product_var, "_TRUE ~", tech_var, "_t_", lag, "| company_id + date")), 
                  cluster = c("company_id"), 
                  family = "quasipoisson",
                  data = exports_data)
  
  exp_ppml<-modelsummary_ppml(exp_ppml)
  
  # Number of traded products not belonging to the category (e.g., #non-e-commerce products)
  exp_non_ppml<-feglm(as.formula(paste0(product_var, "_FALSE ~", tech_var, "_t_", lag, "| company_id + date")), 
                      cluster = c("company_id"), 
                      family = "quasipoisson",
                      data = exports_data)
  
  exp_non_ppml<-modelsummary_ppml(exp_non_ppml)
  
  
  # Probability estimation ---
  
  # Imports ===
  imp_prob<-feols(as.formula(paste0("traded_", product_var, "~",  tech_var, "_t_", lag, "| company_id + date")), 
                  cluster = c("company_id"), 
                  data = imports_data)
  
  # Exports ===
  exp_prob<-feols(as.formula(paste0("traded_", product_var, "~", tech_var, "_t_", lag , "| company_id + date")), 
                  cluster = c("company_id"), 
                  data = exports_data)
  
  
  # List with results ---
  results<-list(imp_ols, imp_non_ols, exp_ols, exp_non_ols, 
                imp_ppml, imp_non_ppml, exp_ppml, exp_non_ppml, 
                imp_prob, exp_prob)
  
  
  return(results)
  
}

# n_country_partners
# Function for number of partner countries
ext_marg_reg_n_countries<-function(imports_data, exports_data, n_countries_var, tech_var, lag){
  
  # OLS model Log(1 + #countries) ---
  
  # Imports ===
  
  # Number of import partner countries 
  imp_n_countries_ols<-feols(as.formula(paste0("log(1+", n_countries_var, ") ~", tech_var, "_t_", lag ,"| company_id + date")), 
                             cluster = c("company_id"), 
                             data = imports_data)
  
  # Exports ===
  
  # Number of export partner countries
  exp_n_countries_ols<-feols(as.formula(paste0("log(1+", n_countries_var, ") ~", tech_var, "_t_", lag ,"| company_id+date")), 
                             cluster = c("company_id"), 
                             data = exports_data)
  
  # PPML ---
  
  # Imports ===
  
  # Number of import partner countries
  imp_n_countries_ppml<-feglm(as.formula(paste0(n_countries_var, "~", tech_var, "_t_", lag,"| company_id + date")), 
                  cluster = c("company_id"), 
                  family = "quasipoisson",
                  data = imports_data)
  
  imp_n_countries_ppml<-modelsummary_ppml(imp_n_countries_ppml)
  
  
  # Exports ===
  
  # Number of export partner countries
  exp_n_countries_ppml<-feglm(as.formula(paste0(n_countries_var, "~", tech_var, "_t_", lag, "| company_id + date")), 
                              cluster = c("company_id"), 
                              family = "quasipoisson",
                              data = exports_data)
  
  exp_n_countries_ppml<-modelsummary_ppml(exp_n_countries_ppml)
  
  
  # List with results ---
  results<-list(imp_n_countries_ols, exp_n_countries_ols, 
                imp_n_countries_ppml, exp_n_countries_ppml)
  
  
  return(results)
  
}





# Function for creating stylish tables with Panels (each panel will have a lag in the technology variable)
# This function will be used to generate tables for the model where we try to measure if tech adoption 
# affects trade outcomes (Model 1) ----
panel_table_mod1<-function(imports_data, exports_data, product_var, tech_var, 
                           name_tech_var, table_notes, country_name,
                           name_product, short_name_prod, short_name_non_prod){
  
  # Labels for tables
  coef_labels<-c("pay_or_ecomnod_t_1" = "E-payment or E-commerce (t-1)",
                 "pay_or_ecomnod_t_2" = "E-payment or E-commerce (t-2)",
                 "pay_or_ecomnod_t_3" = "E-payment or E-commerce (t-3)"
  )
  
  # Create list with panels (each panel corresponds to a lag in the tech var)
  panels<-list()
  for(lag in 1:3){
    # Run regressions for each panel
    panels[[lag]]<-ext_marg_reg(imports_data, exports_data, product_var, tech_var, lag)
    # Assign dependent variable names 
    names(panels[[lag]])<-c(rep(c(short_name_prod, short_name_non_prod), 4), "Import", "Export")
  }
  
  # Assign name to panels
  names(panels)<-c(paste("Panel A: 1-Lag in", name_tech_var, "variable"), 
                   paste("Panel B: 2-Lags in", name_tech_var, "variable"), 
                   paste("Panel C: 3-Lags in", name_tech_var, "variable")
  )
  
  # Create table with panels 
  table_latex<-modelsummary(
    panels,
    coef_rename = coef_labels, 
    shape = "rbind",
    gof_map = gm,
    stars = c('*' = .1, '**' = .05, '***'= 0.01),
    add_rows = FE, 
    output = "latex",
    align = paste(c("l", rep("c", 10)), sep="", collapse=""),
    notes = c(table_notes, 
              "Clustered-standard errors at the firm level. PPML refers to Poisson Pseudo Maximum Likelihood and LPM to Linear Probability Model estimated using OLS.", 
              "R-squared for PPML refers to the squared correlation coefficient between the dependent variable and the fitted values."), 
    title = paste(country_name,"- Extensive margin analysis for", name_product)
  ) %>%
    add_header_above(c(" " = 1, "Log(1 + No.Imp.Prod)" = 2, "Log(1 + No.Exp.Prod)" = 2, 
                       "No.Imp.Prod" = 2, "No.Exp.Prod" = 2, "Propensity" = 2)) %>% 
    add_header_above(c(" " = 1, "OLS" = 4, "PPML" = 4, "LPM" = 2)) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  table_rmd<-modelsummary(
    panels,
    coef_rename = coef_labels, 
    shape = "rbind",
    gof_map = gm,
    stars = c('*' = .1, '**' = .05, '***'= 0.01),
    add_rows = FE, 
    align = paste(c("l", rep("c", 10)), sep="", collapse=""),
    notes = c(table_notes, 
              "Clustered-standard errors at the firm level. PPML refers to Poisson Pseudo Maximum Likelihood and LPM to Linear Probability Model estimated using OLS.", 
              "R-squared for PPML refers to the squared correlation coefficient between the dependent variable and the fitted values."), 
    title = paste(country_name,"- Extensive margin analysis for", name_product)
  ) %>%
    add_header_above(c(" " = 1, "Log(1 + No.Imp.Prod)" = 2, "Log(1 + No.Exp.Prod)" = 2, 
                       "No.Imp.Prod" = 2, "No.Exp.Prod" = 2, "Propensity" = 2)) %>% 
    add_header_above(c(" " = 1, "OLS" = 4, "PPML" = 4, "LPM" = 2)) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  
  
  return(list(table_latex, table_rmd))
  
}


# Function for creating stylish tables with Panels (each panel will have a lag in the technology variable)
# This function will be used to generate tables for the model where we try to measure if tech adoption 
# affects trade outcomes (NUMBER OF PARTNER COUNTRIES) (Model 1) ----
panel_table_mod1_n_countries<-function(imports_data, exports_data, n_countries_var, tech_var, 
                                       name_tech_var, country_name){
  
  # Labels for tables
  coef_labels<-c("pay_or_ecomnod_t_1" = "E-payment or E-commerce (t-1)",
                 "pay_or_ecomnod_t_2" = "E-payment or E-commerce (t-2)",
                 "pay_or_ecomnod_t_3" = "E-payment or E-commerce (t-3)"
  )
  
  # Create list with panels (each panel corresponds to a lag in the tech var)
  panels<-list()
  for(lag in 1:3){
    # Run regressions for each panel
    panels[[lag]]<-ext_marg_reg_n_countries(imports_data, exports_data, n_countries_var, tech_var, lag)
    # Assign dependent variable names 
    names(panels[[lag]])<-rep(c("Imports", "Exports"), 2)
  }
  
  # Assign name to panels
  names(panels)<-c(paste("Panel A: 1-Lag in", name_tech_var, "variable"), 
                   paste("Panel B: 2-Lags in", name_tech_var, "variable"), 
                   paste("Panel C: 3-Lags in", name_tech_var, "variable")
  )
  
  # Create table with panels 
  table_latex<-modelsummary(
    panels,
    coef_rename = coef_labels, 
    shape = "rbind",
    gof_map = gm,
    stars = c('*' = .1, '**' = .05, '***'= 0.01),
    add_rows = FE_n_countries, 
    output = "latex",
    align = paste(c("l", rep("c", 4)), sep="", collapse=""),
    notes = c("Clustered-standard errors at the firm level. PPML refers to Poisson Pseudo Maximum Likelihood estimator.", 
              "R-squared for PPML refers to the squared correlation coefficient between the dependent variable and the fitted values."), 
    title = paste(country_name,"- Extensive margin analysis for number of partner countries")
  ) %>%
    add_header_above(c(" " = 1, "Log(1 + No. Partner Countries)" = 2, "No. Partner Countries" = 2)) %>% 
    add_header_above(c(" " = 1, "OLS" = 2, "PPML" = 2)) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  table_rmd<-modelsummary(
    panels,
    coef_rename = coef_labels, 
    shape = "rbind",
    gof_map = gm,
    stars = c('*' = .1, '**' = .05, '***'= 0.01),
    add_rows = FE_n_countries, 
    align = paste(c("l", rep("c", 4)), sep="", collapse=""),
    notes = c("Clustered-standard errors at the firm level. PPML refers to Poisson Pseudo Maximum Likelihood estimator.", 
              "R-squared for PPML refers to the squared correlation coefficient between the dependent variable and the fitted values."), 
    title = paste(country_name,"- Extensive margin analysis for number of partner countries")
  ) %>%
    add_header_above(c(" " = 1, "Log(1 + No. Partner Countries)" = 2, "No. Partner Countries" = 2)) %>% 
    add_header_above(c(" " = 1, "OLS" = 2, "PPML" = 2)) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  
  return(list(table_latex, table_rmd))
  
}



# Extensive margin regressions: Does Tech Adoption Help to Mitigate COVID Impacts? ----

# Function for number of products
ext_marg_reg_mitig<-function(imports_data, exports_data, product_var, tech_var, covid_var){
  
  # OLS model Log(1 + #products) ---
  
  # Imports ===
  
  # Number of traded products of the category (e.g., #e-commerce products)
  imp_ols<-feols(as.formula(paste0("log(1+", product_var, "_TRUE) ~", tech_var, ":", covid_var,"| company_id + date")), 
                 cluster = c("company_id"), 
                 data = imports_data)
  
  # Number of traded products not belonging to the category (e.g., #non-e-commerce products)
  imp_non_ols<-feols(as.formula(paste0("log(1+", product_var, "_FALSE) ~", tech_var, ":", covid_var,"| company_id + date")), 
                     cluster = c("company_id"), 
                     data = imports_data)
  
  
  # Exports =====
  exp_ols<-feols(as.formula(paste0("log(1+", product_var, "_TRUE) ~", tech_var, ":", covid_var ,"| company_id+date")), 
                 cluster = c("company_id"), 
                 data = exports_data)
  
  exp_non_ols<-feols(as.formula(paste0("log(1+", product_var, "_FALSE) ~", tech_var, ":", covid_var ,"| company_id + date")), 
                     cluster = c("company_id"), 
                     data = exports_data)
  
  
  # PPML ---
  
  # Imports ===
  
  # Number of traded products of the category (e.g., #e-commerce products)
  imp_ppml<-feglm(as.formula(paste0(product_var, "_TRUE ~", tech_var, ":", covid_var,"| company_id + date")), 
                  cluster = c("company_id"), 
                  family = "quasipoisson",
                  data = imports_data)
  
  imp_ppml<-modelsummary_ppml(imp_ppml)
  
  # Number of traded products not belonging to the category (e.g., #non-e-commerce products)
  imp_non_ppml<-feglm(as.formula(paste0(product_var, "_FALSE ~", tech_var, ":", covid_var, "| company_id + date")), 
                      cluster = c("company_id"), 
                      family = "quasipoisson",
                      data = imports_data)
  
  imp_non_ppml<-modelsummary_ppml(imp_non_ppml)
  
  # Exports ===
  
  # Number of traded products of the category (e.g., #e-commerce products)
  exp_ppml<-feglm(as.formula(paste0(product_var, "_TRUE ~", tech_var, ":", covid_var, "| company_id + date")), 
                  cluster = c("company_id"), 
                  family = "quasipoisson",
                  data = exports_data)
  
  exp_ppml<-modelsummary_ppml(exp_ppml)
  
  # Number of traded products not belonging to the category (e.g., #non-e-commerce products)
  exp_non_ppml<-feglm(as.formula(paste0(product_var, "_FALSE ~", tech_var, ":", covid_var, "| company_id + date")), 
                      cluster = c("company_id"), 
                      family = "quasipoisson",
                      data = exports_data)
  
  exp_non_ppml<-modelsummary_ppml(exp_non_ppml)
  
  
  # Probability estimation ---
  
  # Imports ===
  imp_prob<-feols(as.formula(paste0("traded_", product_var, "~",  tech_var, ":", covid_var, "| company_id + date")), 
                  cluster = c("company_id"), 
                  data = imports_data)
  
  # Exports ===
  exp_prob<-feols(as.formula(paste0("traded_", product_var, "~", tech_var, ":", covid_var, "| company_id + date")), 
                  cluster = c("company_id"), 
                  data = exports_data)
  
  
  # List with results ---
  results<-list(imp_ols, imp_non_ols, exp_ols, exp_non_ols, 
                imp_ppml, imp_non_ppml, exp_ppml, exp_non_ppml, 
                imp_prob, exp_prob)
  
  
  return(results)
  
}


# Function for number of partner countries
ext_marg_reg_mitig_n_countries<-function(imports_data, exports_data, n_countries_var, tech_var, covid_var){
  
  # OLS model Log(1 + #partner countries) ---
  
  # Imports ===
  
  # Number of imports partner countries
  imp_n_countries_ols<-feols(as.formula(paste0("log(1+", n_countries_var, ") ~", tech_var, ":", covid_var,"| company_id + date")), 
                 cluster = c("company_id"), 
                 data = imports_data)
  
  
  # Exports =====
  
  # Number of imports partner countries
  exp_n_countries_ols<-feols(as.formula(paste0("log(1+", n_countries_var, ") ~", tech_var, ":", covid_var ,"| company_id+date")), 
                 cluster = c("company_id"), 
                 data = exports_data)
  
  
  # PPML ---
  
  # Imports ===
  
  # Number of traded products of the category (e.g., #e-commerce products)
  imp_n_countries_ppml<-feglm(as.formula(paste0(n_countries_var, "~", tech_var, ":", covid_var,"| company_id + date")), 
                  cluster = c("company_id"), 
                  family = "quasipoisson",
                  data = imports_data)
  
  imp_n_countries_ppml<-modelsummary_ppml(imp_n_countries_ppml)
  
  # Exports ===
  
  # Number of traded products of the category (e.g., #e-commerce products)
  exp_n_countries_ppml<-feglm(as.formula(paste0(n_countries_var, "~", tech_var, ":", covid_var, "| company_id + date")), 
                              cluster = c("company_id"), 
                              family = "quasipoisson",
                              data = exports_data)
  
  exp_n_countries_ppml<-modelsummary_ppml(exp_n_countries_ppml)
  
  # List with results ---
  results<-list(imp_n_countries_ols, exp_n_countries_ols, 
                imp_n_countries_ppml, exp_n_countries_ppml)
  
  
  return(results)
  
}



# Function for creating stylish tables with Panels (each panel will have a lag in the technology variable)
# This function will be used to generate tables for the model where we try to measure if tech adoption 
# mitigates COVID impacts (Model 2) ----

table_mod2<-function(imports_data, exports_data, product_var, tech_var, 
                     covid_var, table_notes, country_name,
                     name_product, short_name_prod, short_name_non_prod){
  
  # Create list with models (run regressions)
  models<-ext_marg_reg_mitig(imports_data, exports_data, product_var, tech_var, covid_var)
  
  # Coefficient labels 
  coef_labels<-c("adopted_pay_or_ecom_before_2019:month_mean_stringency_index"="E-payment or E-commerce 2019 × Monthly Avg. Stringency Index")
  
  # Assign dependent variable names 
  names(models)<-c(rep(c(short_name_prod, short_name_non_prod), 4), "Import", "Export")
  
  # Create table with panels 
  table_latex<-modelsummary(
    models,
    coef_rename = coef_labels, 
    gof_map = gm,
    stars = c('*' = .1, '**' = .05, '***'= 0.01),
    add_rows = FE, 
    fmt = format_se_coef,
    output = "latex",
    align = paste(c("l", rep("c", 10)), sep="", collapse=""),
    notes = c(table_notes, 
              "The variable E-payment or E-commerce 2019 is a dummy equal to 1 if the company adopted the E-payment or E-commerce technology before 2019",
              "Clustered-standard errors at the firm level. PPML refers to Poisson Pseudo Maximum Likelihood and LPM to Linear Probability Model estimated using OLS.", 
              "R-squared for PPML refers to the squared correlation coefficient between the dependent variable and the fitted values."), 
    title = paste(country_name,"- Extensive margin analysis for", name_product)
  ) %>%
    add_header_above(c(" " = 1, "Log(1 + No.Imp.Prod)" = 2, "Log(1 + No.Exp.Prod)" = 2, 
                       "No.Imp.Prod" = 2, "No.Exp.Prod" = 2, "Propensity" = 2)) %>% 
    add_header_above(c(" " = 1, "OLS" = 4, "PPML" = 4, "LPM" = 2)) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  
  table_rmd<-modelsummary(
    models,
    coef_rename = coef_labels, 
    gof_map = gm,
    stars = c('*' = .1, '**' = .05, '***'= 0.01),
    add_rows = FE, 
    fmt = format_se_coef,
    align = paste(c("l", rep("c", 10)), sep="", collapse=""),
    notes = c(table_notes, 
              "The variable E-payment or E-commerce 2019 is a dummy equal to 1 if the company adopted the E-payment or E-commerce technology before 2019",
              "Clustered-standard errors at the firm level. PPML refers to Poisson Pseudo Maximum Likelihood and LPM to Linear Probability Model estimated using OLS.", 
              "R-squared for PPML refers to the squared correlation coefficient between the dependent variable and the fitted values."), 
    title = paste(country_name,"- Extensive margin analysis for", name_product)
  ) %>%
    add_header_above(c(" " = 1, "Log(1 + No.Imp.Prod)" = 2, "Log(1 + No.Exp.Prod)" = 2, 
                       "No.Imp.Prod" = 2, "No.Exp.Prod" = 2, "Propensity" = 2)) %>% 
    add_header_above(c(" " = 1, "OLS" = 4, "PPML" = 4, "LPM" = 2)) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  
  
  return(list(table_latex, table_rmd))
  
}




# Function for creating stylish tables with Panels (each panel will have a lag in the technology variable)
# This function will be used to generate tables for the model where we try to measure if tech adoption 
# mitigates COVID impacts (Model 2) ----

table_mod2_n_countries<-function(imports_data, exports_data,
                                 n_countries_var, tech_var, 
                                 covid_var, country_name){
  
  # Create list with models (run regressions)
  models<-ext_marg_reg_mitig_n_countries(imports_data, exports_data, n_countries_var, tech_var, covid_var)
  
  # Coefficient labels 
  coef_labels<-c("adopted_pay_or_ecom_before_2019:month_mean_stringency_index"="E-payment or E-commerce 2019 × Monthly Avg. Stringency Index")
  
  # Assign dependent variable names 
  names(models)<-rep(c("Imports", "Exports"), 2)
  
  # Create table with panels 
  table_latex<-modelsummary(
    models,
    coef_rename = coef_labels, 
    gof_map = gm,
    stars = c('*' = .1, '**' = .05, '***'= 0.01),
    add_rows = FE_n_countries, 
    fmt = format_se_coef,
    output = "latex",
    align = paste(c("l", rep("c", 4)), sep="", collapse=""),
    notes = c("The variable E-payment or E-commerce 2019 is a dummy equal to 1 if the company adopted the E-payment or E-commerce technology before 2019",
              "Clustered-standard errors at the firm level. PPML refers to Poisson Pseudo Maximum Likelihood estimator.", 
              "R-squared for PPML refers to the squared correlation coefficient between the dependent variable and the fitted values."), 
    title = paste(country_name,"- Extensive margin analysis for number of partner countries")
  ) %>%
    add_header_above(c(" " = 1, "Log(1 + No. Partner Countries)" = 2,  "No. Partner Countries" = 2)) %>% 
    add_header_above(c(" " = 1, "OLS" = 2, "PPML" = 2)) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  
  # Create table with panels 
  table_rmd<-modelsummary(
    models,
    coef_rename = coef_labels, 
    gof_map = gm,
    stars = c('*' = .1, '**' = .05, '***'= 0.01),
    add_rows = FE_n_countries, 
    fmt = format_se_coef,
    align = paste(c("l", rep("c", 4)), sep="", collapse=""),
    notes = c("The variable E-payment or E-commerce 2019 is a dummy equal to 1 if the company adopted the E-payment or E-commerce technology before 2019",
              "Clustered-standard errors at the firm level. PPML refers to Poisson Pseudo Maximum Likelihood estimator.", 
              "R-squared for PPML refers to the squared correlation coefficient between the dependent variable and the fitted values."), 
    title = paste(country_name,"- Extensive margin analysis for number of partner countries")
  ) %>%
    add_header_above(c(" " = 1, "Log(1 + No. Partner Countries)" = 2,  "No. Partner Countries" = 2)) %>% 
    add_header_above(c(" " = 1, "OLS" = 2, "PPML" = 2)) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) 
  
  return(list(table_latex, table_rmd))
  
}

