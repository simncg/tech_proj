#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
#                                                                           #
# Analysis of Indian technology adopters: Regressions for the pre-Covid 
# period  
#                                                                           #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")
library(gridExtra)
library(grid)
library(kableExtra)

# Process imports and exports dataset -----

## Read imports dataset at the firm-month-product level (from the model that measures if tech adoption mitigated COVID impacts)
import_data<- fread("../../Data/India/processed_data/imports_tech_mitigation_model_IND.csv")

## Keep pre-covid period and obtain total import value by firm-product in the entire pre-covid period. 
import_data<-
  import_data %>% 
  filter(
    # Use only pre-covid period 
    date < as.Date("2020-01-01")
  ) %>% 
  # Group by firm-product
  group_by(
    company_id,
    hs6
  ) %>% 
  # Keep adopter types and sum total import value by firm-product in the pre-covid period
  summarize(
    covid_adopter_type = first(covid_adopter_type), # Keep covid adopter type variable
    old_adopter_type = first(old_adopter_type), # Keep old adopter type variable
    adopter_type = first(adopter_type), 
    import = sum(import)
  ) %>% 
  ungroup() %>% 
  # Take the log of import
  mutate(
    log_import = log(import)
    )



## Read exports dataset at the firm-month-product level (from the model that measures if tech adoption mitigated COVID impacts)
export_data<- fread("../../Data/India/processed_data/exports_tech_mitigation_model_IND.csv")


## Keep pre-covid period and obtain total import value by firm-product in the entire pre-covid period. 
export_data<-
  export_data %>% 
  filter(
    # Use only pre-covid period (July 2018 onwards)
    date < as.Date("2020-01-01")
  ) %>% 
  # Group by firm-product
  group_by(
    company_id,
    hs6
  ) %>% 
  # Keep adopter types and sum total import value by firm-product in the pre-covid period
  summarize(
    covid_adopter_type = first(covid_adopter_type), # Keep covid adopter type variable
    old_adopter_type = first(old_adopter_type), # Keep old adopter type variable
    adopter_type = first(adopter_type), 
    export = sum(export)
  ) %>% 
  ungroup() %>% 
  # Take the log of import
  mutate(log_export = log(export))




# Regressions -----


# Regressions comparing all types of adopters vs never adopters
import_reg_all_adopters <- feols(log_import ~ i(factor_var = adopter_type, "never_adopter") | hs6,
                              cluster = c("company_id"),
                              data = import_data )

export_reg_all_adopters <- feols(log_export ~ i(factor_var = adopter_type, "never_adopter") | hs6,
                              cluster = c("company_id"),
                              data = export_data )


# Regressions comparing covid adopters vs old adopters (baseline is old adopter)
import_reg_old_covid_adopters <- feols(log_import ~ i(factor_var = adopter_type, "old_adopter") | hs6,
                              cluster = c("company_id"),
                              data = import_data %>%  filter(adopter_type %in% c("old_adopter", "covid_adopter")))

export_reg_old_covid_adopters <- feols(log_export ~ i(factor_var = adopter_type, "old_adopter") | hs6,
                                       cluster = c("company_id"),
                                       data = export_data %>%  filter(adopter_type %in% c("old_adopter", "covid_adopter")))


# Regressions comparing covid late adopters vs covid early adopters (baseline is covid late adopters)
import_reg_covid_adopters <- feols(log_import ~ i(factor_var = covid_adopter_type, "covid_late_adopter") | hs6,
                                       cluster = c("company_id"),
                                       data = import_data %>%  filter(covid_adopter_type %in% c("covid_early_adopter", "covid_late_adopter")))

export_reg_covid_adopters <- feols(log_export ~ i(factor_var = covid_adopter_type, "covid_late_adopter") | hs6,
                                   cluster = c("company_id"),
                                   data = export_data %>%  filter(covid_adopter_type %in% c("covid_early_adopter", "covid_late_adopter")))


# Regressions comparing old adopters types (baseline is pre-2017 adopters)
import_reg_old_adopters <- feols(log_import ~ i(factor_var = old_adopter_type, "2016_or_pre_2016_adopter") | hs6,
                                   cluster = c("company_id"),
                                   data = import_data %>%  filter(old_adopter_type %in% c("2016_or_pre_2016_adopter", "2017_adopter", "2018_adopter", "2019_adopter")))

export_reg_old_adopters <- feols(log_export ~ i(factor_var = old_adopter_type, "2016_or_pre_2016_adopter") | hs6,
                                   cluster = c("company_id"),
                                   data = export_data %>%  filter(old_adopter_type %in%  c("2016_or_pre_2016_adopter", "2017_adopter", "2018_adopter", "2019_adopter")))


# Produce tables of results ----


# Format N, R2, Adj.R2 in tables
f1 <- function(x) format(round(x, 3), big.mark=",")
f2 <- function(x) format(round(x, 0), big.mark=",")

gm <- list(
  list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = f2),
  #list("raw" = "r.squared", "clean" = "R-squared", "fmt" = f1),
  list("raw" = "adj.r.squared", "clean" = "Adj.R-squared", "fmt"=f1)
)

# Labels for coefficients
coef_labels<-c("adopter_type::covid_adopter" = "COVID Adopter",
               "adopter_type::old_adopter" = "Old Adopter", 
               "covid_adopter_type::covid_early_adopter" = "COVID Early Adopter", 
               "covid_adopter_type::covid_late_adopter" = "COVID Late Adopter", 
               "old_adopter_type::2017_adopter" = "2017-Adopter", 
               "old_adopter_type::2018_adopter" = "2018-Adopter", 
               "old_adopter_type::2019_adopter" = "2019-Adopter"
               )

# Add fixed-effects indicators to tables
FE<-as.data.frame(matrix(c("Product FE", rep("Yes", 2)),
                         nrow = 1, byrow=T))


## Table for regressions with all adopter types ----

# List of models with all adopters 
all_adopters_types_models<-list(import_reg_all_adopters, export_reg_all_adopters)

  
names(all_adopters_types_models)<-c("Log. Imports", "Log. Exports")


table_reg_all_adopters_IND<-
  modelsummary(all_adopters_types_models,
               coef_rename = coef_labels, 
               gof_map = gm, 
               stars = c('*' = .1, '**' = .05, '***'= 0.01), 
               add_rows = FE, 
               align = paste(c("l", rep("c", 2)), sep="", collapse=""), 
               title = "India - Regression Results for COVID, Old and Never Adopters - Pre-Covid Period.") %>% 
  add_header_above(c(" " = 1, "Dependent Variables" = 2)) %>% 
  kable_styling(latex_options = c("HOLD_position")) %>% 
  footnote("Baseline category is never adopter. The regression sample only includes the pre-Covid period. Clustered standard errors at the firm level", 
           threeparttable = TRUE) 
            
  

## Table for regressions with COVID vs old adopter types ----

# List of models with old vs covid adopters 
old_covid_adopters_types_models<-list(import_reg_old_covid_adopters, export_reg_old_covid_adopters)


names(old_covid_adopters_types_models)<-c("Log. Imports", "Log. Exports")


table_reg_old_covid_adopters_IND<-
  modelsummary(old_covid_adopters_types_models,
               coef_rename = coef_labels, 
               gof_map = gm, 
               stars = c('*' = .1, '**' = .05, '***'= 0.01), 
               add_rows = FE, 
               align = paste(c("l", rep("c", 2)), sep="", collapse=""), 
               title = "India - Regression Results for COVID and Old Adopters - Pre-Covid Period.") %>% 
  add_header_above(c(" " = 1, "Dependent Variables" = 2)) %>% 
  kable_styling(latex_options = c("HOLD_position")) %>% 
  footnote("Baseline category is old adopter. The regression sample only includes the pre-Covid period. Clustered standard errors at the firm level", 
           threeparttable = TRUE) 



## Table for regressions with COVID Adopters Types ----

# List of models with covid adopters 
covid_adopters_types_models<-list(import_reg_covid_adopters, export_reg_covid_adopters)


names(covid_adopters_types_models)<-c("Log. Imports", "Log. Exports")


table_reg_covid_adopters_IND<-
  modelsummary(covid_adopters_types_models,
               coef_rename = coef_labels, 
               gof_map = gm, 
               stars = c('*' = .1, '**' = .05, '***'= 0.01), 
               add_rows = FE, 
               align = paste(c("l", rep("c", 2)), sep="", collapse=""), 
               title = "India - Regression Results for COVID Adopters - Pre-Covid Period.") %>% 
  add_header_above(c(" " = 1, "Dependent Variables" = 2)) %>% 
  kable_styling(latex_options = c("HOLD_position")) %>% 
  footnote("Baseline category is COVID late adopter. The regression sample only includes the pre-Covid period. Clustered standard errors at the firm level", 
           threeparttable = TRUE) 





## Table for regressions with Old Adopters Types ----

# List of models with covid adopters 
old_adopters_types_models<-list(import_reg_old_adopters, export_reg_old_adopters)


names(old_adopters_types_models)<-c("Log. Imports", "Log. Exports")


table_reg_old_adopters_IND<-
  modelsummary(old_adopters_types_models,
               coef_rename = coef_labels, 
               gof_map = gm, 
               stars = c('*' = .1, '**' = .05, '***'= 0.01), 
               add_rows = FE, 
               align = paste(c("l", rep("c", 2)), sep="", collapse=""), 
               title = "India - Regression Results for Old Adopters - Pre-Covid Period.") %>% 
  add_header_above(c(" " = 1, "Dependent Variables" = 2)) %>% 
  kable_styling(latex_options = c("HOLD_position")) %>% 
  footnote("Baseline category is pre-2017 adopter. The regression sample only includes the pre-Covid period. Clustered standard errors at the firm level", 
           threeparttable = TRUE) 


