#===========================================================================#
# Date:    June 2023                                                        #
#                                                                           #
# Project: E-commerce and Trade during Crisis Times: Firm-level             #
#          Evidence from India, Indonesia and Mexico                        #
#                                                                           #
#                                                                           
# This program generates tables 7, 8 and 9.                                                                                            
#      
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")


# Initialize list to save results of COVID vs old adopters
models_old_covid_adopters<-list()

# Initialize list to save results of regressions comparing different types of COVID adopters
models_covid_adopters<-list()

# Initialize list to save results of regressions comparing differnt types of OLD adopters
models_old_adopters<-list()

for(country in c("India", "Mexico", "Indonesia")){
  
  # Abbreviation
  if(country == "India"){
    abbr<-"IND"
  } else if(country == "Indonesia"){
    abbr<-"IDN"
  } else if(country == "Mexico"){
    abbr<-"MEX"
  }
  
  # Process imports and exports dataset -----
  
  ## Read imports dataset at the firm-month-product level (from the model that measures if tech adoption mitigated COVID impacts)
  import_data<- fread(paste0("../../Data/processed_data/", country,"/imports_tech_mitigation_model_", abbr, ".csv"))
  
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
  export_data<- fread(paste0("../../Data/processed_data/", country, "/exports_tech_mitigation_model_", abbr, ".csv"))
  
  
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
  
  
  
  # Regressions comparing Covid adopters vs old adopters (baseline is old adopters) 
  
  import_reg_old_covid_adopters <- feols(log_import ~ i(factor_var = adopter_type, "old_adopter") | hs6,
                                         cluster = c("company_id"),
                                         data = import_data %>%  filter(adopter_type %in% c("old_adopter", "covid_adopter")))
  
  export_reg_old_covid_adopters <- feols(log_export ~ i(factor_var = adopter_type, "old_adopter") | hs6,
                                         cluster = c("company_id"),
                                         data = export_data %>%  filter(adopter_type %in% c("old_adopter", "covid_adopter")))
  
  # Save results in list
  models_old_covid_adopters<-c(models_old_covid_adopters, list(import_reg_old_covid_adopters, export_reg_old_covid_adopters))
  
  
  

  
  # Regressions comparing covid late adopters vs covid early adopters (baseline is covid late adopters)
  import_reg_covid_adopters <- feols(log_import ~ i(factor_var = covid_adopter_type, "covid_late_adopter") | hs6,
                                     cluster = c("company_id"),
                                     data = import_data %>%  filter(covid_adopter_type %in% c("covid_early_adopter", "covid_late_adopter")))
  
  export_reg_covid_adopters <- feols(log_export ~ i(factor_var = covid_adopter_type, "covid_late_adopter") | hs6,
                                     cluster = c("company_id"),
                                     data = export_data %>%  filter(covid_adopter_type %in% c("covid_early_adopter", "covid_late_adopter")))
  
  
  
  # Save results in list
  models_covid_adopters<-c(models_covid_adopters, list(import_reg_covid_adopters, export_reg_covid_adopters))
  
  
  # Regressions comparing old adopters types (baseline is pre-2017 adopters)
  
  if(country != "Indonesia"){
    
    import_reg_old_adopters <- feols(log_import ~ i(factor_var = old_adopter_type, "2016_or_pre_2016_adopter") | hs6,
                                     cluster = c("company_id"),
                                     data = import_data %>%  filter(old_adopter_type %in% c("2016_or_pre_2016_adopter", "2017_adopter", "2018_adopter", "2019_adopter")))
    
    export_reg_old_adopters <- feols(log_export ~ i(factor_var = old_adopter_type, "2016_or_pre_2016_adopter") | hs6,
                                     cluster = c("company_id"),
                                     data = export_data %>%  filter(old_adopter_type %in%  c("2016_or_pre_2016_adopter", "2017_adopter", "2018_adopter", "2019_adopter")))
    
  } else if(country == "Indonesia"){ # Since Builtwith for Indonesia is only available from 2017 onwards (we can't define 2016_or_pre_2016_adopter but 2017_or_pre_2017_adopter)
    
    # Regressions comparing old adopters types (baseline is pre-2017 adopters)
    import_reg_old_adopters <- feols(log_import ~ i(factor_var = old_adopter_type, "2017_or_pre_2017_adopter") | hs6,
                                     cluster = c("company_id"),
                                     data = import_data %>%  filter(old_adopter_type %in% c("2017_or_pre_2017_adopter", "2018_adopter", "2019_adopter")))
    
    export_reg_old_adopters <- feols(log_export ~ i(factor_var = old_adopter_type, "2017_or_pre_2017_adopter") | hs6,
                                     cluster = c("company_id"),
                                     data = export_data %>%  filter(old_adopter_type %in%  c("2017_or_pre_2017_adopter", "2018_adopter", "2019_adopter")))
    
  }
  
  models_old_adopters<-c(models_old_adopters, list(import_reg_old_adopters, export_reg_old_adopters))
  
  
  rm(import_data, export_data)
}


# Tables ----
options("modelsummary_format_numeric_latex" = "plain")

# Format N, R2, Adj.R2 in tables
f1 <- function(x) format(round(x, 3), big.mark=",")
f2 <- function(x) format(round(x, 0), big.mark=",")

gm <- list(
  list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = f2),
  list("raw" = "r.squared", "clean" = "R-squared", "fmt" = f1)
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
FE<-as.data.frame(matrix(c("Product FE", rep("Yes", 6)),
                         nrow = 1, byrow=T))


# Names variables 
names(models_old_covid_adopters)<-rep(c("Log. Imports", "Log. Exports"), 3)
names(models_covid_adopters)<-rep(c("Log. Imports", "Log. Exports"), 3)
names(models_old_adopters)<-rep(c("Log. Imports", "Log. Exports"), 3)



## Table for regressions with COVID vs old adopter types ----
table_reg_old_covid_adopters<-
  modelsummary(models_old_covid_adopters,
               coef_rename = coef_labels, 
               gof_map = gm, 
               stars = c('*' = .1, '**' = .05, '***'= 0.01), 
               add_rows = FE, 
               output = "latex",
               align = paste(c("l", rep("c", 6)), sep="", collapse=""), 
               title = "Regression Results for COVID and Old Adopters - Pre-Covid Period.") %>% 
  add_header_above(c(" " = 1, "India" = 2, "Mexico" = 2, "Indonesia" = 2)) %>% 
  kable_styling(latex_options = c("HOLD_position", "scale_down")) %>% 
  footnote("The baseline category is old adopters. The regression sample only includes the pre-Covid period. Clustered standard errors at the firm level.", 
           threeparttable = TRUE) 




capture.output(table_reg_old_covid_adopters, file = "../../Outputs/Tables/reg_old_vs_covid_adopters.tex")




## Table for regressions with COVID Adopters Types ----
table_reg_covid_adopters<-
  modelsummary(models_covid_adopters,
               coef_rename = coef_labels, 
               gof_map = gm, 
               stars = c('*' = .1, '**' = .05, '***'= 0.01), 
               add_rows = FE, 
               output = "latex",
               align = paste(c("l", rep("c", 6)), sep="", collapse=""), 
               title = "Regression Results for COVID Adopters - Pre-Covid Period.") %>% 
  add_header_above(c(" " = 1, "India" = 2, "Mexico" = 2, "Indonesia" = 2)) %>% 
  kable_styling(latex_options = c("HOLD_position", "scale_down")) %>% 
  footnote("The baseline category is COVID late adopters. The regression sample only includes the pre-Covid period. Clustered standard errors at the firm level", 
           threeparttable = TRUE) 


capture.output(table_reg_covid_adopters, file = "../../Outputs/Tables/reg_covid_adopters.tex")





## Table for regressions with Old Adopters Types ----
table_reg_old_adopters<-
  modelsummary(models_old_adopters,
               coef_rename = coef_labels, 
               gof_map = gm, 
               stars = c('*' = .1, '**' = .05, '***'= 0.01), 
               add_rows = FE, 
               output = "latex",
               align = paste(c("l", rep("c", 6)), sep="", collapse=""), 
               title = "Regression Results for Old Adopters - Pre-Covid Period.") %>% 
  add_header_above(c(" " = 1, "India" = 2, "Mexico" = 2, "Indonesia" = 2)) %>% 
  kable_styling(latex_options = c("HOLD_position", "scale_down")) %>% 
  footnote("The baseline category includes pre-2017 adopters for India and Mexico, and pre-2018 adopters for Indonesia. The regression sample only includes the pre-Covid period. Clustered standard errors at the firm level", 
           threeparttable = TRUE) 


capture.output(table_reg_old_adopters, file = "../../Outputs/Tables/reg_old_adopters.tex")
