#===========================================================================#
# Date:    June 2022                                                        #
#                                                                           #
# Project: E-commerce and Trade during Crisis Times: Firm-level             # 
#          Evidence from India, Indonesia and Mexico
#                                                                           #
#                                                                           #
# This program generates Table 10 and 11.                                                             #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")
library(fastDummies)

# Function for running regressions 
source("../src/functions_reg_2019_adopters.R")

# Intensive Margin Regressions -----

model_pre_2019<-list()

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
  
  
  # Read India data for tech mitigation of COVID (data at the firm-year-month-HS product level)----
  import_data<-fread(paste0("../../Data/processed_data/", country ,"/imports_tech_mitigation_model_", abbr, ".csv"))
  export_data<-fread(paste0("../../Data/processed_data/", country, "/exports_tech_mitigation_model_", abbr, ".csv"))
  
  import_data<-as.data.frame(import_data)
  export_data<-as.data.frame(export_data)
  
  
  # Convert from Boolean to dummy (to avoid _TRUE labels in table)
  prod_vars <- c("China_E_commerce")
  
  import_data[prod_vars] <- lapply(import_data[prod_vars], function(x) 1 * x)
  export_data[prod_vars] <- lapply(export_data[prod_vars], function(x) 1 * x)
  
  
  
  # Generate an additional adopter type variable, which will be equal to non-pre-2019-adopter if the firm 
  # is never adopter/2019-adopter/covid-adopter and if it is pre-2019 adopter the variable will be equal 
  # to 2018-adopter, 2017-adopter or pre-2017 adopter depending on the year when the firm adopted the technology
  
  import_data<-import_data %>% 
    mutate(
      pre_2019_adopter_type = 
        case_when(
          year(date_of_adoption) >= 2019 | is.na(date_of_adoption) ~ "non_pre_2019_adopter",
          year(date_of_adoption) < 2019  ~ old_adopter_type,
          TRUE ~ NA # Although this case never happens
        )
    )
  
  
  export_data<-export_data %>% 
    mutate(
      pre_2019_adopter_type = 
        case_when(
          year(date_of_adoption) >= 2019 | is.na(date_of_adoption) ~ "non_pre_2019_adopter",
          year(date_of_adoption) < 2019  ~ old_adopter_type,
          TRUE ~ NA # Although this case never happens
        )
    )
  
  
  
  if(country != "Indonesia"){
    # Create dummies for 2017-adopter, 2018-adopter, pre-2017 adopter, we use as baseline (ref) the non-pre-2019 adopter
    export_data<-data.frame(export_data, i(export_data$pre_2019_adopter_type, ref = "non_pre_2019_adopter")) %>% 
      rename(`pre_2017_adopter` = `X2016_or_pre_2016_adopter`, 
             `adopter_2017` = `X2017_adopter`, 
             `adopter_2018` = `X2018_adopter`, 
      )
    
    
    import_data<-data.frame(import_data, i(import_data$pre_2019_adopter_type, ref = "non_pre_2019_adopter")) %>% 
      rename(`pre_2017_adopter` = `X2016_or_pre_2016_adopter`, 
             `adopter_2017` = `X2017_adopter`, 
             `adopter_2018` = `X2018_adopter`, 
      ) 
    
    
    indep_vars <- c("pre_2017_adopter", "adopter_2017", "adopter_2018")
    
  } else if(country == "Indonesia"){
    
    # Create dummies for 2017-adopter, 2018-adopter, pre-2017 adopter, we use as baseline (ref) the non-pre-2019 adopter
    export_data<-data.frame(export_data, i(export_data$pre_2019_adopter_type, ref = "non_pre_2019_adopter")) %>% 
      rename(`pre_2018_adopter` = `X2017_or_pre_2017_adopter`, 
             `adopter_2018` = `X2018_adopter` 
      )
    
    
    import_data<-data.frame(import_data, i(import_data$pre_2019_adopter_type, ref = "non_pre_2019_adopter")) %>% 
      rename(`pre_2018_adopter` = `X2017_or_pre_2017_adopter`, 
             `adopter_2018` = `X2018_adopter`
      )
    
    
    indep_vars <- c("pre_2018_adopter", "adopter_2018")
    
    
  }
  
  
  models<-
    reg_models_pre_2019_adopters(import_data = import_data,
                                 export_data = export_data,
                                 country_name = country, 
                                 product_vars = "China_E_commerce",
                                 dep_var_import = "log_import", 
                                 dep_var_export = "log_export", 
                                 covid_var = "month_mean_stringency_index", 
                                 indep_vars = indep_vars)
  
  
  model_pre_2019<-c(model_pre_2019, models)
  
  
  rm(import_data, export_data)

}



# Extensive Margin Regressions -----


ext_model_pre_2019<-list()

# Iterate over countries
for(country in c("India", "Mexico", "Indonesia")){
  
  if(country == "Indonesia"){ # Since BuiltWith dataset for Indonesia is not available before 2017
    indep_vars<-c("pre_2018_adopter", "adopter_2018")
  } else if(country != "Indonesia"){
    indep_vars<-c("pre_2017_adopter", "adopter_2017", "adopter_2018")
  }
  
  reg_results<-
    ext_reg_models_pre_2019_adopters(country_name = country, 
                                     indep_vars = indep_vars, 
                                     covid_var = "month_mean_stringency_index", 
                                     product_vars = c("China_E_commerce"))
  
  ext_model_pre_2019<-c(ext_model_pre_2019, reg_results)
  
}



# Tables -----

# Assign dependent variable name to models names
names(model_pre_2019)<-rep(c("Log. Imports", "Log. Exports"), length(model_pre_2019)/2)

# Labels 
# Labels to be displayed in table of results 
coef_labels<-c(
  "month_mean_stringency_index:China_E_commerce" = "Monthly Avg. Stringency Index × China e-commerce",
  "pre_2017_adopter:month_mean_stringency_index"="Pre-2017 Adopter× Monthly Avg. Stringency Index",
  "pre_2017_adopter:month_mean_stringency_index:China_E_commerce" = "Pre-2017 Adopter× Monthly Avg. Stringency Index× China e-commerce",

  "pre_2018_adopter:month_mean_stringency_index"="Pre-2018 Adopter × Monthly Avg. Stringency Index",
  "pre_2018_adopter:month_mean_stringency_index:China_E_commerce" = "Pre-2018 Adopter × Monthly Avg. Stringency Index × China e-commerce",
  
  "month_mean_stringency_index:adopter_2017"="2017-Adopter× Monthly Avg. Stringency Index",
  "month_mean_stringency_index:China_E_commerce:adopter_2017" = "2017-Adopter× Monthly Avg. Stringency Index × China e-commerce",
  
  "month_mean_stringency_index:adopter_2018"="2018-Adopter× Monthly Avg. Stringency Index",
  "month_mean_stringency_index:China_E_commerce:adopter_2018" = "2018-Adopter× Monthly Avg. Stringency Index × China e-commerce"
  )


# Add fixed-effects indicators to tables
FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(model_pre_2019)), 
                           "Product FE", rep("Yes", length(model_pre_2019)),
                           "Month FE", rep("Yes", length(model_pre_2019))), 
                         nrow = 3, byrow=T))



# Customize latex tables
options("modelsummary_format_numeric_latex" = "plain")

# Format N, R2, Adj.R2 in tables
f1 <- function(x) format(round(x, 3), big.mark=",")
f2 <- function(x) format(round(x, 0), big.mark=",")

gm <- list(
  list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = f2),
  list("raw" = "r.squared", "clean" = "R-squared", "fmt" = f1)
)


format_se_coef <- function(x) format(round(x, 5), nsmall = 3, scientific = FALSE)



# Produce tables 
table_pre_2019_adop<-
  modelsummary(model_pre_2019,
               coef_map = coef_labels, 
               gof_map = gm, 
               estimate = "{estimate}{stars}",
               stars = c('*' = .1, '**' = .05, '***'= 0.01), 
               add_rows = FE, 
               align = paste(c("l", rep("c", length(model_pre_2019))), sep="", collapse=""), 
               fmt = format_se_coef, 
               notes = NULL, 
               output = "latex"
               ) %>% 
  add_header_above(c(" " = 1, "India" = 2, "Mexico" = 2, "Indonesia" = 2)) %>% 
  kable_styling(latex_options = c("scale_down", "HOLD_position")) %>% 
  footnote("Clustered-standard errors at the firm-product level. The baseline category consists of non-pre-2019 adopters, which includes both never adopters and firms that adopted the technology from 2019 onwards.", threeparttable = TRUE) 


capture.output(table_pre_2019_adop, file = "../../Outputs/Tables/reg_pre_2019_adopters_intensive.tex")





format_se_coef <- function(x) format(round(x, 6), nsmall = 3, scientific = FALSE)



# Produce tables 
table_ext_pre_2019_adop<-
  modelsummary(ext_model_pre_2019,
               coef_map = coef_labels, 
               gof_map = gm, 
               estimate = "{estimate}{stars}",
               stars = c('*' = .1, '**' = .05, '***'= 0.01), 
               add_rows = FE, 
               align = paste(c("l", rep("c", length(model_pre_2019))), sep="", collapse=""), 
               fmt = format_se_coef, 
               notes = NULL, 
               output = "latex"
  ) %>% 
  add_header_above(c(" " = 1, "India" = 2, "Mexico" = 2, "Indonesia" = 2)) %>% 
  kable_styling(latex_options = c("scale_down", "HOLD_position")) %>% 
  footnote("Clustered-standard errors at the firm-product level. The baseline category consists of non-pre-2019 adopters, which includes both never adopters and firms that adopted the technology from 2019 onwards.", threeparttable = TRUE) 


capture.output(table_ext_pre_2019_adop, file = "../../Outputs/Tables/reg_pre_2019_adopters_extensive.tex")



