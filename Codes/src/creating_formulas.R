
# This function will create a regression of the form $y_{ipt} = \alpha_0 + \alpha_1 indep_var1 \cdot covid + \alpha_1 indep_var1 \cdot covid \cdot product_p + same last two terms for all indep_vars  + covid \cdot product_p $

reg_models_tech_adopters<-function(import_data, export_data, country_name, 
                                   product_vars, coef_labels, dep_var_import, 
                                   dep_var_export, dep_var_import_label, 
                                   dep_var_export_label, 
                                   covid_var, indep_vars){
  
  # The following list  will contain all formulas of regressions.
  all_formulas <- vector("list", length(product_vars)*2)  # Create an empty list with length of number of models to be estimated
  
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
  
  dep_var_labels<-c(dep_var_import_label, dep_var_export_label)
  
  # Assign dependent variable names to model models 
  names(models)<-rep(dep_var_labels, length(product_vars))
  
  
  
  # Add fixed-effects indicators to tables
  FE<-as.data.frame(matrix(c("Firm FE", rep("Yes", length(models)), 
                             "Product FE", rep("Yes", length(models)),
                             "Month FE", rep("Yes", length(models))), 
                           nrow = 3, byrow=T))
  
  
  if(dep_var_export == "new_destination" & dep_var_import == "new_source"){
    list_notes<-"A new source/new destination is defined with respect to baseline year 2017. The regressions for new source/destination are estimated using a subset of firms that had transactions in 2017 as well."
  } else{
    list_notes<-""
  }
  
  format_se_coef <- function(x) format(round(x, 6), nsmall = 2, scientific = FALSE)
  
  
  # Table summary models
  table<-modelsummary(models,
                      coef_map = coef_labels, 
                      gof_map = gm, 
                      stars = c('*' = .1, '**' = .05, '***'= 0.01), 
                      add_rows = FE, 
                      output = "latex", 
                      align = paste(c("l", rep("c", length(models))), sep="", collapse=""), 
                      fmt = format_se_coef,  
                      title = paste0(country_name," - Regression Results for ", dep_var_labels[1], " and ", dep_var_labels[2], ": e-Bay tradable, China e-commerce, Consumable and Durable products")) |>
    add_header_above(c(" " = 1, "Dependent Variables" = length(models))) %>% 
    kable_styling(latex_options = c("HOLD_position", "scale_down")) %>% 
    footnote(paste0("Clustered-standard errors at the firm-product level. The variable E-payment or E-commerce 2019 is a dummy equal to 1 if the company adopted the E-payment or E-commerce technology before 2019 and 0 if not. The variable 2018-Adopter is equal to the number of months since the firm adopted the E-payment or E-commerce technology. This is applicable only for firms that adopted the technology before 2019. For firms that did not adopt the technology prior to 2019, the '2018-Adopter' variable is set to 0. ",
                    list_notes), 
             threeparttable = TRUE) 
  
  
  return(list(models, table))
  
}


coef_labels<-c(
  "pre_2017_adopter:month_mean_stringency_index"="Pre-2017 Adopter × Monthly Avg. Stringency Index",
  "month_mean_stringency_index:adopter_2017"="2017-Adopter × Monthly Avg. Stringency Index",
  "month_mean_stringency_index:adopter_2018"="2018-Adopter × Monthly Avg. Stringency Index",
  
  "month_mean_stringency_index:cons_BEC" = "Monthly Avg. Stringency Index × Consumable",
  "pre_2017_adopter:month_mean_stringency_index:cons_BEC" = "Pre-2017 Adopter × Monthly Avg. Stringency Index × Consumable",
  "month_mean_stringency_index:cons_BEC:adopter_2017" = "2017-Adopter × Monthly Avg. Stringency Index × Consumable",
  "month_mean_stringency_index:cons_BEC:adopter_2018" = "2018-Adopter × Monthly Avg. Stringency Index × Consumable",
  
  "month_mean_stringency_index:China_E_commerce" = "Monthly Avg. Stringency Index × China e-commerce",
  "pre_2017_adopter:month_mean_stringency_index:China_E_commerce" = "Pre-2017 Adopter × Monthly Avg. Stringency Index × China e-commerce",
  "month_mean_stringency_index:China_E_commerce:adopter_2017" = "2017-Adopter × Monthly Avg. Stringency Index × China e-commerce",
  "month_mean_stringency_index:China_E_commerce:adopter_2018" = "2018-Adopter × Monthly Avg. Stringency Index × China e-commerce",
  
  
  "month_mean_stringency_index:Ebay_tradable" = "Monthly Avg. Stringency Index × eBay-Tradable",
  "pre_2017_adopter:month_mean_stringency_index:Ebay_tradable" = "Pre-2017 Adopter × Monthly Avg. Stringency Index × eBay-Tradable",
  "month_mean_stringency_index:Ebay_tradable:adopter_2017" = "2017-Adopter × Monthly Avg. Stringency Index × eBay-Tradable",
  "month_mean_stringency_index:Ebay_tradable:adopter_2018" = "2018-Adopter × Monthly Avg. Stringency Index × eBay-Tradable",
  
  
  "month_mean_stringency_index:durable_BEC" = "Monthly Avg. Stringency Index × Durable",
  "pre_2017_adopter:month_mean_stringency_index:durable_BEC" = "Pre-2017 Adopter × Monthly Avg. Stringency Index × Durable", 
  "month_mean_stringency_index:durable_BEC:adopter_2017" = "2017-Adopter × Monthly Avg. Stringency Index × Durable",
  "month_mean_stringency_index:durable_BEC:adopter_2018" = "2018-Adopter × Monthly Avg. Stringency Index × Durable"
  
)




tt<-reg_models_tech_adopters(import_data = import_tech_mitig, 
                             export_data = export_tech_mitig, 
                             country_name = "India", 
                             product_vars = c("cons_BEC", 'China_E_commerce', "Ebay_tradable", "durable_BEC"), 
                             coef_labels = coef_labels, 
                             dep_var_import = "log_import", 
                             dep_var_expor = "log_export", 
                             dep_var_import_label = "Log. Import", 
                             dep_var_export_label = "Log. Export", 
                             covid_var = "month_mean_stringency_index", 
                             indep_vars = c("pre_2017_adopter", "adopter_2017", "adopter_2018")
                             )


