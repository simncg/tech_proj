# This function generates descriptive statistics of domestic industry to foreign industry  (number of domestic firms in local industry trading
# with foreign industries, average value of exports per domestic firm, etc), but we take into account another dimension, which is the product classification. 


# Imports function
foreign_prod_imp<-function(data, product_tech_var){
  table_products<-
    data%>% 
    na.omit(product_tech_var) %>% 
    group_by({{product_tech_var}}, industry_local, industry_foreign) %>% 
    mutate(
      # Total number of domestic companies in industry X that trade with foreign firms in industry X
      n_domestic_firms = n_distinct(domestic_company_id), 
      # Compute the total value of imports from domestic firms (sum of imports across firms) in industry X to all foreign firms in industry X.
      tot_imp_to_f_ind = sum(import), 
      # Average per IDN firm of total value of imports from domestic firms in industry X to all foreign firms in industry X
      avg_tot_imp_to_f_ind = tot_imp_to_f_ind/n_domestic_firms
    ) %>% 
    ungroup() %>% 
    # Group by domestic firms, local industry and foreign industry
    group_by(domestic_company_id, {{product_tech_var}}, industry_local, industry_foreign) %>% 
    # Number of foreign exporters by domestic firm (exporters because domestic companies are importing from them)
    mutate(foreign_n_exporters = n_distinct(foreign_company_id)) %>% 
    ungroup() %>% 
    # Group by local industry and foreign industry 
    group_by({{product_tech_var}}, industry_local, industry_foreign) %>% 
    mutate(
      # Average number of foreign partners per firm by local industry and foreign industry 
      avg_n_exporters = mean(foreign_n_exporters),  
    ) %>% 
    ungroup() %>% 
    # Group by domestic firm, foreign firm, local industry and foreign industry
    group_by({{product_tech_var}}, domestic_company_id, foreign_company_id, industry_local, industry_foreign) %>% 
    # Sum the total value of imports between two firms (firm-to-firm imports)
    mutate(imports_firm_to_firm = sum(import)) %>% 
    ungroup() %>% 
    # Group by local industry and foreign industry
    group_by({{product_tech_var}}, industry_local, industry_foreign) %>% 
    # Take the average of the firm-to-firm imports 
    mutate(mean_imp_firm_to_firm = mean(imports_firm_to_firm)) %>% 
    ungroup() %>% 
    select({{product_tech_var}}, industry_local, industry_foreign, 
           n_domestic_firms, avg_tot_imp_to_f_ind, avg_n_exporters, mean_imp_firm_to_firm) %>% 
    distinct({{product_tech_var}}, industry_local, industry_foreign, .keep_all = T) %>% 
    # "yes" means that the summary statistics in table correspond to the ones of the product category or the technology adopted,
    # while non means that does not correspond to the summary statistics of the product category or technology adopted. 
    mutate({{product_tech_var}} := if_else({{product_tech_var}} == TRUE, "yes", "non")) %>%   
    # Pivot wider
    pivot_wider(
      names_from = {{product_tech_var}}, 
      values_from = c(n_domestic_firms:mean_imp_firm_to_firm)
    ) %>% 
    as_tibble()
  
  return(table_products)
  
}



# Exports function
foreign_prod_exp<-function(data, product_tech_var){
  table_products<-
    data%>% 
    na.omit(product_tech_var) %>% 
    group_by({{product_tech_var}}, industry_local, industry_foreign) %>% 
    mutate(
      # Total number of domestic companies in industry X that trade with foreign firms in industry X
      n_domestic_firms = n_distinct(domestic_company_id), 
      # Compute the total value of exports from domestic firms (sum of exports across firms) in industry X to all foreign firms in industry X.
      tot_exp_to_f_ind = sum(export), 
      # Average per IDN firm of total value of exports from domestic firms in industry X to all foreign firms in industry X
      avg_tot_exp_to_f_ind = tot_exp_to_f_ind/n_domestic_firms
    ) %>% 
    ungroup() %>% 
    # Group by domestic firms, local industry and foreign industry
    group_by(domestic_company_id, {{product_tech_var}}, industry_local, industry_foreign) %>% 
    # Number of foreign importers by domestic firm (importers because domestic companies are exporting to them)
    mutate(foreign_n_importers = n_distinct(foreign_company_id)) %>% 
    ungroup() %>% 
    # Group by local industry and foreign industry 
    group_by({{product_tech_var}}, industry_local, industry_foreign) %>% 
    mutate(
      # Average number of foreign partners per firm by local industry and foreign industry 
      avg_n_importers = mean(foreign_n_importers),  
    ) %>% 
    ungroup() %>% 
    # Group by domestic firm, foreign firm, local industry and foreign industry
    group_by({{product_tech_var}}, domestic_company_id, foreign_company_id, industry_local, industry_foreign) %>% 
    # Sum the total value of exports between two firms (firm-to-firm exports)
    mutate(exports_firm_to_firm = sum(export)) %>% 
    ungroup() %>% 
    # Group by local industry and foreign industry
    group_by({{product_tech_var}}, industry_local, industry_foreign) %>% 
    # Take the average of the firm-to-firm imports 
    mutate(mean_exp_firm_to_firm = mean(exports_firm_to_firm)) %>% 
    ungroup() %>% 
    select({{product_tech_var}}, industry_local, industry_foreign, 
           n_domestic_firms, avg_tot_exp_to_f_ind, avg_n_importers, mean_exp_firm_to_firm) %>% 
    distinct({{product_tech_var}}, industry_local, industry_foreign, .keep_all = T) %>% 
    # "yes" means that the summary statistics in table correspond to the ones of the product category or the technology adopted,
    # while non means that does not correspond to the summary statistics of the product category or technology adopted
    mutate({{product_tech_var}} := if_else({{product_tech_var}} == TRUE, "yes", "non")) %>% 
    # Pivot wider
    pivot_wider(
      names_from = {{product_tech_var}}, 
      values_from = c(n_domestic_firms:mean_exp_firm_to_firm)
    ) %>% 
    as_tibble()
  
  return(table_products)
  
}

