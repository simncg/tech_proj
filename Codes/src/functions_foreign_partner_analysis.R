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



# Functions to create plots ----


plot_total_values<-function(exports_data, imports_data){
  
  
  p1<-exports_data %>% 
    mutate(
      industry_local = case_when(
        industry_local == "MANUF" ~ "Manufacturing",
        industry_local == "WHL-RT" ~ "Wholesale/Retail",
        TRUE ~ industry_local
      ),
      industry_foreign = case_when(
        industry_foreign == "MANUF" ~ "Manufacturing",
        industry_foreign == "WHL-RT" ~ "Wholesale/Retail",
        TRUE ~ industry_foreign
      )
    ) %>% 
    mutate(avg_tot_exp_to_f_ind = avg_tot_exp_to_f_ind/1000000, mean_exp_firm_to_firm = mean_exp_firm_to_firm/1000000) %>% 
    rename(`No. Indonesian Firms` = n_domestic_firms, `No. US Firms` = n_foreign_firms, `Avg. Export Value per IDN \n Firm to US Industry (USD Million)` = avg_tot_exp_to_f_ind, 
           `Avg. No US Importers \n per IDN firm` = avg_n_importers, `Avg. Value of Firm-to-Firm Exports \n (USD Million)` = mean_exp_firm_to_firm
    ) %>% 
    pivot_longer(cols = -c(industry_local, industry_foreign), 
                 names_to = "variable") %>% 
    mutate(industries = paste("IDN", industry_local, "-", "US", industry_foreign)) %>% 
    ggplot(aes(x = variable, y = value, fill = industries))+
    geom_col(position = position_dodge(width = 1.1), width = 1)+
    labs(fill = "Bilateral Industry Relation:")+
    xlab("")+
    ylab("")+
    ggtitle("")+
    theme_minimal()+
    theme(legend.key.size = unit(0.3, 'cm'), 
          legend.text = element_text(size = 5), 
          legend.title = element_text(size = 9, face = "bold"), 
          strip.text = element_blank(), 
          axis.text.x = element_text(size = 6), 
          axis.text.y = element_text(size = 6), 
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(), 
          plot.title = element_text(size = 12, face = "bold", hjust = 0.5))+
    guides(colour = guide_legend(override.aes = list(size = 3)),
           shape = guide_legend(override.aes = list(size = 3), keyheight = 0.6))+
    scale_fill_economist()+
    facet_wrap(~variable, scales = "free", nrow = 3)
  
  
  
  
  p2<-imports_data %>% 
    mutate(avg_tot_imp_to_f_ind = avg_tot_imp_to_f_ind/1000000, mean_imp_firm_to_firm = mean_imp_firm_to_firm/1000000) %>% 
    rename(`No. Indonesian Firms` = n_domestic_firms, `No. US Firms` = n_foreign_firms, `Avg. Import Value per IDN \n Firm to US Industry (USD Million)` = avg_tot_imp_to_f_ind, 
           `Avg. No US Exporters \n per IDN firm` = avg_n_exporters, `Avg. Value of Firm-to-Firm Imports \n (USD Million)` = mean_imp_firm_to_firm
    ) %>% 
    mutate(
      industry_local = case_when(
        industry_local == "MANUF" ~ "Manufacturing",
        industry_local == "WHL-RT" ~ "Wholesale/Retail",
        TRUE ~ industry_local
      ),
      industry_foreign = case_when(
        industry_foreign == "MANUF" ~ "Manufacturing",
        industry_foreign == "WHL-RT" ~ "Wholesale/Retail",
        TRUE ~ industry_foreign
      )
    ) %>% 
    pivot_longer(cols = -c(industry_local, industry_foreign), 
                 names_to = "variable") %>% 
    mutate(industries = paste("IDN", industry_local, "-", "US", industry_foreign)) %>% 
    ggplot(aes(x = variable, y = value, fill = industries))+
    geom_col(position = position_dodge(width = 1.1), width = 1)+
    labs(fill = "Bilateral Industry Relation:")+
    xlab("")+
    ylab("")+
    ggtitle("")+
    theme_minimal()+
    theme(legend.key.size = unit(0.3, 'cm'), 
          legend.text = element_text(size = 5), 
          legend.title = element_text(size = 9, face = "bold"), 
          strip.text = element_blank(), 
          axis.text.x = element_text(size = 6), 
          axis.text.y = element_text(size = 6), 
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(), 
          plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
          
    )+
    guides(colour = guide_legend(override.aes = list(size = 3)),
           shape = guide_legend(override.aes = list(size = 3), keyheight = 0.6))+
    scale_fill_economist()+
    facet_wrap(~variable, scales = "free", nrow = 3)
  
  
  plot<-ggarrange(
    p1, p2, labels = c("Exports", "Imports"),
    common.legend = TRUE, 
    legend = "right", 
    font.label = list(size = 12, color = "black", face = "italic", family = NULL)
    
  )
  
  
  return(plot)
}


plot_differences<-function(exports_data, imports_data, ylab_title){
  
  p1<- exports_data %>% 
    relocate(matches("yes"), .before = 3) %>% 
    mutate(
      industry_local = case_when(
        industry_local == "MANUF" ~ "Manufacturing",
        industry_local == "WHL-RT" ~ "Wholesale/Retail",
        TRUE ~ industry_local
      ),
      industry_foreign = case_when(
        industry_foreign == "MANUF" ~ "Manufacturing",
        industry_foreign == "WHL-RT" ~ "Wholesale/Retail",
        TRUE ~ industry_foreign
      ), 
      across(c(contains("avg_tot"), contains("mean")), ~./1000000),
      # Take differences 
      diff_n_domestic_firms = n_domestic_firms_yes - n_domestic_firms_non, 
      diff_avg_tot_exp_to_f_ind = avg_tot_exp_to_f_ind_yes - avg_tot_exp_to_f_ind_non, 
      diff_avg_n_importers = avg_n_importers_yes - avg_n_importers_non, 
      diff_mean_exp_firm_to_firm = mean_exp_firm_to_firm_yes - mean_exp_firm_to_firm_non
    ) %>%
    select(industry_local, industry_foreign, starts_with("diff")) %>% 
    rename(`Difference No. \n Indonesian Firms` = diff_n_domestic_firms, 
           `Difference Avg. Export Value \n per IDN Firm to US Industry \n (USD Million)` = diff_avg_tot_exp_to_f_ind, 
           `Difference Avg. No US \n Importers per IDN firm` = diff_avg_n_importers,
           `Difference Avg. Value of \n Firm-to-Firm Exports \n (USD Million)` = diff_mean_exp_firm_to_firm) %>% 
    pivot_longer(cols = -c(industry_local, industry_foreign), names_to = "variable") %>% 
    mutate(industries = paste("IDN", industry_local, "-", "US", industry_foreign)) %>% 
    ggplot(aes(x = variable, y = value, fill = industries))+
    geom_col(position = position_dodge(width = 1.1), width = 1)+
    labs(fill = "Bilateral Industry Relation:")+
    ylab(ylab_title)+
    xlab("")+
    ggtitle("")+
    theme_minimal()+
    theme(legend.key.size = unit(0.3, 'cm'), 
          legend.text = element_text(size = 5), 
          legend.title = element_text(size = 9, face = "bold"), 
          strip.text = element_blank(), 
          axis.text.x = element_text(size = 7), 
          axis.text.y = element_text(size = 6), 
          axis.title.y = element_text(size = 7),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
    )+
    guides(colour = guide_legend(override.aes = list(size = 3)),
           shape = guide_legend(override.aes = list(size = 3), keyheight = 0.6))+
    scale_fill_economist()+
    facet_wrap(~variable, scales = "free", nrow = 2)+
    scale_y_continuous()
  
  
  
  p2 <- imports_data %>% 
    relocate(matches("yes"), .before = 3) %>% 
    mutate(
      industry_local = case_when(
        industry_local == "MANUF" ~ "Manufacturing",
        industry_local == "WHL-RT" ~ "Wholesale/Retail",
        TRUE ~ industry_local
      ),
      industry_foreign = case_when(
        industry_foreign == "MANUF" ~ "Manufacturing",
        industry_foreign == "WHL-RT" ~ "Wholesale/Retail",
        TRUE ~ industry_foreign
      ), 
      across(c(contains("avg_tot"), contains("mean")), ~./1000000),
      # Take differences 
      diff_n_domestic_firms = n_domestic_firms_yes - n_domestic_firms_non, 
      diff_avg_tot_imp_to_f_ind = avg_tot_imp_to_f_ind_yes - avg_tot_imp_to_f_ind_non, 
      diff_avg_n_exporters = avg_n_exporters_yes - avg_n_exporters_non, 
      diff_mean_imp_firm_to_firm = mean_imp_firm_to_firm_yes - mean_imp_firm_to_firm_non
    ) %>%
    select(industry_local, industry_foreign, starts_with("diff")) %>% 
    rename(`Difference No. \n Indonesian Firms` = diff_n_domestic_firms, 
           `Difference Avg. Import Value \n per IDN Firm to US Industry \n (USD Million)` = diff_avg_tot_imp_to_f_ind, 
           `Difference Avg. No US \n Exporters per IDN firm` = diff_avg_n_exporters,
           `Difference Avg. Value of \n Firm-to-Firm Imports \n (USD Million)` = diff_mean_imp_firm_to_firm) %>% 
    pivot_longer(cols = -c(industry_local, industry_foreign), names_to = "variable") %>% 
    mutate(industries = paste("IDN", industry_local, "-", "US", industry_foreign)) %>% 
    ggplot(aes(x = variable, y = value, fill = industries))+
    geom_col(position = position_dodge(width = 1.1), width = 1)+
    labs(fill = "Bilateral Industry Relation:")+
    ylab("")+
    xlab("")+
    ggtitle("")+
    theme_minimal()+
    theme(legend.key.size = unit(0.3, 'cm'), 
          legend.text = element_text(size = 5), 
          legend.title = element_text(size = 9, face = "bold"), 
          strip.text = element_blank(), 
          axis.text.x = element_text(size = 7), 
          axis.text.y = element_text(size = 6), 
          axis.title.y = element_text(size = 7),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
    )+
    guides(colour = guide_legend(override.aes = list(size = 3)),
           shape = guide_legend(override.aes = list(size = 3), keyheight = 0.6))+
    scale_fill_economist()+
    facet_wrap(~variable, scales = "free", nrow = 2)+
    scale_y_continuous()
  
  
  plot_diff<-ggarrange(
    p1, p2, labels = c("Exports", "Imports"),
    common.legend = TRUE, 
    legend = "right", 
    font.label = list(size = 12, color = "black", face = "italic", family = NULL)
  )
  
  return(plot_diff)
  
}







