#===========================================================================#
# Date:    June 2022                                                        #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
#                                                                           #
#                                                                           #
# This program generates summary statistics and descriptive tables. Tables  #
# 1 and 2.                                                                  # 
#                                                                           #
#                                                                           #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")

# SAMPLE 1: Sample with never adopters and firms that adopted the technology during the period of analysis
# SAMPLE 2: Sample with all firms (firms that adopted the technology pre-2019, never adopters and after 2018)

# Regression sample of firms adopting the technology during period of analysis ----

# Imports and exports datasets of Indonesia used for regressions 
import_data_IDN<-as.data.frame(fread("../../Data/processed_data/Indonesia/imports_product_model_IDN.csv"))
export_data_IDN<-as.data.frame(fread("../../Data/processed_data/Indonesia/exports_product_model_IDN.csv"))

# Imports and exports datasets of India used for regressions
import_data_IND<-as.data.frame(fread("../../Data/processed_data/India/imports_product_model_IND.csv"))
export_data_IND<-as.data.frame(fread("../../Data/processed_data/India/exports_product_model_IND.csv"))


# Imports and exports datasets of Mexico used for regressions
import_data_MEX<-as.data.frame(fread("../../Data/processed_data/Mexico/imports_product_model_MEX.csv"))
export_data_MEX<-as.data.frame(fread("../../Data/processed_data/Mexico/exports_product_model_MEX.csv"))


# Format big numbers in tables
big_number_format<-function(x){format(x, big.mark = ",")}


# Summary statistics table of sample with never adopters and firms that adopted the technology during period of analysis
table_sample_1<-
  # Obtain number of firms in regression sample -----
  # Indonesian Number of Exporting Firms
  export_data_IDN %>% 
    distinct(company_id) %>% 
    summarize(Data = "Exports",
              `No. Firms` = n()) %>% 
    # Indonesian Number of Importing Firms 
    bind_rows(
      import_data_IDN %>% 
        distinct(company_id) %>% 
        summarize(Data = "Imports",
                  `No. Firms` = n()) 
    ) %>% 
    # Indian Number of Exporting Firms
    bind_rows(export_data_IND %>% 
                distinct(company_id) %>% 
                summarize(Data = "Exports ", 
                          `No. Firms` = n())
    ) %>% 
    # Indian Number of Importing Firms
    bind_rows(import_data_IND %>% 
                distinct(company_id) %>% 
                summarize(Data = "Imports ",
                          `No. Firms` = n())
    ) %>% 
    # Mexican Number of Exporting Firms
    bind_rows(export_data_MEX %>% 
                distinct(company_id) %>% 
                summarize(Data = "Exports  ",
                          `No. Firms` = n())
    ) %>% 
    # Mexican Number of Importing firms
    bind_rows(import_data_MEX %>% 
                distinct(company_id) %>% 
                summarize(Data = "Imports  ",
                          `No. Firms` = n())
    ) %>% 
    mutate(across(`No. Firms`, big_number_format)) %>% 
    # Summarize Export and Import Value -----
    bind_cols(
      export_data_IDN %>% 
        select(export) %>% 
        filter(export > 0) %>% 
        summarize_all(list(p25 = ~quantile(., 0.25), Median = median, p75 = ~quantile(., 0.75), Mean = mean, SD = sd)) %>%
        bind_rows(import_data_IDN %>% 
                    select(import) %>%
                    filter(import > 0) %>% 
                    summarize_all(list(p25 = ~quantile(., 0.25), Median = median, p75 = ~quantile(., 0.75),  Mean = mean, SD = sd)
                                  )
                  )%>% 
        # India
        bind_rows(export_data_IND %>% 
                    select(export) %>% 
                    filter(export > 0) %>% 
                    summarize_all(list(p25 = ~quantile(., 0.25), Median = median, p75 = ~quantile(., 0.75), Mean = mean, SD = sd)) %>% 
                    bind_rows(import_data_IND %>% 
                                select(import) %>% 
                                filter(import > 0) %>% 
                                summarize_all(list(p25 = ~quantile(., 0.25), Median = median, p75 = ~quantile(., 0.75), Mean = mean, SD = sd)) 
                    )
                  ) %>% 
        # Mexico
        bind_rows(export_data_MEX %>% 
                    select(export) %>% 
                    filter(export > 0) %>% 
                    summarize_all(list(p25 = ~quantile(., 0.25), Median = median, p75 = ~quantile(., 0.75), Mean = mean, SD = sd)) %>% 
                    bind_rows(import_data_MEX %>% 
                                select(import) %>% 
                                filter(import > 0) %>% 
                                summarize_all(list(p25 = ~quantile(., 0.25), Median = median, p75 = ~quantile(., 0.75), Mean = mean, SD = sd))
                    )
        ) %>% 
        mutate(across(c(p25, Median, p75, Mean, SD), round, 2), 
               across(c(p25, Median, p75, Mean, SD), big_number_format), 
               across(c(p25, Median, p75, Mean, SD), ~paste0("$", .)))
      ) %>% 
    # Proportion of Observations with Dummy Variables Equal to 1 for E-payment/E-commerce, China E-commerce and Consumable Product Categories ----
    bind_cols(
      # Indonesia Exports
      export_data_IDN %>% 
        select(tech = pay_or_ecomnod_t_2, China_E_commerce, cons_BEC) %>% 
        na.omit() %>% 
        summarize_all(list(Mean = mean)) %>% 
        pivot_longer(cols = everything(), 
                     names_to = c("variable", "statistic"),
                     names_pattern = "([A-Za-z_]+)_(Mean)") %>% 
        select(-statistic) %>% 
        mutate(across(where(is.numeric), round, 3)) %>% 
        pivot_wider(names_from = variable, 
                    values_from = value) %>% 
        bind_rows(
          # Indonesia Imports
          import_data_IDN %>% 
            select(tech = pay_or_ecomnod_t_2, China_E_commerce, cons_BEC) %>% 
            na.omit() %>% 
            summarize_all(list(Mean = mean)) %>% 
            pivot_longer(cols = everything(), 
                         names_to = c("variable", "statistic"),
                         names_pattern = "([A-Za-z_]+)_(Mean)") %>% 
            select(-statistic) %>% 
            mutate(across(where(is.numeric), round, 3)) %>% 
            pivot_wider(names_from = variable, 
                        values_from = value)
        ) %>% 
        bind_rows(
          # India Exports
          export_data_IND %>% 
            select(tech = pay_or_ecomnod_t_2, China_E_commerce, cons_BEC) %>% 
            na.omit() %>% 
            summarize_all(list(Mean = mean)) %>% 
            pivot_longer(cols = everything(), 
                         names_to = c("variable", "statistic"),
                         names_pattern = "([A-Za-z_]+)_(Mean)") %>% 
            select(-statistic) %>% 
            mutate(across(where(is.numeric), round, 3)) %>% 
            pivot_wider(names_from = variable, 
                        values_from = value) %>% 
            bind_rows(
              # India Imports
              import_data_IND %>% 
                select(tech = pay_or_ecomnod_t_2, China_E_commerce, cons_BEC) %>% 
                na.omit() %>% 
                summarize_all(list(Mean = mean)) %>% 
                pivot_longer(cols = everything(), 
                             names_to = c("variable", "statistic"),
                             names_pattern = "([A-Za-z_]+)_(Mean)") %>% 
                select(-statistic) %>% 
                mutate(across(where(is.numeric), round, 3)) %>% 
                pivot_wider(names_from = variable, 
                            values_from = value)
            )
          
        )  %>% 
        bind_rows(
          # Mexico Exports
          export_data_MEX %>% 
            select(tech = pay_or_ecomnod_t_2, China_E_commerce, cons_BEC) %>% 
            na.omit() %>% 
            summarize_all(list(Mean = mean)) %>% 
            pivot_longer(cols = everything(), 
                         names_to = c("variable", "statistic"),
                         names_pattern = "([A-Za-z_]+)_(Mean)") %>% 
            select(-statistic) %>% 
            mutate(across(where(is.numeric), round, 3)) %>% 
            pivot_wider(names_from = variable, 
                        values_from = value) %>% 
            bind_rows(
              # Mexico Imports
              import_data_MEX %>% 
                select(tech = pay_or_ecomnod_t_2, China_E_commerce, cons_BEC) %>% 
                na.omit() %>% 
                summarize_all(list(Mean = mean)) %>% 
                pivot_longer(cols = everything(), 
                             names_to = c("variable", "statistic"),
                             names_pattern = "([A-Za-z_]+)_(Mean)") %>% 
                select(-statistic) %>% 
                mutate(across(where(is.numeric), round, 3)) %>% 
                pivot_wider(names_from = variable, 
                            values_from = value)
            )
          
        ) %>% 
        rename(`China E-commerce` = China_E_commerce, Consumable = cons_BEC, `E-payment or E-commerce` = tech)
    ) %>% 
  tibble::column_to_rownames("Data") %>% 
  # Create Latex Table
  kbl(caption = "Summary statistics - Regression sample with never adopters and firms that adopted the technology during period of analysis", centering = T, booktabs = T, align = "c", 
      format = "latex") %>%
  kable_classic() %>%
  kable_styling(latex_options = c("HOLD_position", "scale_down")) %>% 
  pack_rows("Indonesia", 1, 2) %>%
  pack_rows("India", 3, 4) %>% 
  pack_rows("Mexico", 5, 6) %>% 
  column_spec(column = 8:10) %>% 
  add_header_above(c(" " = 2, "Export or Import Trade Value (USD)" = 5, " " = 3))%>% 
  footnote(general ="This table provides summary statistics for the regression sample that includes never adopter firms and firms that adopted the technology during the analysis period. It details the count of exporting and importing firms, distribution moments of trade value in USD dollar, and the proportion of observations where the dummy variables for the Technology Variable (E-payment or E-commerce), China E-commerce, and Consumable Product Categories are equal to 1.", threeparttable = TRUE)


capture.output(table_sample_1, file = "../../Outputs/Tables/summary_statistics_sample_1.tex")


rm(import_data_IDN, import_data_IND, import_data_MEX, 
   export_data_IDN, export_data_IND, export_data_MEX)

gc()

# Summary Statistics of table with all firms ---- 

# Imports and exports datasets of Indonesia used for regressions 
import_data_IDN<-as.data.frame(fread("../../Data/processed_data/Indonesia/imports_tech_mitigation_model_IDN.csv"))
export_data_IDN<-as.data.frame(fread("../../Data/processed_data/Indonesia/exports_tech_mitigation_model_IDN.csv"))

# Imports and exports datasets of India used for regressions
import_data_IND<-as.data.frame(fread("../../Data/processed_data/India/imports_tech_mitigation_model_IND.csv"))
export_data_IND<-as.data.frame(fread("../../Data/processed_data/India/exports_tech_mitigation_model_IND.csv"))


# Imports and exports datasets of Mexico used for regressions
import_data_MEX<-as.data.frame(fread("../../Data/processed_data/Mexico/imports_tech_mitigation_model_MEX.csv"))
export_data_MEX<-as.data.frame(fread("../../Data/processed_data/Mexico/exports_tech_mitigation_model_MEX.csv"))




# Summary statistics table of sample with never adopters and firms that adopted the technology during period of analysis
table_sample_2<-
  # Obtain number of firms in regression sample -----
  # Indonesian Number of Exporting Firms
  export_data_IDN %>% 
    distinct(company_id) %>% 
    summarize(Data = "Exports",
              `No. Firms` = n()) %>% 
    # Indonesian Number of Importing Firms 
    bind_rows(
      import_data_IDN %>% 
        distinct(company_id) %>% 
        summarize(Data = "Imports",
                  `No. Firms` = n()) 
    ) %>% 
    # Indian Number of Exporting Firms
    bind_rows(export_data_IND %>% 
                distinct(company_id) %>% 
                summarize(Data = "Exports ", 
                          `No. Firms` = n())
    ) %>% 
    # Indian Number of Importing Firms
    bind_rows(import_data_IND %>% 
                distinct(company_id) %>% 
                summarize(Data = "Imports ",
                          `No. Firms` = n())
    ) %>% 
    # Mexican Number of Exporting Firms
    bind_rows(export_data_MEX %>% 
                distinct(company_id) %>% 
                summarize(Data = "Exports  ",
                          `No. Firms` = n())
    ) %>% 
    # Mexican Number of Importing firms
    bind_rows(import_data_MEX %>% 
                distinct(company_id) %>% 
                summarize(Data = "Imports  ",
                          `No. Firms` = n())
    ) %>% 
    mutate(across(`No. Firms`, big_number_format)) %>% 
    # Summarize Export and Import Value -----
  bind_cols(
    export_data_IDN %>% 
      select(export) %>% 
      filter(export > 0) %>% 
      summarize_all(list(p25 = ~quantile(., 0.25), Median = median, p75 = ~quantile(., 0.75), Mean = mean, SD = sd)) %>%
      bind_rows(import_data_IDN %>% 
                  select(import) %>%
                  filter(import > 0) %>% 
                  summarize_all(list(p25 = ~quantile(., 0.25), Median = median, p75 = ~quantile(., 0.75),  Mean = mean, SD = sd)
                  )
      )%>% 
      # India
      bind_rows(export_data_IND %>% 
                  select(export) %>% 
                  filter(export > 0) %>% 
                  summarize_all(list(p25 = ~quantile(., 0.25), Median = median, p75 = ~quantile(., 0.75), Mean = mean, SD = sd)) %>% 
                  bind_rows(import_data_IND %>% 
                              select(import) %>% 
                              filter(import > 0) %>% 
                              summarize_all(list(p25 = ~quantile(., 0.25), Median = median, p75 = ~quantile(., 0.75), Mean = mean, SD = sd)) 
                  )
      ) %>% 
      # Mexico
      bind_rows(export_data_MEX %>% 
                  select(export) %>% 
                  filter(export > 0) %>% 
                  summarize_all(list(p25 = ~quantile(., 0.25), Median = median, p75 = ~quantile(., 0.75), Mean = mean, SD = sd)) %>% 
                  bind_rows(import_data_MEX %>% 
                              select(import) %>% 
                              filter(import > 0) %>% 
                              summarize_all(list(p25 = ~quantile(., 0.25), Median = median, p75 = ~quantile(., 0.75), Mean = mean, SD = sd))
                  )
      ) %>% 
      mutate(across(c(p25, Median, p75, Mean, SD), round, 2), 
             across(c(p25, Median, p75, Mean, SD), big_number_format), 
             across(c(p25, Median, p75, Mean, SD), ~paste0("$", .)))
  ) %>% 
    # Proportion of Observations with Dummy Variables Equal to 1 for E-payment/E-commerce, China E-commerce and Consumable Product Categories ----
  bind_cols(
    # Indonesia Exports
    export_data_IDN %>% 
      select(tech = adopted_pay_or_ecom_before_2019, China_E_commerce, cons_BEC) %>% 
      na.omit() %>% 
      summarize_all(list(Mean = mean)) %>% 
      pivot_longer(cols = everything(), 
                   names_to = c("variable", "statistic"),
                   names_pattern = "([A-Za-z_]+)_(Mean)") %>% 
      select(-statistic) %>% 
      mutate(across(where(is.numeric), round, 3)) %>% 
      pivot_wider(names_from = variable, 
                  values_from = value) %>% 
      bind_rows(
        # Indonesia Imports
        import_data_IDN %>% 
          select(tech = adopted_pay_or_ecom_before_2019, China_E_commerce, cons_BEC) %>% 
          na.omit() %>% 
          summarize_all(list(Mean = mean)) %>% 
          pivot_longer(cols = everything(), 
                       names_to = c("variable", "statistic"),
                       names_pattern = "([A-Za-z_]+)_(Mean)") %>% 
          select(-statistic) %>% 
          mutate(across(where(is.numeric), round, 3)) %>% 
          pivot_wider(names_from = variable, 
                      values_from = value)
      ) %>% 
      bind_rows(
        # India Exports
        export_data_IND %>% 
          select(tech = adopted_pay_or_ecom_before_2019, China_E_commerce, cons_BEC) %>% 
          na.omit() %>% 
          summarize_all(list(Mean = mean)) %>% 
          pivot_longer(cols = everything(), 
                       names_to = c("variable", "statistic"),
                       names_pattern = "([A-Za-z_]+)_(Mean)") %>% 
          select(-statistic) %>% 
          mutate(across(where(is.numeric), round, 3)) %>% 
          pivot_wider(names_from = variable, 
                      values_from = value) %>% 
          bind_rows(
            # India Imports
            import_data_IND %>% 
              select(tech = adopted_pay_or_ecom_before_2019, China_E_commerce, cons_BEC) %>% 
              na.omit() %>% 
              summarize_all(list(Mean = mean)) %>% 
              pivot_longer(cols = everything(), 
                           names_to = c("variable", "statistic"),
                           names_pattern = "([A-Za-z_]+)_(Mean)") %>% 
              select(-statistic) %>% 
              mutate(across(where(is.numeric), round, 3)) %>% 
              pivot_wider(names_from = variable, 
                          values_from = value)
          )
        
      )  %>% 
      bind_rows(
        # Mexico Exports
        export_data_MEX %>% 
          select(tech = adopted_pay_or_ecom_before_2019, China_E_commerce, cons_BEC) %>% 
          na.omit() %>% 
          summarize_all(list(Mean = mean)) %>% 
          pivot_longer(cols = everything(), 
                       names_to = c("variable", "statistic"),
                       names_pattern = "([A-Za-z_]+)_(Mean)") %>% 
          select(-statistic) %>% 
          mutate(across(where(is.numeric), round, 3)) %>% 
          pivot_wider(names_from = variable, 
                      values_from = value) %>% 
          bind_rows(
            # Mexico Imports
            import_data_MEX %>% 
              select(tech = adopted_pay_or_ecom_before_2019, China_E_commerce, cons_BEC) %>% 
              na.omit() %>% 
              summarize_all(list(Mean = mean)) %>% 
              pivot_longer(cols = everything(), 
                           names_to = c("variable", "statistic"),
                           names_pattern = "([A-Za-z_]+)_(Mean)") %>% 
              select(-statistic) %>% 
              mutate(across(where(is.numeric), round, 3)) %>% 
              pivot_wider(names_from = variable, 
                          values_from = value)
          )
        
      ) %>% 
      rename(`China E-commerce` = China_E_commerce, Consumable = cons_BEC, `E-payment or E-commerce 2019` = tech)
  ) %>% 
    tibble::column_to_rownames("Data") %>% 
    # Create Latex Table
    kbl(caption = "Summary statistics - Regression sample of all firms  (Never Adopters, Adopters during the Analysis Period, and Prior Adopters).", centering = T, booktabs = T, align = "c", 
        format = "latex") %>%
    kable_classic() %>%
    kable_styling(latex_options = c("HOLD_position", "scale_down")) %>% 
    pack_rows("Indonesia", 1, 2) %>%
    pack_rows("India", 3, 4) %>% 
    pack_rows("Mexico", 5, 6) %>% 
    column_spec(column = 8:10) %>% 
    add_header_above(c(" " = 2, "Export or Import Trade Value (USD)" = 5, " " = 3))%>% 
    footnote(general ="This table presents summary statistics for the regression sample of all types of firms - those that never adopted the technology, those that adopted it during the analysis period, and those that adopted prior to the analysis period. The details include counts of exporting and importing firms, distribution moments of trade values in USD, and the proportion of observations where the dummy variables for the Technology Variable (Pre-2019 adoption of E-payment or E-commerce), China E-commerce, and Consumable Product Categories are equal to 1.", threeparttable = TRUE)


capture.output(table_sample_2, file = "../../Outputs/Tables/summary_statistics_sample_2.tex")





