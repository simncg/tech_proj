#===========================================================================#
# Date:    June 2022                                                        #
#                                                                           #
# Project: E-commerce and Trade during Crisis Times: Firm-level             #
#          Evidence from India, Indonesia and Mexico                        # 
#                                                                           #
# This program generates Tables A.4 and A.5                                 #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)


# Format big numbers in tables
big_number_format<-function(x){format(x, big.mark = ",")}


# Libraries to be used ----
source("../src/packages.R")

# SAMPLE 1: Sample with never adopters and firms that adopted the technology during the period of analysis
# SAMPLE 2: Sample with all firms (firms that adopted the technology pre-2020, never adopters and after 2018)

# Regression sample of firms adopting the technology during period of analysis and never-adopters ----

# Imports and exports datasets of Indonesia used for regressions 
import_data_IDN<-as.data.frame(fread("../../Data/processed_data/Indonesia/imports_product_model_IDN.csv"))
export_data_IDN<-as.data.frame(fread("../../Data/processed_data/Indonesia/exports_product_model_IDN.csv"))

# Imports and exports datasets of India used for regressions
import_data_IND<-as.data.frame(fread("../../Data/processed_data/India/imports_product_model_IND.csv"))
export_data_IND<-as.data.frame(fread("../../Data/processed_data/India/exports_product_model_IND.csv"))


# Imports and exports datasets of Mexico used for regressions
import_data_MEX<-as.data.frame(fread("../../Data/processed_data/Mexico/imports_product_model_MEX.csv"))
export_data_MEX<-as.data.frame(fread("../../Data/processed_data/Mexico/exports_product_model_MEX.csv"))



table_sample_1<-
  export_data_IDN %>% 
  select(hs6, China_E_commerce, cons_BEC) %>% 
  summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), # Number of firm-hs-product observations with e-tradeable = 1
            n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% # Number of firm-hs-product observations with consumer BEC = 1
  mutate(measure = "No. Observations") %>% 
  bind_rows(
    export_data_IDN %>% 
      select(hs6, China_E_commerce, cons_BEC) %>% 
      distinct(hs6, .keep_all = T) %>% 
      summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), 
                n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% 
      mutate(measure = "No. HS6 Products")
  ) %>% 
  mutate(data = "Exports", 
         country = "Indonesia") %>%
  bind_rows(
    import_data_IDN %>% 
      select(hs6, China_E_commerce, cons_BEC) %>% 
      summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), # Number of firm-hs-product observations with e-tradeable = 1
                n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% # Number of firm-hs-product observations with consumer BEC = 1
      mutate(measure = "No. Observations") %>% 
      bind_rows(
        import_data_IDN %>% 
          select(hs6, China_E_commerce, cons_BEC) %>% 
          distinct(hs6, .keep_all = T) %>% 
          summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), 
                    n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% 
          mutate(measure = "No. HS6 Products")
      ) %>% 
      mutate(data = "Imports", 
             country = "Indonesia")
  ) %>% 
  bind_rows(
    export_data_IND %>% 
      select(hs6, China_E_commerce, cons_BEC) %>% 
      summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), # Number of firm-hs-product observations with e-tradeable = 1
                n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% # Number of firm-hs-product observations with consumer BEC = 1
      mutate(measure = "No. Observations") %>% 
      bind_rows(
        export_data_IND %>% 
          select(hs6, China_E_commerce, cons_BEC) %>% 
          distinct(hs6, .keep_all = T) %>% 
          summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), 
                    n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% 
          mutate(measure = "No. HS6 Products")
      ) %>% 
      mutate(data = "Exports", 
             country = "India") %>%
      bind_rows(
        import_data_IND %>% 
          select(hs6, China_E_commerce, cons_BEC) %>% 
          summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), # Number of firm-hs-product observations with e-tradeable = 1
                    n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% # Number of firm-hs-product observations with consumer BEC = 1
          mutate(measure = "No. Observations") %>% 
          bind_rows(
            import_data_IND %>% 
              select(hs6, China_E_commerce, cons_BEC) %>% 
              distinct(hs6, .keep_all = T) %>% 
              summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), 
                        n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% 
              mutate(measure = "No. HS6 Products")
          ) %>% 
          mutate(data = "Imports", 
                 country = "India")
      )
    
  ) %>% 
  bind_rows(
    export_data_MEX %>% 
      select(hs6, China_E_commerce, cons_BEC) %>% 
      summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), # Number of firm-hs-product observations with e-tradeable = 1
                n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% # Number of firm-hs-product observations with consumer BEC = 1
      mutate(measure = "No. Observations") %>% 
      bind_rows(
        export_data_MEX %>% 
          select(hs6, China_E_commerce, cons_BEC) %>% 
          distinct(hs6, .keep_all = T) %>% 
          summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), 
                    n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% 
          mutate(measure = "No. HS6 Products")
      ) %>% 
      mutate(data = "Exports", 
             country = "Mexico") %>%
      bind_rows(
        import_data_MEX %>% 
          select(hs6, China_E_commerce, cons_BEC) %>% 
          summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), # Number of firm-hs-product observations with e-tradeable = 1
                    n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% # Number of firm-hs-product observations with consumer BEC = 1
          mutate(measure = "No. Observations") %>% 
          bind_rows(
            import_data_MEX %>% 
              select(hs6, China_E_commerce, cons_BEC) %>% 
              distinct(hs6, .keep_all = T) %>% 
              summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), 
                        n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% 
              mutate(measure = "No. HS6 Products")
          ) %>% 
          mutate(data = "Imports", 
                 country = "Mexico")
      )
  ) %>% 
  relocate(measure) %>% 
  mutate(measure = paste0(measure, " ", seq_len(12)))%>% # Just to assign row names that do not repeat
  tibble::column_to_rownames("measure") %>% 
  mutate(across(c(n_measure_e_tradeable, n_measure_consumer), big_number_format)) %>% 
  rename(`E-tradeable Goods` = n_measure_e_tradeable, 
         `BEC Consumer Goods` = n_measure_consumer) %>% 
  select(-data, -country) %>% 
  kbl(caption = "Count of observations and products classified as E-tradeable Goods and BEC Consumer Goods at the year-month-firm-HS6 product level - Sample with firms that never adopt digital technology and firms that adopt digital technology in 2020-2021 period", 
      centering = T, booktabs = T, align = "c", format = "latex") %>%
  kable_styling(latex_options = c("HOLD_position")) %>% 
  group_rows("Panel A: Indonesia", 1, 4, hline_after = T) %>%
  group_rows("Exports", 1, 2, italic = T) %>%
  group_rows("Imports", 3, 4, italic = T) %>% 
  group_rows("Panel B: India", 5, 8, hline_after = T) %>%
  group_rows("Exports", 5, 6, italic = T) %>%
  group_rows("Imports", 7, 8, italic = T) %>% 
  group_rows("Panel C: Mexico", 9, 12, hline_after = T) %>%
  group_rows("Exports", 9, 10, italic = T) %>%
  group_rows("Imports", 11, 12, italic = T) %>%
  footnote(general = "", threeparttable = T)

capture.output(table_sample_1, file = "../../Outputs/Tables/count_e_tradeable_consumer_goods_sample1.tex")



rm(import_data_IDN, import_data_IND, import_data_MEX, export_data_IDN, export_data_IND, export_data_MEX)

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



table_sample_2<-
  export_data_IDN %>% 
  select(hs6, China_E_commerce, cons_BEC) %>% 
  summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), # Number of firm-hs-product observations with e-tradeable = 1
            n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% # Number of firm-hs-product observations with consumer BEC = 1
  mutate(measure = "No. Observations") %>% 
  bind_rows(
    export_data_IDN %>% 
      select(hs6, China_E_commerce, cons_BEC) %>% 
      distinct(hs6, .keep_all = T) %>% 
      summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), 
                n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% 
      mutate(measure = "No. HS6 Products")
  ) %>% 
  mutate(data = "Exports", 
         country = "Indonesia") %>%
  bind_rows(
    import_data_IDN %>% 
      select(hs6, China_E_commerce, cons_BEC) %>% 
      summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), # Number of firm-hs-product observations with e-tradeable = 1
                n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% # Number of firm-hs-product observations with consumer BEC = 1
      mutate(measure = "No. Observations") %>% 
      bind_rows(
        import_data_IDN %>% 
          select(hs6, China_E_commerce, cons_BEC) %>% 
          distinct(hs6, .keep_all = T) %>% 
          summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), 
                    n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% 
          mutate(measure = "No. HS6 Products")
      ) %>% 
      mutate(data = "Imports", 
             country = "Indonesia")
  ) %>% 
  bind_rows(
    export_data_IND %>% 
      select(hs6, China_E_commerce, cons_BEC) %>% 
      summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), # Number of firm-hs-product observations with e-tradeable = 1
                n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% # Number of firm-hs-product observations with consumer BEC = 1
      mutate(measure = "No. Observations") %>% 
      bind_rows(
        export_data_IND %>% 
          select(hs6, China_E_commerce, cons_BEC) %>% 
          distinct(hs6, .keep_all = T) %>% 
          summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), 
                    n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% 
          mutate(measure = "No. HS6 Products")
      ) %>% 
      mutate(data = "Exports", 
             country = "India") %>%
      bind_rows(
        import_data_IND %>% 
          select(hs6, China_E_commerce, cons_BEC) %>% 
          summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), # Number of firm-hs-product observations with e-tradeable = 1
                    n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% # Number of firm-hs-product observations with consumer BEC = 1
          mutate(measure = "No. Observations") %>% 
          bind_rows(
            import_data_IND %>% 
              select(hs6, China_E_commerce, cons_BEC) %>% 
              distinct(hs6, .keep_all = T) %>% 
              summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), 
                        n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% 
              mutate(measure = "No. HS6 Products")
          ) %>% 
          mutate(data = "Imports", 
                 country = "India")
      )
    
  ) %>% 
  bind_rows(
    export_data_MEX %>% 
      select(hs6, China_E_commerce, cons_BEC) %>% 
      summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), # Number of firm-hs-product observations with e-tradeable = 1
                n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% # Number of firm-hs-product observations with consumer BEC = 1
      mutate(measure = "No. Observations") %>% 
      bind_rows(
        export_data_MEX %>% 
          select(hs6, China_E_commerce, cons_BEC) %>% 
          distinct(hs6, .keep_all = T) %>% 
          summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), 
                    n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% 
          mutate(measure = "No. HS6 Products")
      ) %>% 
      mutate(data = "Exports", 
             country = "Mexico") %>%
      bind_rows(
        import_data_MEX %>% 
          select(hs6, China_E_commerce, cons_BEC) %>% 
          summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), # Number of firm-hs-product observations with e-tradeable = 1
                    n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% # Number of firm-hs-product observations with consumer BEC = 1
          mutate(measure = "No. Observations") %>% 
          bind_rows(
            import_data_MEX %>% 
              select(hs6, China_E_commerce, cons_BEC) %>% 
              distinct(hs6, .keep_all = T) %>% 
              summarize(n_measure_e_tradeable = sum(China_E_commerce, na.rm = T), 
                        n_measure_consumer = sum(cons_BEC, na.rm = T)) %>% 
              mutate(measure = "No. HS6 Products")
          ) %>% 
          mutate(data = "Imports", 
                 country = "Mexico")
      )
  ) %>% 
  relocate(measure) %>% 
  mutate(measure = paste0(measure, " ", seq_len(12)))%>% # Just to assign row names that do not repeat
  tibble::column_to_rownames("measure") %>% 
  mutate(across(c(n_measure_e_tradeable, n_measure_consumer), big_number_format)) %>% 
  rename(`E-tradeable Goods` = n_measure_e_tradeable, 
         `BEC Consumer Goods` = n_measure_consumer) %>% 
  select(-data, -country) %>% 
  kbl(caption = "Count of observations and products classified as E-tradeable Goods and BEC Consumer Goods at the year-month-firm-HS6 product level - Sample with all firms", 
      centering = T, booktabs = T, align = "c", format = "latex") %>%
  kable_styling(latex_options = c("HOLD_position")) %>% 
  group_rows("Panel A: Indonesia", 1, 4, hline_after = T) %>%
  group_rows("Exports", 1, 2, italic = T) %>%
  group_rows("Imports", 3, 4, italic = T) %>% 
  group_rows("Panel B: India", 5, 8, hline_after = T) %>%
  group_rows("Exports", 5, 6, italic = T) %>%
  group_rows("Imports", 7, 8, italic = T) %>% 
  group_rows("Panel C: Mexico", 9, 12, hline_after = T) %>%
  group_rows("Exports", 9, 10, italic = T) %>%
  group_rows("Imports", 11, 12, italic = T) %>%
  footnote(general = "", threeparttable = T)




capture.output(table_sample_2, file = "../../Outputs/Tables/count_e_tradeable_consumer_goods_sample_all_firms.tex")
