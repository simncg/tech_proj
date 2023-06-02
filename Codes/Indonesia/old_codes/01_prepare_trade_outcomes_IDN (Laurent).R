#===========================================================================#
# Date of last update: May 2023                                             #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  prepare_trade_outcomes_IDN.R                                     #
#                                                                           #
# This program generates the trade outcomes to be used in the regression    #
# analysis. In particular, we generate the following outcomes: Log. Import  # 
# and Log. Export, number of distinct destinations or sources, dummy for    #
# exporting to a new destination or importing from a new source (we take a  #
# a baseline year to define the existing destinations/sources).             #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")






IDN_Exports_Monthly_foreign_domestic <-
  read_dta("updated_IDN_data/IDN_Exports_Monthly_foreign_domestic_level_dropdup.dta") %>%
  filter(!str_starts(hs, "27"))

IDN_Exports_Monthly_domestic <-
  read_dta("updated_IDN_data/IDN_Exports_Monthly_domestic_level_dropdup.dta") %>%
  filter(!str_starts(hs, "27"))


IDN_Imports_Monthly_foreign_domestic <-
  read_dta("updated_IDN_data/IDN_Imports_Monthly_foreign_domestic_level_dropdup.dta") %>%
  filter(!str_starts(hs, "27"))


IDN_Imports_Monthly_domestic <-
  read_dta("updated_IDN_data/IDN_Imports_Monthly_domestic_level_dropdup.dta") %>%
  filter(!str_starts(hs, "27"))




income_level_countries <- readxl::read_excel("enriched_data/OGHIST.xlsx", sheet = "2019_data")%>% 
  mutate(continent = countrycode::countrycode(country, origin = "iso3c", destination = "continent"))

# number of trade partners 
# number of countries
# log trade
# 


export_summary_by_HS_code <- IDN_Exports_Monthly_domestic %>%
  select(company_id  , year, month,  hs, export, country) %>%
  mutate(hs6 = substr(hs, 1, 6)) %>%
  select(-hs) %>%
  group_by(company_id  , year, month,  hs6) %>%
  summarise(export = sum(export, na.rm = TRUE),
            n_countries_export = n_distinct(country)) %>%
  ungroup() %>%
  mutate(log_export = log(export),
         export_dummy = TRUE) %>%
  mutate(date = ymd(paste0(year, "-", month, "-01"))) %>%
  mutate(date_labels = format(date, "%Y-%b")) %>%
  mutate(date_fact = as.factor(date_labels)) %>%
  mutate(date_fact = fct_reorder(date_fact, date)) %>%
  mutate(date_character = as.character(date_fact)) %>%
  select(-date_fact, -date_labels) %>%
  left_join(
    IDN_Exports_Monthly_foreign_domestic %>%
      select(
        domestic_company_id   ,
        foreign_company_id ,
        year,
        month,
        hs,
        export,
        country
      ) %>%
      mutate(hs6 = substr(hs, 1, 6)) %>%
      select(-hs) %>%
      group_by(domestic_company_id  , year, month,  hs6) %>%
      summarise(n_partner_export = n_distinct(foreign_company_id)) %>%
      ungroup()  %>%
      mutate(date = ymd(paste0(
        year, "-", month, "-01"
      ))) %>%
      mutate(date_labels = format(date, "%Y-%b")) %>%
      mutate(date_fact = as.factor(date_labels)) %>%
      mutate(date_fact = fct_reorder(date_fact, date)) %>%
      mutate(date_character = as.character(date_fact)) %>%
      select(-date_fact, -date_labels),
    by = c(
      "company_id" = "domestic_company_id",
      "date_character" = "date_character",
      "hs6" = "hs6",
      "year" = "year",
      "month" = "month",
      "date" = "date"
    )
  ) %>%
  left_join(
    IDN_Exports_Monthly_domestic %>%
      select(company_id  , year, month,  hs, export, country) %>%
      left_join(income_level_countries, by = c('country' = 'country')) %>%
      filter(!is.na(income_level)) %>%
      mutate(
        high_vs_rest = ifelse(income_level == "H", "high_export", "not_high_export")
      ) %>%
      mutate(hs6 = substr(hs, 1, 6)) %>%
      select(-hs) %>%
      group_by(company_id  , year, month,  hs6, high_vs_rest) %>%
      summarise(export = sum(export, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(log_export = log(export)) %>%
      select(-export) %>%
      pivot_wider(names_from = high_vs_rest, values_from = log_export),
    by = c("company_id", "year", "month", "hs6")
  ) %>%
  left_join(
    IDN_Exports_Monthly_domestic %>%
      select(company_id  , year, month,  hs, export, country) %>%
      left_join(income_level_countries, by = c('country' = 'country')) %>%
      filter(!is.na(income_level)) %>%
      mutate(
        high_vs_rest = ifelse(income_level %in% c("H", "UM") , "H_UM_export", "L_LM_export")
      ) %>%
      mutate(hs6 = substr(hs, 1, 6)) %>%
      select(-hs) %>%
      group_by(company_id  , year, month,  hs6, high_vs_rest) %>%
      summarise(export = sum(export, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(log_export = log(export)) %>%
      select(-export) %>%
      pivot_wider(names_from = high_vs_rest, values_from = log_export),
    by = c("company_id", "year", "month", "hs6")
  ) %>%
  left_join(
    IDN_Exports_Monthly_domestic %>%
      select(company_id  , year, month,  hs, export, country) %>%
      left_join(income_level_countries, by = c('country' = 'country')) %>%
      filter(!is.na(income_level)) %>%
      mutate(
        Asia_vs_rest = ifelse(continent %in% c("Asia"), "asia_export", "non_Asia_export")
      ) %>%
      mutate(hs6 = substr(hs, 1, 6)) %>%
      select(-hs) %>%
      group_by(company_id  , year, month,  hs6, Asia_vs_rest) %>%
      summarise(export = sum(export, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(log_export = log(export)) %>%
      select(-export) %>%
      pivot_wider(names_from = Asia_vs_rest, values_from = log_export),
    by = c("company_id", "year", "month", "hs6")
  ) %>% relocate(
    company_id,
    year,
    month,
    date,
    date_character,
    hs6,
    export,
    log_export,
    n_partner_export,
    n_countries_export,
    export_dummy
  ) %>%
  arrange(company_id, date)


import_summary_by_HS_code <- IDN_Imports_Monthly_domestic %>%
  select(company_id  , year, month,  hs, import, country) %>%
  mutate(hs6 = substr(hs, 1, 6)) %>%
  select(-hs) %>%
  group_by(company_id  , year, month,  hs6) %>%
  summarise(import = sum(import, na.rm = TRUE),
            n_countries_import = n_distinct(country)) %>%
  ungroup() %>%
  mutate(log_import = log(import),
         import_dummy = TRUE) %>%
  mutate(date = ymd(paste0(year, "-", month, "-01"))) %>%
  mutate(date_labels = format(date, "%Y-%b")) %>%
  mutate(date_fact = as.factor(date_labels)) %>%
  mutate(date_fact = fct_reorder(date_fact, date)) %>%
  mutate(date_character = as.character(date_fact)) %>%
  select(-date_fact, -date_labels) %>%
  left_join(
    IDN_Imports_Monthly_foreign_domestic %>%
      select(
        domestic_company_id   ,
        foreign_company_id ,
        year,
        month,
        hs,
        import,
        country
      ) %>%
      mutate(hs6 = substr(hs, 1, 6)) %>%
      select(-hs) %>%
      group_by(domestic_company_id  , year, month,  hs6) %>%
      summarise(n_partner_import = n_distinct(foreign_company_id)) %>%
      ungroup()  %>%
      mutate(date = ymd(paste0(
        year, "-", month, "-01"
      ))) %>%
      mutate(date_labels = format(date, "%Y-%b")) %>%
      mutate(date_fact = as.factor(date_labels)) %>%
      mutate(date_fact = fct_reorder(date_fact, date)) %>%
      mutate(date_character = as.character(date_fact)) %>%
      select(-date_fact, -date_labels),
    by = c(
      "company_id" = "domestic_company_id",
      "date_character" = "date_character",
      "hs6" = "hs6",
      "year" = "year",
      "month" = "month",
      "date" = "date"
    )
  ) %>%
  left_join(
    IDN_Imports_Monthly_domestic %>%
      select(company_id  , year, month,  hs, import, country) %>%
      left_join(income_level_countries, by = c('country' = 'country')) %>%
      filter(!is.na(income_level)) %>%
      mutate(
        high_vs_rest = ifelse(income_level == "H", "high_import", "not_high_import")
      ) %>%
      mutate(hs6 = substr(hs, 1, 6)) %>%
      select(-hs) %>%
      group_by(company_id  , year, month,  hs6, high_vs_rest) %>%
      summarise(import = sum(import, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(log_import = log(import)) %>%
      select(-import) %>%
      pivot_wider(names_from = high_vs_rest, values_from = log_import),
    by = c("company_id", "year", "month", "hs6")
  ) %>%
  left_join(
    IDN_Imports_Monthly_domestic %>%
      select(company_id  , year, month,  hs, import, country) %>%
      left_join(income_level_countries, by = c('country' = 'country')) %>%
      filter(!is.na(income_level)) %>%
      mutate(
        high_vs_rest = ifelse(income_level %in% c("H", "UM") , "H_UM_import", "L_LM_import")
      ) %>%
      mutate(hs6 = substr(hs, 1, 6)) %>%
      select(-hs) %>%
      group_by(company_id  , year, month,  hs6, high_vs_rest) %>%
      summarise(import = sum(import, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(log_import = log(import)) %>%
      select(-import) %>%
      pivot_wider(names_from = high_vs_rest, values_from = log_import),
    by = c("company_id", "year", "month", "hs6")
  ) %>%
  left_join(
    IDN_Imports_Monthly_domestic %>%
      select(company_id  , year, month,  hs, import, country) %>%
      left_join(income_level_countries, by = c('country' = 'country')) %>%
      filter(!is.na(income_level)) %>%
      mutate(
        Asia_vs_rest = ifelse(continent %in% c("Asia"), "asia_import", "non_Asia_import")
      ) %>%
      mutate(hs6 = substr(hs, 1, 6)) %>%
      select(-hs) %>%
      group_by(company_id  , year, month,  hs6, Asia_vs_rest) %>%
      summarise(import = sum(import, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(log_import = log(import)) %>%
      select(-import) %>%
      pivot_wider(names_from = Asia_vs_rest, values_from = log_import),
    by = c("company_id", "year", "month", "hs6")
  ) %>% relocate(
    company_id,
    year,
    month,
    date,
    date_character,
    hs6,
    import,
    log_import,
    n_partner_import,
    n_countries_import,
    import_dummy
  ) %>%
  arrange(company_id, date)






write_csv(export_summary_by_HS_code, "export_summary_by_firm_month_HS_code_updated.csv")
write_csv(import_summary_by_HS_code, "import_summary_by_firm_month_HS_code_updated.csv")



export_summary_by_firm <- IDN_Exports_Monthly_domestic %>%
  select(company_id  , year, month, export, country) %>%
  group_by(company_id  , year, month) %>%
  summarise(export = sum(export, na.rm = TRUE),
            n_countries_export = n_distinct(country)) %>%
  ungroup() %>%
  mutate(log_export = log(export),
         export_dummy = TRUE) %>%
  mutate(date = ymd(paste0(year, "-", month, "-01"))) %>%
  mutate(date_labels = format(date, "%Y-%b")) %>%
  mutate(date_fact = as.factor(date_labels)) %>%
  mutate(date_fact = fct_reorder(date_fact, date)) %>%
  mutate(date_character = as.character(date_fact)) %>%
  select(-date_fact, -date_labels) %>% left_join(
    IDN_Exports_Monthly_foreign_domestic %>%
      select(
        domestic_company_id   ,
        foreign_company_id ,
        year,
        month,
        hs,
        export,
        country
      ) %>%
      group_by(domestic_company_id  , year, month) %>%
      summarise(n_partner_export = n_distinct(foreign_company_id)) %>%
      ungroup()  %>%
      mutate(date = ymd(paste0(
        year, "-", month, "-01"
      ))) %>%
      mutate(date_labels = format(date, "%Y-%b")) %>%
      mutate(date_fact = as.factor(date_labels)) %>%
      mutate(date_fact = fct_reorder(date_fact, date)) %>%
      mutate(date_character = as.character(date_fact)) %>%
      select(-date_fact, -date_labels),
    by = c(
      "company_id" = "domestic_company_id",
      "date_character" = "date_character",
      "year" = "year",
      "month" = "month",
      "date" = "date"
    )
  ) %>%
  left_join(IDN_Exports_Monthly_domestic %>% 
              select(company_id  , year, month, export, country) %>%
              left_join(income_level_countries, by = c('country' ='country') ) %>% 
              filter(!is.na(income_level)) %>% 
              mutate(high_vs_rest = ifelse(income_level == "H", "high_export", "not_high_export")) %>% 
              group_by(company_id  , year, month, high_vs_rest) %>%
              summarise(export = sum(export, na.rm = TRUE)) %>%
              ungroup() %>%
              mutate(log_export = log(export)) %>% 
              select(-export) %>% 
              pivot_wider(names_from = high_vs_rest, values_from = log_export)) %>% 
  left_join(IDN_Exports_Monthly_domestic %>% 
              select(company_id  , year, month, export, country) %>%
              left_join(income_level_countries, by = c('country' ='country') ) %>% 
              filter(!is.na(income_level)) %>% 
              mutate(high_vs_rest = ifelse(income_level %in% c("H","UM") , "H_UM_export", "L_LM_export")) %>% 
              group_by(company_id  , year, month, high_vs_rest) %>%
              summarise(export = sum(export, na.rm = TRUE)) %>%
              ungroup() %>%
              mutate(log_export = log(export)) %>% 
              select(-export) %>% 
              pivot_wider(names_from = high_vs_rest, values_from = log_export)) %>% 
  left_join(IDN_Exports_Monthly_domestic %>% 
              select(company_id  , year, month, export, country) %>%
              left_join(income_level_countries, by = c('country' ='country') ) %>% 
              filter(!is.na(income_level)) %>% 
              mutate(Asia_vs_rest = ifelse(continent %in% c("Asia"), "asia_export", "non_Asia_export")) %>% 
              group_by(company_id  , year, month, Asia_vs_rest) %>%
              summarise(export = sum(export, na.rm = TRUE)) %>%
              ungroup() %>%
              mutate(log_export = log(export)) %>% 
              select(-export) %>% 
              pivot_wider(names_from = Asia_vs_rest, values_from = log_export)) %>% 
  relocate(
    company_id,
    year,
    month,
    date,
    date_character,
    export,
    log_export,
    n_partner_export,
    n_countries_export,
    export_dummy
  ) %>% 
  arrange(company_id, date)


import_summary_by_firm <- IDN_Imports_Monthly_domestic %>%
  select(company_id  , year, month, import, country) %>%
  group_by(company_id  , year, month) %>%
  summarise(import = sum(import, na.rm = TRUE),
            n_countries_import = n_distinct(country)) %>%
  ungroup() %>%
  mutate(log_import = log(import),
         import_dummy = TRUE) %>%
  mutate(date = ymd(paste0(year, "-", month, "-01"))) %>%
  mutate(date_labels = format(date, "%Y-%b")) %>%
  mutate(date_fact = as.factor(date_labels)) %>%
  mutate(date_fact = fct_reorder(date_fact, date)) %>%
  mutate(date_character = as.character(date_fact)) %>%
  select(-date_fact, -date_labels) %>% left_join(
    IDN_Imports_Monthly_foreign_domestic %>%
      select(
        domestic_company_id   ,
        foreign_company_id ,
        year,
        month,
        hs,
        import,
        country
      ) %>%
      group_by(domestic_company_id  , year, month) %>%
      summarise(n_partner_import = n_distinct(foreign_company_id)) %>%
      ungroup()  %>%
      mutate(date = ymd(paste0(
        year, "-", month, "-01"
      ))) %>%
      mutate(date_labels = format(date, "%Y-%b")) %>%
      mutate(date_fact = as.factor(date_labels)) %>%
      mutate(date_fact = fct_reorder(date_fact, date)) %>%
      mutate(date_character = as.character(date_fact)) %>%
      select(-date_fact, -date_labels),
    by = c(
      "company_id" = "domestic_company_id",
      "date_character" = "date_character",
      "year" = "year",
      "month" = "month",
      "date" = "date"
    )
  ) %>%
  left_join(IDN_Imports_Monthly_domestic %>% 
              select(company_id  , year, month, import, country) %>%
              left_join(income_level_countries, by = c('country' ='country') ) %>% 
              filter(!is.na(income_level)) %>% 
              mutate(high_vs_rest = ifelse(income_level == "H", "high_import", "not_high_import")) %>% 
              group_by(company_id  , year, month, high_vs_rest) %>%
              summarise(import = sum(import, na.rm = TRUE)) %>%
              ungroup() %>%
              mutate(log_import = log(import)) %>% 
              select(-import) %>% 
              pivot_wider(names_from = high_vs_rest, values_from = log_import)) %>% 
  left_join(IDN_Imports_Monthly_domestic %>% 
              select(company_id  , year, month, import, country) %>%
              left_join(income_level_countries, by = c('country' ='country') ) %>% 
              filter(!is.na(income_level)) %>% 
              mutate(high_vs_rest = ifelse(income_level %in% c("H","UM") , "H_UM_import", "L_LM_import")) %>% 
              group_by(company_id  , year, month, high_vs_rest) %>%
              summarise(import = sum(import, na.rm = TRUE)) %>%
              ungroup() %>%
              mutate(log_import = log(import)) %>% 
              select(-import) %>% 
              pivot_wider(names_from = high_vs_rest, values_from = log_import)) %>% 
  left_join(IDN_Imports_Monthly_domestic %>% 
              select(company_id  , year, month, import, country) %>%
              left_join(income_level_countries, by = c('country' ='country') ) %>% 
              filter(!is.na(income_level)) %>% 
              mutate(Asia_vs_rest = ifelse(continent %in% c("Asia"), "asia_import", "non_Asia_import")) %>% 
              group_by(company_id  , year, month, Asia_vs_rest) %>%
              summarise(import = sum(import, na.rm = TRUE)) %>%
              ungroup() %>%
              mutate(log_import = log(import)) %>% 
              select(-import) %>% 
              pivot_wider(names_from = Asia_vs_rest, values_from = log_import)) %>% 
  relocate(
    company_id,
    year,
    month,
    date,
    date_character,
    import,
    log_import,
    n_partner_import,
    n_countries_import,
    import_dummy
  ) %>% 
  arrange(company_id, date)

write_csv(export_summary_by_firm, "export_summary_by_firm_updated.csv")
write_csv(import_summary_by_firm, "import_summary_by_firm_updated.csv")

