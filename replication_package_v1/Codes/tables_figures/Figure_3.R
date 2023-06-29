#===============================================================================#
# Date:    June 2023                                                        
#                                                                           
# Project: E-commerce and Trade during Crisis Times: Firm-level Evidence    
#          from India, Indonesia and Mexico                                 
#                                                                           
# This program generates charts on technology adoption. Figure 3.                  
#
# Inputs: - Data/processed_data/India/tech_data_IND.parquet
#         - Data/processed_data/India/imports_tech_mitigation_model_IND.csv
#         - Data/processed_data/India/exports_tech_mitigation_model_IND.csv
#         - Data/processed_data/Indonesia/tech_data_IDN.parquet
#         - Data/processed_data/Indonesia/imports_tech_mitigation_model_IDN.csv
#         - Data/processed_data/Indonesia/imports_tech_mitigation_model_IDN.csv
#         - Data/processed_data/Mexico/tech_data_MEX.parquet
#         - Data/processed_data/Mexico/imports_tech_mitigation_model_MEX.csv
#         - Data/processed_data/Mexico/exports_tech_mitigation_model_IDN.csv
#         - Data/Extra Data/OxCGRT_timeseries_all.xlsx
#
# Outputs: - Graphs/tech_adoption_v1.png
#          - Graphs/tech_adoption_v2.png
#                                                                           
#                                                                                                                                                                                                                                        #
#==============================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
library(data.table)
library(arrow)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(glue)


# India data ----

# India Technology data
tech_data_IND <- read_parquet("../../Data/processed_data/India/tech_data_IND.parquet", 
                              col_select = c("company_id", "date", "pay_or_ecomnod"))

gc()

# India Imports: (regression sample with all firms)
import_data_IND <-
  fread("../../Data/processed_data/India/imports_tech_mitigation_model_IND.csv", 
                                  select = "company_id") %>% 
  distinct() %>% 
  mutate(data_imp = "Imports")

# India Exports: (regression sample with all firms)
export_data_IND <- 
  fread("../../Data/processed_data/India/exports_tech_mitigation_model_IND.csv", 
                                  select = "company_id") %>% 
  distinct() %>% 
  mutate(data_exp = "Exports")
  
# India tech data 
tech_data_IND<-
  tech_data_IND %>% 
  left_join(import_data_IND, by = c("company_id")) %>% 
  left_join(export_data_IND, by = c("company_id")) %>% 
  filter(date >= ymd("2018-07-01"), 
         date < ymd("2022-01-01"))


# Filter to period of analysis 

rm(import_data_IND, export_data_IND)


# Indonesia Data ----

# Indonesia Technology data
tech_data_IDN <- read_parquet("../../Data/processed_data/Indonesia/tech_data_IDN.parquet", 
                              col_select = c("company_id", "date", "pay_or_ecomnod"))

gc()

# Indonesia Imports: (regression sample with all firms)
import_data_IDN <-
  fread("../../Data/processed_data/Indonesia/imports_tech_mitigation_model_IDN.csv", 
        select = "company_id") %>% 
  distinct() %>% 
  mutate(data_imp = "Imports")

# Indonesia Exports: (regression sample with all firms)
export_data_IDN <- 
  fread("../../Data/processed_data/Indonesia/exports_tech_mitigation_model_IDN.csv", 
        select = "company_id") %>% 
  distinct() %>% 
  mutate(data_exp = "Exports")

# Indonesia tech data 
tech_data_IDN<-
  tech_data_IDN %>% 
  left_join(import_data_IDN, by = c("company_id")) %>% 
  left_join(export_data_IDN, by = c("company_id")) %>% 
  filter(date >= ymd("2019-02-01"), 
         date < ymd("2021-10-01"))


rm(import_data_IDN, export_data_IDN)

# Mexico Data ---- 


# Mexico Technology data
tech_data_MEX <- read_parquet("../../Data/processed_data/Mexico/tech_data_MEX.parquet", 
                              col_select = c("company_id", "date", "pay_or_ecomnod"))

gc()

# Mexico Imports: (regression sample with all firms)
import_data_MEX <-
  fread("../../Data/processed_data/Mexico/imports_tech_mitigation_model_MEX.csv", 
        select = "company_id") %>% 
  distinct() %>% 
  mutate(data_imp = "Imports")

# Mexico Exports: (regression sample with all firms)
export_data_MEX <- 
  fread("../../Data/processed_data/Mexico/exports_tech_mitigation_model_MEX.csv", 
        select = "company_id") %>% 
  distinct() %>% 
  mutate(data_exp = "Exports")

# Mexico tech data 
tech_data_MEX<-
  tech_data_MEX %>% 
  left_join(import_data_MEX, by = c("company_id")) %>% 
  left_join(export_data_MEX, by = c("company_id")) %>% 
  filter(date >= ymd("2018-07-01"), 
         date < ymd("2022-01-01"))


rm(import_data_MEX, export_data_MEX)

# Read Data with Monthly Stringency Index for the 3 countries
stringency_index <- readxl::read_excel("../../Data/Extra Data/OxCGRT_timeseries_all.xlsx", sheet ="stringency_index"   )


# Get mean stringency index during Covid period
covid_data <- stringency_index %>%
  filter(country_name == "India") %>%
  pivot_longer(
    cols = -c(country_code, country_name),
    names_to = "date",
    values_to = "stringency_index"
  ) %>%
  mutate(date = parse_date_time(date, orders = "%d%b%Y")) %>%
  mutate(month_year = ymd(glue(
    "{year(date)}-{month(date)}-01"
  ))) %>%
  group_by(month_year) %>%
  summarise(month_mean_stringency_index  = mean(stringency_index , na.rm = TRUE)) %>% 
  mutate(country = "India") %>% 
  bind_rows(
    stringency_index %>%
      filter(country_name == "Indonesia") %>%
      pivot_longer(
        cols = -c(country_code, country_name),
        names_to = "date",
        values_to = "stringency_index"
      ) %>%
      mutate(date = parse_date_time(date, orders = "%d%b%Y")) %>%
      mutate(month_year = ymd(glue(
        "{year(date)}-{month(date)}-01"
      ))) %>%
      group_by(month_year) %>%
      summarise(month_mean_stringency_index  = mean(stringency_index , na.rm = TRUE)) %>% 
      mutate(country = "Indonesia")
  ) %>% 
  bind_rows(
    stringency_index %>%
      filter(country_name == "Mexico") %>%
      pivot_longer(
        cols = -c(country_code, country_name),
        names_to = "date",
        values_to = "stringency_index"
      ) %>%
      mutate(date = parse_date_time(date, orders = "%d%b%Y")) %>%
      mutate(month_year = ymd(glue(
        "{year(date)}-{month(date)}-01"
      ))) %>%
      group_by(month_year) %>%
      summarise(month_mean_stringency_index  = mean(stringency_index , na.rm = TRUE)) %>% 
      mutate(country = "Mexico")
  ) %>% 
  filter(month_year < as.Date("2022-01-01")) %>% 
  rename(date = month_year)
  

## Create plot of technology adoption for imports dataset ---- 
data_tech_adoption<-
  tech_data_IND %>% 
  # Filter to importing firms
  filter(data_imp == "Imports") %>% 
  group_by(date) %>% 
  # Share of technology adoption
  summarize(`E-commerce or E-payment` = mean(pay_or_ecomnod)) %>% 
  mutate(data = "Imports", 
         country = "India") %>% 
  bind_rows(
    tech_data_IND %>% 
      # Filter to exporting firms
      filter(data_exp == "Exports") %>% 
      group_by(date) %>% 
      # Share of technology adoption
      summarize(`E-commerce or E-payment` = mean(pay_or_ecomnod)) %>% 
      mutate(data = "Exports", 
             country = "India") 
  ) %>% 
  bind_rows(
    tech_data_IDN %>% 
      # Filter to importing firms
      filter(data_imp == "Imports", 
             date <= as.Date("2021-06-01")) %>% 
      group_by(date) %>% 
      # Share of technology adoption
      summarize(`E-commerce or E-payment` = mean(pay_or_ecomnod)) %>% 
      mutate(data = "Imports", 
             country = "Indonesia") %>% 
      bind_rows(
        tech_data_IDN %>% 
          # Filter to exporting firms
          filter(data_exp == "Exports") %>% 
          group_by(date) %>% 
          # Share of technology adoption
          summarize(`E-commerce or E-payment` = mean(pay_or_ecomnod)) %>% 
          mutate(data = "Exports", 
                 country = "Indonesia")
      )
  ) %>% 
  bind_rows(
    tech_data_MEX %>% 
      # Filter to importing firms
      filter(data_imp == "Imports") %>% 
      group_by(date) %>% 
      # Share of technology adoption
      summarize(`E-commerce or E-payment` = mean(pay_or_ecomnod)) %>% 
      mutate(data = "Imports", 
             country = "Mexico") %>% 
      bind_rows(
        tech_data_MEX %>% 
          # Filter to exporting firms
          filter(data_exp == "Exports") %>% 
          group_by(date) %>% 
          # Share of technology adoption
          summarize(`E-commerce or E-payment` = mean(pay_or_ecomnod)) %>% 
          mutate(data = "Exports", 
                 country = "Mexico")
      )
  ) %>% 
  # Join stringency indes
  left_join(
    covid_data, 
    by = c("country", "date")
  ) %>% 
  mutate(month_mean_stringency_index = ifelse(is.na(month_mean_stringency_index), 0,  month_mean_stringency_index))
  
  
# Create plots -----

# Set fonts
windowsFonts()
windowsFonts(`Open Sans` = windowsFont('Open Sans'))

# Version 1: show the actual usage (covering ALSO non-users and early adopters) in each time period


ggplot(data_tech_adoption)+
  geom_line(aes(x = date, y = `E-commerce or E-payment`, color = data))+
  geom_line(aes(x = date, y = month_mean_stringency_index/220, colour = "Avg. Stringency Index"))+
  facet_wrap(~country) +
  theme_light() +
  ylab("Share of firms with technology adopted")+
  xlab("")+
  labs(colour = "")+
  scale_colour_manual(values = c("gray80", "#83d0cb", "#145277"), 
                      labels = c("Avg. Stringency Index", "Exporters", "Importers"))+
  theme(
    legend.position = "bottom", 
    axis.text = element_text(size = 7.5),
    legend.text = element_text(family = "Open Sans"),
    legend.key.size = unit(1.5, "lines"), 
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14), 
    plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 10), 
    panel.grid.major = element_line(color = "gray97"), 
    panel.grid.minor = element_line(color = "gray93"), 
    axis.title.y = element_text(size = 7.5, family = "Open Sans"), 
    strip.background = element_blank(), 
    strip.text = element_text(color = "black", family = "Open Sans")
  ) +
  guides(colour = guide_legend(override.aes = list(size = 1.5)))+
  scale_y_continuous(n.breaks = 6, 
                     sec.axis = sec_axis(~.*220, name="Avg. Stringency Index"))



ggsave("../../Outputs/Graphs/tech_adoption_v1.png", dpi = 300,
       width = 6.5, height = 4.5, units = "in")



# Version 2: normalize the values to the tech adoption rate in the first period

data_tech_adoption<-
  data_tech_adoption %>% 
  group_by(country, data) %>% 
  # The tech adoption rate in the first period is the same as the minimum since 
  # the tech adoption rate is always increasing 
  mutate(normalized_tech_adop = `E-commerce or E-payment`/min(`E-commerce or E-payment`)) %>% 
  ungroup()





ggplot(data_tech_adoption)+
  geom_line(aes(x = date, y = normalized_tech_adop*100, color = data))+
  geom_line(aes(x = date, y = month_mean_stringency_index + 100, colour = "Avg. Stringency Index"))+
  facet_wrap(~country) +
  theme_light() +
  ylab("Normalized technology adoption rate index")+
  xlab("")+
  labs(colour = "")+
  scale_colour_manual(values = c("gray80", "#83d0cb", "#145277"), 
                      labels = c("Avg. Stringency Index", "Exporters", "Importers"))+
  theme(
    legend.position = "bottom", 
    axis.text = element_text(size = 7.5),
    legend.text = element_text(family = "Open Sans"),
    legend.key.size = unit(2, "lines"), 
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14), 
    plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 10), 
    panel.grid.major = element_line(color = "gray97"), 
    panel.grid.minor = element_line(color = "gray93"), 
    axis.title.y = element_text(size = 7.5, family = "Open Sans"), 
    strip.background = element_blank(), 
    strip.text = element_text(color = "black", family = "Open Sans")
  ) +
  #guides(colour = guide_legend(override.aes = list(size = 1.5)))+
  scale_y_continuous(n.breaks = 6, 
                     sec.axis = sec_axis(~.- 100, breaks = seq(0, 100, 20) ,name="Avg. Stringency Index"))



ggsave("../../Outputs/Graphs/tech_adoption_v2.png", dpi = 300,
       width = 6.5, height = 4.5, units = "in")
