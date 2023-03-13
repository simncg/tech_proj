#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Script:  plots_trade_data_IND.R                                           #
#                                                                           #                                                                           #                                                                         
#                                                                           #                                             #           -                                                                                                                                                  #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
library(tidyverse)
library(ggplot2)
library(data.table)
library(scales)

# Imports data
import_data <-fread("../../Data/India/import_summaries_by_firm_month_HS_code_complete.csv") %>% 
  select(domestic_company_id, year, month, date, date_character,hs6, import) %>% 
  # Compute total imports and number of firms per month
  group_by(date_character) %>% 
  mutate(import_total = sum(import), 
         number_firms = n_distinct(domestic_company_id)) %>% 
  ungroup() %>% 
  distinct(date_character, .keep_all = T) %>% 
  select(date, date_character, import_total, number_firms)

gc()

# Exports data
export_data <-fread("../../Data/India/export_summaries_by_firm_month_HS_code_complete.csv") %>% 
  select(domestic_company_id, year, month, date, date_character,hs6, export) %>% 
  # Compute total imports and number of firms per month
  group_by(date_character) %>% 
  mutate(export_total = sum(export), 
         number_firms = n_distinct(domestic_company_id)) %>% 
  ungroup() %>% 
  distinct(date_character, .keep_all = T) %>% 
  select(date, date_character, export_total, number_firms)

gc()

# Plots, total imports and exports per year-month 
p1<-ggplot()+
  geom_line(data = import_data, aes(x = date, y = import_total, colour = "Import"))+
  geom_line(data = export_data, aes(x = date, y = export_total, colour = "Exports"))+
  ylab("")+
  xlab("Date")+
  ggtitle("Total Exports and Imports", 
          subtitle = "India")+
  theme_minimal() + 
  labs(colour = "")+
  theme(legend.position = "bottom", 
        axis.text.y = element_text(size=8), 
        axis.text.x = element_text(size=5, angle = 30),
        plot.title = element_text(face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(face="italic", hjust = 0.5)) +
  scale_colour_manual(values = c("navyblue", "orange")) +
  scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-9, sep="", prefix = "$"))+
  scale_x_date(date_breaks = "3 month", date_labels ="%Y-%b")


ggsave("../../Outputs/Graphs/Total imports and exports IND.pdf", width = 7, height = 5)

# Plots, importing and exporting firms per year-month 
p2<-ggplot()+
  geom_line(data = import_data, aes(x = date, y = number_firms, colour = "Importing firms"))+
  geom_line(data = export_data, aes(x = date, y = number_firms, colour = "Exporting firms"))+
  ylab("")+
  xlab("Date")+
  ggtitle("Number of exporting and importing firms", 
          subtitle = "India")+
  theme_minimal() + 
  labs(colour = "")+
  theme(legend.position = "bottom", 
        axis.text.y = element_text(size=8), 
        axis.text.x = element_text(size=4, angle = 30),
        plot.title = element_text(face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(face="italic", hjust = 0.5)) +
  scale_colour_manual(values = c("navyblue", "orange")) +
  scale_y_continuous(labels = scales::comma)+
  scale_x_date(date_breaks = "3 month", date_labels ="%Y-%b")

ggsave("../../Outputs/Graphs/Total importing and exporting firms IND.pdf", width = 7, height = 5)



  
  

