#===========================================================================#
# Date of last update: March 2024                                           #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Description: This script creates correspondence tables for Indonesia      #
#              between the old firm IDs and the new firm IDs                #
#                                                                           #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")

# Data with new IDs and raw firm names ----
new_ids_raw_names<-read_csv("../../Data/Indonesia/raw_data/IDN_central_dataset_full_11-12.csv") %>% 
  rename(new_id = ID_domestic) %>% 
  select(domestic, new_id) %>% 
  na.omit(new_id) 

# Data with old IDs and raw firm names ---- 

# Exporting firms 
old_ids_raw_names_export<-read.csv("../../Data/Indonesia/raw_data/IDN_Exports_Contact_Fin.csv") %>%  
  mutate(old_id = paste0("IDN", our_domestic_id)) %>% 
  select(domestic, old_id) %>% 
  distinct(domestic, old_id)

# Importing firms 
old_ids_raw_names_import<-read.csv("../../Data/Indonesia/raw_data/IDN_Imports_Contact_Fin.csv") %>% 
  mutate(old_id = paste0("IDN", our_domestic_id)) %>% 
  select(domestic, old_id) %>% 
  distinct(domestic, old_id)


# Create correspondence table between old ids and new ids ----
corresp_table<-
  old_ids_raw_names_export %>% 
  # Join with data of new ids using the raw name 
  left_join(new_ids_raw_names, by = "domestic") %>% 
  bind_rows(
    old_ids_raw_names_import %>% 
      # Join with data of new ids using the raw name 
      left_join(new_ids_raw_names, by = "domestic")
  ) %>% 
  # For those firms that are present both in the imports and exports dataset 
  # they will appear twice, then we will drop duplicates to just keep one record 
  # by raw firm name-old id and new id. 
  distinct(domestic, old_id, new_id, .keep_all = T) %>% 
  # Identify old IDs that represent multiple new IDs. This is to avoid 
  # many-to-many relationship when merging with 
  # Builtwith and Aberdeen.
  group_by(old_id) %>% 
  mutate(different_new_ids = n_distinct(new_id) > 1) %>% 
  ungroup() %>% 
  # Drop old IDs that represent multiple new IDs
  filter(!different_new_ids) %>% 
  select(-different_new_ids) %>% 
  # Keep just one observation per old_id and new_id, as it might be 
  # duplicates in terms of these variables because the raw firm name 
  # might be different. 
  distinct(new_id, old_id)


# Save file 
write.csv(corresp_table, "../../Data/Indonesia/processed_data/correspondence_table_old_new_ids_IDN.csv", 
          row.names = F)

# Notes -----
# 1. There is 3% of old IDs with no new ID correspondence
# 2. 0.91% of old IDs represent multiple new IDs 


# USEFUL FOR WHEN I WANT TO MAKE THE CORRESPONDENCE FOR BUILTWITH
# 
# tech_data<-  read_parquet("../../Data/Indonesia/processed_data/tech_data_IDN.parquet") %>% 
#    left_join(corresp_table,
#               by = c("company_id" = "old_id")) %>% 
#    filter(!is.na(new_id))
#  
# 
