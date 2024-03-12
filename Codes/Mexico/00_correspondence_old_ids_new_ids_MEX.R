#===========================================================================#
# Date of last update: March 2024                                           #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in Mexico,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Description: This script creates correspondence tables for Mexico         #
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
new_ids_raw_names<-read_csv("../../Data/Mexico/raw_data/MEX_central_dataset_full_11-12.csv") %>% 
  rename(new_id = ID_domestic) %>% 
  select(domestic, new_id) %>% 
  na.omit(new_id) 

# Data with old IDs and raw firm names ---- 
old_ids_raw_names<-read.csv("../../Data/Mexico/raw_data/MEX_finalfirms_addresses.csv") %>%  
  rename(old_id = domestic_firm_id, 
         domestic = domestic_firm_name_raw) %>% 
  select(domestic, old_id) %>%
  # Drop duplicates because there are some firms with same IDs and raw names but different address, panjivaid etc
  distinct(domestic, old_id)

# Create correspondence table between old ids and new ids ----
corresp_table<-
  old_ids_raw_names %>% 
  # Join with data of new ids using the raw name 
  left_join(new_ids_raw_names, by = "domestic") %>% 
  arrange(old_id, new_id) %>% 
  # Identify those firms that have the same old id but different new IDs and 
  # drop them. This is to avoid many-to-many relationship when merging with 
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
write.csv(corresp_table, "../../Data/Mexico/processed_data/correspondence_table_old_new_ids_MEX.csv", 
          row.names = F)


# Notes----

# 1. There is a 0% of old IDs with no new ID correspondence. 
# 2. 98 old IDs --- 0.07% old IDs have multiple new IDs. 
# 


# USEFUL FOR WHEN I WANT TO MAKE THE CORRESPONDENCE FOR BUILTWITH
# 
# tech_data<-  read_parquet("../../Data/Mexico/processed_data/tech_data_MEX.parquet") %>%
#   left_join(corresp_table,
#              by = c("company_id" = "old_id")) %>%
#   filter(!is.na(new_id))