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
  # Identify new IDs that are represented by multiple old IDs
  group_by(new_id) %>%
  mutate(different_old_ids = n_distinct(old_id) > 1) %>%
  ungroup()




# Percentage of old IDs with multiple new IDs ----
problematic_old_ids<-
  corresp_table %>%
  distinct(
    old_id, .keep_all = T
  ) %>%
  summarize(
    multiple = mean(different_new_ids)*100
  )


# Percentage of new IDs with multiple old IDs ----
problematic_new_ids<-
  corresp_table %>%
  distinct(
    new_id, .keep_all = T
  ) %>%
  summarize(
    multiple = mean(different_old_ids)*100
  )


# Relevant cases to analyze ----- 

# Load Aberdeen data ----
aberdeen_data <- arrow::read_parquet("../../Data/Indonesia/raw_data/master_file_builtwith_updated.parquet",
                                     col_select = c("In_aberdeen","company_id","date", "date_character", 
                                                    "number_of_employees", "NAICS6_CODE", "SIC_group")) %>% 
  rename(EMPLE = number_of_employees) %>% 
  distinct(company_id, .keep_all = T) %>% 
  filter(In_aberdeen) %>%  
  select(-In_aberdeen, -date, -date_character)




# Load additional data of Aberdeen (to add more variables obtained from Aberdeen)
aberdeen_data_2 <- read_xlsx("../../Data/Indonesia/raw_data/matched_data_Indonesia.xlsx") 

# Correspondance table of IDs for Aberdeen of Indonesia
IDN_Domestic_Ids_Corresp <- read_xlsx("../../Data/Indonesia/raw_data/IDN_Domestic_Ids_Corresp_update.xlsx")

# Use correspondance, get SITEID and Aberdeen Company Name
aberdeen_data_2 <-IDN_Domestic_Ids_Corresp  %>% 
  select(prev_our_domestic_id, new_our_domestic_id ) %>% 
  distinct() %>% 
  filter(prev_our_domestic_id  %in% aberdeen_data_2$our_domestic_id) %>% 
  left_join(aberdeen_data_2,  by = c("prev_our_domestic_id" = "our_domestic_id")) %>% 
  rename(company_id = new_our_domestic_id) %>% 
  distinct(company_id, .keep_all = T) %>% 
  select(company_id, SITEID, Aberdeen_COMPANY)

# Join both datasets with Aberdeen data
aberdeen_data<-left_join(aberdeen_data, aberdeen_data_2, by = "company_id") %>% 
  relocate(company_id, SIC_group, SITEID) %>% 
  rename(SICGRP = SIC_group)

rm(aberdeen_data_2)


old_ids_raw_names<-
  old_ids_raw_names_export %>% 
  bind_rows(
    old_ids_raw_names_import
  ) %>% 
  distinct(domestic, old_id)
  



aberdeen_data_2<-
  aberdeen_data %>%
  select(company_id, Aberdeen_COMPANY) %>%
  rename(old_id = company_id) %>%
  left_join(
    old_ids_raw_names,
    by = "old_id"
  ) %>%
  group_by(old_id) %>%
  mutate(different_raw_names = n_distinct(domestic) > 1) %>%
  ungroup() %>%
  rename(aberdeen_name = Aberdeen_COMPANY, panjiva_raw_name = domestic) %>%
  left_join(
    new_ids_raw_names,
    by = c("panjiva_raw_name" = "domestic")
  ) %>%
  group_by(old_id) %>%
  mutate(different_new_ids = n_distinct(new_id) > 1) %>%
  ungroup()


# Cases with same old IDs, different new IDs and raw names
relevant_cases<-aberdeen_data_2 %>%
  filter(
    different_raw_names == T & different_new_ids == T
  ) %>%
  select(
    -different_new_ids, 
    -different_raw_names
  )

# Save data of relevant cases - same old IDs with multiple new IDs
write_csv(relevant_cases, "../../Data/Indonesia/processed_data/cases_old_id_multiple_new_ids_IDN.csv")




# IDN749 is the only problematic case because is the only that is also in aberdeen 
# and has different new IDs. 
tt<-corresp_table %>% 
  filter(different_new_ids) %>% 
  distinct(old_id,.keep_all = T) %>% 
  select(old_id, different_new_ids)

tt2<-
  aberdeen_data %>% 
  distinct(company_id) %>% 
  mutate(in_aberdeen = T) %>% 
  left_join(
    tt, 
    by = c("company_id" = "old_id")
  ) %>% 
  filter(in_aberdeen, different_new_ids)


