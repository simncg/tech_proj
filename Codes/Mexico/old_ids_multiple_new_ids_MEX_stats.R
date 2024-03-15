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
  ungroup() #%>% 
  # Drop old IDs that represent multiple new IDs
  #filter(!different_new_ids) %>% 
  #select(-different_new_ids) %>% 
  # Keep just one observation per old_id and new_id, as it might be 
  # duplicates in terms of these variables because the raw firm name 
  # might be different. 
 # select(-domestic) %>% 
#  distinct(new_id, old_id, .keep_all = T)




# Aberdeen data 
aberdeen_data <- read_csv("../../Data/Mexico/raw_data/matched_Aberdeen_to_Panjiva_data_Mexico.csv") %>%
  rename(company_id = our_ID)


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



relevant_cases<-aberdeen_data_2 %>% 
  filter(different_raw_names == T & different_new_ids == T) %>% 
  select(-different_new_ids, -different_raw_names)


# How many of the cases (old IDs with multiple new IDs) we dropped matched to Aberdeen?
tt<-corresp_table %>% 
  filter(different_new_ids) %>% 
  distinct(old_id) %>% 
  left_join(aberdeen_data %>% 
              select(old_id = company_id) %>% 
              mutate(in_aberdeen = 1), 
            by = "old_id")

(sum(tt$in_aberdeen, na.rm = T)/nrow(tt))*100
  



# How many of the cases (old IDs witj muliple new IDs) we dropped matched to Builtwith

tech_data<- read_parquet("../../Data/Mexico/processed_data/tech_data_MEX.parquet")

tt2<-corresp_table %>% 
  filter(different_new_ids) %>% 
  distinct(old_id) %>% 
  left_join(tech_data %>% 
              distinct(company_id)%>%
              select(old_id = company_id) %>% 
              mutate(in_builtwith = 1), 
            by = "old_id")


(sum(tt2$in_builtwith, na.rm = T)/nrow(tt2))*100

  



