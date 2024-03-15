#===========================================================================#
# Date of last update: March 2024                                           #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
# Description: This script creates correspondence tables for India          #
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
new_ids_raw_names<-read_csv("../../Data/India/raw_data/IND_central_dataset_full_19-02.csv") %>% 
  rename(new_id = ID_domestic) %>% 
  select(domestic, new_id) %>% 
  na.omit(new_id) 

# Data with old IDs and raw firm names ---- 
old_ids_raw_names<-read.csv("../../Data/India/raw_data/india_domestic_contact_all.csv") %>%  
  rename(old_id = ID) %>% 
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
  ungroup()
  # filter(!different_new_ids) %>% 
  # select(-different_new_ids) %>%  # Drop old IDs with multiple new IDs. 
  # # Keep just one observation per old_id and new_id, as it might be 
  # # duplicates in terms of these variables because the raw firm name 
  # # might be different. 
  # distinct(new_id, old_id)


# Data is at the raw firm name-new id-old id level. It must be at this level 
# since the raw firm name-old id is the pair that has unique new id identifiers 
# An old id might have multiple new ids. 

# Percentage of old IDs with no correspondence
#(sum(is.na(corresp_table$new_id))/nrow(corresp_table))*100


# Note: 1. There is a 2.67% of old IDs with no new ID correspondence
#       2. 5% of old IDs has multiple new IDs. 
# 



# Load matched Aberdeen to Panjiva data ----
aberdeen_data <- read_csv("../../Data/India/raw_data/matched_Aberdeen_to_Panjiva_data_India_v2.csv") %>%
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

tech_data<- read_parquet("../../Data/India/processed_data/tech_data_IND.parquet")

tt2<-corresp_table %>% 
  filter(different_new_ids) %>% 
  distinct(old_id) %>% 
  left_join(tech_data %>% 
              distinct(company_id)%>%
              select(old_id = company_id) %>% 
              mutate(in_builtwith = 1), 
            by = "old_id")


(sum(tt2$in_builtwith, na.rm = T)/nrow(tt2))*100



rm(tech_data)
gc()

# What percentage of trade value represent this 5% that we need to drop?
exports<-read_dta("../../Data/India/raw_data/IND_exports_Monthly_dom_locmode.dta") %>% 
  group_by(domestic_firm_id) %>% 
  summarize(total_export = sum(export))

gc()



export_case<-exports %>%
  ungroup() %>% 
  left_join(corresp_table %>% 
              filter(different_new_ids) %>% 
              distinct(new_id) %>% 
              mutate(problematic = 1), 
            by = c("domestic_firm_id" = "new_id")
            ) %>%
  group_by(problematic) %>% 
  summarize(export_value_group = sum(total_export)) %>%
  ungroup() %>% 
  mutate(total = sum(export_value_group), 
         share = export_value_group/total)
  
  
  

# It's 5% of old IDS, but each one of these old IDs represent multiple firms (multiple new IDs). Specifically, 
# they represent 16.8% new IDs (new firms). As seen below:


kaka<-corresp_table %>% 
  filter(different_new_ids) %>% 
  distinct(new_id) %>% 
  mutate(problematic = 1)

(nrow(kaka)/nrow(exports))*100 

  


