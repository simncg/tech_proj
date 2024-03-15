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
  filter(!different_new_ids) %>% 
  select(-different_new_ids) %>%  # Drop old IDs with multiple new IDs. 
  # Keep just one observation per old_id and new_id, as it might be 
  # duplicates in terms of these variables because the raw firm name 
  # might be different. 
  distinct(new_id, old_id)


# Data is at the raw firm name-new id-old id level. It must be at this level 
# since the raw firm name-old id is the pair that has unique new id identifiers 
# An old id might have multiple new ids. 

# Percentage of old IDs with no correspondence
#(sum(is.na(corresp_table$new_id))/nrow(corresp_table))*100

# Save file 
write.csv(corresp_table, "../../Data/India/processed_data/correspondence_table_old_new_ids_IND.csv", 
          row.names = F)



# Note: 1. There is a 2.67% of old IDs with no new ID correspondence
#       2. 5% of old IDs has multiple new IDs. 