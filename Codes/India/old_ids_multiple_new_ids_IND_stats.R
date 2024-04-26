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
  ungroup() %>%
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


# Note: 1. There is a 2.67% of old IDs with no new ID correspondence
#       2. 5% of old IDs has multiple new IDs.
#       3. 2.86% of new IDs has multiple old IDs.


# What percentage of export value represent this 5% of old IDs with multiple new IDs ----
exports<-read_dta("../../Data/India/raw_data/IND_exports_Monthly_dom_locmode.dta") %>%
  mutate(date = ymd(paste0(year, "-", month, "-01"))) %>%
  # Keep period of analysis and same period received in the previous version of Panjiva
  filter(date <= ymd("2022-03-31"))


exports_by_new_id<- exports %>%
  group_by(domestic_firm_id) %>%
  summarize(total_export = sum(export)) %>%
  ungroup()

gc()


share_export_mult_new_ids<-
  exports_by_new_id %>%
  left_join(
    corresp_table %>%
      distinct(old_id, new_id, .keep_all = T) %>%
      select(old_id, new_id, different_new_ids),
    by = c("domestic_firm_id" = "new_id")
   ) %>%
   rename(new_id = domestic_firm_id) %>%
   # After the left join new ids with multiple old ids generate duplicated 
   # rows -- just with a different old id. 
   distinct(new_id, .keep_all = T) %>% 
   group_by(different_new_ids) %>%
   summarize(export = sum(total_export)) %>%
   ungroup() %>%
   mutate(total_export = sum(export),
          share = export/total_export)


share_export_mult_old_ids<-
  exports_by_new_id %>%
  left_join(
    corresp_table %>%
      distinct(old_id, new_id, .keep_all = T) %>%
      select(old_id, new_id, different_old_ids),
    by = c("domestic_firm_id" = "new_id")
  ) %>%
  rename(new_id = domestic_firm_id) %>%
  # After the left join new ids with multiple old ids generate duplicated 
  # rows -- just with a different old id. 
  distinct(new_id, .keep_all = T) %>% 
  group_by(different_old_ids) %>%
  summarize(export = sum(total_export)) %>%
  ungroup() %>%
  mutate(total_export = sum(export),
         share = export/total_export)


imports<-read_dta("../../Data/India/raw_data/IND_imports_Monthly_dom_locmode.dta") %>%
  mutate(date = ymd(paste0(year, "-", month, "-01"))) %>%
  # Keep period of analysis and same period received in the previous version of Panjiva
  filter(date <= ymd("2022-03-31"))


imports_by_new_id<- imports %>%
  group_by(domestic_firm_id) %>%
  summarize(total_import = sum(import)) %>%
  ungroup()


share_import_mult_new_ids<-
  imports_by_new_id %>%
  left_join(
    corresp_table %>%
      distinct(old_id, new_id, .keep_all = T) %>%
      select(old_id, new_id, different_new_ids),
    by = c("domestic_firm_id" = "new_id")
  ) %>%
  rename(new_id = domestic_firm_id) %>%
  # After the left join new ids with multiple old ids generate duplicated 
  # rows -- just with a different old id. 
  distinct(new_id, .keep_all = T) %>%
  group_by(different_new_ids) %>%
  summarize(import = sum(total_import)) %>%
  ungroup() %>%
  mutate(total_import = sum(import),
         share = import/total_import)


share_import_mult_old_ids<-
  imports_by_new_id %>%
  left_join(
    corresp_table %>%
      distinct(old_id, new_id, .keep_all = T) %>%
      select(old_id, new_id, different_old_ids),
    by = c("domestic_firm_id" = "new_id")
  ) %>%
  rename(new_id = domestic_firm_id) %>%
  # After the left join new ids with multiple old ids generate duplicated 
  # rows -- just with a different old id. 
  distinct(new_id, .keep_all = T) %>%
  group_by(different_old_ids) %>%
  summarize(import = sum(total_import)) %>%
  ungroup() %>%
  mutate(total_import = sum(import),
         share = import/total_import)



################################################################

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
write_csv(relevant_cases, "../../Data/India/processed_data/cases_old_id_multiple_new_ids_IND.csv")

# How many firms or new IDs are represented by the old IDs with multiple new IDs 
n_firms_mult_new_ids<-
  corresp_table %>%
  filter(different_new_ids) %>%
  distinct(new_id) %>%
  # Total number of new IDs represented by the old IDs with multiple new IDs
  summarize(n_new_ids = n_distinct(new_id)) %>% 
  bind_cols(
    corresp_table %>%
      distinct(new_id) %>% 
      summarize(total_new_ids = n_distinct(new_id))
  ) %>% 
  mutate(share = n_new_ids/total_new_ids)







# # How many of the cases (old IDs with multiple new IDs) we dropped matched to Aberdeen?
# tt<-corresp_table %>% 
#   filter(different_new_ids) %>% 
#   distinct(old_id) %>% 
#   left_join(aberdeen_data %>% 
#               select(old_id = company_id) %>% 
#               mutate(in_aberdeen = 1), 
#             by = "old_id")
# 
# (sum(tt$in_aberdeen, na.rm = T)/nrow(tt))*100
# 
# 
# # How many of the cases (old IDs with muliple new IDs) we dropped matched to Builtwith
# 
# tech_data<- read_parquet("../../Data/India/processed_data/tech_data_IND.parquet")
# 
# tt2<-corresp_table %>% 
#   filter(different_new_ids) %>% 
#   distinct(old_id) %>% 
#   left_join(tech_data %>% 
#               distinct(company_id)%>%
#               select(old_id = company_id) %>% 
#               mutate(in_builtwith = 1), 
#             by = "old_id")
# 
# 
# (sum(tt2$in_builtwith, na.rm = T)/nrow(tt2))*100
# 
# 
# 
# rm(tech_data)
# gc()
# 
# 
# 
