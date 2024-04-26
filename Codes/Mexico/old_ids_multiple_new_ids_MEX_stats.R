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



# Relevant cases of old IDs that match to Aberdeen but have multiple new IDs ----

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


# Save data of relevant cases - same old IDs with multiple new IDs
write_csv(relevant_cases, "../../Data/Mexico/processed_data/cases_old_id_multiple_new_ids_MEX.csv")




# What percentage of export value represent this 5% of old IDs with multiple new IDs ----
exports<-read_dta("../../Data/Mexico/raw_data/MEX_exports_Monthly_dom_locmode_hs6.dta") %>%
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

rm(exports, exports_by_new_id)
gc()


imports<-read_dta("../../Data/Mexico/raw_data/MEX_imports_Monthly_dom_locmode_hs6.dta") %>%
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



# Another way to compute share represented by new ids with multiple old ids for imports ---
kaka<-corresp_table %>% 
  filter(different_old_ids) %>% 
  distinct(new_id, .keep_all = T) %>% 
  select(new_id, different_old_ids) %>% 
  mutate(problematic = 1)


k2<-
  imports_by_new_id %>% 
  left_join(
    kaka, 
    by = c("domestic_firm_id" = "new_id")
  )

sum_rel<-
  k2 %>% 
  mutate(problematic = ifelse(is.na(problematic), 0, problematic)) %>% 
  group_by(problematic) %>% 
  summarize(t_imp = sum(total_import), 
            n_firms = n_distinct(domestic_firm_id)) %>% 
  ungroup() %>% 
  mutate(total = sum(t_imp), 
         share = t_imp/total)
  

View(k2 %>% 
       filter(problematic == 1) %>% 
       arrange(problematic, total_import)
     )




# Another way to compute share represented by new ids with multiple old ids for exports ---
k3<-
  exports_by_new_id %>% 
  left_join(
    kaka, 
    by = c("domestic_firm_id" = "new_id")
  )

sum_rel_exp<-
  k3 %>% 
  mutate(problematic = ifelse(is.na(problematic), 0, problematic)) %>% 
  group_by(problematic) %>% 
  summarize(t_exp = sum(total_export), 
            n_firms = n_distinct(domestic_firm_id)) %>% 
  ungroup() %>% 
  mutate(total = sum(t_exp), 
         share = t_exp/total)



# How many firms or new IDs are represented by the old IDs with multiple new IDs ---- 
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



# How many firms or new IDs have multiple old IDs  ---- 
n_firms_mult_old_ids<-
  corresp_table %>%
  filter(different_old_ids) %>%
  distinct(new_id) %>%
  # Total number of new IDs with multiple old IDs
  summarize(n_new_ids = n_distinct(new_id)) %>% 
  bind_cols(
    corresp_table %>%
      distinct(new_id) %>% 
      summarize(total_new_ids = n_distinct(new_id))
  ) %>% 
  mutate(share = n_new_ids/total_new_ids)



# 
# 
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
# 
# 
# # How many of the cases (old IDs witj muliple new IDs) we dropped matched to Builtwith
# 
# tech_data<- read_parquet("../../Data/Mexico/processed_data/tech_data_MEX.parquet")
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


