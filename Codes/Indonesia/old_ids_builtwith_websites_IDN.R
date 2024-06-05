#===========================================================================#
# Date:    April 2024                                                       #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in Indonesia,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
#                                                                           #                                       
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Identify the URLs associated to each old id that were matched to Builtwith. Identify if 
# the new ids with multiple old ids have the same URL addresses or not. 

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
  # IDs of export firms
  old_ids_raw_names_export %>% 
  # Join with data of new ids using the raw name 
  left_join(new_ids_raw_names, by = "domestic") %>% 
  bind_rows(
    # IDs of import firms
    old_ids_raw_names_import %>% 
      # Join with data of new ids using the raw name 
      left_join(new_ids_raw_names, by = "domestic")
  ) %>% 
  # For those firms that are present both in the imports and exports dataset 
  # they will appear twice, then we will drop duplicates to just keep one record 
  # by raw firm name-old id and new id. 
  distinct(domestic, old_id, new_id, .keep_all = T) %>% 
  # Identify old IDs with multiple new IDs 
  group_by(old_id) %>%
  mutate(different_new_ids = n_distinct(new_id) > 1) %>%
  ungroup() %>%
  # Identify new IDs with multiple old IDs 
  group_by(new_id) %>%
  mutate(different_old_ids = n_distinct(old_id) > 1) %>%
  ungroup() 


# Dataset with old IDs with no new IDs
old_ids_no_new_ids<-
  corresp_table %>% 
  filter(is.na(new_id)) %>% 
  select(-different_new_ids, -different_old_ids)

# Correspondence table removing old IDs that do not have new IDs
corresp_table<-corresp_table %>%
  filter(!is.na(new_id))

# Save data
write_dta(old_ids_no_new_ids, "../../Data/Indonesia/processed_data/old_ids_with_no_new_ids_IDN.dta")
write_dta(corresp_table, "../../Data/Indonesia/processed_data/correspondence_table_complete_IDN.dta")



# Read RDS that includes the old_ids and the URL websites used to look for the information of technologies used in BuiltWith 
builtwith_old_ids<-read_csv("../../Data/Indonesia/raw_data/Builtwith_IDN.csv") %>% 
  rename(old_id = New_ID_1, 
         builtwith_website = D) %>% 
  select(old_id, builtwith_website, year, month, ecom = ecom_nod, payrobust = pay_robust_nod)


# 1. Multiple websites for the same ID 
# 2. If you check old_ids_raw_names_export and the builtwith_old_ids you can see that 
#    the ID is assigned to a totally different website than it should. For example, 
#    IDN51 is Hino in old_ids_raw_names_export but in the builtwith_old_ids it is 
#    associated with IDN52. See below what is happening here:


# Drop second website when old ID has two websites 
mult_websites<-
  builtwith_old_ids %>% 
  distinct(old_id, builtwith_website) %>% 
  group_by(old_id) %>% 
  mutate(different_websites = n_distinct(builtwith_website) > 1) %>% 
  filter(different_websites == T) %>% 
  group_by(old_id) %>% 
  mutate(n = row_number()) 

mult_websites_to_drop<-
  mult_websites %>% 
  filter(n == 2) %>% 
  select(old_id, builtwith_website) %>% 
  mutate(drop_obs = 1)

builtwith_old_ids<-
  builtwith_old_ids %>% 
  left_join(
    mult_websites_to_drop, 
    by = c("old_id", "builtwith_website")
  ) %>% 
  mutate(drop_obs = ifelse(is.na(drop_obs), 0, drop_obs)) %>% 
  filter(drop_obs != 1)



# This data shows the inconsistency between IDs, names and websites 
inspection<-
  old_ids_raw_names_export %>% 
  bind_rows(
    # IDs of import firms
    old_ids_raw_names_import
  ) %>% 
  # For those firms that are present both in the imports and exports dataset 
  # they will appear twice, then we will drop duplicates to just keep one record 
  # by raw firm name-old id and new id. 
  distinct(domestic, old_id, .keep_all = T) %>% 
  left_join(
    builtwith_old_ids %>% 
      select(old_id, builtwith_website) %>% 
      distinct(old_id, builtwith_website),
    by = c("old_id")
  ) %>% 
  left_join(
    mult_websites %>% 
      distinct(old_id) %>% 
      mutate(mult_website = T), 
    by = "old_id"
  )




builtiwith_websites_panjiva<-inspection %>% 
  filter(!is.na(builtwith_website)) %>% 
  select(-mult_website)%>% 
  # Merge new IDs
  left_join(corresp_table, 
            by = c("old_id", "domestic")) %>% 
  rename(panjiva_raw_firm_name = domestic) %>% 
  relocate(old_id, new_id, panjiva_raw_firm_name, builtwith_website, different_old_ids, different_new_ids) %>% 
  filter(!is.na(new_id))


# At the beginning the website in the previous ID makes sense, but then the website simply 
# does not make any sense. Take into account this if we want to use score. Cannot use score 
# in Indonesia then. 


# # Now we want to see if the variables are the same in the tech_data and in builtwith_old_ids
# tech_data<-read_parquet("../../Data/Indonesia/processed_data/tech_data_IDN.parquet") %>% 
#   filter(date>=ymd("2019-01-01"), 
#          date < ymd("2022-01-01")) %>% 
#   mutate(year = year(date), 
#          month = month(date)) %>% 
#   select(old_id = company_id, year, month, ecom, payrobust)
# 
# 
# kaka<-builtwith_old_ids %>% 
#   left_join(
#     tech_data %>% 
#       mutate(merge = 1), 
#     by = c("old_id", "year", "month", "ecom", "payrobust")
#   ) %>% 
#   mutate(merge = ifelse(is.na(merge), 0, merge)) %>% 
#   left_join(
#     mult_websites %>% 
#       distinct(old_id) %>% 
#       mutate(mult_website = T), 
#     by = "old_id"
#   )
# 
# 
# # We could simply ignore those cases that did not match. 
# (table(kaka$merge)/nrow(kaka))*100
# 
# 
# 
# diff<-kaka %>% 
#   group_by(old_id) %>% 
#   summarize(different = ifelse((n_distinct(merge) > 1), 1, 0)) %>% 
#   left_join(
#     mult_websites %>% 
#       distinct(old_id) %>% 
#       mutate(mult_website = T), 
#     by = "old_id"
#   ) %>% 
#   mutate(order = as.numeric(str_replace(old_id, "IDN", "")), 
#          mult_website = ifelse(is.na(mult_website), FALSE, mult_website)) %>% 
#   arrange(order) %>% 
#   select(-order)


# This data only includes cases where the new id has multiple old ids 
same_new_id_diff_old_ids_builtwith<-builtiwith_websites_panjiva %>% 
  filter(different_old_ids) %>% 
  select(-different_new_ids) %>% 
  # For each new id with multiple old ids, we compute the proportion of 
  # old ids with the same URL address.
  group_by(new_id, builtwith_website) %>%
  mutate(# Count the number of old IDs within the same new ID that have the same URL address
    n_url = n()
  ) %>% 
  ungroup() %>% 
  group_by(new_id) %>% 
  mutate(
    n_total_old_ids = n(), 
    mode_url_count = max(n_url),
    share_url_mode = ifelse(n_total_old_ids == 1, NA, mode_url_count/n_total_old_ids),
    obs_has_mode = ifelse(n_url == mode_url_count, 1, 0), 
    # If it is not multimodal then number of observations that have the mode will be equal to the count of the URL mode
    # The sum of observations that have mode should be different of 2 because this is the case where there is only two observations 
    # and the distribution is bimodal (two modes because two observations with two different builtwith websites)
    is_multimodal = ifelse( (sum(obs_has_mode) > mode_url_count), 1, 0), 
    # Is there a clear mode (unique_mode)
    unimodal = ifelse(sum(obs_has_mode) == mode_url_count, 1, 0), 
    # Replace with missings share of URL mode if it is multimodal 
    share_url_mode = ifelse(is_multimodal == 1, NA, share_url_mode)
  ) %>%
  ungroup() %>%
  # For the new ids that have multiple old ids in the correspondence table, but that then it was not possible to find a 
  # builtwith website for one of these old ids, and then we only have one old id with builtwith website. The final builtwith 
  # website that will be assigned is the only one that we were able to retrieve. 
  mutate(only_one_url_retrieved = ifelse(n_total_old_ids == 1, 1, 0)) %>% 
  #filter(only_one_url_retrieved!=1) %>% 
  arrange(new_id, builtwith_website)
  

# Save in dta
write_dta(builtiwith_websites_panjiva, "../../Data/Indonesia/processed_data/old_ids_new_ids_builtwith_websites_IDN.dta")

# Save in dta
write_dta(same_new_id_diff_old_ids_builtwith, "../../Data/Indonesia/processed_data/new_ids_with_multiple_old_ids_builtwith_websites_IDN.dta")




# Share of new IDs with multiple old IDs that have a clear website mode (unimodal) and share of new IDs that are multimodal
share_uni_multi_modal<-same_new_id_diff_old_ids_builtwith %>% 
  # Not take into account those new IDs with multiple old IDs for which it was only possible to retrieve the builtwith website of one of the old IDs
  filter(only_one_url_retrieved==0) %>% 
  select(new_id, unimodal, is_multimodal) %>% 
  distinct(new_id, .keep_all = T) %>% 
  summarize(share_unimodal = mean(unimodal),
            share_multimodal = mean(is_multimodal))


# Summary statistics of share of old IDs with the same new IDs that have the same builtwith website (for unimodal cases) 
summary_stats_unimodal<-same_new_id_diff_old_ids_builtwith %>% 
  filter(only_one_url_retrieved==0) %>% 
  filter(unimodal == 1) %>% 
  summarize(min = min(share_url_mode), 
            p5 = quantile(share_url_mode, 0.05),
            p10 = quantile(share_url_mode, 0.1),
            p25 = quantile(share_url_mode,0.25), 
            median = quantile(share_url_mode,0.5), 
            p75 = quantile(share_url_mode, 0.75), 
            p90 = quantile(share_url_mode, 0.9),
            max = max(share_url_mode))

# Share of new IDs with multiple old IDs with only one old ID for which it was possible to retrieve a builtwith website. 
share_one_website_retrieved<-same_new_id_diff_old_ids_builtwith %>% 
  distinct(new_id, .keep_all = T) %>% 
  summarize(share_one_retrieved = mean(only_one_url_retrieved))
  

# Do the fuzzy matching in Python for cases where there are 2 old IDs with the----
# same new ID, and both have a different builtwith website. 
cases_for_fuzzy<-same_new_id_diff_old_ids_builtwith %>% 
  # Not take into account those new IDs with multiple old IDs for which it was only possible to retrieve the builtwith website of one of the old IDs
  filter(only_one_url_retrieved==0, 
  # Keep multimodal observations 
         is_multimodal == 1) 
  
  
write_csv(cases_for_fuzzy, "../../Data/Indonesia/processed_data/new_ids_cases_for_fuzzy_builtwith_IDN.csv")




# Shares 
shares<-same_new_id_diff_old_ids_builtwith %>% 
  mutate(
    type = 
      case_when(
        is_multimodal == 1 ~ "multimodal", 
        unimodal == 1 & only_one_url_retrieved == 0 ~ "unimodal", 
        only_one_url_retrieved == 1  ~ "only_one_url_retrieved"  
      )
  ) %>% 
  distinct(new_id, .keep_all = T) %>% 
  select(new_id, type) %>%
  group_by(type) %>% 
  summarize(
    n = n()
  ) %>% 
  ungroup() %>% 
  mutate(shares = n / sum(n))



# Create a dataset with the new ids with multiple old ids but for the cases where only one url was retrieved 

one_url_retrieved<-same_new_id_diff_old_ids_builtwith %>% 
  filter(only_one_url_retrieved == 1) 


write.csv(one_url_retrieved, "../../Data/Indonesia/processed_data/one_url_retrieved_cases_for_fuzzy_IDN.csv")



# HERE ----

# Do the same for Aberdeen -----------------------


# 2.	In the cases where you have multiple different raw Panjiva names with the same ID, which of these multiple raw names did you use for matching? Or did you perform the matching for each one separately (name by name)? Or did you just keep one?
#   a.	In case there was more than one raw name, the first two raw names were used (in order they appeared in the dataset) to determine the matching, in which they were treated with equal weight (as they are considered to be the same ‘firm’ anyway and the two names might offer additional information. In the scoring algorithm, effectively only the name that would create most points was used. 
#             


old_ids_raw_names<-
  old_ids_raw_names_export %>% 
  bind_rows(
    # IDs of import firms
    old_ids_raw_names_import
  ) %>% 
  # For those firms that are present both in the imports and exports dataset 
  # they will appear twice, then we will drop duplicates to just keep one record 
  # by raw firm name-old id and new id. 
  distinct(domestic, old_id, .keep_all = T) 



# Keep just raw names used in the matching
old_ids_raw_names_filtered<-old_ids_raw_names %>% 
  group_by(old_id) %>% 
  mutate(position = row_number()) %>% 
  ungroup() %>% 
  arrange(old_id) %>% 
  # When an old ID has multiple raw names, Laurent only used the first two raw 
  # names for the matching process with aberdeen. We keep only the first raw name
  # for simplicity of the exercise
  filter(position==1) %>% 
  select(old_id, domestic) 

# Load matched Aberdeen to Panjiva data

# Load Aberdeen data ----
aberdeen_data <- arrow::read_parquet("../../Data/Indonesia/raw_data/master_file_builtwith_updated.parquet",
                                     col_select = c("In_aberdeen","company_id")) %>% 
  distinct(company_id, .keep_all = T) %>% 
  filter(In_aberdeen) %>%  
  select(-In_aberdeen, old_id = company_id)




# Load additional data of Aberdeen (to add more variables obtained from Aberdeen)
aberdeen_data_2 <- read_xlsx("../../Data/Indonesia/raw_data/matched_data_Indonesia.xlsx") 

# Correspondance table of IDs for Aberdeen of Indonesia
IDN_Domestic_Ids_Corresp <- read_xlsx("../../Data/Indonesia/raw_data/IDN_Domestic_Ids_Corresp_update.xlsx")

# Use correspondance. get Aberdeen Company Name
aberdeen_data_2 <-IDN_Domestic_Ids_Corresp  %>% 
  select(prev_our_domestic_id, new_our_domestic_id ) %>% 
  distinct() %>% 
  filter(prev_our_domestic_id  %in% aberdeen_data_2$our_domestic_id) %>% 
  left_join(aberdeen_data_2,  by = c("prev_our_domestic_id" = "our_domestic_id")) %>% 
  rename(company_id = new_our_domestic_id) %>% 
  distinct(company_id, .keep_all = T) %>% 
  select(old_id = company_id,  aberdeen_name = Aberdeen_COMPANY)

# Join both datasets with Aberdeen data
aberdeen_data<-left_join(aberdeen_data, aberdeen_data_2, by = "old_id") %>% 
  relocate(old_id) %>% 
  left_join(
    old_ids_raw_names_filtered, 
    by = c("old_id")
  )  %>% 
  # Merge correspondence table 
  left_join(
    corresp_table, 
    by = c("old_id", "domestic")
  ) %>% 
  # Drop old IDs with no new IDs or no panjiva raw name
  filter(!is.na(new_id))
  


rm(aberdeen_data_2, IDN_Domestic_Ids_Corresp, old_ids_raw_names_filtered)



same_new_id_diff_old_ids_aberdeen<-aberdeen_data %>% 
  filter(different_old_ids) %>% 
  select(-different_new_ids) %>% 
  arrange(new_id) %>% 
  # For each new id with multiple old ids, we compute the proportion of 
  # old ids with the same aberdeen name
  group_by(new_id, aberdeen_name) %>%
  mutate(# Count the number of old IDs within the same new ID that have the same aberdeen name
    n_aberdeen_name = n()
  ) %>% 
  ungroup() %>% 
  group_by(new_id) %>% 
  mutate(
    n_total_old_ids = n(), 
    mode_aberdeen_count = max(n_aberdeen_name),
    share_aberdeen_mode = ifelse(n_total_old_ids == 1, NA, mode_aberdeen_count/n_total_old_ids),
    obs_has_mode = ifelse(n_aberdeen_name == mode_aberdeen_count, 1, 0), 
    # If it is not multimodal then number of observations that have the mode will be equal to the count of the aberdeen mode
    # The sum of observations that have mode should be different of 2 because this is the case where there is only two observations 
    # and the distribution is bimodal (two modes because two observations with two different builtwith websites)
    is_multimodal = ifelse( (sum(obs_has_mode) > mode_aberdeen_count), 1, 0), 
    # Is there a clear mode (unique_mode)
    unimodal = ifelse(sum(obs_has_mode) == mode_aberdeen_count, 1, 0), 
    # Replace with missings share of aberdeen mode if it is multimodal 
    share_aberdeen_mode = ifelse(is_multimodal == 1, NA, share_aberdeen_mode)
  ) %>%
  ungroup() %>%
  # For the new ids that have multiple old ids in the correspondence table, but
  # that then it was not possible to find an aberdeen matching name for some of 
  # these old ids, and then we only have one old id with aberdeen name. 
  # The final aberdeen name that will be assigned is the only one that we were able to match. 
  mutate(only_one_aberdeen_matched = ifelse(n_total_old_ids == 1, 1, 0)) %>% 
  #filter(only_one_aberdeen_retrieved!=1) %>% 
  arrange(new_id, aberdeen_name)


# Save in dta
write_dta(aberdeen_data, "../../Data/Indonesia/processed_data/old_ids_new_ids_aberdeen_names_IDN.dta")

# Save in dta
write_dta(same_new_id_diff_old_ids_aberdeen, "../../Data/Indonesia/processed_data/new_ids_with_multiple_old_ids_aberdeen_names_IDN.dta")



# Summary statistics of share of old IDs with the same new IDs that have the same builtwith website (for unimodal cases) 
summary_stats_unimodal_aberdeen<-same_new_id_diff_old_ids_aberdeen %>% 
  filter(only_one_aberdeen_matched==0) %>% 
  filter(unimodal == 1) %>% 
  summarize(min = min(share_aberdeen_mode), 
            p5 = quantile(share_aberdeen_mode, 0.05),
            p10 = quantile(share_aberdeen_mode, 0.1),
            p25 = quantile(share_aberdeen_mode,0.25), 
            median = quantile(share_aberdeen_mode,0.5), 
            p75 = quantile(share_aberdeen_mode, 0.75), 
            p90 = quantile(share_aberdeen_mode, 0.9),
            max = max(share_aberdeen_mode))


# Do the fuzzy matching in Python for cases where there are 2 old IDs with the----
# same new ID, and both have a different builtwith website. 
cases_for_fuzzy_aberdeen<-same_new_id_diff_old_ids_aberdeen %>% 
  # Not take into account those new IDs with multiple old IDs for which it was only possible to retrieve the builtwith website of one of the old IDs
  filter(only_one_aberdeen_matched==0, 
         # Keep multimodal observations 
         is_multimodal == 1) 


write_csv(cases_for_fuzzy_aberdeen, "../../Data/Inonesia/processed_data/new_ids_cases_for_fuzzy_aberdeen_IDN.csv")




# Shares 
shares_aberdeen<-same_new_id_diff_old_ids_aberdeen %>% 
  mutate(
    type = 
      case_when(
        is_multimodal == 1 ~ "multimodal", 
        unimodal == 1 & only_one_aberdeen_matched == 0 ~ "unimodal", 
        only_one_aberdeen_matched == 1  ~ "only_one_aberdeen_matched"  
      )
  ) %>% 
  distinct(new_id, .keep_all = T) %>% 
  select(new_id, type) %>%
  group_by(type) %>% 
  summarize(
    n = n()
  ) %>% 
  ungroup() %>% 
  mutate(shares = n / sum(n))



one_aberdeen_name_matched<-same_new_id_diff_old_ids_aberdeen %>% 
  filter(only_one_aberdeen_matched == 1) 


write.csv(one_aberdeen_name_matched, "../../Data/Indonesia/processed_data/one_aberdeen_name_matched_cases_for_fuzzy_IDN.csv")


