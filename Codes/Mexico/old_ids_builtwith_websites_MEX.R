#===========================================================================#
# Date:    October 2023                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
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


write_dta(corresp_table, "../../Data/Mexico/processed_data/correspondence_table_complete_MEX.dta")




# Read RDS that includes the old_ids and the URL websites used to look for the information of technologies used in BuiltWith 
buitwith_old_ids<-readRDS("../../Data/Mexico/raw_data/all_data_list_v2.RDS")

# Process each data frame to select columns and the first row
processed_list <- lapply(buitwith_old_ids, function(df) df[1, c("D", "our_ID")])


# Combine all the processed data frames into one
old_ids_builtwith_websites <- bind_rows(processed_list) %>% 
  rename(builtwith_website = D, old_id = our_ID)

# Panjiva data enriched (after querying google API) (done by Laurent) that contains the raw name that he used to query the Google API ----
panjiva_enriched<-
  read_parquet("../../Data/Mexico/raw_data/Panjiva_data_clean_wide_and_enriched_Mexico.parquet") %>% 
  select(old_id = our_ID, panjiva_raw_firm_name = domestic_01)
  
# Merge with final_df
final_df<-left_join(old_ids_builtwith_websites, panjiva_enriched, by = c("old_id")) %>% 
  # Merge new IDs
  left_join(corresp_table, 
            by = c("old_id", "panjiva_raw_firm_name" = "domestic")) %>% 
  relocate(old_id, new_id, panjiva_raw_firm_name, builtwith_website, different_old_ids, different_new_ids)


# This data only includes cases where the new id has multiple old ids 
same_new_id_diff_old_ids_builtwith<-final_df %>% 
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
write_dta(final_df, "../../Data/Mexico/processed_data/old_ids_new_ids_builtwith_websites_MEX.dta")

# Save in dta
write_dta(same_new_id_diff_old_ids_builtwith, "../../Data/Mexico/processed_data/new_ids_with_multiple_old_ids_builtwith_websites_MEX.dta")





# Share of new IDs with multiple old IDs that have a clear website mode (unimodal) and share of new IDs that are multimodal
share_uni_multi_modal_builtwith<-same_new_id_diff_old_ids_builtwith %>% 
  # Not take into account those new IDs with multiple old IDs for which it was only possible to retrieve the builtwith website of one of the old IDs
  filter(only_one_url_retrieved==0) %>% 
  select(new_id, unimodal, is_multimodal) %>% 
  distinct(new_id, .keep_all = T) %>% 
  summarize(share_unimodal = mean(unimodal),
            share_multimodal = mean(is_multimodal))


# Summary statistics of share of old IDs with the same new IDs that have the same builtwith website (for unimodal cases) 
summary_stats_unimodal_builtwith<-same_new_id_diff_old_ids_builtwith %>% 
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
cases_for_fuzzy_builtwith<-same_new_id_diff_old_ids_builtwith %>% 
  # Not take into account those new IDs with multiple old IDs for which it was only possible to retrieve the builtwith website of one of the old IDs
  filter(only_one_url_retrieved==0, 
         # Keep multimodal observations 
         is_multimodal == 1) 


write_csv(cases_for_fuzzy_builtwith, "../../Data/Mexico/processed_data/new_ids_cases_for_fuzzy_builtwith_MEX.csv")




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


write.csv(one_url_retrieved, "../../Data/Mexico/processed_data/one_url_retrieved_cases_for_fuzzy_MEX.csv")
  


#########################################


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
aberdeen_data <- read_csv("../../Data/Mexico/raw_data/matched_Aberdeen_to_Panjiva_data_Mexico.csv") %>%
  rename(old_id = our_ID, 
         aberdeen_name = Aberdeen_COMPANY
  ) %>% 
  select(
    old_id,
    aberdeen_name
  ) %>% 
  # Merge panjiva raw firm names 
  left_join(
    old_ids_raw_names_filtered, 
    by = c("old_id")
  )  %>% 
  # Merge correspondence table 
  left_join(
    corresp_table, 
    by = c("old_id", "domestic")
  ) 


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
write_dta(aberdeen_data, "../../Data/Mexico/processed_data/old_ids_new_ids_aberdeen_names_MEX.dta")

# Save in dta
write_dta(same_new_id_diff_old_ids_aberdeen, "../../Data/Mexico/processed_data/new_ids_with_multiple_old_ids_aberdeen_names_MEX.dta")



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


write_csv(cases_for_fuzzy_aberdeen, "../../Data/Mexico/processed_data/new_ids_cases_for_fuzzy_aberdeen_MEX.csv")




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


write.csv(one_aberdeen_name_matched, "../../Data/Mexico/processed_data/one_aberdeen_name_matched_cases_for_fuzzy_MEX.csv")

