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
  full_join(new_ids_raw_names, by = "domestic") %>% 
  bind_rows(
    old_ids_raw_names_import %>% 
      # Join with data of new ids using the raw name 
      full_join(new_ids_raw_names, by = "domestic")
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




# Identify new firms 
new_firms<-corresp_table %>% 
  filter(is.na(old_id)) %>% 
  mutate(new_firm = 1) %>% 
  select(-old_id) %>% 
  group_by(new_id) %>% 
  mutate(n = n()) %>% 
  distinct(new_id, .keep_all = T) %>% 
  select(-domestic, -n)


# Load new datasets of Panjiva 
exports<-read_dta("../../Data/Indonesia/raw_data/IDN_exports_collapsed_dom_hs6.dta")


firm_exports<-exports %>% 
  group_by(domestic_firm_id) %>% 
  summarize(firm_export = sum(export)) %>% 
  left_join(new_firms, by = c("domestic_firm_id" = "new_id")) %>% 
  mutate(new_firm = ifelse(is.na(new_firm), 0, new_firm))


share_exports_new_firms<-firm_exports %>%
  group_by(new_firm) %>% 
  summarize(total_export = sum(firm_export)) %>% 
  ungroup() %>% 
  mutate(share = total_export/sum(total_export))




# Period of activity 
exports_by_month_new_firm<-exports %>% 
  left_join(new_firms, by = c("domestic_firm_id" = "new_id")) %>% 
  mutate(new_firm = ifelse(is.na(new_firm), 0, new_firm)) %>% 
  group_by(new_firm, year, month) %>% 
  summarize(export_new_firm = sum(export)) %>% 
  ungroup() %>% 
  group_by(year, month) %>% 
  mutate(share = export_new_firm/sum(export_new_firm))



p2<-exports_by_month_new_firm %>% 
  mutate(date = make_date(year, month, 1)) %>% 
  ggplot() +
  geom_line(aes(x = date, y = share, color = as.factor(new_firm))) +
  labs(color = "")+
  theme_minimal() +
  xlab("")+
  ggtitle("Indonesian share of exports represented by new firms and old firms")+
  ylab("Share of exports")+
  scale_color_manual(values = c("#023047", "#8ecae6"), labels = c("Old Firms", "New Firms"))+
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_y_continuous(n.breaks = 6) 


ggsave(p2,filename="../../Outputs/Graphs/indonesian_share_exports_new_firms.pdf", width=10, height=6, dpi = 300)




rm(tot_exports_new_firms, firm_exports, exports)
gc()


imports<-read_dta("../../Data/Indonesia/raw_data/IDN_imports_collapsed_dom_hs6.dta")


firm_imports<-imports %>% 
  group_by(domestic_firm_id) %>% 
  summarize(firm_import = sum(import)) %>% 
  left_join(new_firms, by = c("domestic_firm_id" = "new_id")) %>% 
  mutate(new_firm = ifelse(is.na(new_firm), 0, new_firm))


share_imports_new_firms<-firm_imports %>%
  group_by(new_firm) %>% 
  summarize(total_import = sum(firm_import)) %>% 
  ungroup() %>% 
  mutate(share = total_import/sum(total_import))


gc()


write_csv(new_firms, "../../Data/Indonesia/processed_data/new_firms_MEX.csv")

gc()


# Period of activity 
imports_by_month_new_firm<-imports %>% 
  left_join(new_firms, by = c("domestic_firm_id" = "new_id")) %>% 
  mutate(new_firm = ifelse(is.na(new_firm), 0, new_firm)) %>% 
  group_by(new_firm, year, month) %>% 
  summarize(import_new_firm = sum(import)) %>% 
  ungroup() %>% 
  group_by(year, month) %>% 
  mutate(share = import_new_firm/sum(import_new_firm))


p1<-imports_by_month_new_firm %>% 
  mutate(date = make_date(year, month, 1)) %>% 
  ggplot() +
  geom_line(aes(x = date, y = share, color = as.factor(new_firm))) +
  labs(color = "")+
  theme_minimal() +
  xlab("")+
  ggtitle("Indonesian share of imports represented by new firms and old firms")+
  ylab("Share of imports")+
  scale_color_manual(values = c("#023047", "#8ecae6"), labels = c("Old Firms", "New Firms"))+
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_y_continuous(n.breaks = 6) 


ggsave(p1,filename="../../Outputs/Graphs/indonesian_share_imports_new_firms.pdf", width=10, height=6, dpi = 300)


rm(firm_imports, imports)
