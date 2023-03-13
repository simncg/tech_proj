# Using quarters
# 1671038880 = 1.67 billion rows (Exports India)
# 1659373856 = 1.65 billion rows (Imports India)

# Using months 
# 4978121568 = 4.97 billion rows (Imports India)
# 5013116640 = 5.01 billion rows (Exports India)

# Average number of products exported per month

# Firms that import yearly less than 1000 usd in the years they appear  
import_less_than_1000usd<-read_csv("../../Data/India/processed_data/import_firms_trade_less_than_1000usd.csv")

import_data <-fread("../../Data/India/import_summaries_by_firm_month_HS_code_complete.csv") %>% 
  select(company_id = domestic_company_id, year, month, date, date_character,hs6, import, log_import, import_dummy) %>% 
  # filter to analysis period
  filter(date >= ymd("2018-07-01"),
         date <= ymd("2021-12-31")) %>% 
  mutate(hs6  = str_pad(as.character(hs6) , 6, "left", "0")) %>% 
  #Drop firms that imported less than 1000 usd in all years between 2019-2021
  left_join(import_less_than_1000usd, by = c("company_id", "year")) %>% 
  filter(!less_than_1000usd) %>% 
  select(-less_than_1000usd, -sum_import, -n_years)

gc()



# Exports data India ----

# Firms that export yearly less than 1000 usd in the years they appear  
export_less_than_1000usd<-read_csv("../../Data/India/processed_data/export_firms_trade_less_than_1000usd.csv")


export_data <-fread("../../Data/India/export_summaries_by_firm_month_HS_code_complete.csv") %>% 
  select(company_id = domestic_company_id, year, month, date, date_character,hs6, export, log_export, export_dummy) %>% 
  # filter to analysis period
  filter(date >= ymd("2018-07-01"),
         date <= ymd("2021-12-31")) %>% 
  mutate(hs6  = str_pad(as.character(hs6) , 6, "left", "0")) %>% 
  # Drop firms that exported less than 1000 usd in all years between 2019-2021
  left_join(export_less_than_1000usd, by = c("company_id", "year"))%>% 
  filter(!less_than_1000usd) %>% 
  select(-less_than_1000usd, -sum_export, -n_years)

gc()




n_products_import_IND<-import_data %>%
  group_by(date, company_id) %>%
  summarize(n_prods_imported = n_distinct(hs6)) %>% 
  arrange(company_id, date) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  mutate(monthly_n_prods_imported = mean(n_prods_imported)) %>% 
  ungroup() %>% 
  mutate(mean_n_prods_imported = mean(n_prods_imported))
  
n_products_export_IND<-export_data %>%
  group_by(date, company_id) %>%
  summarize(n_prods_exported = n_distinct(hs6)) %>% 
  arrange(company_id, date) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  mutate(monthly_n_prods_exported = mean(n_prods_exported)) %>% 
  ungroup() %>% 
  mutate(mean_n_prods_exported = mean(n_prods_exported))




monthly_mean_n_prods_imp<-n_products_import_IND %>% 
  distinct(date, .keep_all = T) %>% 
  select(date, monthly_n_prods_imported)



monthly_mean_n_prods_exp<-n_products_export_IND %>% 
  distinct(date, .keep_all = T) %>% 
  select(date, monthly_n_prods_exported)



ggplot()+
  geom_line(data = monthly_mean_n_prods_exp, 
            aes(x = date, y = monthly_n_prods_exported, colour = "Exports"))+
  geom_line(data = monthly_mean_n_prods_imp, 
            aes(x = date, y = monthly_n_prods_imported, colour = "Imports"))+
  theme_minimal()+
  labs(colour = "")+
  scale_colour_manual(values = c("deepskyblue4", "gray48"))+
  ylab("Avg. Number of Exported/Imported Products")+
  xlab("")+
  ggtitle("Monthlty average number of exported/imported products by Firm", 
          subtitle = "India")+
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(face = "italic", hjust = 0.5), 
        axis.title = element_text(size = 8))
  


ggsave("../../Outputs/Graphs/Number of Exported-Imported Products India.pdf", 
       width = 7, height = 5)



# Indonesia ---- 
# less than 1000 usd in this year. Also another variable that does the same but for 2019.  
imports_less_than_1000usd<-read_csv("../../Data/Indonesia/processed_data/import_firms_trade_less_than_1000usd.csv")

# Read imports data at the firm-month-HS6 level
import_data <- read_csv("../../Data/Indonesia/import_summary_by_firm_month_HS_code.csv") %>% 
  select(company_id, date, date_character,hs6,log_import, import, import_dummy, n_countries_import)%>% 
  mutate(hs6  = str_pad(hs6 , 6, "left", "0"), 
         year = year(date)) %>% 
  # Drop firms that imported less than 1000 usd in both 2019 and 2020
  # Drop firms that only appears in 2019 and imported less than 1000 usd in this year
  # Drop firms that only appears in 2020 and imported less than 1000 usd in this year
  left_join(imports_less_than_1000usd, 
            by = c("company_id", "year")) %>% 
  filter(!less_than_1000USD_import_19_20, 
         !less_than_1000USD_import_2019, 
         !less_than_1000USD_import_2020) %>% 
  # Select variables to be analyzed 
  select(-year, -less_than_1000USD_import_19_20, -n_years,
         -less_than_1000USD_import_2020, 
         -less_than_1000USD_import_2019)

# Read exports data at the firm-month-HS6 code level ----


# First read data with variable that indicates if a firm export less than 1000 usd in both 2019 
# and 2020. Also includes a variable that indicates if a firm that only appears in 2020 exports
# less than 1000 usd in this year. Also, another variable that does the same but for 2019. 

exports_less_than_1000usd<-read_csv("../../Data/Indonesia/processed_data/export_firms_trade_less_than_1000usd.csv")


# Read exports data at the firm-month-HS6 level
export_data <- read_csv("../../Data/Indonesia/export_summary_by_firm_month_HS_code.csv") %>% 
  select(company_id, date, date_character,hs6,log_export, export, export_dummy, n_countries_export ) %>% 
  mutate(hs6  = str_pad(hs6 , 6, "left", "0"), 
         year = year(date)) %>% 
  # Drop firms that in a particular year export less than 1000 usd
  left_join(exports_less_than_1000usd, 
            by = c("company_id", "year")) %>% 
  filter(!less_than_1000USD_export_19_20,
         !less_than_1000USD_export_2019, 
         !less_than_1000USD_export_2020) %>% 
  # Select variables to be analyzed 
  select(-year, -less_than_1000USD_export_19_20)



n_products_import_IDN<-import_data %>%
  group_by(date, company_id) %>%
  summarize(n_prods_imported = n_distinct(hs6)) %>% 
  arrange(company_id, date) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  mutate(monthly_n_prods_imported = mean(n_prods_imported)) %>% 
  ungroup() %>% 
  mutate(mean_n_prods_imported = mean(n_prods_imported))

n_products_export_IDN<-export_data %>%
  group_by(date, company_id) %>%
  summarize(n_prods_exported = n_distinct(hs6)) %>% 
  arrange(company_id, date) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  mutate(monthly_n_prods_exported = mean(n_prods_exported)) %>% 
  ungroup() %>% 
  mutate(mean_n_prods_exported = mean(n_prods_exported))




monthly_mean_n_prods_imp_IDN<-n_products_import_IDN %>% 
  distinct(date, .keep_all = T) %>% 
  select(date, monthly_n_prods_imported)



monthly_mean_n_prods_exp_IDN<-n_products_export_IDN %>% 
  distinct(date, .keep_all = T) %>% 
  select(date, monthly_n_prods_exported)



ggplot()+
  geom_line(data = monthly_mean_n_prods_exp_IDN, 
            aes(x = date, y = monthly_n_prods_exported, colour = "Exports"))+
  geom_line(data = monthly_mean_n_prods_imp_IDN, 
            aes(x = date, y = monthly_n_prods_imported, colour = "Imports"))+
  theme_minimal()+
  labs(colour = "")+
  scale_colour_manual(values = c("deepskyblue4", "gray48"))+
  ylab("Avg. Number of Exported/Imported Products")+
  xlab("")+
  ggtitle("Monthlty average number of exported/imported products by Firm", 
          subtitle = "Indonesia")+
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(face = "italic", hjust = 0.5), 
        axis.title = element_text(size = 8))



ggsave("../../Outputs/Graphs/Number of Exported-Imported Products Indonesia.pdf", 
       width = 7, height = 5)



