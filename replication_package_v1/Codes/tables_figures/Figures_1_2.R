#================================================================================#
# Date:    December 2022                                                    
#                                                                           
# Project: E-commerce and Trade during Crisis Times: Firm-level Evidence   
#          from India, Indonesia and Mexico                               
#                                                                         
#                                                                         
# This program generates charts on trade trends over time by country.     
# Figure 1 and 2.
#
# Inputs: - Data/processed_data/India/imports_tech_mitigation_model_IND.csv
#         - Data/processed_data/India/exports_tech_mitigation_model_IND.csv
#         - Data/processed_data/Mexico/imports_tech_mitigation_model_MEX.csv
#         - Data/processed_data/Mexico/exports_tech_mitigation_model_MEX.csv
#         - Data/processed_data/Indonesia/imports_tech_mitigation_model_IDN.csv
#         - Data/processed_data/Indonesia/exports_tech_mitigation_model_IDN.csv
#
# Outputs: - Outputs/Graphs/china_e_com_com_avg_trade_value_firm_hs6_month.png
#          - Outputs/Graphs/avg_trade_value_firm_hs6_month.png
#
#
#
#
#                                                                                                                                                                                                                                        #
#================================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)


# Libraries to be used ----
source("../src/packages.R")

# Read datasets for India
import_data_IND<-fread("../../Data/processed_data/India/imports_tech_mitigation_model_IND.csv") %>% 
  select(company_id, hs6, date, import, China_E_commerce)


export_data_IND<-fread("../../Data/processed_data/India/exports_tech_mitigation_model_IND.csv") %>% 
  select(company_id, hs6, date, export, China_E_commerce)



# Read datasets for Mexico
import_data_MEX<-fread("../../Data/processed_data/Mexico/imports_tech_mitigation_model_MEX.csv") %>% 
  select(company_id, hs6, date, import, China_E_commerce)
export_data_MEX<-fread("../../Data/processed_data/Mexico/exports_tech_mitigation_model_MEX.csv")%>% 
  select(company_id, hs6, date, export, China_E_commerce)


# Read datasets for India
import_data_IDN<-fread("../../Data/processed_data/Indonesia/imports_tech_mitigation_model_IDN.csv")%>% 
  select(company_id, hs6, date, import, China_E_commerce)

export_data_IDN<-fread("../../Data/processed_data/Indonesia/exports_tech_mitigation_model_IDN.csv")%>% 
  select(company_id, hs6, date, export, China_E_commerce)

# Remove top 1% percentile in the exports dataset (to remove the outlier)
cutoff <- quantile(export_data_IDN$export, 0.99999)

export_data_IDN <- export_data_IDN[export_data_IDN$export <= cutoff, ]


gc()

# Create plots ----

# Set fonts for plots
windowsFonts()
windowsFonts(`Open Sans` = windowsFont('Open Sans'))


## Plot Type 1 ----


# Type 1:  2 lines – 1 line for average exports by firm-HS6-month over time
# and 1 line for average imports by firm-HS6-month over time.
data_avg_values<-
  import_data_IND %>% 
    group_by(date) %>% 
    summarize(mean_import = mean(import, na.rm = T)) %>% 
    left_join(export_data_IND %>% 
                group_by(date) %>% 
                summarize(mean_export = mean(export, na.rm = T)),
              by = "date") %>% 
    mutate(country = "India") %>%
    bind_rows(
      import_data_IDN %>% 
        group_by(date) %>% 
        summarize(mean_import = mean(import, na.rm = T)) %>% 
        full_join(export_data_IDN %>% 
                    group_by(date) %>% 
                    summarize(mean_export = mean(export, na.rm = T)),
                  by = "date") %>% 
        mutate(country = "Indonesia") 
    ) %>% 
    bind_rows(
      import_data_MEX %>% 
        group_by(date) %>% 
        summarize(mean_import = mean(import, na.rm = T)) %>% 
        left_join(export_data_MEX %>% 
                    group_by(date) %>% 
                    summarize(mean_export = mean(export, na.rm = T)),
                  by = "date") %>% 
        mutate(country = "Mexico") 
    ) %>% 
  pivot_longer(
    cols = c(mean_import, mean_export), 
    values_to = "mean_value",
    names_to = "trade_var"
  )



# -	Type 1:  2 lines – 1 line for average exports by firm-HS6-month over time
# and 1 line for average imports by firm-HS6-month over time.



ggplot(data_avg_values) +
  geom_line(aes(x = date, y = mean_value/1000, colour = trade_var)) + # divide y-axis values by 1000
  facet_wrap(~country) +
  theme_light() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "K")) + 
  ylab("")+
  xlab("")+
  labs(colour = "")+
  scale_colour_manual(values = c("#4682B4", "#87CEEB"), labels = c("Avg. Export Value", "Avg. Import Value"))+
  theme(
    legend.position = "bottom", 
    axis.text = element_text(size = 7.5, family = "Open Sans"),
    legend.text = element_text(family = "Open Sans"),
    legend.key.size = unit(1.5, "lines"), 
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14, family = "Open Sans"), 
    plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 10, family = "Open Sans"), 
    panel.grid.major = element_line(color = "gray95"), 
    panel.grid.minor = element_line(color = "gray90"), 
    strip.background = element_blank(), 
    strip.text = element_text(color = "black", family = "Open Sans")
  ) +
  guides(colour = guide_legend(override.aes = list(size = 1.5)))
  


ggsave("../../Outputs/Graphs/avg_trade_value_firm_hs6_month.png", dpi = 300,
       width = 6.5, height = 4.5, units = "in")


## Plot Type 2 ----



# Type 2: 4 lines – 2 lines for average exports by firm-HS6-month over time 
# split between the average for HS6 with E-tradeable Goods=1 and the average for 
# HS6 with E-tradeable Goods=0 and 2 lines for average imports by firm-HS6-month
# over time split between the average for HS6 with E-tradeable Goods=1 and the 
# average for HS6 with E-tradeable Goods=0.

data_china_ecom_avg_values<-
  import_data_IND %>% 
  group_by(date, China_E_commerce) %>% 
  summarize(mean_import = mean(import, na.rm = T)) %>% 
  left_join(export_data_IND %>% 
              group_by(date, China_E_commerce) %>% 
              summarize(mean_export = mean(export, na.rm = T)),
            by = c("date", "China_E_commerce")) %>% 
  mutate(country = "India") %>%
  bind_rows(
    import_data_IDN %>% 
      group_by(date, China_E_commerce) %>% 
      summarize(mean_import = mean(import, na.rm = T)) %>% 
      full_join(export_data_IDN %>% 
                  group_by(date, China_E_commerce) %>% 
                  summarize(mean_export = mean(export, na.rm = T)),
                by = c("date", "China_E_commerce")) %>% 
      mutate(country = "Indonesia") 
  ) %>% 
  bind_rows(
    import_data_MEX %>% 
      group_by(date, China_E_commerce) %>% 
      summarize(mean_import = mean(import, na.rm = T)) %>% 
      left_join(export_data_MEX %>% 
                  group_by(date, China_E_commerce) %>% 
                  summarize(mean_export = mean(export, na.rm = T)),
                by = c("date", "China_E_commerce")) %>% 
      mutate(country = "Mexico") 
  ) %>% 
  pivot_longer(
    cols = c(mean_import, mean_export),
    values_to = "mean_value",
    names_to = "trade_var"
  ) %>% 
  na.omit(China_E_commerce)



ggplot(data_china_ecom_avg_values) +
  geom_line(aes(x = date, y = mean_value/1000, colour = paste(trade_var, China_E_commerce))) + # divide y-axis values by 1000
  facet_wrap(~country) +
  theme_light() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "K")) + 
  ylab("")+
  xlab("")+
  #ggtitle("Trade Trends Over Time", 
  #        subtitle = "Avg. Export/Import Value by Firm-HS6 Product")+
  labs(colour = "")+
  scale_colour_manual(values = c("gray65", "#274c77", "#6096ba", '#a3cef1'), 
                      labels = c("Avg. Export: E-tradeable Goods = 0", "Avg. Export: E-tradeable Goods = 1", 
                                 "Avg. Import: E-tradeable Goods = 0", "Avg. Import: E-tradeable Goods = 1")
                      )+
  theme(
    legend.position = "bottom", 
    axis.text = element_text(size = 7.5, family = "Open Sans"),
    legend.key.size = unit(1.5, "lines"), 
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14), 
    plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 10), 
    panel.grid.major = element_line(color = "gray95"), 
    panel.grid.minor = element_line(color = "gray90"), 
    legend.text = element_text(size = 7.5, family = "Open Sans"), 
    strip.background = element_blank(), 
    strip.text = element_text(color = "black", family = "Open Sans")
  ) +
  guides(colour = guide_legend(override.aes = list(size = 2), nrow = 2))


ggsave("../../Outputs/Graphs/china_e_com_com_avg_trade_value_firm_hs6_month.png", dpi = 300,
       width = 6.5, height = 4.5, units = "in")


