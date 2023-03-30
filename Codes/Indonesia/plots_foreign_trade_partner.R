
# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")

# Read tables of foreign analysis
tables<-readRDS('../../Outputs/Indonesia/foreign_partners_analysis/Tables.RDS')



imports_analysis<-imports_f_data %>% 
  # Only analyze those firms with SIC identified
  na.omit(SICGRP, foreign_SICGRP) %>% 
  # Keep only firms in SIC groups to be analyzed
  filter(SICGRP %in% included_SIC_Groups, 
         foreign_SICGRP %in% included_SIC_Groups) %>% 
  # Categorize in 3 industries (Wholesale-retail, Manufacturing and the rest)
  mutate(industry_local = if_else(!(SICGRP %in% c("MANUF", "WHL-RT")), "Rest", SICGRP), 
         industry_foreign = if_else(!(foreign_SICGRP %in% c("MANUF", "WHL-RT")), "Rest", foreign_SICGRP)
  ) %>% 
  # Filter for USA-IDN trade flow relationship
  filter(foreign_country_panjiva == "USA") %>% 
  mutate(n_firms = n_distinct(domestic_company_id)) %>% 
  as_tibble()

# Exports complete: Plot
tt<-tables[["imports_complete"]] %>% 
  mutate(
    industry_local = case_when(
      industry_local == "MANUF" ~ "Manufacturing",
      industry_local == "WHL-RT" ~ "Wholesale/Retail",
      TRUE ~ industry_local
    ),
    industry_foreign = case_when(
      industry_foreign == "MANUF" ~ "Manufacturing",
      industry_foreign == "WHL-RT" ~ "Wholesale/Retail",
      TRUE ~ industry_foreign
    ), 
    Industries = paste(industry_local, "-", industry_foreign)
  ) %>% 
  pivot_longer(cols = c(n_domestic_firms, n_foreign_firms), 
               names_to = "local_or_foreign_ind", 
               values_to = "n_firms") %>% 
  ggplot(data_long, aes(x = Industry_Combination, y = Firms, fill = Industry)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Number of Firms by Bilateral Industry Pair",
    x = "Industry ",
    y = "Number of Firms",
    fill = "Industry"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
  
  
  









