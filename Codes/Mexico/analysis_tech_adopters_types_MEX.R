#===========================================================================#
# Date:    December 2022                                                    #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
#                                                                           #
# Analysis of technology adopters: plots and t-tests                        #  
#                                                                           #
#                                                                           #                                                                                                                                                             #
#===========================================================================#

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")
library(gridExtra)
library(grid)
library(kableExtra)

# Functions for performing t-tests for comparing adopters types
source("../src/functions_t_tests_adopters_types.R")


# Create plots comparing adopters types ----


# This imports sample dataset is from the model that measures if tech adoption mitigates Covid impacts 
import_tech_mitig_MEX<- fread("../../Data/Mexico/processed_data/imports_tech_mitigation_reg_firm_month_MEX.csv") 


# Plots -----

## Importers adopter types ---- 

imp_adopters_type_summary<-
  import_tech_mitig_MEX %>% 
  select(company_id, adopter_type, covid_adopter_type, old_adopter_type) %>% 
  distinct(company_id, .keep_all = T) %>% 
  select(company_id, covid_adopter_type, old_adopter_type) %>% 
  pivot_longer(
    cols = c(covid_adopter_type, old_adopter_type),
    names_to = "old_or_covid_adopter",
    values_to = "adopter_type"
  ) %>% 
  group_by(old_or_covid_adopter, adopter_type) %>% 
  summarise(n = n())


# Replace type of adopters labels with more meaningful ones
imp_adopters_type_summary$old_or_covid_adopter <- recode(imp_adopters_type_summary$old_or_covid_adopter,
                                                         covid_adopter_type = "COVID Adopters",
                                                         old_adopter_type = "Old Adopters"
)

imp_adopters_type_summary$adopter_type <- recode(imp_adopters_type_summary$adopter_type,
                                                 "2016_or_pre_2016_adopter" = "Pre-2017 Adopter",
                                                 "2017_adopter" = "2017 Adopter", 
                                                 "2018_adopter" = "2018 Adopter", 
                                                 "2019_adopter" = "2019 Adopter", 
                                                 "never_adopter" = "Never Adopter",
                                                 "non_covid_adopter" = "Non-Covid Adopter",
                                                 "covid_early_adopter" = "Covid Early Adopter", 
                                                 "covid_late_adopter" = "Covid Late Adopter", 
                                                 "non_old_adopter" = "Non-Old Adopter"
)


# plot
p_imp<-ggplot(data = imp_adopters_type_summary[!(imp_adopters_type_summary$adopter_type %in% c("Never Adopter", "Non-Covid Adopter", "Non-Old Adopter")), ], 
              aes(fill = adopter_type, y = n, x = old_or_covid_adopter)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(adopter_type, ": ", n)), 
            position = position_stack(vjust = 0.5), 
            size = 2.6) +
  scale_fill_manual(values = c("#3E647D", "#7B92A8", "#82C0E9", "#fdbe85", 
                               "#fd8d3c", "#008BBC")) +
  labs(title = "Importers",
       x = "",
       y = "",
       fill = "Adopter Type") +
  theme_light() +
  theme(
    legend.position = "",
    plot.title = element_text(hjust = 0.5, face = "italic", size = 10), 
    panel.grid.major.x = element_blank(), 
    axis.text.x = element_text(face = "italic"), 
    axis.text.y = element_text(size = 8), 
    axis.title.y = element_text(size = 9)
  )

# Firm-month level exports (model that measures if tech adoption mitigates COVID impacts) -----


# This exports sample dataset is from the model that measures if tech adoption mitigates Covid impacts 
export_tech_mitig_MEX<- fread( "../../Data/Mexico/processed_data/exports_tech_mitigation_reg_firm_month_MEX.csv")



exp_adopters_type_summary<-
  export_tech_mitig_MEX %>% 
  select(company_id, adopter_type, covid_adopter_type, old_adopter_type) %>% 
  distinct(company_id, .keep_all = T) %>%
  select(company_id, covid_adopter_type, old_adopter_type) %>% 
  pivot_longer(
    cols = c(covid_adopter_type, old_adopter_type),
    names_to = "old_or_covid_adopter",
    values_to = "adopter_type"
  )%>% 
  group_by(old_or_covid_adopter, adopter_type) %>% 
  summarise(n = n())

# replace old_or_covid_adopter labels with more meaningful ones
exp_adopters_type_summary$old_or_covid_adopter <- recode(exp_adopters_type_summary$old_or_covid_adopter,
                                                         covid_adopter_type = "COVID Adopters",
                                                         old_adopter_type = "Old Adopters"
)

exp_adopters_type_summary$adopter_type <- recode(exp_adopters_type_summary$adopter_type,
                                                 "2016_or_pre_2016_adopter" = "Pre-2017 Adopter",
                                                 "2017_adopter" = "2017 Adopter", 
                                                 "2018_adopter" = "2018 Adopter", 
                                                 "2019_adopter" = "2019 Adopter", 
                                                 "never_adopter" = "Never Adopter",
                                                 "non_covid_adopter" = "Non-Covid Adopter",
                                                 "covid_early_adopter" = "Covid Early Adopter", 
                                                 "covid_late_adopter" = "Covid Late Adopter", 
                                                 "non_old_adopter" = "Non-Old Adopter"
)


# plot
p_exp<-ggplot(data = exp_adopters_type_summary[!(exp_adopters_type_summary$adopter_type %in% c("Never Adopter", "Non-Covid Adopter", "Non-Old Adopter")), ], 
              aes(fill = adopter_type, y = n, x = old_or_covid_adopter)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(adopter_type, ": ", n)), 
            position = position_stack(vjust = 0.5), 
            size = 2.6) +
  scale_fill_manual(values = c("#3E647D", "#7B92A8", "#82C0E9", "#fdbe85", 
                               "#fd8d3c", "#008BBC")) +
  labs(title = "Exporters",
       x = "",
       y = "Number of Firms",
       fill = "Adopter Type") +
  theme_light() +
  theme(
    legend.position = "",
    plot.title = element_text(hjust = 0.5, face = "italic", size = 10), 
    panel.grid.major.x = element_blank(), 
    axis.text.x = element_text(face = "italic"), 
    axis.text.y = element_text(size = 8), 
    axis.title.y = element_text(size = 9)
  )


# Join plots of importers and exporters
title <- textGrob("Technology Adopter Types Among Mexican Importers and Exporters", gp=gpar(fontface="bold", fontsize=14), hjust = 0.5)

final_plot_MEX<-grid.arrange(p_exp, p_imp, ncol=2, top = title)

rm(exp_adopters_type_summary, imp_adopters_type_summary, title, p_exp, p_imp)


# Perform t-tests and create table of t-tests results ---- 


## Comparison between old vs covid adopters ----
t_tests_old_covid_adopters_MEX<-
  conduct_t_tests_and_create_table(import_data = import_tech_mitig_MEX,
                                   export_data = export_tech_mitig_MEX,
                                   var_adopter_types = "adopter_type",
                                   adopter_types = c("covid_adopter", "old_adopter"), 
                                   country = "Mexico")


## Comparison between covid early vs covid late adopters ----
t_tests_covid_early_late_adopters_MEX<-
  conduct_t_tests_and_create_table(import_data = import_tech_mitig_MEX,
                                   export_data = export_tech_mitig_MEX,
                                   var_adopter_types = "covid_adopter_type",
                                   adopter_types = c("covid_early_adopter", "covid_late_adopter"), 
                                   country = "Mexico")


## Comparison between pre-2017 vs 2017-2019 adopters ----

import_tech_mitig_MEX<-import_tech_mitig_MEX %>% 
  mutate(
    old_adopter_type = case_when(
      old_adopter_type == "2016_or_pre_2016_adopter" ~ "pre_2017_adopter",
      old_adopter_type == "2017_adopter" ~ "2017_to_2019_adopter", 
      old_adopter_type == "2018_adopter" ~ "2017_to_2019_adopter", 
      old_adopter_type == "2019_adopter" ~ "2017_to_2019_adopter", 
      TRUE ~ old_adopter_type
    )
  )


export_tech_mitig_MEX<-export_tech_mitig_MEX %>% 
  mutate(
    old_adopter_type = 
      case_when(
        old_adopter_type == "2016_or_pre_2016_adopter" ~ "pre_2017_adopter",
        old_adopter_type == "2017_adopter" ~ "2017_to_2019_adopter", 
        old_adopter_type == "2018_adopter" ~ "2017_to_2019_adopter", 
        old_adopter_type == "2019_adopter" ~ "2017_to_2019_adopter", 
        TRUE ~ old_adopter_type
      )
  )



# Adopters during 2018 and 2019
t_tests_old_adopters_MEX<-
  conduct_t_tests_and_create_table(import_data = import_tech_mitig_MEX,
                                   export_data = export_tech_mitig_MEX,
                                   var_adopter_types = "old_adopter_type",
                                   adopter_types = c("pre_2017_adopter", "2017_to_2019_adopter"), 
                                   country = "Mexico")
