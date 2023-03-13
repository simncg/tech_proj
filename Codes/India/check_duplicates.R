# =====================================================================#
#                                                                      #                                                                     #
# Duplicated firms in matched abeerden to panjiva data India           #
#                                                                      #
# January 24, 2023                                                     #
#                                                                      #
# Simon Caicedo                                                        #
#                                                                      #
#======================================================================#

# Load matched Aberdeen to Panjiva data ----
matched_data <- read_csv("../../Data/India/matched_Aberdeen_to_Panjiva_data_India.csv")


# Duplicated firms with different ID (our_ID) but same SITEID, EMPLE and Aberdeen_COMPANY. 
dups_firms<-matched_data %>% 
  group_by(SITEID, EMPLE, Aberdeen_COMPANY, SICGRP) %>% 
  mutate(n_dups = n())  %>% 
  filter(n_dups > 1)%>% 
  distinct(SITEID, EMPLE, Aberdeen_COMPANY, SICGRP) %>% 
  nrow()

# Not duplicated firms 
not_dups_firms<-matched_data %>% 
  group_by(SITEID, EMPLE, Aberdeen_COMPANY, SICGRP) %>% 
  mutate(n_dups = n())  %>% 
  filter(n_dups == 1)%>% 
  distinct(SITEID, EMPLE, Aberdeen_COMPANY, SICGRP) %>% 
  nrow()

# Share duplicated firms 
share_dup_firms = (dups_firms/(dups_firms + not_dups_firms))*100


# Number of duplicated rows in the dataset
dups_rows<-matched_data %>% 
  group_by(SITEID, EMPLE, Aberdeen_COMPANY, SICGRP) %>% 
  mutate(n_dups = n())  %>% 
  filter(n_dups > 1) %>% 
  nrow()

# Share of duplicated rows in the dataset 
share_dup_rows = (dups_rows/nrow(matched_data))*100


