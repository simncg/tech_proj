#===========================================================================#
# Date:    March 2023                                                       #
#                                                                           #
# Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   #
#          Indonesia and Mexico.                                            #
#                                                                           #
# Author:  Simon Caicedo - DECTI - The World Bank                           #
#                                                                           #
#                                                                           #
# This program generates tables and graphs to analyze the trade partners of #
# Indonesia, i.e., the trade flow between Indonesia and another country     #
# (e.g., U.S).                                                              #                                         
#                                                                           #
#                                                                           #           -                                                                                                                                                  #
#===========================================================================#



# The task is to create descriptive statistics of the trade partners Indonesia-US. 

# Stats in table of presentation 18th october: 

# - Foregn firms overall (partners of firms matched to both Aberdeen and Builtwith)
# - Foreign firms in Aberdeen countries 
# - Median import partners per firm 
# - Median export partners per firm 
# - Median import partners per firm/hs6 product
# - Mean import partners per firm/hs6 product 
# - Median export partners per firm/hs6 product
# - Mean export partners per firm/hs6 product 
# - Median hs6 import products per firm 
# - Mean hs6 import products per firm 
# - Median hs6 export products per firm 
# - Mean hs6 export products per firm 


# Stats proposed by Devaki:

# Number of trading partners for firms in e-commerce or not 
# Number of ecommerce/non-ecommerce trading partners for firms that are in ecommerce or not. Basically, do (non) ecommerce firms trade more with other (non) ecommerce firms


# SIC Groups to analyze ----
included_SIC_Groups <- c('AG-M-C', # Agricultural, Construction and Mining
                         "MANUF",  # Manufacturing
                         "SVCS",   # Services
                         "F-I-RE", # Finance Insurance, Real Estate 
                         "TR-UTL", # Transport & Utilities
                         "WHL-RT"  # Wholesale-retail
                        )


# Focus only on 3 relations 

# wholesale/retail vs manufacturing vs the rest (3 categories).

# 1. Wholesale/Retail vs Manufacturing 
# 2. Wholesale/Retail vs The Rest of SIC
# 3. Manufacturing vs The Rest




