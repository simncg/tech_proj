
# China ecommerce tax list

china_ecom_list<- read_dta("../../Data/Extra Data/china_ecommerce_tax_list.dta") %>% 
  mutate(hs6 = substr(hs8, 1, 6), 
         ecommerce_china_tax_list = T) 



# Load HS Code Classification data with products information ----


hs_data <- read_csv("../../Data/India/HS_code_classifications.csv")%>% 
  mutate(hs_2017   = str_pad(hs_2017  , 6, "left", "0")) %>% 
  select(hs_2017, contains("China")) %>% 
  left_join(china_ecom_list %>% 
              select(-hs8),
            by = c("hs_2017" = "hs6")) %>% 
  mutate(ecommerce_china_tax_list = ifelse(is.na(ecommerce_china_tax_list), F, 
                                           ecommerce_china_tax_list))



(sum(hs_data$China_E_commerce == hs_data$ecommerce_china_tax_list)/nrow(hs_data))*100


diff<-hs_data[hs_data$China_E_commerce !=  hs_data$ecommerce_china_tax_list, c(1,3,5)]

write_xlsx(diff, "HS6 codes difference.xlsx")



t<-data.table::CJ(c(1,1,1,4,5), c("hola", "chao"), unique = T)
