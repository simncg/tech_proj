

# Set Working Directory ----
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Libraries to be used ----
source("../src/packages.R")


# India 
tech_data_IND<-read_parquet("../../Data/India/processed_data/tech_data_IND.parquet", 
                            col_select = c("company_id", "date", "months_since_adoption"))


imports_grid_mitig_IND<-read_parquet("../../Data/India/processed_data/imports_grid_mitig_model_IND.parquet")


imports_grid_mitig_IND<-
  imports_grid_mitig_IND %>% 
  left_join(tech_data_IND, by = c("company_id", "date"))


write_parquet(imports_grid_mitig_IND, "../../Data/India/processed_data/imports_grid_mitig_model_IND.parquet")

rm(imports_grid_mitig_IND)
gc()
  
exports_grid_mitig_IND<-read_parquet("../../Data/India/processed_data/exports_grid_mitig_model_IND.parquet")


exports_grid_mitig_IND<-
  exports_grid_mitig_IND %>% 
  left_join(tech_data_IND, by = c("company_id", "date"))


write_parquet(exports_grid_mitig_IND, "../../Data/India/processed_data/exports_grid_mitig_model_IND.parquet")

rm(exports_grid_mitig_IND, tech_data_IND)
gc()
# Mexico 
tech_data_MEX<-read_parquet("../../Data/Mexico/processed_data/tech_data_MEX.parquet", 
                            col_select = c("company_id", "date", "months_since_adoption"))

imports_grid_mitig_MEX<-read_parquet("../../Data/Mexico/processed_data/imports_grid_mitig_model_MEX.parquet")


imports_grid_mitig_MEX<-
  imports_grid_mitig_MEX %>% 
  left_join(tech_data_MEX, by = c("company_id", "date"))


write_parquet(imports_grid_mitig_MEX, "../../Data/Mexico/processed_data/imports_grid_mitig_model_MEX.parquet")

rm(imports_grid_mitig_MEX)


exports_grid_mitig_MEX<-read_parquet("../../Data/Mexico/processed_data/exports_grid_mitig_model_MEX.parquet")

exports_grid_mitig_MEX<-
  exports_grid_mitig_MEX %>% 
  left_join(tech_data_MEX, by = c("company_id", "date"))


write_parquet(exports_grid_mitig_MEX, "../../Data/Mexico/processed_data/exports_grid_mitig_model_MEX.parquet")

rm(exports_grid_mitig_MEX, tech_data_MEX)
gc()


# Indonesia 
tech_data_IDN<-read_parquet("../../Data/Indonesia/processed_data/tech_data_IDN.parquet", 
                            col_select = c("company_id", "date", "months_since_adoption"))

imports_grid_mitig_IDN<-read_parquet("../../Data/Indonesia/processed_data/imports_grid_mitig_model_IDN.parquet")

imports_grid_mitig_IDN<-
  imports_grid_mitig_IDN %>% 
  left_join(tech_data_IDN, by = c("company_id", "date"))


write_parquet(imports_grid_mitig_IDN, "../../Data/Indonesia/processed_data/imports_grid_mitig_model_IDN.parquet")


rm(imports_grid_mitig_IDN)
gc()

exports_grid_mitig_IDN<-read_parquet("../../Data/Indonesia/processed_data/exports_grid_mitig_model_IDN.parquet")

exports_grid_mitig_IDN<-
  exports_grid_mitig_IDN %>% 
  left_join(tech_data_IDN, by = c("company_id", "date"))


rm(exports_grid_mitig_IDN)
