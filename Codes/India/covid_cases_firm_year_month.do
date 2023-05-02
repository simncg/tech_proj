*=============================================================================
* Date:    April 2023
* Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   
*          Indonesia and Mexico. 
*
* Author:  Simon Caicedo
*
* Description: Get covid variables (cases, deaths and recoveries) for each firm
*              district by year and month.  
*                 
* Inputs: 
* 
* Outputs: 
*         
* Stata version 
* version 17.0
*=============================================================================

local files "imports_collapsed_dom_hs6_onlyvalue exports_collapsed_dom_hs6_onlyvalue"


foreach f of local files {
	
	* extract the first part of the file name (import or export)
	if strpos("`f'", "imports"){
		local data "imports"  
	} 
	else if strpos("`f'", "exports"){
		local data "exports"
	}
	
	* Use panjiva data with districts information and covid data for each firm
	use "$path_data/India/processed_data/`f'_districts.dta", clear
	keep year month domestic_id district1  total_deaths total_cases total_recoveries l2*
	sort domestic_id year month
	* This will be a dataset at the firm-month level with information of the 
	* district of the firm and the covid cases, deaths, recoveries in this 
	* district. 
	duplicates drop domestic_id year month, force
	save "$path_data/India/processed_data/`data'_firms_district_covid_data.dta", replace
	
}
