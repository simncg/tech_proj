*=============================================================================
* Date:    April 2023
* Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   
*          Indonesia and Mexico. 
*
* Author:  Simón Caicedo 
*
* Description: This do-file runs the IV analysis for India for the model that 
*              measures if tech adoption mitigates COVID impacts.
*                 
* Inputs: 
* 
* Outputs: 
*         
* Stata version 
* version 17.0
*=============================================================================

* Clear environment 
clear all

* Initialize system
set more off

* Globals for directories
global path "C:\Users\\`c(username)'\\OneDrive - WBG\Tech Project\Analysis"      // Project folder
global path_data "$path/Data"                                                    // Data folder 
global path_outputs  "$path/Outputs"                                             // Outputs folder


/*--------------------------------------------------------------------------
 1. Read IV data for India     
 ---------------------------------------------------------------------------*/
 
* Use India data at the firm-month-HS6 level 
import delimited "$path_data/India/raw_data/IV_data_based_on_Panjiva_data.csv", clear varnames(1)
rename our_id company_id
keep company_id bing_lightning_at_firm bing_ookla_d_speed_at_firm bing_ookla_u_speed_at_firm google_lightning_at_firm google_ookla_d_speed_at_firm google_ookla_u_speed_at_firm
* Convert NAs (missing values) to missing values in stata format
foreach var of varlist bing* google*{
	replace `var' = "." if `var' == "NA"
	destring `var', replace 
	*replace `var' = log(1 + `var')  
}

tempfile iv_data
save `iv_data', replace 


/*--------------------------------------------------------------------------
 2. Read covid variables for India     
 ---------------------------------------------------------------------------*/
 
import delimited "$path_data/India/raw_data/monthly_covid_measures_India.csv", clear varnames(1)
rename month_year date

foreach var of varlist month_mean_stringency_index - month_mean_confirmed_cases{
	replace `var' = "." if `var' == "NA"
	destring `var', replace
}

* Save in temporary file
tempfile covid_data
save `covid_data', replace

* Local to iterate over imports or exports dataset
local imp_exp `"imports exports"'
 
foreach data of local imp_exp {
		
	di "`data'"
	* Define outcomes 
	if("`data'" == "imports"){
		local y log_import 
	} 
	else{
		local y log_export
	}
		
	/*--------------------------------------------------------------------------
		3. Read trade data for India     
	---------------------------------------------------------------------------*/
	* Use India data at the firm-month level 
	import delimited "$path_data/India/processed_data/`data'_tech_mitigation_reg_firm_month_IND.csv", clear
			
		
	keep company_id year month date date_character `y' adopted_pay_or_ecom_before_2019


	/*--------------------------------------------------------------------------
	 4. Join datasets of trade, covid and instrumental variables    
	 ---------------------------------------------------------------------------*/

	* Merge instrumental variables
	merge m:1 company_id using `iv_data', keep(3) nogen

	* Merge covid variables
	merge m:1 date using `covid_data', keep(1 3) nogen
	
	* Fill missing values in covid variables (pre-covid period) with 0s 
	foreach var of varlist month_* {
		replace `var' = 0 if missing(`var')
	}
	
	/*--------------------------------------------------------------------------
	 5. IV Regressions (No Product Categories)
	 ---------------------------------------------------------------------------*/
	* Rename to avoid long names in estimates store
	rename google_lightning_at_firm ltg    // lightning strikes google
	rename google_ookla_d_speed_at_firm dspg // download speed google
	rename google_ookla_u_speed_at_firm uspg // upload speed google
	rename bing_lightning_at_firm ltb    // lightning strikes bing
	rename bing_ookla_d_speed_at_firm dspb // download speed bing
	rename bing_ookla_u_speed_at_firm uspb // upload speed bing
	rename adopted_pay_or_ecom_before_2019 tech   // technology variable (pre-2019 adoption of technology)
	rename month_mean_stringency_index sti    // stringency index in t since endogenous variable is covid_t*tech_i
		
	* Generate interaction technology variable and covid variable	
	gen techcovid = tech*sti
	
	
	* Run regressions for model that measures if tech adoption affects trade outcomes 
	foreach iv of varlist ltg dspg uspg ltb dspb uspb  {	
		gen `iv'covid = `iv'*sti  
		
		* Run first-stage manually
		reghdfe techcovid `iv'covid, absorb(date company_id) cluster(company_id)  residuals(res_techcovid_`iv')
		
		* Fitted tech variable
		predict techcovid_fitted_`iv'_covid
						
	}
	
	
	* Keep only firm-year-month and fitted technology variables 
	keep company_id year month techcovid_fitted* 
	
	* Save data 
	save "$path_data/India/processed_data/tech_fitted_IV_firm_month_`data'_mitig_IND.dta"
	
}
