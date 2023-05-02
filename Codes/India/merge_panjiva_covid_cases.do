*=============================================================================
* Date:    April 2023
* Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   
*          Indonesia and Mexico. 
*
* Author:  Simon Caicedo based on code of Jan Gabriel Oledan
*
* Description: This do-file merges Indian districts and covid lockdowns data to
*              Panjiva data
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
	Create lags in COVID cases by district 
----------------------------------------------------------------------------*/


use "$path_data/India/raw_data/imputed_COVID_districts_2011_jan2023Extension.dta", clear
gen month_string = string(month)
replace month_string = "0" + string(month) if month<10
gen month_year = string(year) + "-" + month_string
egen month_year_encoded = group(month_year) 
xtset district_code month_year_encoded

foreach var of varlist total* {
	gen l2_`var' = l2.`var'  // 2 lags in covid variables (as there are 2 lags in technology variable)
}


* replace as 0 variables with lags
foreach c of varlist l2*{
    replace `c' = 0 if missing(`c')
}
	
tempfile covid_data
save `covid_data', replace 

/*------------------------------------------------------------------------------
step 1. extract a subset of districts in the trade data that were formed 
		as a result of taking parts of existing districts A and B to form
		new district C
		- redistribute the trade values based on a population share rule
------------------------------------------------------------------------------*/

* include the list of trade data to iterate over
local files "imports_collapsed_dom_hs6_onlyvalue exports_collapsed_dom_hs6_onlyvalue"

foreach f of local files {

	* read in trade data
	use "$path_data/India/raw_data/`f'.dta", clear
	
	* keep special cases
	keep if inlist(district, 702, 754, 730, 726, 727)
	gen StateCode = state
	gen fd_district = district
	
	* join with data set of these special district cases
	joinby StateCode fd_district using "$path_data/India/raw_data/split_district_cases.dta"
	
	replace district = DistrictCode
	
	* drop vars to clean data set
	drop StateCode-pop_share

	* change directory if needed 
	save "$path_data/India/processed_data/`f'_districts_special_cases.dta", replace
	
	/*------------------------------------------------------------------------------
	step 2. make the full trade data set
		- recode other newly formed districts to their 2011 counterparts
		- drop the special case districts from step 1
		- append the corrected special cases from step 1
		- merge extended COVID data in 
	------------------------------------------------------------------------------*/
		
	* Use trade data
	use "$path_data/India/raw_data/`f'.dta", clear

	// recode new district codes to their 2011 equivalents 
	// recode all of these
	* recode telangana districts in trade to 2011 equivalents
	* adilabad, nizamabad, karimnagar, medak, hyderabad, rangareddy, mahabubnagar, 
	* nalgonda, warangal urban, khammam
	recode district ///
		(733 734 735 = 532) ///
		(736 = 533) ///
		(737 738 739 = 534) ///
		(740 741 = 535) ///
		(742 = 537) ///
		(743 744 745 746 = 538) ///
		(747 748 = 539) ///
		(749 750 751 752 = 540) ///
		(753 = 541)
		
	* new districts in other states
	recode district ///
		(773=35) ///
		(701=43) ///
		(765=81) ///
		(703=98) ///
		(704=133) ///
		(705=140) ///
		(706=171) ///
		(762=259) ///
		(708=289) ///
		(710=292) ///
		(757=305) ///
		(755=311) ///
		(774=328) ///
		(777=335) ///
		(776=344) ///
		(715=401) ///
		(716=401) ///
		(717=406) ///
		(719=409) ///
		(718=409) ///
		(721=410) ///
		(722=414) ///
		(724=436) ///
		(725=472) ///
		(728=477) ///
		(729=479) ///
		(731=486) ///
		(732=517) ///
		(756=306) ///
		(775=327) ///
		(714=299)

	* drop special case districts from the previous code chunk - to append later
	drop if inlist(district, 702, 754, 730, 726, 727)

	* append the corrected special cases made in step 1
	append using "$path_data/India/processed_data/`f'_districts_special_cases.dta"
		
	* merge with COVID cases extended to jan2023
	* make district_code var to match COVID data set
	gen district_code = district
	merge m:1 district_code month year using `covid_data', keep(1 3) gen(merge_covidCases)
	
	* replace variables with 0 if year <= 2019
	foreach c of varlist total_deaths total_cases total_recoveries l2* {
		replace `c' = 0 if missing(`c') & year <= 2019
	}
	
	
	lab var merge_covidCases "merge indicator for COVID cases up to jan2023"
	* drop district_code var created to merge
	drop district_code
	
	*save 
	save "$path_data/India/processed_data/`f'_districts.dta", replace
	
}
	