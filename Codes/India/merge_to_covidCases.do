clear all
set more off

global main "D:/India Supply Chains Analysis with Nora"
global dataraw "$main/Data"
global dataout "$main/Output"
global temp "$main/Temp"
global results "$main/Results"
global code "$main/Code"
global figuresGPH "$main/Figures/gph"
global figuresPDF "$main/Figures/pdf"
global figuresPNG "$main/Figures/png"

/* !!!!
	NOTE: you might want to change the output directories
		on lines 58, 128, 144
*/ 

/*------------------------------------------------------------------------------
step 1. extract a subset of districts in the trade data that were formed 
		as a result of taking parts of existing districts A and B to form
		new district C
		- redistribute the trade values based on a population share rule
------------------------------------------------------------------------------*/

* include the list of trade data to iterate over
local files "imports_collapsed_dom_hs6_onlyvalue_Nora exports_collapsed_dom_hs6_onlyvalue_Nora imports_collapsed_fd_hs6_onlyvalue_Nora exports_collapsed_fd_hs6_onlyvalue_Nora"
foreach f of local files {
	
	* extract the first part of the file name (import or export)
	local v = substr("`s'", 1, 6)
	di "`v'"
	
	* read in trade data
	use "D:\Alejandro\IND\Outputs\`f'.dta", clear
	
	* keep special cases
	keep if inlist(district, 702, 754, 730, 726, 727)
	gen StateCode = state
	gen fd_district = district
	
	* join with data set of these special district cases
	joinby StateCode fd_district using "$dataout/Geography/split_district_cases.dta"

	* remake import/export variables based on population share rule
	replace `v' = `v'*pop_share
	replace total_`v' = total_`v'*pop_share
	replace `v'_qty = `v'_qty*pop_share
	replace `v'_price = `v'_price*pop_share
	
	replace district = DistrictCode
	
	* drop vars to clean data set
	drop StateCode-pop_share

	* change directory if needed 
	*save "$dataout/Trade Values - Special Cases/`f'_specialCases.dta", replace
}

/*------------------------------------------------------------------------------
step 2. make the full trade data set
	- recode other newly formed districts to their 2011 counterparts
	- drop the special case districts from step 1
	- append the corrected special cases from step 1
	- merge extended COVID data in 
------------------------------------------------------------------------------*/
local files "imports_collapsed_dom_hs6_onlyvalue_Nora exports_collapsed_dom_hs6_onlyvalue_Nora imports_collapsed_fd_hs6_onlyvalue_Nora exports_collapsed_fd_hs6_onlyvalue_Nora"
foreach f of local files {
		
	// input trade data here - of the same 
	use "D:/Alejandro/IND/Outputs/`f'.dta", clear

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
	*append using "$dataout/Trade Values - Special Cases/`f'_specialCases.dta"
		
	* merge with COVID cases extended to jan2023
	* make district_code var to match COVID data set
	gen district_code = district
	merge m:1 district_code month year using "$dataout/Controls/imputed_COVID_districts_2011_jan2023Extension.dta", keep(1 3) gen(merge_covidCases)
	
	* replace variables with 0 if year <= 2019
	foreach c of varlist total_deaths total_cases total_recoveries {
		replace `c' = 0 if missing(`c') & year <= 2019
	}
	
	lab var merge_covidCases "merge indicator for COVID cases up to jan2023"
	* drop district_code var created to merge
	drop district_code
	
	*save <output here>
}
		