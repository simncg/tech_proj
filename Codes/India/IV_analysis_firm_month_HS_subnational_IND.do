*=============================================================================
* Date:    April 2023
* Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   
*          Indonesia and Mexico. 
*
* Author:  Simón Caicedo 
*
* Description: This do-file runs the IV analysis for India using subnational
*              COVID variables.
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
	replace `var' = log(1 + `var')  // Take the logs in IV variables 
}

tempfile iv_data
save `iv_data', replace 


* Local to iterate over imports or exports dataset
local imp_exp `"imports exports"'
 
foreach data of local imp_exp {
	
	di "`data'"
	
	/*-------------------------------------------------------------------------
		2. Read subnational COVID data
	-------------------------------------------------------------------------*/    
	use "$path_data/India/processed_data/`data'_firms_district_covid_data.dta", clear 	
	
	rename domestic_id company_id 
	
	// Take logs in the COVID variables and in the lagged (t-2) COVID variables (l2)
	foreach var of varlist total* l2*{  
	    * Take the logarithms of the covid variables
	    replace `var' = log(1 + `var')
	}
	
	* Save in temp file
	tempfile covid_data_`data'
	save `covid_data_`data'', replace 
			
	/*--------------------------------------------------------------------------
		3. Read trade data for India     
	---------------------------------------------------------------------------*/
	* Define outcomes 
	if("`data'" == "imports"){
		local y log_import 
	} 
	else{
		local y log_export
	}
	
	* Use India data at the firm-month-HS6 level 
	import delimited "$path_data/India/processed_data/`data'_product_model_IND.csv", clear
		
	keep company_id year month date date_character hs6 `y' ebay_tradable cons_bec china_e_commerce durable_bec pay_or_ecomnod_t_2

	foreach var of varlist ebay_tradable cons_bec china_e_commerce durable_bec {
		replace `var' = "1" if `var' == "TRUE"
		replace `var' = "0" if `var' == "FALSE"
		replace `var' = "." if `var' == "NA" 
		destring `var', replace
	}


	/*--------------------------------------------------------------------------
	 4. Join datasets of trade, covid and instrumental variables    
	 ---------------------------------------------------------------------------*/

	* Merge instrumental variables
	merge m:1 company_id using `iv_data', keep(3) nogen

	* Merge covid variables
	merge m:1 company_id year month using `covid_data_`data'', keep(3) nogen
	
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
	rename pay_or_ecomnod_t_2 tech   // technology variable
	rename l2_total_cases cas       // logarithm of lagged (t-2) COVID total cases (at subnational level)
		
	* Run regressions for model that measures if tech adoption affects trade outcomes 
	foreach iv of varlist  ltg dspg uspg ltb dspb uspb  {
		foreach covid_var of varlist cas {
			
			gen `iv'_`covid_var' = `iv'*`covid_var'
			eststo `y'`iv'`covid_var': ivreghdfe `y' (tech = `iv'_`covid_var'), absorb(date company_id hs6) cluster(company_id hs6) savefirst savefprefix(f`y'`iv'`covid_var') 	
				
			estadd scalar f_fs = `e(widstat)': f`y'`iv'`covid_var'tech // Save first-stage f
				
		}
	}
				
}


/*--------------------------------------------------------------------------
	 6. Create tables 
---------------------------------------------------------------------------*/



* FIRST-STAGE ------------------------------
local dep_vars `"log_import log_export"'


* Iterate over dependent variable 
foreach y of local dep_vars {
	
	* Table for first-stage regression tables  
	
	* Creating tex file with the table for first stage
	cap file close fh 
	file open fh using "$path_outputs/India/regressions_results/products_model/first_stage_subnational_log_iv_`y'.tex", write replace  // Opening latex file for first stage

	file write fh "{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}\resizebox{\textwidth}{!}{  \begin{tabular}{l*{6}{c}} \hline\hline \toprule & \multicolumn{6}{c}{Dependent Variable: E-payment or E-commerce (t-2)} \\ \toprule & \multicolumn{3}{c}{Instrumental Variable (Google)} & \multicolumn{3}{c}{Instrumental Variable (Bing)} \\ \cmidrule(lr){2-4} \cmidrule(lr){5-7}  & Log(1+Lightning Strikes) & Log(1+Download Speed) & Log(1+Upload Speed) & Log(1+Lightning Strikes) & Log(1+Download Speed) & Log(1+Upload Speed)\\ \hline" _n // Header of the table
	
	
	* Iterate over covid variables 
	foreach covid_var of varlist cas {
		* Iterate over IV variables 
		foreach iv of varlist ltg dspg uspg ltb dspb uspb {
				
			estimates restore f`y'`iv'`covid_var'tech // Restore estimates 
			local f_`iv'_`covid_var' = e(f_fs)   // Kleibergen-Paap F-First stage 
			local N_`iv'_`covid_var' = e(N) // Number of observations	first-stage
			local b_`iv'_`covid_var' = _b[`iv'_`covid_var']      // Coefficient first-stage
			local se_`iv'_`covid_var' = _se[`iv'_`covid_var']    // Standard-Error first-stage			
			local t = `b_`iv'_`covid_var''/`se_`iv'_`covid_var'' // t-stat first-stage
			local p_value = 2*ttail(e(df_r),abs(`t'))            // p-value first-stage						
			if (`p_value' < 0.01){
				local star_`iv'_`covid_var' "\sym{***}"            
			}
			else if (`p_value' < 0.05){
				local star_`iv'_`covid_var' "\sym{**}"
			}
			else if (`p_value' < 0.1){
				local star_`iv'_`covid_var' "\sym{*}"
			}
			else{
				local star_`iv'_`covid_var' ""
			}			
		}

		* Create panel name  
		if("`covid_var'"== "cas"){
			file write fh "\hline \multicolumn{6}{l}{\textbf{COVID variable: Log(1+ Subnational COVID Cases)}}\\" _n  
		}
		
		* Adding coefficients and significance level stars 
		file write fh " $ covid_{i,t-2} \cdot Z_i $ " "&" %7.6fc (`b_ltg_`covid_var'') "`star_ltg_`covid_var''" "&"  %7.6fc (`b_dspg_`covid_var'') "`star_dspg_`covid_var''" "&"  %7.6fc (`b_uspg_`covid_var'') "`star_uspg_`covid_var''" "&"  %7.6fc (`b_ltb_`covid_var'') "`star_ltb_`covid_var''" "&"  %7.6fc (`b_dspb_`covid_var'') "`star_dspb_`covid_var''" "&"  %7.6fc (`b_uspb_`covid_var'') "`star_uspb_`covid_var''" "\\" _n		
		
		* Adding standard errors 
		file write fh "&" "(" %7.6fc (`se_ltg_`covid_var'') ")" "&" "(" %7.6fc (`se_dspg_`covid_var'')  ")" "&" "(" %7.6fc (`se_uspg_`covid_var'') ")" "&" "(" %7.6fc (`se_ltb_`covid_var'')  ")" "&" "(" %7.6fc (`se_dspb_`covid_var'')  ")" "&" "(" %7.6fc (`se_uspb_`covid_var'')  ") \\" _n  
		
		
		* Adding first-stage F-stat
		file write fh "Kleibergen-Paap F-stat" "&" %4.2fc (`f_ltg_`covid_var'') "&"  %4.2fc (`f_dspg_`covid_var'') "&"  %4.2fc (`f_uspg_`covid_var'')  "&" %4.2fc (`f_ltb_`covid_var'') "&"  %4.2fc (`f_dspb_`covid_var'') "&"  %4.2fc (`f_uspb_`covid_var'') "\\" _n
		
		
	}
	
	file write fh "\hline" _n
	* Adding Number of observations (it's the same across IV and covid vars so we only use one)
   file write fh "Number of Observations" "&" %9.0fc (`N_ltg_cas') "&"  %9.0fc (`N_dspg_cas') "&"  %9.0fc (`N_uspg_cas')  "&" %9.0fc (`N_ltb_cas') "&"  %9.0fc (`N_dspb_cas') "&"  %9.0fc (`N_uspb_cas') "\\" _n
						
    * Writting the end of the table
	file write fh "\hline\hline \end{tabular}}}" _n
	
	* Closing file 
	file close fh 

}

* SECOND-STAGE --------------------------

foreach y of local dep_vars {
	
	* Creating tex file with the table for first stage
	cap file close fh 
	file open fh using "$path_outputs/India/regressions_results/products_model/second_stage_subnational_log_iv_`y'.tex", write replace  // Opening latex file for first stage
	
	if("`y'" == "log_import"){
		local y_label "Log. Imports" 
	} 
	else{
		local y_label "Log. Exports"
	}
	

	file write fh "{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}\resizebox{\textwidth}{!}{  \begin{tabular}{l*{6}{c}} \hline\hline \toprule & \multicolumn{6}{c}{Dependent Variable: `y_label' } \\ \toprule & \multicolumn{3}{c}{Instrumental Variable (Google)} & \multicolumn{3}{c}{Instrumental Variable (Bing)} \\ \cmidrule(lr){2-4} \cmidrule(lr){5-7}  & Log(1+Lightning Strikes) & Log(1+Download Speed) & Log(1+Upload Speed) & Log(1+Lightning Strikes) & Log(1+Download Speed) & Log(1+Upload Speed)\\ \hline" _n // Header of the table
	
	
	* Iterate over covid variables 
	foreach covid_var of varlist cas {
		* Iterate over IV variables 
		foreach iv of varlist ltg dspg uspg ltb dspb uspb {
				
			estimates restore `y'`iv'`covid_var' // Restore estimates of second-stage
			local N_`iv'_`covid_var' = e(N) // Number of observations	second-stage
			local b_`iv'_`covid_var' = _b[tech]      // Coefficient tech var second-stage
			local se_`iv'_`covid_var' = _se[tech]    // Standard-Error second-stage			
			local t = `b_`iv'_`covid_var''/`se_`iv'_`covid_var'' // t-stat second-stage
			local p_value = 2*ttail(e(df_r),abs(`t'))            // p-value second-stage						
			if (`p_value' < 0.01){
				local star_`iv'_`covid_var' "\sym{***}"            
			}
			else if (`p_value' < 0.05){
				local star_`iv'_`covid_var' "\sym{**}"
			}
			else if (`p_value' < 0.1){
				local star_`iv'_`covid_var' "\sym{*}"
			}
			else{
				local star_`iv'_`covid_var' ""
			}			
		}

		* Create panel name  
		if("`covid_var'"== "cas"){
			file write fh "\hline \multicolumn{6}{l}{\textbf{COVID variable: Log( 1 + Subnational COVID Cases)}}\\" _n  
		}
		
		* Adding coefficients and significance level stars 
		file write fh "E-payment or E-commerce (t-2)" "&" %4.3fc (`b_ltg_`covid_var'') "`star_ltg_`covid_var''" "&"  %4.3fc (`b_dspg_`covid_var'') "`star_dspg_`covid_var''" "&"  %4.3fc (`b_uspg_`covid_var'') "`star_uspg_`covid_var''" "&"  %4.3fc (`b_ltb_`covid_var'') "`star_ltb_`covid_var''" "&"  %4.3fc (`b_dspb_`covid_var'') "`star_dspb_`covid_var''" "&"  %4.3fc (`b_uspb_`covid_var'') "`star_uspb_`covid_var''" "\\" _n		
		
		* Adding standard errors 
		file write fh "&" "(" %4.3fc (`se_ltg_`covid_var'') ")" "&" "(" %4.3fc (`se_dspg_`covid_var'')  ")" "&" "(" %4.3fc (`se_uspg_`covid_var'') ")" "&" "(" %4.3fc (`se_ltb_`covid_var'')  ")" "&" "(" %4.3fc (`se_dspb_`covid_var'')  ")" "&" "(" %4.3fc (`se_uspb_`covid_var'')  ") \\" _n  
		
		
		
	}
	
	file write fh "\hline" _n
	
	* Adding Number of observations (it's the same across IV and covid vars so we only use one)
   file write fh "Number of Observations" "&" %9.0fc (`N_ltg_cas') "&"  %9.0fc (`N_dspg_cas') "&"  %9.0fc (`N_uspg_cas')  "&" %9.0fc (`N_ltb_cas') "&"  %9.0fc (`N_dspb_cas') "&"  %9.0fc (`N_uspb_cas') "\\" _n
						
    * Writting the end of the table
	file write fh "\hline\hline \end{tabular}}}" _n
	
	* Closing file 
	file close fh 
		
}
