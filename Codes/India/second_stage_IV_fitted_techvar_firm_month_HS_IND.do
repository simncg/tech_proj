*=============================================================================
* Date:    April 2023
* Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   
*          Indonesia and Mexico. 
*
* Author:  Simón Caicedo 
*
* Description: This do-file runs the second stage of the IV analysis at the
*              firm-month-hs6 level for India using the fitted technology variable
*              obtained from a first-stage at the firm-month level. 
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
	 4. Join dataset of fitted technology variable (obtained from a regression at the firm-month level) 
	 ---------------------------------------------------------------------------*/

	/* Merge with fitted technology variable. There are several tech vars since 
	 each one depends on the covid variable and instrument in which we regress 
	 the original tech var.*/
	 
	merge m:1 company_id year month using "$path_data/India/processed_data/tech_fitted_IV_firm_month_`data'_IND.dta", keep(3) nogen
	
	/*--------------------------------------------------------------------------
	 5. IV Regressions - Second Stage 
	 ---------------------------------------------------------------------------*/
	
		
	* Run regressions for model that measures if tech adoption affects trade outcomes 
	foreach iv in ltg dspg uspg ltb dspb uspb  { // lightning strikes (google), download speed(google), upload speed (google), lightning strikes (bing), download speed(bing), upload speed (bing)
		foreach covid_var in cas sti dwc {  // log covid cases, monthly stringency index, dummy workplace closure 
			
			eststo `y'`iv'`covid_var': ivreghdfe `y' tech_fitted_`iv'_`covid_var', absorb(date company_id hs6) cluster(company_id hs6) 
								
		}
	}
				
}


/*--------------------------------------------------------------------------
	 6. Create tables 
---------------------------------------------------------------------------*/



* SECOND-STAGE ------------------------------
local dep_vars `"log_import log_export"'

foreach y of local dep_vars {
	
	* Creating tex file with the table for first stage
	cap file close fh 
	file open fh using "$path_outputs/India/regressions_results/products_model/second_stage_log_iv_`y'_fitted_tech.tex", write replace  // Opening latex file for first stage
	
	if("`y'" == "log_import"){
		local y_label "Log. Imports" 
	} 
	else{
		local y_label "Log. Exports"
	}
	

	file write fh "{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}\resizebox{\textwidth}{!}{  \begin{tabular}{l*{6}{c}} \hline\hline \toprule & \multicolumn{6}{c}{Dependent Variable: `y_label' } \\ \toprule & \multicolumn{3}{c}{Instrumental Variable (Google)} & \multicolumn{3}{c}{Instrumental Variable (Bing)} \\ \cmidrule(lr){2-4} \cmidrule(lr){5-7}  & Log(1+Lightning Strikes) & Log(1+Download Speed) & Log(1+Upload Speed) & Log(1+Lightning Strikes) & Log(1+Download Speed) & Log(1+Upload Speed)\\ \hline" _n // Header of the table
	
	
	* Iterate over covid variables 
	foreach covid_var in cas sti dwc {
		* Iterate over IV variables 
		foreach iv in ltg dspg uspg ltb dspb uspb {
				
			estimates restore `y'`iv'`covid_var' // Restore estimates of second-stage
			local N_`iv'_`covid_var' = e(N) // Number of observations	second-stage
			local b_`iv'_`covid_var' = _b[tech_fitted_`iv'_`covid_var']      // Coefficient tech var second-stage
			local se_`iv'_`covid_var' = _se[tech_fitted_`iv'_`covid_var']    // Standard-Error second-stage			
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
			file write fh "\hline \multicolumn{6}{l}{\textbf{COVID variable: Log(1+COVID Cases)}}\\" _n  
		}
		else if("`covid_var'" == "sti"){
			file write fh "\hline \multicolumn{6}{l}{\textbf{COVID variable: Monthly Stringency Index}}\\" _n  
		}
		else if("`covid_var'" == "dwc"){
			file write fh "\hline \multicolumn{6}{l}{\textbf{COVID variable: Workplace Closure Dummy}}\\" _n  
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

	