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
	import delimited "$path_data/India/processed_data/`data'_tech_mitigation_model_IND.csv", clear
		
		
	keep company_id year month date date_character hs6 `y' ebay_tradable cons_bec china_e_commerce durable_bec adopted_pay_or_ecom_before_2019

	foreach var of varlist ebay_tradable cons_bec china_e_commerce durable_bec {
		replace `var' = "1" if `var' == "TRUE"
		replace `var' = "0" if `var' == "FALSE"
		replace `var' = "." if `var' == "NA" 
		destring `var', replace
	}


	/*--------------------------------------------------------------------------
	 4. Join dataset of fitted technology variable interacted with covid variable  
	 ---------------------------------------------------------------------------*/

	* Merge with dataset of fitted technology variable interacted with covid variable (monthlys tringency index) obtained from a regression run at the 
	* firm-month level 
	
	merge m:1 company_id year month using "$path_data/India/processed_data/tech_fitted_IV_firm_month_`data'_mitig_IND.dta", keep(3) nogen

	
	/*--------------------------------------------------------------------------
	 5. IV Regressions (No Product Categories)
	 ---------------------------------------------------------------------------*/
	
	* Run regressions for model that measures if tech adoption affects trade outcomes 
	foreach iv in ltg dspg uspg ltb dspb uspb  {	
		eststo `y'`iv'covid: reghdfe `y' techcovid_fitted_`iv'_covid, absorb(date company_id hs6) cluster(company_id hs6)  					
	}
				
}


/*--------------------------------------------------------------------------
	 6. Create tables 
---------------------------------------------------------------------------*/

* SECOND-STAGE --------------------------
local dep_vars `"log_import log_export"'


foreach y of local dep_vars {
	
	* Creating tex file with the table for first stage
	cap file close fh 
	file open fh using "$path_outputs/India/regressions_results/tech_mitigation_covid_model/second_stage_log_iv_`y'_mitig_fitted_tech.tex", write replace  // Opening latex file for first stage
	
	if("`y'" == "log_import"){
		local y_label "Log. Imports" 
	} 
	else{
		local y_label "Log. Exports"
	}
	

	file write fh "{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}\resizebox{\textwidth}{!}{  \begin{tabular}{l*{6}{c}} \hline\hline \toprule & \multicolumn{6}{c}{Dependent Variable: `y_label' } \\ \toprule & \multicolumn{3}{c}{Instrumental Variable (Google)} & \multicolumn{3}{c}{Instrumental Variable (Bing)} \\ \cmidrule(lr){2-4} \cmidrule(lr){5-7}  & Log(1+Lightning Strikes) & Log(1+Download Speed) & Log(1+Upload Speed) & Log(1+Lightning Strikes) & Log(1+Download Speed) & Log(1+Upload Speed)\\ \hline" _n // Header of the table
	
	
	
	* Iterate over IV variables 
	foreach iv in ltg dspg uspg ltb dspb uspb {
				
		estimates restore `y'`iv'covid // Restore estimates of second-stage
		local N_`iv' = e(N) // Number of observations	second-stage
		local b_`iv' = _b[techcovid_fitted_`iv'_covid]      // Coefficient tech var second-stage
		local se_`iv' = _se[techcovid_fitted_`iv'_covid]    // Standard-Error second-stage			
		local t = `b_`iv''/`se_`iv'' // t-stat second-stage
		local p_value = 2*ttail(e(df_r),abs(`t'))            // p-value second-stage						
		if (`p_value' < 0.01){
			local star_`iv' "\sym{***}"            
		}
		else if (`p_value' < 0.05){
			local star_`iv' "\sym{**}"
		}
		else if (`p_value' < 0.1){
			local star_`iv' "\sym{*}"
		}
		else{
			local star_`iv' ""
		}			
	}

		
	file write fh "\hline \multicolumn{6}{l}{\textbf{COVID variable: Monthly Stringency Index}}\\" _n  
	
		
	* Adding coefficients and significance level stars 
	file write fh "Pre 2019 E-payment or E-commerce $\cdot covid_t$" "&" %4.3fc (`b_ltg') "`star_ltg'" "&"  %4.3fc (`b_dspg') "`star_dspg'" "&"  %4.3fc (`b_uspg') "`star_uspg'" "&"  %4.3fc (`b_ltb') "`star_ltb'" "&"  %4.3fc (`b_dspb') "`star_dspb'" "&"  %4.3fc (`b_uspb') "`star_uspb'" "\\" _n		
		
	* Adding standard errors 
	file write fh "&" "(" %4.3fc (`se_ltg') ")" "&" "(" %4.3fc (`se_dspg')  ")" "&" "(" %4.3fc (`se_uspg') ")" "&" "(" %4.3fc (`se_ltb')  ")" "&" "(" %4.3fc (`se_dspb')  ")" "&" "(" %4.3fc (`se_uspb')  ") \\" _n  
		
	file write fh "\hline" _n
	
	* Adding Number of observations (it's the same across IV and covid vars so we only use one)
	file write fh "Number of Observations" "&" %9.0fc (`N_ltg') "&"  %9.0fc (`N_dspg') "&"  %9.0fc (`N_uspg')  "&" %9.0fc (`N_ltb') "&"  %9.0fc (`N_dspb') "&"  %9.0fc (`N_uspb') "\\" _n
							
	   * Writting the end of the table
	file write fh "\hline\hline \end{tabular}}}" _n
		
	* Closing file 
	file close fh 
			
		
}
	
		















