*=============================================================================
* Date:    April 2023
* Project: Analysis of the COVID-19 Shock, Technology and Trade in India,   
*          Indonesia and Mexico. 
*
* Author:  Simón Caicedo 
*
* Description: This do-file runs the IV analysis for India for the model that 
*              measures if tech adoption mitigates COVID impacts. In the IV 
*              regressions we include a different set of fixed effects (i + pt)
*              compared to the initial set of FE (FE_i, FE_p, FE_t).
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
 
import delimited "$path_data/India/raw_data/IV_data_based_on_Panjiva_data.csv", clear varnames(1)
rename our_id company_id
keep company_id bing_lightning_at_firm bing_ookla_d_speed_at_firm bing_ookla_u_speed_at_firm google_lightning_at_firm google_ookla_d_speed_at_firm google_ookla_u_speed_at_firm
* Convert NAs (missing values) to missing values in stata format
foreach var of varlist bing* google*{
	replace `var' = "." if `var' == "NA"
	destring `var', replace 
	replace `var' = log(1 + `var')  
}

tempfile iv_data
save `iv_data', replace 


/*--------------------------------------------------------------------------
 2. Read covid variables for India     
 ---------------------------------------------------------------------------*/
 
* Use India data at the firm-month-HS6 level 
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
	* Use India data at the firm-month-HS6 level 
	import delimited "$path_data/India/processed_data/`data'_tech_mitigation_model_IND.csv", clear
		
		
	keep company_id year month date date_character hs6 `y' ebay_tradable cons_bec china_e_commerce durable_bec adopted_pay_or_ecom_before_2019

	foreach var of varlist ebay_tradable cons_bec china_e_commerce durable_bec {
		replace `var' = "1" if `var' == "TRUE"
		replace `var' = "0" if `var' == "FALSE"
		replace `var' = "." if `var' == "NA" 
		destring `var', replace
	}
	
	
	* Generate variable with combination between product and date (year-month)
	gen pt = string(hs6) + "_" + date
	label var pt "Product-date combination"


	/*--------------------------------------------------------------------------
	 4. Join datasets of trade, covid and instrumental variables    
	 ---------------------------------------------------------------------------*/

	* Merge instrumental variables
	merge m:1 company_id using `iv_data', keep(3) nogen

	* Merge covid variables
	merge m:1 date using `covid_data', keep(1 3) nogen
	
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
	rename adopted_pay_or_ecom_before_2019 tech   // technology variable
	rename month_mean_stringency_index sti    // stringency index in t since endogenous variable is covid_t*tech_i
		
		
	gen techcovid = tech*sti
	
	
	* Run regressions for model that measures if tech adoption affects trade outcomes 
	foreach iv of varlist  ltg dspg uspg ltb dspb uspb  {	
		gen `iv'covid = `iv'*sti  
		eststo `y'`iv'covid: ivreghdfe `y' (techcovid = `iv'covid), absorb(company_id pt) cluster(company_id pt) savefirst savefprefix(f`y'`iv') 	
				
		estadd scalar f_fs = `e(widstat)': f`y'`iv'techcovid // Save first-stage f
				
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
	file open fh using "$path_outputs/India/regressions_results/tech_mitigation_covid_model/first_stage_log_iv_`y'_mitig_FE_i_pt.tex", write replace  // Opening latex file for first stage

	file write fh "{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}\resizebox{\textwidth}{!}{  \begin{tabular}{l*{6}{c}} \hline\hline \toprule & \multicolumn{6}{c}{Dependent Variable: Pre 2019 E-payment or E-commerce $\cdot covid_t$} \\ \toprule & \multicolumn{3}{c}{Instrumental Variable (Google)} & \multicolumn{3}{c}{Instrumental Variable (Bing)} \\ \cmidrule(lr){2-4} \cmidrule(lr){5-7}  & Log(1+Lightning Strikes) & Log(1+Download Speed) & Log(1+Upload Speed) & Log(1+Lightning Strikes) & Log(1+Download Speed) & Log(1+Upload Speed)\\ \hline" _n // Header of the table
	
	
	* Iterate over covid variables 
	* Iterate over IV variables 
	foreach iv of varlist ltg dspg uspg ltb dspb uspb {
				
		estimates restore f`y'`iv'techcovid // Restore estimates 
		local f_`iv' = e(f_fs)   // Kleibergen-Paap F-First stage 
		local N_`iv' = e(N) // Number of observations	first-stage
		local b_`iv' = _b[`iv'covid]      // Coefficient first-stage
		local se_`iv' = _se[`iv'covid]    // Standard-Error first-stage			
		local t = `b_`iv''/`se_`iv'' // t-stat first-stage
		local p_value = 2*ttail(e(df_r),abs(`t'))            // p-value first-stage						
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
	file write fh " $ covid_t \cdot Z_i $ " "&" %7.6fc (`b_ltg') "`star_ltg'" "&"  %7.6fc (`b_dspg') "`star_dspg'" "&"  %7.6fc (`b_uspg') "`star_uspg'" "&"  %7.6fc (`b_ltb') "`star_ltb'" "&"  %7.6fc (`b_dspb') "`star_dspb'" "&"  %7.6fc (`b_uspb') "`star_uspb'" "\\" _n		
		
	* Adding standard errors 
	file write fh "&" "(" %7.6fc (`se_ltg') ")" "&" "(" %7.6fc (`se_dspg')  ")" "&" "(" %7.6fc (`se_uspg') ")" "&" "(" %7.6fc (`se_ltb')  ")" "&" "(" %7.6fc (`se_dspb')  ")" "&" "(" %7.6fc (`se_uspb')  ") \\" _n  
		
		
	* Adding first-stage F-stat
	file write fh "Kleibergen-Paap F-stat" "&" %4.2fc (`f_ltg') "&"  %4.2fc (`f_dspg') "&"  %4.2fc (`f_uspg')  "&" %4.2fc (`f_ltb') "&"  %4.2fc (`f_dspb') "&"  %4.2fc (`f_uspb') "\\" _n
		
	
	file write fh "\hline" _n
	* Adding Number of observations (it's the same across IV and covid vars so we only use one)
	file write fh "Number of Observations" "&" %9.0fc (`N_ltg') "&"  %9.0fc (`N_dspg') "&"  %9.0fc (`N_uspg')  "&" %9.0fc (`N_ltb') "&"  %9.0fc (`N_dspb') "&"  %9.0fc (`N_uspb') "\\" _n
							
	* Writting the end of the table
	file write fh "\hline\hline \end{tabular}}}" _n
		
	* Closing file 
	file close fh 
	
}
	



* SECOND-STAGE --------------------------

foreach y of local dep_vars {
	
	* Creating tex file with the table for first stage
	cap file close fh 
	file open fh using "$path_outputs/India/regressions_results/tech_mitigation_covid_model/second_stage_log_iv_`y'_mitig_FE_i_pt.tex", write replace  // Opening latex file for first stage
	
	if("`y'" == "log_import"){
		local y_label "Log. Imports" 
	} 
	else{
		local y_label "Log. Exports"
	}
	

	file write fh "{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}\resizebox{\textwidth}{!}{  \begin{tabular}{l*{6}{c}} \hline\hline \toprule & \multicolumn{6}{c}{Dependent Variable: `y_label' } \\ \toprule & \multicolumn{3}{c}{Instrumental Variable (Google)} & \multicolumn{3}{c}{Instrumental Variable (Bing)} \\ \cmidrule(lr){2-4} \cmidrule(lr){5-7}  & Log(1+Lightning Strikes) & Log(1+Download Speed) & Log(1+Upload Speed) & Log(1+Lightning Strikes) & Log(1+Download Speed) & Log(1+Upload Speed)\\ \hline" _n // Header of the table
	
	
	
	* Iterate over IV variables 
	foreach iv of varlist ltg dspg uspg ltb dspb uspb {
				
		estimates restore `y'`iv'covid // Restore estimates of second-stage
		local N_`iv' = e(N) // Number of observations	second-stage
		local b_`iv' = _b[techcovid]      // Coefficient tech var second-stage
		local se_`iv' = _se[techcovid]    // Standard-Error second-stage			
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
	
		