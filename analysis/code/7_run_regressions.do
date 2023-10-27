********************************************************************************
* Replication: TOU in Spain
********************************************************************************


/// GETTING TO PATH
cd "/Users/marreguant/Library/CloudStorage/GoogleDrive-mar.reguant@bse.eu/.shortcut-targets-by-id/1BU5l14i0SrXBAmBrDVi9LbwgG6Ew1s_t/ENECML/11_ToU/repository_data/analysis/output"

/// LOADING DATA
 
set type double
import delimited using "data/df_reg.csv", clear


/// VARIABLE CREATION
 
replace tou_real = 4 if tou_real==1
replace tou_fake = 4 if tou_fake==1

gen policy_tou = policy*tou_real
gen placebo_tou = placebo*tou_real

encode dist, gen(disti)
encode week, gen(weekd)

gen tempd = 0
replace tempd = 1 if temp > 20

drop if temp == .

* weekend / weekday
gen policy_tou_wk = 0
replace policy_tou_wk = policy*tou_real if week=="true"
gen placebo_tou_wk = 0
replace placebo_tou_wk = placebo*tou_real if week=="true"
gen policy_tou_we = 0
replace policy_tou_we = policy*tou_fake if weekend=="true"
gen placebo_tou_we = 0
replace placebo_tou_we = placebo*tou_fake if weekend=="true"

* triple diff vars
foreach c in "policy" "placebo" {
	forvalues i = 2(1)4 {
		gen `c'_tou_wed_`i' = 0
		replace `c'_tou_wed_`i' = 1 if `c'_tou_we == `i' | `c'_tou_wk == `i'
	}
}

drop week_c temph 
drop if year==2020

order disti year month day tou_fake weekd month_count 

egen clust = group(month_count disti)
egen clust_bi = group(bimonth disti)

replace cons_w = round(cons_w * 1000.0)

/// SPECS
 
 global clust = "clust"
 global fe1 "weekd#month_count#hour weekd#disti#hour"
 global fe2 "weekd#month_count#hour weekd#disti#hour#month"
 global fe3 "weekd#month_count#hour weekd#disti#hour#month weekd#hour#tempd#c.temp"
 global fe4 "weekd#month_count#hour weekd#disti#hour#month weekd#disti#month_count"
 

 
/// BASELINE ESTIMATES
est clear
foreach yvar in "log_demand" "cons_res_lasso" "cons_res_rf" "cons_res_lasso_log" "cons_res_rf_log" {

	forvalues s = 1(1)4 {
		reghdfe `yvar' i.policy_tou i.placebo_tou [fw=cons_w], ///
		 absorb(${fe`s'}) vce(cluster $clust)
		est sto spec`s'
	
	}

	estout spec* using "tables/`yvar'.tex", replace /// 
		style(tex) drop(0.policy_tou 0.placebo_tou _cons) cells(b(star fmt(3)) se(par fmt(3))) collabels(none) mlabels(none) ///
		stats(r2_a_within N, fmt(3 %9.0gc) label("\midrule R-sqr" Observations)) ///
		order(4.policy_tou 2.policy_tou 3.policy_tou " " 4.placebo_tou 2.placebo_tou 3.placebo_tou) ///
		refcat(4.policy_tou "\textbf{Policy}" 4.placebo_tou "\textbf{Placebo}", nolabel) ///
		label varlabels(4.policy_tou "  Off-peak" 2.policy_tou "  Mid-peak" 3.policy_tou "  Peak" ///
			4.placebo_tou "  Off-peak" 2.placebo_tou "  Mid-peak" 3.placebo_tou "  Peak") nodropped noomitted
	
}


/// WEEKEND ESTIMATES
foreach yvar in "log_demand" "cons_res_lasso" "cons_res_rf" "cons_res_lasso_log" "cons_res_rf_log" {

	est clear
	forvalues s = 1(1)4 {
		reghdfe `yvar' i.policy_tou_wk i.placebo_tou_wk i.policy_tou_we i.placebo_tou_we [fw=cons_w], ///
		 absorb(${fe`s'}) vce(cluster $clust)
		est sto spec`s'
	
	}
	
	estout spec* using "tables/`yvar'_wk.tex", replace collabels(none) mlabels(none)  /// 
		style(tex) drop(0.policy_tou_wk 0.placebo_tou_wk 0.policy_tou_we 0.placebo_tou_we _cons)  cells(b(star fmt(3)) se(par fmt(3))) ///
		stats(r2_a_within N, fmt(3 %9.0gc) label("\midrule R-sqr" Observations)) ///
		order(4.policy_tou_wk 2.policy_tou_wk 3.policy_tou_wk 4.policy_tou_we 2.policy_tou_we 3.policy_tou_we  ///
			   4.placebo_tou_wk 2.placebo_tou_wk 3.placebo_tou_wk 4.placebo_tou_we 2.placebo_tou_we 3.placebo_tou_we) ///
		refcat(4.policy_tou_wk "\textbf{Policy Weekday}" 4.policy_tou_we "\textbf{Policy Weekend}" ///
				4.placebo_tou_wk "\textbf{Placebo Weekday}" 4.placebo_tou_we "\textbf{Placebo Weekend}", nolabel) ///
		label varlabels(4.policy_tou_wk "  Off-peak" 2.policy_tou_wk "  Mid-Peak" 3.policy_tou_wk "  Peak" ///
			4.placebo_tou_wk "  Off-peak" 2.placebo_tou_wk "  Mid-Peak" 3.placebo_tou_wk "  Peak" ///
			4.policy_tou_we "  Off-peak" 2.policy_tou_we "  Mid-Peak" 3.policy_tou_we "  Peak" ///
			4.placebo_tou_we "  Off-peak" 2.placebo_tou_we "  Mid-Peak" 3.placebo_tou_we "  Peak" ) nodropped noomitted
			
	* no placebo
	est clear
	forvalues s = 1(1)4 {
		reghdfe `yvar' i.policy_tou_wk i.policy_tou_we [fw=cons_w], ///
		 absorb(${fe`s'}) vce(cluster $clust)
		est sto spec`s'
	
	}
	
	estout spec* using "tables/`yvar'_wk_nop.tex", replace collabels(none) mlabels(none)  /// 
		style(tex) drop(0.policy_tou_wk 0.policy_tou_we _cons)  cells(b(star fmt(3)) se(par fmt(3))) ///
		stats(r2_a_within N, fmt(3 %9.0gc) label("\midrule R-sqr" Observations)) ///
		order(4.policy_tou_wk 2.policy_tou_wk 3.policy_tou_wk 4.policy_tou_we 2.policy_tou_we 3.policy_tou_we) ///
		refcat(4.policy_tou_wk "\textbf{Policy Weekday}" 4.policy_tou_we "\textbf{Policy Weekend}", nolabel) ///
		label varlabels(4.policy_tou_wk "  Off-peak" 2.policy_tou_wk "  Mid-Peak" 3.policy_tou_wk "  Peak" ///
			4.policy_tou_we "  Off-peak" 2.policy_tou_we "  Mid-Peak" 3.policy_tou_we "  Peak") nodropped noomitted			
	
}

/// TRIPLE-DIFF ESTIMATES
* 
foreach yvar in "log_demand" "cons_res_lasso" "cons_res_rf" "cons_res_lasso_log" "cons_res_rf_log"  {

	est clear 
	
	forvalues s = 1(1)4 {
		reghdfe `yvar' policy_tou_wed* placebo_tou_wed* i.policy_tou_wk i.placebo_tou_wk [fw=cons_w], ///
		 absorb(${fe`s'}) vce(cluster $clust)
		est sto spec`s'
	
	}
	
	estout spec* using "tables/`yvar'_wk_diff.tex", replace collabels(none) mlabels(none)  /// 
		style(tex) drop(0.policy_tou_wk 0.placebo_tou_wk _cons)  cells(b(star fmt(3)) se(par fmt(3))) ///
		stats(r2_a_within N, fmt(3 %9.0gc) label("\midrule R-sqr" Observations)) ///
		order(policy_tou_wed_4 policy_tou_wed_2 policy_tou_wed_3 4.policy_tou_wk 2.policy_tou_wk 3.policy_tou_wk  ///
			   placebo_tou_wed_4 placebo_tou_wed_2 placebo_tou_wed_3 4.placebo_tou_wk 2.placebo_tou_wk 3.placebo_tou_wk) ///
		refcat(policy_tou_wed_4 "\textbf{Policy}"  4.policy_tou_wk "\textbf{$\Delta$ Policy Weekday}" ///
				placebo_tou_wed_4 "\textbf{Placebo}" 4.placebo_tou_wk "\textbf{$\Delta$ Placebo Weekday}", nolabel) ///
		label varlabels(policy_tou_wed_4 "  Off-peak" policy_tou_wed_2 "  Mid-Peak" policy_tou_wed_3 "  Peak" ///
			placebo_tou_wed_4 "  Off-peak" placebo_tou_wed_2 "  Mid-Peak" placebo_tou_wed_3 "  Peak" ///
			4.policy_tou_wk "  Off-peak" 2.policy_tou_wk "  Mid-Peak" 3.policy_tou_wk "  Peak" ///
			4.placebo_tou_wk "  Off-peak" 2.placebo_tou_wk "  Mid-Peak" 3.placebo_tou_wk "  Peak" ) nodropped noomitted
	
	* no placebo
	est clear
	forvalues s = 1(1)4 {
		reghdfe `yvar' policy_tou_wed* i.policy_tou_wk [fw=cons_w], ///
		 absorb(${fe`s'}) vce(cluster $clust)
		est sto spec`s'
	
	}
	
	estout spec* using "tables/`yvar'_wk_diff_nop.tex", replace collabels(none) mlabels(none)  /// 
		style(tex) drop(0.policy_tou_wk _cons)  cells(b(star fmt(3)) se(par fmt(3))) ///
		stats(r2_a_within N, fmt(3 %9.0gc) label("\midrule R-sqr" Observations)) ///
		order(policy_tou_wed_4 policy_tou_wed_2 policy_tou_wed_3 4.policy_tou_wk 2.policy_tou_wk 3.policy_tou_wk) ///
		refcat(policy_tou_wed_4 "\textbf{Policy}"  4.policy_tou_wk "\textbf{$\Delta$ Policy Weekday}", nolabel) ///
		label varlabels(policy_tou_wed_4 "  Off-peak" policy_tou_wed_2 "  Mid-Peak" policy_tou_wed_3 "  Peak" ///
			4.policy_tou_wk "  Off-peak" 2.policy_tou_wk "  Mid-Peak" 3.policy_tou_wk "  Peak") nodropped noomitted
	
}


/// BI-MONTH STD ERROR
{

	est clear
	
		reghdfe log_demand i.policy_tou i.placebo_tou [fw=cons_w], ///
		 absorb(${fe2}) vce(cluster $clust)
		est sto spec1
	
		reghdfe log_demand i.policy_tou i.placebo_tou [fw=cons_w], ///
		 absorb(${fe2}) vce(cluster clust_bi)
		est sto spec2
		
		reghdfe cons_res_lasso i.policy_tou i.placebo_tou [fw=cons_w], ///
		 absorb(${fe2}) vce(cluster $clust)
		est sto spec3
		
		reghdfe cons_res_lasso i.policy_tou i.placebo_tou [fw=cons_w], ///
		 absorb(${fe2}) vce(cluster clust_bi)
		est sto spec4	
		
		
	estout spec* using "tables/clusters.tex", replace /// 
		style(tex) drop(0.policy_tou 0.placebo_tou _cons) cells(b(star fmt(3)) se(par fmt(3))) collabels(none) mlabels(none) ///
		stats(r2_a_within N, fmt(3 %9.0gc) label("\midrule R-sqr" Observations)) ///
		order(4.policy_tou 2.policy_tou 3.policy_tou " " 4.placebo_tou 2.placebo_tou 3.placebo_tou) ///
		refcat(4.policy_tou "\textbf{Policy}" 4.placebo_tou "\textbf{Placebo}", nolabel) ///
		label varlabels(4.policy_tou "  Off-peak" 2.policy_tou "  Mid-peak" 3.policy_tou "  Peak" ///
			4.placebo_tou "  Off-peak" 2.placebo_tou "  Mid-peak" 3.placebo_tou "  Peak") nodropped noomitted
	
}


/// IV REGRESSION

cap drop price 
cap drop mean_price
cap drop mprice 
cap drop post
cap drop price_pt
cap drop log_tou_pmean
gen post = 0
replace post = 1 if month_count >= 42

gen price = exp(log_price)
replace price = . if dist == "PT_reg"
bys year month day: egen mean_price = mean(price)
gen price_pt = 1
_pctile mean_price if post==1, percentiles(25 75 90)
replace price_pt = 1 if mean_price < r(r1) & post == 1
replace price_pt = 2 if mean_price >= r(r1) & mean_price < r(r2) & post == 1
replace price_pt = 3 if mean_price >= r(r2) & mean_price < r(r3) & post == 1
replace price_pt = 4 if mean_price >= r(r3) & post == 1
drop mean_price

replace price = exp(log_price)
replace price = exp(log_price)-exp(log_tou) if dist!="PT_reg"
replace price = . if post==1
bys dist: egen mprice = mean(price)
replace mprice = 100
gen log_tou_pmean = log(mprice + exp(log_tou_pe))

replace price = exp(log_price)
replace price = exp(log_price)-exp(log_tou) if dist!="PT_reg"

cap drop price1-inst4
forvalues pt = 1(1)4 {
	gen price`pt' = 0
	replace price`pt' = log_price if price_pt==`pt' | post == 0
	gen inst`pt' = 0
	replace inst`pt' = log_tou_pmean if price_pt==`pt' | post == 0
}


foreach yvar in "log_demand" "cons_res_lasso" {

	est clear
	forvalues s = 1(1)4 {
		ivreghdfe `yvar' ( log_price = log_tou_pmean ) [fw=cons_w], ///
		 absorb(${fe`s'}) vce(cluster ${clust})
		eststo spec`s'
	}
	
	ivreghdfe `yvar' (log_price price2 price3 price4 = log_tou_pmean inst2 inst3 inst4) [fw=cons_w], ///
		 absorb(${fe2}) cluster(${clust})
	eststo specsplit
	
	estout spec* using "tables/iv_`yvar'.tex", replace /// 
		style(tex) cells(b(star fmt(3)) se(par fmt(3))) collabels(none) mlabels(none) ///
		stats(N, fmt(%9.0gc) label("\midrule Observations")) ///
		label varlabels(log_price "Log(price)"  price1 "  $<$25th" ///
			price2 "$\Delta$  25 - 75th " price3 "$\Delta$ 75 - 90th" price4 "$\Delta$ $>$90th") ///
		nodropped noomitted
}


/// BLOCK EFFECTS - ALL
gen block = ceil(hour/3)
summ block
cap qui forvalues h = 1(1)`r(max)' {
 	gen policy_tou_wk_`h' = 0
	replace policy_tou_wk_`h' = policy if block==`h' & weekd == 2
	gen placebo_tou_wk_`h' = 0
	replace placebo_tou_wk_`h' = placebo if block==`h' & weekd == 2
	gen policy_tou_we_`h' = 0
	replace policy_tou_we_`h' = policy if block==`h' & weekd == 1
	gen placebo_tou_we_`h' = 0
	replace placebo_tou_we_`h' = placebo if block==`h' & weekd == 1
 }
 
foreach yvar in "log_demand" "cons_res_lasso" "cons_res_rf" "cons_res_lasso_log" "cons_res_rf_log" {
 forvalues spec = 1(1)3 {
 	
	reghdfe `yvar' i.policy_tou_wk_* i.placebo_tou_wk_* i.policy_tou_we_* i.placebo_tou_we_* [fw=cons_w], ///
		absorb(${fe`spec'}) vce(cluster disti#month_count)

	if (`spec'==1 & "`yvar'"=="log_demand") {
		regsave using results, replace addlabel(model, "`yvar'", spec, `spec')
	}
	else {
		regsave using results, append addlabel(model, "`yvar'", spec, `spec')
	}
 }
}

* cleaning up coefficients
preserve
use results.dta, clear
drop if var=="_cons"
gen prefix = substr(var,1,1)
drop if prefix=="0"
replace var = subinstr(var,"_"," ",.)
replace var = subinstr(var,"."," ",.)
gen treatment = word(var, 2)
gen period = word(var, 4)
gen hour = real(word(var, 5))

drop prefix var

order model spec period treatment hour
sort model spec period treatment hour

export delimited using "data/results_plot.csv", replace
restore


/// BLOCK EFFECTS - ALL
summ block
cap qui forvalues h = 1(1)`r(max)' {
	replace placebo_tou_wk_`h' = placebo + policy if block==`h' & weekd == 2
	replace placebo_tou_we_`h' = placebo + policy if block==`h' & weekd == 1
 }
 
foreach yvar in "log_demand" "cons_res_lasso" "cons_res_rf" "cons_res_lasso_log" "cons_res_rf_log" {
 forvalues spec = 1(1)3 {
 	
	reghdfe `yvar' i.policy_tou_wk_* i.placebo_tou_wk_* i.policy_tou_we_* i.placebo_tou_we_* [fw=cons_w], ///
		absorb(${fe`spec'}) vce(cluster disti#month_count)

	if (`spec'==1 & "`yvar'"=="log_demand") {
		regsave using results, replace addlabel(model, "`yvar'", spec, `spec')
	}
	else {
		regsave using results, append addlabel(model, "`yvar'", spec, `spec')
	}
 }
}

* cleaning up coefficients
preserve
use results.dta, clear
drop if var=="_cons"
gen prefix = substr(var,1,1)
drop if prefix=="0"
replace var = subinstr(var,"_"," ",.)
replace var = subinstr(var,"."," ",.)
gen treatment = word(var, 2)
gen period = word(var, 4)
gen hour = real(word(var, 5))

drop prefix var

order model spec period treatment hour
sort model spec period treatment hour

export delimited using "data/results_plot_diff.csv", replace
restore


/// BLOCK EFFECTS - DIST
summ block
cap qui forvalues h = 1(1)`r(max)' {
	foreach d in "ENDESA" "IBERDROLA" "NATURGY" "REPSOL" "EDP" {
 	gen policy_dist_wk_`h'_`d' = 0
	replace policy_dist_wk_`h'_`d' = policy if block==`h' & weekd == 2 & dist=="`d'"
	gen placebo_dist_wk_`h'_`d' = 0
	replace placebo_dist_wk_`h'_`d' = placebo if block==`h' & weekd == 2 & dist=="`d'"
	gen policy_dist_we_`h'_`d' = 0
	replace policy_dist_we_`h'_`d' = policy if block==`h' & weekd == 1 & dist=="`d'"
	gen placebo_dist_we_`h'_`d' = 0
	replace placebo_dist_we_`h'_`d' = placebo if block==`h' & weekd == 1 & dist=="`d'"
 }
}
foreach yvar in "log_demand" "cons_res_lasso" "cons_res_rf" "cons_res_lasso_log" "cons_res_rf_log" {
 forvalues spec = 1(1)3 {
 	
	reghdfe `yvar' i.policy_dist_wk_* i.placebo_dist_wk_* ///
		i.policy_dist_we_* i.placebo_dist_we_* [fw=cons_w], ///
		absorb(${fe`spec'}) vce(cluster disti#month_count)

	if (`spec'==1 & "`yvar'"=="log_demand") {
		regsave using results_dist, replace addlabel(model, "`yvar'", spec, `spec')
	}
	else {
		regsave using results_dist, append addlabel(model, "`yvar'", spec, `spec')
	}
 }
}

* cleaning up coefficients
preserve
use results_dist.dta, clear
drop if var=="_cons"
gen prefix = substr(var,1,1)
drop if prefix=="0"
replace var = subinstr(var,"_"," ",.)
replace var = subinstr(var,"."," ",.)
gen treatment = word(var, 2)
gen period = word(var, 4)
gen hour = real(word(var, 5))
gen dist = word(var, 6)

drop prefix var

order model spec dist period treatment hour
sort model spec dist period treatment hour

export delimited using "data/results_plot_dist.csv", replace
restore
