/*
This script imports cleaned data and replicates the regression analysis performed in original_analysis_script.do

*/

clear
*cd "D:\Research\Charter\Charterdata"
cd "C:\Users\nickm\OneDrive\Acer (new laptop)\Documents\PhD\Tulane University\Projects\Charter School Heterogeneity\data"
global control "logenroll perwht perblk perhsp perfrl perspeced urban suburb town rural p_rev p_exp str tea_salary num_magnet charter_eff"

global sum "enrollment perwht perblk perhsp perfrl perspeced urban suburb town rural p_rev p_exp str tea_salary num_magnet"
set scheme s2color  



*****************************************************************************
***** Table 1 Summary statistics
*****************************************************************************

// AFGR

use charter_afgr2, clear
gen treated=maxshare2>0
duplicates drop district, force
bysort state: egen obsn=sum(treated)
keep if obsn==0 | obsn>=5
keep district

merge 1:m district using charter_afgr2, keep(3) nogen
merge m:1 district using credible_afgr, nogen
gen sample2=cred_dist==1 | maxshare2==0
gen sample3=pyear>1995    // drop always treated


eststo clear		
eststo:  quietly estpost  sum afgr  $sum  [aw=eweight]
eststo:  quietly estpost  sum afgr  $sum  [aw=eweight] if sample2==1
eststo:  quietly estpost  sum afgr  $sum  [aw=eweight] if sample3==1
esttab,  cells(" mean(fmt(%20.3fc)) ") stats(N)

preserve
duplicates drop district, force
sum year
sum year if sample2==1
sum year if sample3==1
restore


// SEDA

use charter_seda, clear
gen treated=maxshare2>0
duplicates drop district, force
bysort state: egen obsn=sum(treated)
keep if obsn==0 | obsn>=5
keep district

merge 1:m district using charter_seda, keep(3) nogen

merge m:1 district using credible_seda, nogen

gen sample2=cred_dist==1 | maxshare2==0
gen sample3=pyear>2008    // drop always treated

eststo clear		
eststo:  quietly estpost  sum st_math st_ela $sum [aw=eweight]
eststo:  quietly estpost  sum st_math st_ela $sum [aw=eweight] if sample2==1
eststo:  quietly estpost  sum st_math st_ela $sum [aw=eweight] if sample3==1
esttab,  cells(" mean(fmt(%20.3fc)) ") stats(N)


preserve
duplicates drop district, force
sum year
sum year if sample2==1
sum year if sample3==1
restore






*****************************************************************************
*    Table 2  Average Treatment Effects
*****************************************************************************

 
******************************************************************************
** AFGR
use charter_afgr2, clear
gen treated=maxshare2>0
duplicates drop district, force
bysort state: egen obsn=sum(treated)
keep if obsn==0 | obsn>=5
keep district

merge 1:m district using charter_afgr2, keep(3) nogen



merge m:1 district using credible_afgr, nogen
gen sample2=cred_dist==1 | maxshare2==0
gen sample3=pyear>1995    // drop always treated


gen inter=lag_share
eststo clear 
gen outcome=afgr
eststo: quietly reghdfe outcome inter  [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter $control  [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)

eststo: quietly reghdfe outcome inter  if sample2==1 [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter $control  if sample2==1  [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)

eststo: quietly reghdfe outcome inter  if sample3==1 [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter $control  if sample3==1 [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)


 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)"  ) keep(inter) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
 stats(r2 N c1 c2 c3, fmt(%9.3f %9.0fc) labels("R-squared" "Observations" ///
 "District, year FE" "District covariates" "State by year FE")) ///
 plain b(%9.3f) se(%9.3f) se brackets coef(inter "Charter") varwidth(32) ///
 title(Regression results:Dose)  


******************************************************************************
** SEDA

 
 
 
use charter_seda, clear
gen treated=maxshare2>0
duplicates drop district, force
bysort state: egen obsn=sum(treated)
keep if obsn==0 | obsn>=5
keep district

merge 1:m district using charter_seda, keep(3) nogen

merge m:1 district using credible_seda, nogen
gen sample2=cred_dist==1 | maxshare2==0
gen sample3=pyear>2008    // drop always treated



gen inter=lag_grade
eststo clear 
gen outcome=st_math
eststo: quietly reghdfe outcome inter  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)

eststo: quietly reghdfe outcome inter if sample2==1  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter $control  if sample2==1   [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)

eststo: quietly reghdfe outcome inter if sample3==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter $control  if sample3==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)



 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" ) keep(inter) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
 stats(r2 N c1 c2 c3, fmt(%9.3f %9.0fc) labels("R-squared" "Observations" ///
 "District & year FE" "District-level covariates" "State by year FE")) ///
 plain b(%9.3f) se(%9.3f) se brackets coef(inter "Charter") varwidth(32) 


 

eststo clear 
replace outcome=st_ela
eststo: quietly reghdfe outcome inter  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)

eststo: quietly reghdfe outcome inter   if sample2==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter $control  if sample2==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)

eststo: quietly reghdfe outcome inter  if sample3==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter $control  if sample3==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)


 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" ) keep(inter) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
 stats(r2 N c1 c2 c3, fmt(%9.3f %9.0fc) labels("R-squared" "Observations" ///
 "District & year FE" "District-level covariates" "State by year FE")) ///
 plain b(%9.3f) se(%9.3f) se brackets coef(inter "Charter") varwidth(32) 
 
 
*****************************************************************************
***** Table 3 Compare with CREDO (2023)
*****************************************************************************

// Math


use charter_seda, clear
gen treated=maxshare2>0
duplicates drop district, force
bysort state: egen obsn=sum(treated)
keep if obsn==0 | obsn>=5
keep district
merge 1:m district using charter_seda, keep(3) nogen

gen inter=lag_grade

quietly reghdfe st_math inter $control [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
regsave inter using math_s1, replace tstat pval addlabel(state,0, postal, All)

drop group
egen group=group(state) 
foreach y of numlist 1/51 {
display `y'
quietly sum state if group==`y'
local S = r(mean)
capture quietly reghdfe st_math inter $control [pw=eweight] if group==`y', absorb(sgyear district##c.year) vce(cluster district)
capture regsave inter using math_s1, append tstat pval addlabel(state,`S')
 }
 
// sample 2
use charter_seda, clear
gen treated=maxshare2>0
duplicates drop district, force
bysort state: egen obsn=sum(treated)
keep if obsn==0 | obsn>=5
keep district
merge 1:m district using charter_seda, keep(3) nogen
merge m:1 district using credible_seda, nogen
gen sample2=cred_dist==1 | maxshare2==0

keep if sample2==1

gen inter=lag_grade
quietly reghdfe st_math inter $control [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
regsave inter using math_s2, replace tstat pval addlabel(state,0, postal, All)

drop group
egen group=group(state) 
foreach y of numlist 1/51 {
display `y'
quietly sum state if group==`y'
local S = r(mean)
capture quietly reghdfe st_math inter $control [pw=eweight] if group==`y', absorb(sgyear district##c.year) vce(cluster district)
capture regsave inter using math_s2, append tstat pval addlabel(state,`S')
 }
// Sample 3

use charter_seda, clear
gen sample3=pyear>2008    // drop always treated
keep if sample3==1
gen treated=maxshare2>0
duplicates drop district, force
bysort state: egen obsn=sum(treated)
keep if obsn==0 | obsn>=5
keep district
merge 1:m district using charter_seda, keep(3) nogen

gen inter=lag_grade

quietly reghdfe st_math inter $control [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
regsave inter using math_s3, replace tstat pval addlabel(state,0, postal, All)

drop group
egen group=group(state) 
foreach y of numlist 1/51 {
display `y'
quietly sum state if group==`y'
local S = r(mean)
capture quietly reghdfe st_math inter $control [pw=eweight] if group==`y', absorb(sgyear district##c.year) vce(cluster district)
capture regsave inter using math_s3, append tstat pval addlabel(state,`S')
 }

 
 
// ELA


use charter_seda, clear
gen treated=maxshare2>0
duplicates drop district, force
bysort state: egen obsn=sum(treated)
keep if obsn==0 | obsn>=5
keep district
merge 1:m district using charter_seda, keep(3) nogen

gen inter=lag_grade

quietly reghdfe st_ela inter $control [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
regsave inter using ela_s1, replace tstat pval addlabel(state,0, postal, All)

drop group
egen group=group(state) 
foreach y of numlist 1/51 {
display `y'
quietly sum state if group==`y'
local S = r(mean)
capture quietly reghdfe st_ela inter $control [pw=eweight] if group==`y', absorb(sgyear district##c.year) vce(cluster district)
capture regsave inter using ela_s1, append tstat pval addlabel(state,`S')
 }
 
// Sample 2
use charter_seda, clear
gen treated=maxshare2>0
duplicates drop district, force
bysort state: egen obsn=sum(treated)
keep if obsn==0 | obsn>=5
keep district
merge 1:m district using charter_seda, keep(3) nogen
merge m:1 district using credible_seda, nogen
gen sample2=cred_dist==1 | maxshare2==0
keep if sample2==1


gen inter=lag_grade
quietly reghdfe st_ela inter $control [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
regsave inter using ela_s2, replace tstat pval addlabel(state,0, postal, All)

drop group
egen group=group(state) 
foreach y of numlist 1/51 {
display `y'
quietly sum state if group==`y'
local S = r(mean)
capture quietly reghdfe st_ela inter $control [pw=eweight] if group==`y', absorb(sgyear district##c.year) vce(cluster district)
capture regsave inter using ela_s2, append tstat pval addlabel(state,`S')
 }
// Sample 3

use charter_seda, clear
gen sample3=pyear>2008    // drop always treated
keep if sample3==1
gen treated=maxshare2>0
duplicates drop district, force
bysort state: egen obsn=sum(treated)
keep if obsn==0 | obsn>=5
keep district
merge 1:m district using charter_seda, keep(3) nogen


gen inter=lag_grade

quietly reghdfe st_ela inter $control [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
regsave inter using ela_s3, replace tstat pval addlabel(state,0, postal, All)

drop group
egen group=group(state) 
foreach y of numlist 1/51 {
display `y'
quietly sum state if group==`y'
local S = r(mean)
capture quietly reghdfe st_ela inter $control [pw=eweight] if group==`y', absorb(sgyear district##c.year) vce(cluster district)
capture regsave inter using ela_s3, append tstat pval addlabel(state,`S')
 }

 
 
 
 
use math_s1, clear
merge 1:1 state using statename, update nogen
keep coef pval postal statename state credo_math credo_ela
rename (coef pval) (coef_math1 pval_math1)
merge 1:1 state using math_s2, nogen
rename (coef pval) (coef_math2 pval_math2)
merge 1:1 state using math_s3, nogen
rename (coef pval) (coef_math3 pval_math3)
merge 1:1 state using ela_s1, nogen
rename (coef pval) (coef_ela1 pval_ela1)
merge 1:1 state using ela_s2, nogen
rename (coef pval) (coef_ela2 pval_ela2)
merge 1:1 state using ela_s3, nogen
rename (coef pval) (coef_ela3 pval_ela3)


keep postal statename state credo_math credo_ela coef_math* pval_math* coef_ela* pval_ela* 
order statename state coef_math1 pval_math1 coef_ela1 pval_ela1 coef_math2 pval_math2 coef_ela2 pval_ela2 coef_math3 pval_math3 coef_ela3 pval_ela3
 



foreach y of varlist pval_math1 pval_math2 pval_math3 pval_ela1 pval_ela2 pval_ela3 {
gen sig_`y'=""
replace sig_`y'="***" if `y'<=0.01 
replace sig_`y'="**" if `y'>0.01 & `y'<=0.05
replace sig_`y'="*" if `y'>0.05 & `y'<=0.1
}



foreach y of varlist credo_math credo_ela {
gen var1=regexr(`y',"[.\}\)\*a-zA-Z]+","")  
gen var2=regexr(`y',"-","")
gen var3=regexr(var2,"[.0-9]+","")
destring var1, force replace
replace var1=var1/5.7
tostring var1, force replace format(%9.2f) 
replace `y'=var1+var3
drop var1 var2 var3
}

 tostring coef_*, force replace format(%9.2f) 
foreach x of newlist math ela {
gen `x'=""
foreach y of numlist 1 2 3 {
 gen `x'`y'=coef_`x'`y'+sig_pval_`x'`y'
}
}
drop if state==0
keep postal math1 math2 math3 ela1 ela2 ela3 credo_math credo_ela
order postal math1 ela1 math2 ela2 math3 ela3 credo_math credo_ela
 
foreach y of varlist credo_* math* ela* {
split `y', parse("*") destring
}
corr credo_math1 math11 
corr credo_ela1 ela11 
corr credo_math1 math21 
corr credo_ela1 ela21 
corr credo_math1 math31
corr credo_ela1 ela31

*****************************************************************************
***** Table 4  Heterogeneous  result
*****************************************************************************
 
 
 
 
* A  Heterogeneous  results: Graduation rate
*****************************************************************************


use charter_afgr2, clear
merge m:1 district using credible_afgr, nogen
gen sample2=cred_dist==1 | maxshare2==0
gen sample3=pyear>1995    // drop always treated
merge m:1 state using  policy3, keep(3) nogen


gen base=afgr
bysort district: egen baseyear=min(year)
bysort district: egen noexcuses_mean=mean(noexcuses_share)
replace noexcuses_share=noexcuses_mean

// interaction of base value
 foreach y of varlist perblk perhsp perfrl base {   // nacsa napcs cer
 gen base_`y'=`y' if year==baseyear
 replace base_`y'=0 if base_`y'==.
 bysort district: egen base_`y'2=max(base_`y')
 gen inter_`y'=lag_share*base_`y'2
 drop base_`y' base_`y'2
}
// convert to 0-1
 foreach y of varlist  napcs14_nocaps napcs14_transpar napcs14_perf napcs14_non_renew napcs14_exem napcs14_eq_funding {  // napcs14_index
 replace `y'=0 if `y'==.
 sum `y'
 replace `y'=`y'/`r(max)'
 gen inter_`y'=lag_share*`y'
}
gen inter_=lag_share
sum inter_*


eststo clear 
gen outcome=afgr
eststo: quietly reghdfe outcome inter_ inter_perblk inter_perhsp  $control  [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter_ inter_perfrl $control  [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter_ inter_base $control  [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter_ inter_perblk inter_perhsp  inter_perfrl inter_base $control  [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter_*  $control  [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)


 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)"  ) keep(inter*) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
 stats(r2 N c1 c2 c3, fmt(%9.3f %9.0fc) labels("R-squared" "Observations" ///
 "District, year FE" "District covariates" "State by year FE")) ///
 plain b(%9.3f) se(%9.3f) se brackets coef(inter "Charter") varwidth(32) 


 
 //  seda
 
use charter_seda, clear
merge m:1 district using credible_seda, nogen
gen sample2=cred_dist==1 | maxshare2==0
gen sample3=pyear>2008    // drop always treated
merge m:1 state using  policy3, keep(3) nogen




gen base_math=st_math
gen base_ela=st_math
bysort district grade: egen baseyear=min(year)
// interaction of base value
 foreach y of varlist perblk perhsp perfrl base_math base_ela  {   // nacsa napcs cer
 gen base_`y'=`y' if year==baseyear
 replace base_`y'=0 if base_`y'==.
 bysort district grade: egen base_`y'2=max(base_`y')
 gen inter_`y'=lag_grade*base_`y'2
 drop base_`y' base_`y'2
}
// convert to 0-1
 foreach y of varlist napcs14_nocaps napcs14_transpar napcs14_perf napcs14_non_renew napcs14_exem napcs14_eq_funding {  // napcs14_index
 replace `y'=0 if `y'==.
 sum `y'
 replace `y'=`y'/`r(max)'
 gen inter_`y'=lag_grade*`y'
}


gen inter_=lag_grade
rename  (inter_base_ela inter_base_math) (inter_base notnow)
gen outcome=st_math
eststo clear 
eststo: quietly reghdfe outcome inter_ inter_perblk inter_perhsp $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter_ inter_perfrl $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter_ inter_base $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter_ inter_perblk inter_perhsp  inter_perfrl inter_base $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter_* $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)

 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" ) keep(inter*) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
 stats(r2 N c1 c2 c3, fmt(%9.3f %9.0fc) labels("R-squared" "Observations" ///
 "District & year FE" "District-level covariates" "State by year FE")) ///
 plain b(%9.3f) se(%9.3f) se brackets coef(inter "Charter") varwidth(32) 
 


rename  (inter_base notnow) (notnow inter_base) 
replace outcome=st_ela
eststo clear 
eststo: quietly reghdfe outcome inter_ inter_perblk inter_perhsp $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter_ inter_perfrl $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter_ inter_base $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter_ inter_perblk inter_perhsp  inter_perfrl inter_base $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter_* $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)




 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" ) keep(inter*) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
 stats(r2 N c1 c2 c3, fmt(%9.3f %9.0fc) labels("R-squared" "Observations" ///
 "District & year FE" "District-level covariates" "State by year FE")) ///
 plain b(%9.3f) se(%9.3f) se brackets coef(inter "Charter") varwidth(32) 
 
 
 
 
 
 
*****************************************************************************
***** Table 4  Heterogeneous  result
*****************************************************************************
 
 
 
 
* A  Heterogeneous  results: Graduation rate
*****************************************************************************


use charter_afgr2, clear
merge m:1 district using credible_afgr, nogen
gen sample2=cred_dist==1 | maxshare2==0
gen sample3=pyear>1995    // drop always treated
merge m:1 state using  policy3, keep(3) nogen


gen base=afgr
bysort district: egen baseyear=min(year)
bysort district: egen noexcuses_mean=mean(noexcuses_share)
replace noexcuses_share=noexcuses_mean

// interaction of base value
 foreach y of varlist perblk perhsp perfrl base {   // nacsa napcs cer
 gen base_`y'=`y' if year==baseyear
 replace base_`y'=0 if base_`y'==.
 bysort district: egen base_`y'2=max(base_`y')
 gen inter_`y'=lag_share*base_`y'2
 drop base_`y' base_`y'2
}
// convert to 0-1
 foreach y of varlist  napcs14_nocaps napcs14_transpar napcs14_perf napcs14_non_renew napcs14_exem napcs14_eq_funding {  // napcs14_index
 replace `y'=0 if `y'==.
 sum `y'
 replace `y'=`y'/`r(max)'
 gen inter_`y'=lag_share*`y'
}
gen inter_=lag_share
sum inter_*


eststo clear 
gen outcome=afgr
eststo: quietly reghdfe outcome inter_* $control  [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter_* $control  if sample2==1 [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter_* $control  if sample3==1 [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)


 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)"  ) keep(inter*) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
 stats(r2 N c1 c2 c3, fmt(%9.3f %9.0fc) labels("R-squared" "Observations" ///
 "District, year FE" "District covariates" "State by year FE")) ///
 plain b(%9.3f) se(%9.3f) se brackets coef(inter "Charter") varwidth(32) 


 
 //  seda
 
use charter_seda, clear
merge m:1 district using credible_seda, nogen
gen sample2=cred_dist==1 | maxshare2==0
gen sample3=pyear>2008    // drop always treated
merge m:1 state using  policy3, keep(3) nogen




gen base_math=st_math
gen base_ela=st_math
bysort district grade: egen baseyear=min(year)
// interaction of base value
 foreach y of varlist perblk perhsp perfrl base_math base_ela  {   // nacsa napcs cer
 gen base_`y'=`y' if year==baseyear
 replace base_`y'=0 if base_`y'==.
 bysort district grade: egen base_`y'2=max(base_`y')
 gen inter_`y'=lag_grade*base_`y'2
 drop base_`y' base_`y'2
}
// convert to 0-1
 foreach y of varlist napcs14_nocaps napcs14_transpar napcs14_perf napcs14_non_renew napcs14_exem napcs14_eq_funding {  // napcs14_index
 replace `y'=0 if `y'==.
 sum `y'
 replace `y'=`y'/`r(max)'
 gen inter_`y'=lag_grade*`y'
}


gen inter_=lag_grade
rename  (inter_base_ela inter_base_math) (inter_base notnow)
gen outcome=st_math
eststo: quietly reghdfe outcome inter_* $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter_* $control  if sample2==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter_* $control  if sample3==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)


rename  (inter_base notnow) (notnow inter_base) 
replace outcome=st_ela
eststo: quietly reghdfe outcome inter_* $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter_* $control  if sample2==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome inter_* $control  if sample3==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)





 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" ) keep(inter*) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
 stats(r2 N c1 c2 c3, fmt(%9.3f %9.0fc) labels("R-squared" "Observations" ///
 "District & year FE" "District-level covariates" "State by year FE")) ///
 plain b(%9.3f) se(%9.3f) se brackets coef(inter "Charter") varwidth(32) 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 


