

* nlsd_authorizer napcs_cmo


*cd "D:\Research\Charter\Charterdata"
cd "C:\Users\nickm\OneDrive\Acer (new laptop)\Documents\PhD\Tulane University\Projects\Charter School Heterogeneity\data"
global control "logenroll perwht perblk perhsp perfrl perspeced urban suburb town rural p_rev p_exp str tea_salary num_magnet charter_eff"

global sum "enrollment perwht perblk perhsp perfrl perspeced urban suburb town rural p_rev p_exp str tea_salary num_magnet"
set scheme s2color  


********************************************************************************
***** Cleaning AFGR
********************************************************************************
/*
use charter_afgr, clear
sort district year
bysort district: gen afgr_g=afgr/afgr[_n-1]-1
bysort district: egen afgr_m=mean(afgr)
gen extreme=(afgr_g!=. & afgr_g>=0.1 | afgr_g<=-0.1)
bysort district: replace extreme=1 if extreme==0 & extreme[_n+1]==1
gen afgr_dif=afgr-afgr_m if extreme==1
gen afgr_a=abs(afgr_dif)
bysort district: gen afgr_change=afgr_a>afgr_a[_n-1] if afgr_a!=.
bysort district: replace afgr_change=1 if afgr_a>afgr_a[_n+1]  & afgr_a!=.
order district year afgr  afgr_g extreme afgr_m afgr_dif afgr_a afgr_change

gen afgr_new=afgr

bysort district: replace afgr_new=(afgr_new[_n-1]+afgr_new[_n+1])/2 if afgr_change==1
replace afgr_new=afgr_new[_n+1] if afgr_new==. & afgr_change==1
order district year afgr_g extreme afgr_m afgr_dif afgr_a afgr_change afgr afgr_new
replace afgr=afgr_new
drop afgr_g extreme afgr_m afgr_dif afgr_a afgr_change afgr_new
save charter_afgr2, replace

*/


********************************************************************************
***** Figure 1 Histogram 
********************************************************************************

 // 1073
** AFGR

/*
use charter_afgr2, clear
gen inter=lag_share
quietly reghdfe afgr inter $control if state==5 [pw=eweight] , absorb(year district) vce(cluster district)
regsave inter using figure1, tstat pval replace


sort district year
gen base_cha=cha_enroll if cha_share2>0 & cha_share2[_n-1]==0
replace base_cha=0 if base_cha==.
bysort district: egen base_charter=max(base_cha)

bysort district: egen share_max=max(cha_share2)
bysort district: egen share_min=min(cha_share2)
gen share_change=int((share_max-share_min)*100)
gen change5=share_change>=5

gen treated =(maxshare2>0 & maxshare2!=.) 
duplicates drop district, force
sort state district treated
bysort state: egen obs2=mean(treated)
drop if obs2==0
keep state treated district base_charter change5
bysort district: egen tot_charter=sum(base_charter)
egen order1=group(state)
gsort -treated order1 district
gen order2=[_n]
gen sample=1
order state treated district 
save order_afgr, replace

use charter_afgr2, clear
gen inter=lag_share
merge m:1 district using order_afgr, keep(3) nogen

foreach y of numlist 1/1073 {
quietly sum order1 if order2==`y'
di  "group" " " `r(mean)' "  " "id" " " `y'
replace sample=((treated==0 | order2==`y') & order1==`r(mean)' ) 
quietly reghdfe afgr inter $control if sample==1 [pw=eweight] , absorb(year district) vce(cluster district)
quietly sum order1 if order2==`y'
capture regsave inter using figure1, tstat pval  addlabel(order1,`r(mean)',order2,`y') append
}
*/
 
 
use figure1, clear
drop if order2==.
merge 1:1 order2 using order_afgr, keep(3) nogen
tab state if order1==4
*drop if  order1==4
sum coef,de 
histogram coef if coef>`r(p5)' & coef<=`r(p95)',  width(0.2) freq  ///
xtitle("Coefficient" ,size(small)) ytitle("Number of districts ",size(small)) legend(off)  ///
ylabel(,nogrid labsize(small)) graphregion(fcolor(white) lcolor(white)) 
 

/*
// 1366
** SEDA
 
use charter_seda, clear
gen inter=lag_grade
quietly reghdfe st_ela inter $control if state==5 [pw=eweight] , absorb(year grade district) vce(cluster district)
regsave inter using figure2, tstat pval replace

sort district year
gen base_cha=cha_enroll38 if cha_enroll38>0 & cha_enroll38[_n-1]==0
replace base_cha=0 if base_cha==.
bysort district: egen base_charter=max(base_cha)

bysort district year: egen share_mean=mean(cha_share2)
bysort district: egen share_max=max(share_mean)
bysort district: egen share_min=min(share_mean)
gen share_change=int((share_max-share_min)*100)
gen change5=share_change>=5

gen treated =(maxshare2>0 & maxshare2!=.) 
duplicates drop district, force
sort state district treated
bysort state: egen obs2=mean(treated)
drop if obs2==0
keep state treated district base_charter change5
bysort district: egen tot_charter=sum(base_charter)
egen order1=group(state)
gsort -treated order1 district
gen order2=[_n]
gen sample=1
order state treated district 
save order_seda, replace


use charter_seda, clear
gen inter=lag_grade
merge m:1 district using order_seda, keep(3) nogen

foreach y of numlist 1/1366 {
quietly sum order1 if order2==`y'
di  "group" " " `r(mean)' "  " "id" " " `y'
replace sample=((treated==0 | order2==`y') & order1==`r(mean)' ) 
quietly reghdfe st_ela inter $control if sample==1 [pw=eweight] , absorb(year grade district) vce(cluster district)
quietly sum order1 if order2==`y'
capture regsave inter using figure2, tstat pval  addlabel(order1,`r(mean)',order2,`y', subject,ela) append
quietly reghdfe st_math inter $control if sample==1 [pw=eweight] , absorb(year grade district) vce(cluster district)
quietly sum order1 if order2==`y'
capture regsave inter using figure2, tstat pval  addlabel(order1,`r(mean)',order2,`y', subject,math) append
}
*/




use figure2, clear
drop if order2==.
merge m:1 order2 using order_seda, keep(3) nogen

keep if subject=="math"
tab state if order1==4
*drop if order1==4

tab state if coef>0.4 & coef<=0.6

sum coef,de 
histogram coef if coef>`r(p5)' & coef<=`r(p95)',  width(0.2) freq   ///
xtitle("Coefficient" ,size(small)) ytitle("Number of districts ",size(small)) legend(off)  ///
ylabel(,nogrid labsize(small)) graphregion(fcolor(white) lcolor(white)) 
 
 
use figure2, clear
drop if order2==.
merge m:1 order2 using order_seda, keep(3) nogen

keep if subject=="ela"
tab state if order1==4
tab state if coef>0.1 & coef<=0.3
*drop if order1==4
sum coef,de 
sum coef,de 
histogram coef if coef>`r(p5)' & coef<=`r(p95)',  width(0.2) freq  ///
xtitle("Coefficient" ,size(small)) ytitle("Number of districts ",size(small)) legend(off)  ///
ylabel(,nogrid labsize(small)) graphregion(fcolor(white) lcolor(white)) 
 


********************************************************************************
***** Figure 2 Non-linear  baseline share
********************************************************************************



** AFGR
use charter_afgr2, clear
gen treated=maxshare2>0
duplicates drop district, force
bysort state: egen obsn=sum(treated)
keep if obsn==0 | obsn>=5
keep district

merge 1:m district using charter_afgr2, keep(3) nogen


bysort district: egen baseshare1999=max(cha_share2) if year<2000  // baseshare 1995-1999
replace baseshare1999=0 if baseshare1999==.
bysort district: egen baseshare=max(baseshare1999)

replace baseshare=int(baseshare*100)
replace baseshare=30 if baseshare>30

gen group=int(baseshare/5)+1
tab group
replace group=0 if baseshare==0
replace group=6 if group>6

*drop if pyear<=1995   // drop always treated


quietly reghdfe afgr ib0.group##c.lag_share $control  [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
   
foreach y of numlist 1/6 {
gen co`y'= _b[`y'.group#c.lag_share]
gen up`y'=co`y'+invttail(e(df_r),0.025)*_se[`y'.group#c.lag_share] 
gen low`y'=co`y'-invttail(e(df_r),0.025)*_se[`y'.group#c.lag_share] 
}
keep in 1
keep co* up* low*
gen id=1

reshape long co up low, i(id) j(group)
keep group co up low
gen share=group*5

twoway (scatter co share, lpattern(solid)) (rcap up low share, lwidth(t) lcolor(gs11)), ///
ytitle("Coefficient",size(small)) xtitle("",size(small)) ylabel(-0.8(0.4)0.8,nogrid labsize(small)) ///
legend(order(1 2) label(1 "Cofficient") label(2 "95% CI")size(small)) ///
yline(0, lcolor(maroon) lwidth(thin)) xlab(5(5)30 30 ">=30",labsize(small)) ///
graphregion(fcolor(white) lcolor(white)) xtitle("Baseline charter share (1995-1999)",size(small)) 





** Math
use charter_seda, clear
gen treated=maxshare2>0
duplicates drop district, force
bysort state: egen obsn=sum(treated)
keep if obsn==0 | obsn>=5
keep district

merge 1:m district using charter_seda, keep(3) nogen

gen baseshare2009=cha_share2 if year==2009
replace baseshare2009=0 if baseshare2009==.
bysort district grade: egen baseshare=max(baseshare2009)


drop group
gen share30=int(baseshare*100)
replace share30=30 if share30>30
gen group=int(share30/5)+1
tab group
replace group=0 if share30==0
replace group=6 if group>6
* replace group=. if baseshare>0.3
tab group


*drop if pyear<=2009   // drop always treated


quietly reghdfe st_math ib0.group##c.lag_grade $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
   
foreach y of numlist 1/6 {
gen co`y'= _b[`y'.group#c.lag_grade]
gen up`y'=co`y'+invttail(e(df_r),0.025)*_se[`y'.group#c.lag_grade] 
gen low`y'=co`y'-invttail(e(df_r),0.025)*_se[`y'.group#c.lag_grade] 
}
keep in 1
keep co* up* low*
gen id=1

reshape long co up low, i(id) j(group)
keep group co up low
gen share=group*5

twoway (scatter co share, lpattern(solid)) (rcap up low share, lwidth(t) lcolor(gs11)), ///
ytitle("Coefficient",size(small)) xtitle("",size(small)) ylabel(-1.2(0.6)1.2,nogrid labsize(small)) ///
legend(order(1 2) label(1 "Cofficient") label(2 "95% CI")size(small)) ///
yline(0, lcolor(maroon) lwidth(thin)) xlab(5(5)30 30 ">=30",labsize(small)) ///
graphregion(fcolor(white) lcolor(white)) xtitle("Baseline charter share (2009)",size(small)) 




** ELA
use charter_seda, clear
gen treated=maxshare2>0
duplicates drop district, force
bysort state: egen obsn=sum(treated)
keep if obsn==0 | obsn>=5
keep district

merge 1:m district using charter_seda, keep(3) nogen

gen baseshare2009=cha_share2 if year==2009
replace baseshare2009=0 if baseshare2009==.
bysort district grade: egen baseshare=max(baseshare2009)


drop group
gen share30=int(baseshare*100)
replace share30=30 if share30>30
gen group=int(share30/5)+1
tab group
replace group=0 if share30==0
replace group=6 if group>6
* replace group=. if baseshare>0.3
tab group

*drop if pyear<=2009   // drop always treated


quietly reghdfe st_ela ib0.group##c.lag_grade $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
   
foreach y of numlist 1/6 {
gen co`y'= _b[`y'.group#c.lag_grade]
gen up`y'=co`y'+invttail(e(df_r),0.025)*_se[`y'.group#c.lag_grade] 
gen low`y'=co`y'-invttail(e(df_r),0.025)*_se[`y'.group#c.lag_grade] 
}
keep in 1
keep co* up* low*
gen id=1

reshape long co up low, i(id) j(group)
keep group co up low
gen share=group*5

twoway (scatter co share, lpattern(solid)) (rcap up low share, lwidth(t) lcolor(gs11)), ///
ytitle("Coefficient",size(small)) xtitle("",size(small)) ylabel(-1.2(0.6)1.2,nogrid labsize(small)) ///
legend(order(1 2) label(1 "Cofficient") label(2 "95% CI")size(small)) ///
yline(0, lcolor(maroon) lwidth(thin)) xlab(5(5)30 30 ">=30",labsize(small)) ///
graphregion(fcolor(white) lcolor(white)) xtitle("Baseline charter share (2009)",size(small)) 




/*
********************************************************************************
***** Identifying the "credible set" of districts
********************************************************************************

 // 1073
** AFGR

use charter_afgr2, clear

sort district year
bysort district: egen max_charter=max(cha_enroll)


gen base_cha=cha_enroll if cha_share2>0 & cha_share2[_n-1]==0
replace base_cha=0 if base_cha==.
bysort district: egen base_charter=max(base_cha)
bysort district: egen share_max=max(cha_share2)
bysort district: egen share_min=min(cha_share2)
gen share_change=int((share_max-share_min)*100)
gen change5=share_change>=5

gen treated =(maxshare2>0 & maxshare2!=.) 
duplicates drop district, force
sort state district treated
bysort state: egen obs2=mean(treated)
drop if obs2==0
keep state treated district base_charter change5 districtname max_charter
bysort district: egen tot_charter=sum(base_charter)
egen order1=group(state)
gsort -treated order1 district
gen order2=[_n]
gen sample=1
gsort state -max_charter
bysort state: gen order3=[_n]
order state treated district  districtname max_charter
save order_afgr, replace



use figure1, clear
drop if order2==.
merge 1:1 order2 using order_afgr, keep(3) nogen
sum coef,de 
* keep if coef>=`r(p10)' & coef<=`r(p90)' & change5==1
keep if coef>=`r(p5)' & coef<=`r(p95)' 

keep district state base_charter base_charter tot_charter
gen cred_dist=1
bysort state: egen number_state=sum(cred_dist)
bysort state: egen tot_charter2=sum(base_charter)
gen base_share=tot_charter2/tot_charter
gen cred_state=(number_state>=5 & base_share>=0.2)
save credible_afgr, replace

duplicates drop state, force
tab cred_state
keep state cred_state
save credible_afgr2, replace





// 1366
** SEDA
 
use charter_seda, clear

sort district year
bysort district: egen max_charter=max(cha_enroll38)
gen base_cha=cha_enroll38 if cha_enroll38>0 & cha_enroll38[_n-1]==0
replace base_cha=0 if base_cha==.
bysort district: egen base_charter=max(base_cha)

bysort district year: egen share_mean=mean(cha_share2)
bysort district: egen share_max=max(share_mean)
bysort district: egen share_min=min(share_mean)
gen share_change=int((share_max-share_min)*100)
gen change5=share_change>=5

gen treated =(maxshare2>0 & maxshare2!=.) 
duplicates drop district, force
sort state district treated
bysort state: egen obs2=mean(treated)
drop if obs2==0
keep state treated district base_charter change5 districtname max_charter
bysort district: egen tot_charter=sum(base_charter)
egen order1=group(state)
gsort -treated order1 district
gen order2=[_n]
gen sample=1
gsort state -max_charter
bysort state: gen order3=[_n]
order state treated district 
save order_seda, replace



use figure2, clear
drop if order2==.
keep if subject=="ela"
rename coef coef_ela
keep order* coef*
tempfile ela
save `ela'

use figure2, clear
drop if order2==.
keep if subject=="math"
rename coef coef_math
keep order* coef*
merge 1:1 order2 using `ela', nogen

merge 1:1 order2 using order_seda, keep(3) nogen


gen coef5_95=1
sum coef_ela,de 
replace coef5_95=0 if coef_ela<`r(p5)' | coef_ela>`r(p95)'
sum coef_math,de 
replace coef5_95=0 if coef_math<`r(p5)' | coef_math>`r(p95)'

* keep if coef5_95==1  & change5==1
keep if coef5_95==1  
keep district state base_charter base_charter tot_charter
gen cred_dist=1
bysort state: egen number_state=sum(cred_dist)
bysort state: egen tot_charter2=sum(base_charter)
gen base_share=tot_charter2/tot_charter
gen cred_state=(number_state>=5 & base_share>=0.2)
save credible_seda, replace

duplicates drop state, force
tab cred_state
keep state cred_state
save credible_seda2, replace

*/



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
 

/*
*****************************************************************************
***** Table 5  Charter Years of Experience
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
 foreach y of varlist perblk perhsp perfrl base noexcuses_share urban_share {   // nacsa napcs cer
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

gen length1_3=(length>=1 & length<=3)
gen length4_6=(length>=4 & length<=6)
gen length7_9=(length>=7 & length<=9)
gen length10=(length>=10)

gen interl1=lag_share*length1_3
gen interl2=lag_share*length4_6
gen interl3=lag_share*length7_9
gen interl4=lag_share*length10




eststo clear 
gen outcome=afgr
eststo: quietly reghdfe outcome interl* $control  [pw=eweight] , absorb(stateyear district) vce(cluster district)
eststo: quietly reghdfe outcome interl* inter_* $control  [pw=eweight] , absorb(stateyear district) vce(cluster district)
eststo: quietly reghdfe outcome interl* $control  if sample2==1 [pw=eweight] , absorb(stateyear district) vce(cluster district)
eststo: quietly reghdfe outcome interl* inter_* $control if sample2==1 [pw=eweight] , absorb(stateyear district) vce(cluster district)
eststo: quietly reghdfe outcome interl* $control  if sample3==1 [pw=eweight] , absorb(stateyear district) vce(cluster district)
eststo: quietly reghdfe outcome interl* inter_* $control  if sample3==1 [pw=eweight] , absorb(stateyear district) vce(cluster district)


 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)"  ) keep(interl*) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
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
 foreach y of varlist perblk perhsp perfrl base_math base_ela noexcuses_share urban_share {   // nacsa napcs cer
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



gen length1_3=(length>=1 & length<=3)
gen length4_6=(length>=4 & length<=6)
gen length7_9=(length>=7 & length<=9)
gen length10=(length>=10)

gen interl1=lag_grade*length1_3
gen interl2=lag_grade*length4_6
gen interl3=lag_grade*length7_9
gen interl4=lag_grade*length10


winsor2 st_math, cuts(10 90)
winsor2 st_ela, cuts(10 90)

eststo clear 
rename  (inter_base_ela inter_base_math) (inter_base notnow)
gen outcome=st_math
eststo: quietly reghdfe outcome interl* $control  [pw=eweight] , absorb(sgyear district) vce(cluster district)
eststo: quietly reghdfe outcome interl* inter_* $control  [pw=eweight] , absorb(sgyear district) vce(cluster district)
eststo: quietly reghdfe outcome interl* $control if sample2==1 [pw=eweight] , absorb(sgyear district) vce(cluster district)
eststo: quietly reghdfe outcome interl* inter_* $control if sample2==1 [pw=eweight] , absorb(sgyear district) vce(cluster district)
eststo: quietly reghdfe outcome interl* $control  if sample3==1 [pw=eweight] , absorb(sgyear district) vce(cluster district)
eststo: quietly reghdfe outcome interl* inter_* $control  if sample3==1 [pw=eweight] , absorb(sgyear district) vce(cluster district)

 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" ) keep(interl*) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
 stats(r2 N c1 c2 c3, fmt(%9.3f %9.0fc) labels("R-squared" "Observations" ///
 "District & year FE" "District-level covariates" "State by year FE")) ///
 plain b(%9.3f) se(%9.3f) se brackets coef(inter "Charter") varwidth(32) 
 
 
 
 
eststo clear 
replace outcome=st_ela
rename  (inter_base notnow) (notnow inter_base) 
eststo: quietly reghdfe outcome interl* $control  [pw=eweight] , absorb(sgyear district) vce(cluster district)
eststo: quietly reghdfe outcome interl* inter_* $control  [pw=eweight] , absorb(sgyear district) vce(cluster district)
eststo: quietly reghdfe outcome interl* $control if sample2==1 [pw=eweight] , absorb(sgyear district) vce(cluster district)
eststo: quietly reghdfe outcome interl* inter_* $control if sample2==1 [pw=eweight] , absorb(sgyear district) vce(cluster district)
eststo: quietly reghdfe outcome interl* $control  if sample3==1 [pw=eweight] , absorb(sgyear district) vce(cluster district)
eststo: quietly reghdfe outcome interl* inter_* $control  if sample3==1 [pw=eweight] , absorb(sgyear district) vce(cluster district)



 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" ) keep(interl*) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
 stats(r2 N c1 c2 c3, fmt(%9.3f %9.0fc) labels("R-squared" "Observations" ///
 "District & year FE" "District-level covariates" "State by year FE")) ///
 plain b(%9.3f) se(%9.3f) se brackets coef(inter "Charter") varwidth(32) 
 
 
*/
 
 
 
 
 
 
*****************************************************************************
***** Table 6  Placebo  result
*****************************************************************************
 
 
 
 
* A  Heterogeneous  results: Graduation rate
*****************************************************************************


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
merge m:1 state using  policy3, keep(3) nogen


replace cha_sharea=0 if cha_sharea==.
* keep if maxshare2>0         // charter districts

gen base=afgr
bysort district: egen baseyear=min(year)
bysort district: egen noexcuses_mean=mean(noexcuses_share)
replace noexcuses_share=noexcuses_mean

// interaction of base value
 foreach y of varlist perblk perhsp perfrl base  {   // nacsa napcs cer
 gen base_`y'=`y' if year==baseyear
 replace base_`y'=0 if base_`y'==.
 bysort district: egen base_`y'2=max(base_`y')
 gen inter_`y'=lag_share*base_`y'2
 gen interp_`y'=cha_sharea*base_`y'2
 drop base_`y' base_`y'2
}
// convert to 0-1
 foreach y of varlist  napcs14_nocaps napcs14_transpar napcs14_perf napcs14_non_renew napcs14_exem napcs14_eq_funding {  // napcs14_index
 replace `y'=0 if `y'==.
 sum `y'
 replace `y'=`y'/`r(max)'
 gen inter_`y'=lag_share*`y'
 gen interp_`y'=cha_sharea*`y'
}
gen inter_=lag_share
gen interp_=cha_sharea

winsor2 afgr, cuts(10 90)

eststo clear 
gen outcome=afgr
eststo: quietly reghdfe outcome interp_* inter_* $control  [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome interp_* inter_* $control  if sample2==1 [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome interp_* inter_* $control  if sample3==1 [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)


 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)"  ) keep(interp_*) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
 stats(r2 N c1 c2 c3, fmt(%9.3f %9.0fc) labels("R-squared" "Observations" ///
 "District, year FE" "District covariates" "State by year FE")) ///
 plain b(%9.3f) se(%9.3f) se brackets coef(inter "Charter") varwidth(32) 


 
 //  seda
 
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
merge m:1 state using  policy3, keep(3) nogen


gen placebo_grade=cha_share6 if grade<=5 
replace placebo_grade=cha_share5 if grade>=6
* keep if maxshare2>0        // charter districts

gen base_math=st_math
gen base_ela=st_math
bysort district grade: egen baseyear=min(year)
// interaction of base value
 foreach y of varlist perblk perhsp perfrl base_math base_ela  {   // nacsa napcs cer
 gen base_`y'=`y' if year==baseyear
 replace base_`y'=0 if base_`y'==.
 bysort district grade: egen base_`y'2=max(base_`y')
 gen inter_`y'=lag_grade*base_`y'2
 gen interp_`y'=placebo_grade*base_`y'2
 drop base_`y' base_`y'2
}
// convert to 0-1
 foreach y of varlist napcs14_nocaps napcs14_transpar napcs14_perf napcs14_non_renew napcs14_exem napcs14_eq_funding {  // napcs14_index
 replace `y'=0 if `y'==.
 sum `y'
 replace `y'=`y'/`r(max)'
 gen inter_`y'=lag_grade*`y'
 gen interp_`y'=placebo_grade*`y'
}

winsor2 st_math, cuts(10 90)
winsor2 st_ela, cuts(10 90)

gen inter_=lag_grade
gen interp_=placebo_grade
rename  (inter_base_ela inter_base_math interp_base_ela interp_base_math) (inter_base notnow interp_base notnowp)
gen outcome=st_math
eststo: quietly reghdfe outcome interp_* inter_* $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome interp_* inter_* $control  if sample2==1  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome interp_* inter_* $control  if sample3==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)


rename  (inter_base notnow interp_base notnowp) (notnow inter_base notnowp interp_base) 
replace outcome=st_ela
eststo: quietly reghdfe outcome interp_* inter_* $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome interp_* inter_* $control  if sample2==1  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
eststo: quietly reghdfe outcome interp_* inter_* $control  if sample3==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)




 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" ) keep(interp_*) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
 stats(r2 N c1 c2 c3, fmt(%9.3f %9.0fc) labels("R-squared" "Observations" ///
 "District & year FE" "District-level covariates" "State by year FE")) ///
 plain b(%9.3f) se(%9.3f) se brackets coef(inter "Charter") varwidth(32) 
 
 
 
*****************************************************************************
***** Table 7  enrollment growth
*****************************************************************************


/*
 foreach y of numlist 1992/2019 {  
 import delimited "D:\Research\Charter_Hete\enrollment_school\schools_ccd_enrollment_`y'.csv", clear
 tempfile enrollment`y'
 save `enrollment`y''
 }
 clear
  foreach y of numlist 1992/2019 {  
 append using  `enrollment`y''
 }
  save "D:\Research\Charter_Hete\enrollment_school\schools_enrollment", replace

 
 

 
 use "D:\Research\Charter_Hete\enrollment_school\schools_enrollment", clear
 keep if grade>=6 & grade<=8
 replace year=year+1
 keep if year>=1992 & year<=2010
 keep if race==99 & sex==99
 drop if enrollment<=0
 collapse (sum) enrollment, by(ncessch year)
 rename ncessch schoolid
 save growth_afgr, replace
 
 use charter_afgr2, clear
 keep district
 duplicates drop district, force
 merge 1:m district using nlscd_school, keep(3) nogen
 keep schoolid district charter
 duplicates drop schoolid, force
 merge 1:m schoolid using growth_afgr, keep(3) nogen
* keep if charter==0
 collapse (sum) enrollment, by(district year)
 sort district year
 bysort district: gen growth1=(enrollment-enrollment[_n-1])/enrollment[_n-1]
 bysort district: gen growth=(growth1+growth1[_n-1]+growth1[_n-2])/3
 drop if year<1995
 keep district year growth
 drop if growth==.
 winsor2 growth, replace cuts(0 99.5)
 sum growth, de
 gen group3=(growth>=-0.03 & growth<=0.03)
 gen group3n=growth<-0.03
 gen group3p=growth>0.03
 sum
 save afgr_growth, replace
 erase growth_afgr.dta
 
 
 
 use "D:\Research\Charter_Hete\enrollment_school\schools_enrollment", clear
 keep if grade>=0 & grade<=3
 replace year=year+1
 keep if year>=2006 & year<=2018
 keep if race==99 & sex==99
 drop if enrollment<=0
 collapse (sum) enrollment, by(ncessch year)
 rename ncessch schoolid
 save growth_seda, replace
 
 use charter_seda, clear
 keep district
 duplicates drop district, force
 merge 1:m district using nlscd_school, keep(3) nogen
 keep schoolid district charter
 duplicates drop schoolid, force
 merge 1:m schoolid using growth_seda, keep(3) nogen
* keep if charter==0
 collapse (sum) enrollment, by(district year)
 sort district year
 bysort district: gen growth1=(enrollment-enrollment[_n-1])/enrollment[_n-1]
 bysort district: gen growth=(growth1+growth1[_n-1]+growth1[_n-2])/3
 drop if year<2009
 keep district year growth
 drop if growth==.
 winsor2 growth, replace cuts(0 99.5)
 sum growth, de
 gen group3=(growth>=-0.03 & growth<=0.03)
 gen group3n=growth<-0.03 
 gen group3p=growth>=0.03
 sum
 save seda_growth, replace
 erase growth_seda.dta
 
  */
  
  
******************************************************************************
** AFGR

 
use charter_afgr2, clear
gen treated=maxshare2>0
duplicates drop district, force
bysort state: egen obsn=sum(treated)
keep if obsn==0 | obsn>=5
keep district

merge 1:m district using charter_afgr2, keep(3) nogen
merge 1:1 district year using afgr_growth,  nogen
merge m:1 district using credible_afgr, nogen
gen sample2=cred_dist==1 | maxshare2==0
gen sample3=pyear>1995    // drop always treated


eststo clear 
gen inter=lag_share*growth
eststo: quietly reghdfe afgr inter lag_share $control  [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
egen obs1=group(district) if e(sample)==1
eststo: quietly reghdfe afgr inter lag_share $control  if group3n==1 [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
egen obs2=group(district) if e(sample)==1
eststo: quietly reghdfe afgr inter lag_share $control  if group3==1 [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
egen obs3=group(district) if e(sample)==1
eststo: quietly reghdfe afgr inter lag_share $control  if group3p==1 [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
egen obs4=group(district) if e(sample)==1
sum obs*

 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)" ) keep(inter) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
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

merge m:1 district year using seda_growth,  nogen
merge m:1 district using credible_seda, nogen

gen sample2=cred_dist==1 | maxshare2==0
gen sample3=pyear>2008    // drop always treated

eststo clear 
gen inter=lag_grade*growth
eststo: quietly reghdfe st_math inter lag_grade $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
egen obs1=group(district) if e(sample)==1
eststo: quietly reghdfe st_math inter lag_grade $control  if group3n==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
egen obs2=group(district) if e(sample)==1
eststo: quietly reghdfe st_math inter lag_grade $control  if group3==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
egen obs3=group(district) if e(sample)==1
eststo: quietly reghdfe st_math inter lag_grade $control  if group3p==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
egen obs4=group(district) if e(sample)==1
sum obs*


 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)"  ) keep(inter) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
 stats(r2 N c1 c2 c3, fmt(%9.3f %9.0fc) labels("R-squared" "Observations" ///
 "District & year FE" "District-level covariates" "State by year FE")) ///
 plain b(%9.3f) se(%9.3f) se brackets coef(inter "Charter") varwidth(32) 


 
drop obs*
eststo clear 
eststo: quietly reghdfe st_ela inter lag_grade $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
egen obs1=group(district) if e(sample)==1
eststo: quietly reghdfe st_ela inter lag_grade $control  if group3n==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
egen obs2=group(district) if e(sample)==1
eststo: quietly reghdfe st_ela inter lag_grade $control  if group3==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
egen obs3=group(district) if e(sample)==1
eststo: quietly reghdfe st_ela inter lag_grade $control  if group3p==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
egen obs4=group(district) if e(sample)==1
sum obs*


 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)" ) keep(inter) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
 stats(r2 N c1 c2 c3, fmt(%9.3f %9.0fc) labels("R-squared" "Observations" ///
 "District & year FE" "District-level covariates" "State by year FE")) ///
 plain b(%9.3f) se(%9.3f) se brackets coef(inter "Charter") varwidth(32) 
 
 
 
 
// inter=charter
******************************************************************************
** AFGR

 
use charter_afgr2, clear
gen treated=maxshare2>0
duplicates drop district, force
bysort state: egen obsn=sum(treated)
keep if obsn==0 | obsn>=5
keep district

merge 1:m district using charter_afgr2, keep(3) nogen
merge 1:1 district year using afgr_growth,  nogen
merge m:1 district using credible_afgr, nogen
gen sample2=cred_dist==1 | maxshare2==0
gen sample3=pyear>1995    // drop always treated


eststo clear 
gen inter=lag_share
eststo: quietly reghdfe afgr inter $control  [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
egen obs1=group(district) if e(sample)==1
eststo: quietly reghdfe afgr inter $control  if group3n==1 [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
egen obs2=group(district) if e(sample)==1
eststo: quietly reghdfe afgr inter $control  if group3==1 [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
egen obs3=group(district) if e(sample)==1
eststo: quietly reghdfe afgr inter $control  if group3p==1 [pw=eweight] , absorb(stateyear district##c.year) vce(cluster district)
egen obs4=group(district) if e(sample)==1
sum obs*

 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)" ) keep(inter) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
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

merge m:1 district year using seda_growth, nogen
merge m:1 district using credible_seda, nogen

gen sample2=cred_dist==1 | maxshare2==0
gen sample3=pyear>2008    // drop always treated

eststo clear 
gen inter=lag_grade
eststo: quietly reghdfe st_math inter $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
egen obs1=group(district) if e(sample)==1
eststo: quietly reghdfe st_math inter $control  if group3n==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
egen obs2=group(district) if e(sample)==1
eststo: quietly reghdfe st_math inter $control  if group3==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
egen obs3=group(district) if e(sample)==1
eststo: quietly reghdfe st_math inter $control  if group3p==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
egen obs4=group(district) if e(sample)==1
sum obs*


 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)"  ) keep(inter) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
 stats(r2 N c1 c2 c3, fmt(%9.3f %9.0fc) labels("R-squared" "Observations" ///
 "District & year FE" "District-level covariates" "State by year FE")) ///
 plain b(%9.3f) se(%9.3f) se brackets coef(inter "Charter") varwidth(32) 


 
drop obs*
eststo clear 
eststo: quietly reghdfe st_ela inter $control  [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
egen obs1=group(district) if e(sample)==1
eststo: quietly reghdfe st_ela inter $control  if group3n==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
egen obs2=group(district) if e(sample)==1
eststo: quietly reghdfe st_ela inter $control  if group3==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
egen obs3=group(district) if e(sample)==1
eststo: quietly reghdfe st_ela inter $control  if group3p==1 [pw=eweight] , absorb(sgyear district##c.year) vce(cluster district)
egen obs4=group(district) if e(sample)==1
sum obs*


 esttab, label replace ///
 mtitles("(1)" "(2)" "(3)" "(4)" ) keep(inter) star(* 0.10 ** 0.05 *** 0.01) collabels(none)  ///
 stats(r2 N c1 c2 c3, fmt(%9.3f %9.0fc) labels("R-squared" "Observations" ///
 "District & year FE" "District-level covariates" "State by year FE")) ///
 plain b(%9.3f) se(%9.3f) se brackets coef(inter "Charter") varwidth(32) 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 