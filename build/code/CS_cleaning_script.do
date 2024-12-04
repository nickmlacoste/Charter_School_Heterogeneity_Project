/*
This script copies the relevant code from the original_analysis_script.do file that produces cleaned datasets for regression analysis.
It takes about 30 min to run, so it should only be executed once.
*/


clear all

*cd "D:\Research\Charter\Charterdata"
cd "C:\Users\nickm\OneDrive\Acer (new laptop)\Documents\PhD\Tulane University\Projects\Charter School Heterogeneity\data"
global control "logenroll perwht perblk perhsp perfrl perspeced urban suburb town rural p_rev p_exp str tea_salary num_magnet charter_eff"

global sum "enrollment perwht perblk perhsp perfrl perspeced urban suburb town rural p_rev p_exp str tea_salary num_magnet"
set scheme s2color  

********************************************************************************
***** Cleaning AFGR
********************************************************************************

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


********************************************************************************
***** Figure 1 Histogram 
********************************************************************************

 // 1073
** AFGR


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

* Merge with policy3.dta on statename (many-to-one) - creates state policy variables
merge m:1 statename using policy3.dta, nogen

* Merge in district level finance data
merge m:1 district using finance.dta, nogen

gen inter=lag_share
save charter_afgr2_c, replace


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
*keep state treated district base_charter change5 districtname max_charter
keep state treated district base_charter change5 max_charter
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

* creates state policy variables
merge m:1 statename using policy3.dta

* Merge in district level finance data
merge m:1 district using finance.dta, nogen

gen inter=lag_grade
save charter_seda_c, replace






















