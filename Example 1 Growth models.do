log using "C:\Users\rmjdshc\OneDrive - University College London\Desktop\ELSA and COVID\Example 1.log"

*******************************
*Example 1: UEA COVID datathon
*Growth model of depression
********************************

*Some illustrative code for:
*Depression and anxiety in people with cognitive impairment and dementia during the COVID-19 pandemic: Analysis of the English Longitudinal Study of Ageing. 
*PLoS Med 20(4): e1004162.

**************************************************************
*Datasets used:
*SN 5050: Main ELSA study (Harmonized version): h_elsa_g3.dta
*SN 5050: Main ELSA study: wave_9_elsa_data_eul_v1.dta
*SN 8688 COVID wave 1: elsa_covid_w1_eulv2.dta
*SN 8688 COVID wave 2: elsa_covid_w2_eulv2.dta
**************************************************************

*Use of a growth model to examine whether changes in depression were associated with cognitive impairment.

*Dependent variable: depression (CESD) score at 3 time points (pre-pandemic wave 9, and both waves of the COVID-19 sub-study)

*Main Independent variables: measurement wave and cognitive function status (assessed in the paper using an algorithm, including HCAP data).
*(Pre-pandemic) confounders: demographics, SES, health, geography (e.g. region)

*Statistical analysis:

*Here we provide Stata code to estimate a multilevel / mixed effects linear regression model 
*to estimate change in depression across 3 cognitive function groups (no impairment; mild impairment; dementia).
*We also include code to demonstrate use of multiple imputation to fill in missing values on 1 or 2 depression items 
*(especially important for COVID 1 data, where 1 item is missing for 75%+)

*Note that here we do not use the exact dementia algorithm used in this paper. It is not replicable using archived data.
*So we illustrate here using a combination of Wave 9 cognitive function (CF) data (memory scores); and doctor-diagnosed dementia. 
*Therefore please note: this is just an illustration.


*********************
*Wave 9 (SN 5050).
*********************

clear all
clear
set maxvar 15000
set seed 579503
use "C:\Users\rmjdshc\OneDrive - University College London\ELSA\UKDS\ELSA-5050\stata\wave_9_elsa_data_eul_v1.dta"
keep idauniq finstat psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh indager indsex GOR fqethnmr couple cflisen cflisd

***Cognitive function status at wave9.

***Match in variables for doctor-diagnosed dementia and alzheimer's disease from the Harmonized ELSA data (available: SN 5050)
***which uses information from previous waves.

sort idauniq
merge 1:1 idauniq using "C:\Users\rmjdshc\OneDrive - University College London\ELSA\UKDS\ELSA-5050\stata\h_elsa_g3.dta", keepusing(r9demene r9alzhe)
drop if _m==2 // do not add observations: only columns.
drop _m
tab1 r9demene r9alzhe    // ever had dementia/Alzheimer's.

***Memory scores (immediate- and delayed- word recall).
***higher scores = better memory.
mvdecode cflisen cflisd,mv(-9/-1)         // set to missing
summ cflisen cflisd
gen cfw9=cflisen + cflisd                 // sum the immediate and delayed scores.
summ cfw9
tab1 cfw9

*Group the w9 memory score into high; medium; low (arbitrary cut-offs).
generate cf_g3w9 = 1 if inrange(cfw9,8,20)
replace cf_g3w9 = 2 if inrange(cfw9,6,7)
replace cf_g3w9 = 3 if inrange(cfw9,0,5)
label define cflbl 1 "no impairment" 2 "mild-impairment" 3 "dementia"
label values cf_g3w9 cflbl
tab1 cf_g3w9

*Assign those with diagnosed condition into the third group (irrespective of memory score)
replace cf_g3w9=3 if (r9demene==1)|(r9alzhe==1)
tab1 cf_g3w9, m
replace cf_g3w9=1 if cf_g3w9==.    // not imputing CF status in Stata (just use mode)

*Numeric variable for region to match paper.
gen region=1 if GOR=="E12000001"|GOR=="E12000002"|GOR=="E12000003"
replace region=2 if GOR=="E12000004"|GOR=="E12000005"
replace region=3 if GOR=="E12000006"|GOR=="E12000007"
replace region=4 if GOR=="E12000008"|GOR=="E12000009"|GOR=="S92000003"|GOR=="W92000004"
label define gorlbl 1 "North" 2 "Midlands" 3 "London and East" 4 "South"
label values region gorlbl


*Keep core members only (decided here to use Cohorts 1, 3 and 4).
keep if inlist(finstat,1,7,14)
tab1 finstat

************************************************************************************
*CESD scale.
*Deriving a depression score is not a simple task due to need for reverse coding.
*high score to indicate depression
*items d (happy) and f (enjoyed life) coded differently to others.
************************************************************************************

*tab1 psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh
mvdecode psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh,mv(-9/-1)              // set to missing (.)
egen s = rowmiss(psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh)               // count the number of missing items
tab1 s        //  5,343 have all items
di 5343+30+4  //  missing either 0,1,or 2 items.

*impute CESD scores only for those missing only 1 or 2 items: drop the rest.
keep if inlist(s,0,1,2)
count
summ psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh

*Demonstrate how to use Multiple imputation to fill in the items 
*using age and sex as predictors of missing (complete variables).
*method of imputation here is predictive mean matching.

mi set wide
mi register imputed psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh                            // the variables we wish to impute 
mi impute chained (pmm,knn(5)) pscedd pscede pscedf pscedg pscedh = indsex indager, replace add(1)     // the imputation model; number of imputations
*simple way of filling in missing values.
replace pscedd=_1_pscedd  if pscedd ==.
replace pscede=_1_pscede  if pscede ==.
replace pscedf=_1_pscedf  if pscedf ==.
replace pscedg=_1_pscedg  if pscedg ==.
replace pscedh=_1_pscedh  if pscedh ==.
drop _1* s     // drop variables we dont nee
summ psc*

*Now we can create the summary CESD score (range 0-8): number of depressive symptoms.
*making sure each item (zero = not depressed; 1=depressed)

forvalues i = 1/8 {
generate f`i'=-2
}
replace f1=0 if psceda==2
replace f2=0 if pscedb==2
replace f3=0 if pscedc==2
replace f4=0 if pscedd==1    // happy
replace f5=0 if pscede==2
replace f6=0 if pscedf==1    // enjoyed life
replace f7=0 if pscedg==2
replace f8=0 if pscedh==2
replace f1=1 if psceda==1
replace f2=1 if pscedb==1
replace f3=1 if pscedc==1
replace f4=1 if pscedd==2   // happy
replace f5=1 if pscede==1
replace f6=1 if pscedf==2   // enjoyed life
replace f7=1 if pscedg==1
replace f8=1 if pscedh==1
mvdecode f1-f8,mv(-2)
egen cesd9 = rsum(f1-f8)      // sum of the scores (range: 0 to 8) 
summ cesd9

*Binary measure if wish to analyse a cutoff (e.g. score of 4+)
generate cesd_bin9 = cesd9
recode cesd_bin9 (0/3=0) (4/8=1)
label define clbl 0 "not depressed" 1 "depressed"
label values cesd_bin9 clbl
tab1 cesd_bin9    // 12% with depression.
gen inw9=1
count
keep idauniq cf_g3w9 cesd9 cesd_bin9 indager indsex inw9 region fqethnmr couple 
sort idauniq
*this line turned out to be important! please note this is an illustration of how to replace missing values.
*see example 2 for a better way.
mi unset, asis  
save "C:\Users\rmjdshc\OneDrive - University College London\Temp_ELSA_w9.dta", replace

*************************
*Wave 1 COVID: (SN 8688).
*************************

use "C:\Users\rmjdshc\OneDrive - University College London\ELSA\UKDS\Covid-8688\stata\elsa_covid_w1_eulv2.dta", clear
keep idauniq FinStat CvMhCed_CvMhCed1_q - CvMhCed_CvMhCed7_q Sex Age_Arch
*keep core members only (cohorts 1, 3 and 4).
keep if inlist(FinStat,1,7,14)
tab1 FinStat  // 4,558

*CESD at COVID wave 1.
*Error in survey administration:
*Eight depression item not asked to around 75% of respondents (CvMhCed_CvMhCed8_q_final: felt sad): making imputation necessary rather than optional.
tab1 CvMhCed_CvMhCed1_q - CvMhCed_CvMhCed7_q

*rename to be consistent with main study
rename CvMhCed_CvMhCed1_q psceda
rename CvMhCed_CvMhCed2_q pscedb
rename CvMhCed_CvMhCed3_q pscedc
rename CvMhCed_CvMhCed4_q pscedd
rename CvMhCed_CvMhCed5_q pscede
rename CvMhCed_CvMhCed6_q pscedf
rename CvMhCed_CvMhCed7_q pscedh           // could not get going
rename CvMhCed_CvMhCed8_q_final pscedg     // sad
mvdecode psced*,mv(-9/-1)
tab1 psc*

*As in paper: limit imputation to those missing just 1 or 2 items 


egen s = rowmiss(psceda pscedb pscedc pscedd pscede pscedf pscedh pscedg)               // count the number of missing items
tab1 s        
*4535
keep if inlist(s,0,1,2)
count
summ psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh

*Same procedure as main study:
*Multiple imputation using age and sex (complete variables)
mi set wide
mi register imputed psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh                                              // variables to impute
mi impute chained (pmm,knn(5)) psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh = Sex Age_Arch, replace add(1)    // imputation method
replace psceda=_1_psceda  if psceda ==.
replace pscedb=_1_pscedb  if pscedb ==.
replace pscedc=_1_pscedc  if pscedc ==.
replace pscedd=_1_pscedd  if pscedd ==.
replace pscede=_1_pscede  if pscede ==.
replace pscedf=_1_pscedf  if pscedf ==.
replace pscedg=_1_pscedg  if pscedg ==.
replace pscedh=_1_pscedh  if pscedh ==.
drop _1* s
summ psc*

*Now we can create the summary score: CESD number of depressive symptoms.
forvalues i = 1/8 {
generate f`i'=-2
}
replace f1=0 if psceda==2
replace f2=0 if pscedb==2
replace f3=0 if pscedc==2
replace f4=0 if pscedd==1    // happy
replace f5=0 if pscede==2
replace f6=0 if pscedf==1    // enjoyed life
replace f7=0 if pscedg==2
replace f8=0 if pscedh==2
replace f1=1 if psceda==1
replace f2=1 if pscedb==1
replace f3=1 if pscedc==1
replace f4=1 if pscedd==2  // happy
replace f5=1 if pscede==1
replace f6=1 if pscedf==2  // enjoyed life
replace f7=1 if pscedg==1
replace f8=1 if pscedh==1
mvdecode f1-f8,mv(-2)
egen cesd_covid1 = rsum(f1-f8)     // sum of the scores (range 0-8)           
summ cesd_covid1  

*Binary measure (scores of 4+)
generate cesd_bincovid1 = cesd_covid1
recode cesd_bincovid1 (0/3=0) (4/8=1)
label define clbl 0 "not depressed" 1 "depressed"
label values cesd_bincovid1 clbl
summ cesd_covid1 
tab1 cesd_bincovid1      // 20% with depression.
gen incovidw1=1
keep idauniq cesd_covid1 cesd_bincovid1 incovidw1
sort idauniq
*This important step.
mi unset, asis
save "C:\Users\rmjdshc\OneDrive - University College London\Temp_ELSA_COVID_w1.dta", replace

*************************
*Wave 2 COVID: SN 8688.
*************************

use "C:\Users\rmjdshc\OneDrive - University College London\ELSA\UKDS\Covid-8688\stata\elsa_covid_w2_eulv2.dta", clear
keep idauniq Finstat_w1 CvMhCed_CvMhCed1_q CvMhCed_CvMhCed2_q CvMhCed_CvMhCed3_q CvMhCed_CvMhCed4_q CvMhCed_CvMhCed5_q ///
CvMhCed_CvMhCed6_q CvMhCed_CvMhCed7_q CvMhCed_CvMhCed8_q Sex Age_arch
*keep core members only (cohorts 1, 3 and 4).
keep if inlist(Finstat_w1,1,7,14)
tab1 Finstat_w1  // 4,365
tab1 CvMhCed_CvMhCed1_q - CvMhCed_CvMhCed8_q
*rename for consistency.
rename CvMhCed_CvMhCed1_q psceda
rename CvMhCed_CvMhCed2_q pscedb
rename CvMhCed_CvMhCed3_q pscedc
rename CvMhCed_CvMhCed4_q pscedd
rename CvMhCed_CvMhCed5_q pscede
rename CvMhCed_CvMhCed6_q pscedf
rename CvMhCed_CvMhCed7_q pscedh          // could not get going
rename CvMhCed_CvMhCed8_q pscedg          // sad

mvdecode psced*,mv(-9/-1)     // set to missing

*Limit imputation to those missing just 1 or 2 items (now among the 8 available items)
*CESD scale.
egen s = rowmiss(psceda pscedb pscedc pscedd pscede pscedf pscedh pscedg)               // count the number of missing items
tab1 s        // 4295 have all items
di 4295+44+12 // missing 0,1, or 2 items.
*4351
keep if inlist(s,0,1,2)
summ psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh

*same multiple imputation procedure.
*Multiple imputation using age and sex (complete variables)
mi set wide
mi register imputed psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh                                             // variables to impute
mi impute chained (pmm,knn(5)) psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh = Sex Age_arch, replace add(1)   // imputation method
replace psceda=_1_psceda  if psceda ==.
replace pscedb=_1_pscedb  if pscedb ==.
replace pscedc=_1_pscedc  if pscedc ==.
replace pscedd=_1_pscedd  if pscedd ==.
replace pscede=_1_pscede  if pscede ==.
replace pscedf=_1_pscedf  if pscedf ==.
replace pscedg=_1_pscedg  if pscedg ==.
replace pscedh=_1_pscedh  if pscedh ==.
drop _1* s
summ psc* 

*Now we can create the summary scores: CESD (number of depressive symptoms).
forvalues i = 1/8 {
generate f`i'=-2
}
replace f1=0 if psceda==2
replace f2=0 if pscedb==2
replace f3=0 if pscedc==2
replace f4=0 if pscedd==1   // happy
replace f5=0 if pscede==2
replace f6=0 if pscedf==1   // enjoyed
replace f7=0 if pscedg==2
replace f8=0 if pscedh==2
replace f1=1 if psceda==1
replace f2=1 if pscedb==1
replace f3=1 if pscedc==1
replace f4=1 if pscedd==2   // happy
replace f5=1 if pscede==1
replace f6=1 if pscedf==2   // enjoyed
replace f7=1 if pscedg==1
replace f8=1 if pscedh==1
mvdecode f1-f8,mv(-2)
egen cesd_covid2 = rsum(f1-f8)         // sum the scores (range 0-8)           
summ cesd_covid2 

*Binary measure (score of 4+)
generate cesd_bincovid2 = cesd_covid2
recode cesd_bincovid2 (0/3=0) (4/8=1)
label define clbl 0 "not depressed" 1 "depressed"
label values cesd_bincovid2 clbl
tab1 cesd_bincovid2      // 23% with depression.
gen incovidw2=1
keep idauniq cesd_covid2 cesd_bincovid2 incovidw2
sort idauniq
*this important step.
mi unset, asis
save "C:\Users\rmjdshc\OneDrive - University College London\Temp_ELSA_COVID_w2.dta", replace

********************************************************************************.
*Put the 3 datasets together for longitudinal analysis.
*Our analytical sample: condition on having a CESD score at wave 9 (N=5,377).
*For these models not necessary to have a balanced panel.
********************************************************************************.

use "C:\Users\rmjdshc\OneDrive - University College London\Temp_ELSA_w9.dta", clear
sort idauniq
merge 1:1 idauniq using "C:\Users\rmjdshc\OneDrive - University College London\Temp_ELSA_COVID_w1.dta"
drop if _m==2
drop _m
merge 1:1 idauniq using "C:\Users\rmjdshc\OneDrive - University College London\Temp_ELSA_COVID_w2.dta"
drop if _m==2
drop _m
summ cesd9 cesd_covid1 cesd_covid2                         // continuous measures
tab1 cesd_bin9 cesd_bincovid1 cesd_bincovid2               // binary measures

*we now need to reshape dataset from wide (1 row per observation) to long format (1 row for each person-wave).
*do this need to rename variables: specify zero as baseline (wave 9).

rename (cesd9 cesd_covid1 cesd_covid2) (cesd0 cesd1 cesd2)
keep idauniq cesd0 cesd1 cesd2 indsex indager region fqethnmr couple cf_g3w9

*time-varying variables before the comma: without the wave suffix (i.e. 0,1,2).
*i = identifier.
*j = name to denote time period.
reshape long cesd, i(idauniq) j(wave)

*Key predictor for a growth model (time-in-study): here treated as categorical.
label define wavelbl 0 "2018/19" 1 "Jun/July 2020" 2 "Nov/Dec 2020"
label values wave wavelbl

*Growth curve model via the mixed command for continuous outcomes.
*Outcome = CESD score.
*Data is clustered by idauniq.
*Key Predictors = wave (time-in-study); CF status at w9; and time*CF status interaction term: treated as categorical.
*This allows the rate of change in mean depression for a 1-unit increase in time (e.g. from the main study at wave 9 to COVID wave 1) to vary across the CF status groups.
*Adjust for ethnicity; region fqethnmr couple.

*Fixed part of the model before the || 
*random part of the model after the ||.
*note: this is not performed on multiply-imputed data.


mixed cesd i.wave i.cf_g3w9 i.wave#i.cf_g3w9 i.region i.fqethnmr i.couple || idauniq:, variance ml covariance(un) 

*using a post-estimation command (-margins-) we can obtain predicted values for the 9 combinations of time and CF status.
*and plot them using -marginsplot-
margins i.wave#i.cf_g3w9
marginsplot, ytitle("Estimated score") title("Depression score")

***********************
*FINISHED
***********************


**********************************************************
*Illustration of a longitudinal model for binary outcome
*********************************************************

use "C:\Users\rmjdshc\OneDrive - University College London\Temp_ELSA_w9.dta", clear
sort idauniq
merge 1:1 idauniq using "C:\Users\rmjdshc\OneDrive - University College London\Temp_ELSA_COVID_w1.dta"
drop if _m==2
drop _m
merge 1:1 idauniq using "C:\Users\rmjdshc\OneDrive - University College London\Temp_ELSA_COVID_w2.dta"
drop if _m==2
drop _m
tab1 cesd_bin9 cesd_bincovid1 cesd_bincovid2               // binary measures

*we now need to reshape dataset from wide (1 row per observation) to long format (1 row for each person-wave).
*do this need to rename variables: specify zero as baseline (wave 9).
*here we focus on the binary outcome at each of the 3 time-points.

rename (cesd_bin9 cesd_bincovid1 cesd_bincovid2) (cesd0 cesd1 cesd2)
keep idauniq cesd0 cesd1 cesd2 indsex indager region fqethnmr couple cf_g3w9

*time-varying variables before the comma: without the wave suffix (i.e. 0,1,2).
*i = identifier.
*j = name to denote time period.
reshape long cesd, i(idauniq) j(wave)

*Key predictor for a growth model (time-in-study): here treated as categorical.
label define wavelbl 0 "2018/19" 1 "Jun/July 2020" 2 "Nov/Dec 2020"
label values wave wavelbl

*Growth curve model via the melogit command for binary outcomes.
*Outcome = CESD score.
*Data is clustered by idauniq.
*Key Predictors = wave (time-in-study); CF status at w9; and time*CF status interaction term: treated as categorical.
*Adjust for ethnicity; region fqethnmr couple.

*Fixed part of the model before the || 
*random part of the model after the ||.
*note: this is not performed on multiply-imputed data.

*random intercept only
melogit cesd i.wave i.cf_g3w9 i.wave#i.cf_g3w9 i.region i.fqethnmr i.couple || idauniq:, or cov(unstructured) nolog
*estimated probabilities.
margins i.wave#i.cf_g3w9
marginsplot, ytitle("Estimated probability") title("Depression score")

***********************
*FINISHED
***********************

log close
















		



















































       


	   
	   
	   
	   
	   
	   

