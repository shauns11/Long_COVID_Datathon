clear
log using "C:\Users\rmjdshc\OneDrive - University College London\Desktop\ELSA and COVID\Example 2.log"


*****************************************************************************
*Example 2: UEA COVID datathon.
*Single-level model of health outcomes (focus on outcomes at COVID wave 2).
******************************************************************************

*Some illustrative code for:
*Mental health, financial, and social outcomes among older adults with probable COVID-19 infection: A longitudinal cohort study
*PNAS 2022 Vol. 119 No. 27 e2200816119.

*Note that Eleanor's full code can be accessed on GitHub 
*(https://github.com/Ellie25moon/ELSA-COVID-19-Infection).

*I have used some of that code in this illustrative example.
*The work also builds on the R code in the excellent paper mentioned in the Lecture:
*MatchThem:: Matching and Weighting after Multiple Imputation
*by Farhad Pishgar, Noah Greifer, Clémence Leyrat and Elizabeth Stuart
*The R Journal Vol. 13/2, December 2021
*https://journal.r-project.org/archive/2021/RJ-2021-073/RJ-2021-073.pdf

*The paper details many analyses; and has lengthy code.
*I only provide an illustration of these analyses as a starting point for your own efforts.

*In this example:
*Outcome measure: (Binary CESD depression score): at the second wave of the COVID study.
*Key exposure is probable COVID-19 infection (measured at the first wave of the COVID study).

*I illustrate two different approaches to examine associations between probable infection and depression.

*(1) Regression model estimated on multiply-imputed data, combining regression estimates using Rubin's rules, using survey weight.
*Implemented here in Stata.
*Illustrated in R (MICE and svy)

*(2) IPTW using multiply imputed datasets.
*Illustrated in R (MICE and IPTW)


**************************************************************
*Datasets:
*SN 5050: Main ELSA study: wave_9_elsa_data_eul_v1.dta
*SN 8688 COVID wave 1: elsa_covid_w1_eulv2.dta
*SN 8688 COVID wave 2: elsa_covid_w2_eulv2.dta
**************************************************************


*************************************.
*(SN 5050): Wave 9 main ELSA study.
*Pre-pandemic mental health
**************************************.

clear all
clear
set maxvar 15000
set seed 579503
use "C:\Users\rmjdshc\OneDrive - University College London\ELSA\UKDS\ELSA-5050\stata\wave_9_elsa_data_eul_v1.dta"
keep idauniq finstat psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh 

*Keep core members only (decided here to use Cohorts 1, 3 and 4).
keep if inlist(finstat,1,7,14)
tab1 finstat

*tab1 psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh
mvdecode psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh,mv(-9/-1)              // set to missing (.)
egen s = rowmiss(psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh)               // count the number of missing items
tab1 s        //  5,343 have all items
di 5343+30+4  //  missing either 0,1,or 2 items.

*impute CESD scores only for those missing only 1 or 2 items: drop the rest.
keep if inlist(s,0,1,2)
count
summ psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh

*Create the summary score: CESD number of depressive symptoms.
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
tab1 f1-f8
mvdecode f1-f8,mv(-2)
egen cesd_w9 = rsum(f1-f8) if s==0     // sum of the scores (range 0-8)  if answered all 8 items (n=5343)        
summ cesd_w9
tab1 cesd_w9

*Binary measure (scores of 4+)
generate cesd_binw9 = cesd_w9
recode cesd_binw9 (0/3=0) (4/8=1)
label define clbl 0 "not depressed" 1 "depressed"
label values cesd_binw9 clbl
tab1 cesd_binw9, m     // 12% with depression (base: n=5343)
*we will impute the missing later (n=34) if in analytical sample.

sort idauniq
keep idauniq cesd_binw9
save "C:\Users\rmjdshc\OneDrive - University College London\Temp_ELSA_w9.dta", replace


*****************************.
*(SN 8688): Wave 1 COVID: 
*****************************.

use "C:\Users\rmjdshc\OneDrive - University College London\ELSA\UKDS\Covid-8688\stata\elsa_covid_w1_eulv2.dta", clear
renvars, lower
keep if corepartner==1
mvdecode cvsymp01 cvsymp02 cvsymp05 cvtestb cvhosp,mv(-9/-1) 

***Key exposure:
***Definition of probable COVID-19 infection 
** whether had one/two of the three key symptoms of covid.
**This is Ellie's code: we will use definition 1.

*high temp
tab cvsymp01, nolab missing 
*cough 
tab cvsymp02, nolab missing 
*loss smell/taste
tab cvsymp05, nolab missing 

egen keycovsymsum = rowtotal(cvsymp01 cvsymp02 cvsymp05 )
tab keycovsymsum, missing 
generate keycovsymp2=(keycovsymsum>=2) if keycovsymsum !=.
tab keycovsymp2
generate keycovsymp1=(keycovsymsum>=1) if keycovsymsum !=.
tab keycovsymp1

*covid test
tab cvtestb, nolab missing
replace cvtestb = 0 if cvtestb > 1 | cvtestb == . 
tab cvtestb, nolab

*covid hosp 
tab cvhosp, nolab
replace cvhosp = 0 if cvhosp == 2 | cvhosp == . 
tab cvhosp

** Definition 1: positive covid test/ hospitalisation/ one of three core symptoms 
gen covcase1 = (cvtestb == 1 | cvhosp == 1 | keycovsymp1 == 1) if cvtestb !=. | cvhosp !=. | keycovsymp1 !=.
tab covcase1 

** Definition 2: positive covid test/ hospitalisation/ two of three core symptoms 
gen covcase2 = ( cvtestb == 1 | cvhosp == 1 | keycovsymp2 == 1 ) if cvtestb !=. | cvhosp !=. | keycovsymp2 !=.
tab covcase2 

** Definition 3: positive covid test/ hospitalisation/ loss of taste or smell 
gen covcase3 = ( cvtestb == 1 | cvhosp == 1 | cvsymp05 == 1 ) if cvtestb !=. | cvhosp !=. | cvsymp05 !=. 
tab covcase3

*Outcome measure at COVID wave 1.
*For simplicity I will impute the outcome as a binary measure.
tab1 cvmhced_cvmhced1_q-cvmhced_cvmhced7_q

*rename to be consistent with main study
rename cvmhced_cvmhced1_q psceda
rename cvmhced_cvmhced2_q pscedb
rename cvmhced_cvmhced3_q pscedc
rename cvmhced_cvmhced4_q pscedd
rename cvmhced_cvmhced5_q pscede
rename cvmhced_cvmhced6_q pscedf
rename cvmhced_cvmhced7_q pscedh           // could not get going
rename cvmhced_cvmhced8_q_final pscedg     // sad
mvdecode psced*,mv(-9/-1)
tab1 psc*

egen s = rowmiss(psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh)               // count the number of missing items (including the item with a lot of missing)
tab1 s       
di (1350+4408+39)  // drop those with more than 2 missing items
keep if inlist(s,0,1,2)
count
summ psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh

*Create the summary score: CESD number of depressive symptoms.
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
egen cesd_covid1 = rsum(f1-f8) if s==0     // sum of the scores (range 0-8)  if answered all 8 items (n=1350)        
tab1 cesd_covid1
 
*Binary measure (scores of 4+)
generate cesd_bincovid1 = cesd_covid1
recode cesd_bincovid1 (0/3=0) (4/8=1)
label define clbl 0 "not depressed" 1 "depressed"
label values cesd_bincovid1 clbl
tab1 cesd_bincovid1, m     // 22% with depression (n=1350)
*we will impute the missing later if in analytical sample

keep idauniq covcase1 cesd_bincovid1
save "C:\Users\rmjdshc\OneDrive - University College London\Temp_ELSA_covid1.dta", replace

*************************
*Wave 2 COVID: (SN 8688).
*************************

use "C:\Users\rmjdshc\OneDrive - University College London\ELSA\UKDS\Covid-8688\stata\elsa_covid_w2_eulv2.dta", clear
renvars, lower
*key survey (longitudinal) weight.
*Analytical sample: those with a positive weight (only core members receive weights).
*longitudinal weight: COV19LWGTW2, therefore, is for all respondents to wave 2 of the COVID-19 study 
*who took part in ELSA wave 9, irrespective of response to wave 1 of the substudy.

summ cov19lwgtw2b if cov19lwgtw2b>0
*Analytical sample (N=5146)
keep if cov19lwgtw2b>0

*Outcome measure at COVID wave 2.
*For simplicity I will impute the outcome as a binary measure.
tab1 cvmhced_cvmhced1_q-cvmhced_cvmhced8_q

*rename to be consistent with main study
rename cvmhced_cvmhced1_q psceda
rename cvmhced_cvmhced2_q pscedb
rename cvmhced_cvmhced3_q pscedc
rename cvmhced_cvmhced4_q pscedd
rename cvmhced_cvmhced5_q pscede
rename cvmhced_cvmhced6_q pscedf
rename cvmhced_cvmhced7_q pscedh     // could not get going
rename cvmhced_cvmhced8_q pscedg     // sad
mvdecode psced*,mv(-9/-1)                                            // set to missing
tab1 psc*

egen s = rowmiss(psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh)               // count the number of missing items 
tab1 s       
di (5080+44+10)  // drop those with more than 2 missing items
keep if inlist(s,0,1,2)
count
summ psceda pscedb pscedc pscedd pscede pscedf pscedg pscedh

*Create the summary score: CESD number of depressive symptoms.
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
egen cesd_covid2 = rsum(f1-f8) if s==0     // sum of the scores (range 0-8)  if answered all 8 items (n=5080)        
tab1 cesd_covid2 
 
*Binary measure (scores of 4+)
generate cesd_bincovid2 = cesd_covid2
recode cesd_bincovid2 (0/3=0) (4/8=1)
label define clbl 0 "not depressed" 1 "depressed"
label values cesd_bincovid2 clbl
tab1 cesd_bincovid2, m     // 25% with depression (n=5080)
*we will impute the missing later (n=54)

**covid infection at wave 2.
*covid test 
tab cvtestb
tab cvtestb, nolab 
replace cvtestb = 0 if cvtestb > 1 | cvtestb == -1
replace cvtestb = . if cvtestb < -1
tab cvtestb, nolab

* covd hospitalisation 
tab cvhosp 
tab cvhosp, nolab
replace cvhosp = 0 if cvhosp == 2
replace cvhosp = . if cvhosp < 0
tab cvhosp, nolab

* long covid 
tab cvlongcovid
tab cvlongcovid, nolab
replace cvlongcovid = 0 if cvlongcovid == -1 | cvlongcovid == 2
tab cvlongcovid

*COVID infection status at COVID wave 2
gen cov_w11 = ( cvtestb == 1 | cvhosp == 1 | cvlongcovid == 1 ) if cvtestb !=. | cvhosp !=. | cvlongcovid !=.
tab1 cov_w11
label variable cov_w11 "Infection status at COVID wave 2"

*limiting illness 
mvdecode heill,mv(-9/-1) 
tab heill
recode heill (2=0) (1=1) // 1=yes; 0=no
tab heill


*vulnerable to covid 
mvdecode cvvuln,mv(-9/-8) 
tab cvvuln
replace cvvuln = 0 if cvvuln == -1 | cvvuln == 2 
tab cvvuln

*living alone
mvdecode cvnump,mv(-9/-1)
tab cvnump
gen cvalone = (cvnump<2) if cvnump !=.
tab1 cvalone

keep idauniq cesd_bincovid2 cov19lwgtw2b cov_w11 heill cvvuln cvalone sex age_arch ethnicity_arch 
save "C:\Users\rmjdshc\OneDrive - University College London\Temp_ELSA_covid2.dta", replace

****************************************************************************************.
*Put the 3 datasets together for longitudinal analysis.
*Our analytical sample: condition on having a positive weight at COVID wave 2 (N=5,134)
*****************************************************************************************.

use "C:\Users\rmjdshc\OneDrive - University College London\Temp_ELSA_covid2.dta", clear
summ cov19lwgtw2b
sort idauniq
merge 1:1 idauniq using "C:\Users\rmjdshc\OneDrive - University College London\Temp_ELSA_w9.dta"
drop if _m==2
drop _m
merge 1:1 idauniq using "C:\Users\rmjdshc\OneDrive - University College London\Temp_ELSA_covid1.dta"
drop if _m==2
drop _m
count

misstable summarize cesd_binw9 cesd_bincovid1 cesd_bincovid2   // depression 
misstable summarize covcase1  // exposure
misstable summarize cvvulnb heill cov_w11 cvalone // confounders

****************************************
*Regression approach 1:
*Multiple imputation and estimation.
***************************************

*Estimates combined using Rubin rules (pooled estimates across 20 imputed datasets).
*weighted using survey weight.
*Outcome (cesd_bincovid2)
*Exposure (probable infection at COVID wave 1): covcase1

*Models adjusted for 8 variables: 
*pre-COVID-19 outcome (cesd_binw9)
*Nov-Dec 2020 COVID-19 infection (cov_w11) 
*whether living alone (cvalone)
*whether vulnerable to COVID-19 (cvvulnb)
*limiting long-standing illness (heill)
*sex (sex) 
*age (age_arch)
*ethnicity (ethnicity_arch)

*not adjusting for COVID 1 outcome here (cesd_bincovid1).

set seed 1234543
mi set mlong
mi register imputed cesd_binw9 cesd_bincovid2 covcase1 cvvulnb heill cvalone                      										// variables to impute
mi svyset [pw=cov19lwgtw2b], psu(idauniq)
mi impute chain (logit) cesd_binw9 cesd_bincovid2 covcase1 cvvulnb heill cvalone = sex age_arch ethnicity_arch cov19lwgtw2b, add(2)     // imputation model
mi estimate, or: svy: logit cesd_bincovid2 i.covcase1 cesd_binw9 cov_w11 cvalone cvvulnb heill sex age_arch ethnicity_arch                // estimate and combine across datasets using Rubins rules
*OR = 1.47; (95% CI: 1.09; 1.99)

**********************************
*FINISHED 
**********************************

log close



























 


















































