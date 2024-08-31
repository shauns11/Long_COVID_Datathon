******************************************************************************************
*UEA Long COVID Datathon (9//9/2024)
*Descriptive estimates presented in Lecture: estimates of COVID data collected at wave 10.
*Shaun Scholes (UCL).
*******************************************************************************************

*Datasets required:
*SN 8688: COVID sub-study wave 1 (elsa_covid_w1_eulv2.dta)
*SN 8688: COVID sub-study wave 2 (elsa_covid_w2_eulv2.dta)
*SN 5050: ELSA Wave 10 interview data (wave_10_elsa_data_eul_v3)
*merge together using -idauniq-.

*From the COVID sub-study, use the variables giving test results.
*CvTest "Have you been tested for coronavirus (Covid-19)?"
*CvTestB "What was the result of your coronavirus (Covid-19) test?"

use "C:\Users\rmjdshc\OneDrive - University College London\ELSA\UKDS\Covid-8688\stata\elsa_covid_w1_eulv2.dta", clear
keep idauniq CvTestB
tab1 CvTestB // result of test 
rename CvTestB cvTest1
sort idauniq
save "N:\Temp\Temp1.dta", replace

use "C:\Users\rmjdshc\OneDrive - University College London\ELSA\UKDS\Covid-8688\stata\elsa_covid_w2_eulv2.dta", clear
keep idauniq CvTestB
tab1 CvTestB // result of test
rename CvTestB cvTest2
sort idauniq
save "N:\Temp\Temp2.dta", replace


*Wave 10 data (n=7589).
clear
use "S:\FPHS_EPH_ELSA_Shared\W10\August 2024\UKDA-5050-stata\stata\stata13_se\wave_10_elsa_data_eul_v3.dta"
count
tab1 mode 

*merge in the results of tests from the COVID sub-study.
sort idauniq
merge 1:1 idauniq using "N:\Temp\Temp1.dta", keepusing(cvTest1)
drop if _m==2
drop _m
merge 1:1 idauniq using "N:\Temp\Temp2.dta", keepusing(cvTest2)
drop if _m==2
drop _m
count

*******************************************************************
*Analytical sample (n=6041): 
*(i) non-proxies; 
*(ii) cross-sectional weight (ELSA core members aged 50+.
******************************************************************.

drop if askpx==1
keep if w10xwgt>0 & !missing(w10xwgt)
tab1 corepartner

*survey characteristics: just interested in estimates here.
svyset [pweight=w10xwgt]

*Wave 10 variables:
*hecvtsta                 // any test
*hecvtstb                 // result of test
*hecvconf                 // whether confirms previous
*hecvsym                  // any symptoms when have COVID
*hecvsyme                 // respondent experience
*hecvlong                 // any long-standing illness/disability
*hecvlconfa-hecvlconot   // new diagnosed conditions linked to COVID-19
*hecvllim                // whether limits activities in any way
*hecvvac                 // booster vaccine 


**********************************************
*1. % having a COVID test (Denominator: all)
***********************************************

tab hecvtsta
count if (hecvtsta==-1)  //[(IFFW[spno].CvTestB <> 1)]: 56 not applicable (positive test in substudy.
count if (cvTest1==1)|(cvTest2==1)

clonevar anytest=hecvtsta
replace anytest=1 if (cvTest1==1)|(cvTest2==1)
tab1 anytest
mvdecode anytest,mv(-9/-1)  // set to missing
tab1 anytest
svy:tab anytest

******************************************************************
*2. % having a +ve test (Denominator: those having a COVID test)
******************************************************************

tab1 hecvtstb  // result of test
count if hecvtstb==-1
count if inlist(hecvtsta,-8,2)|((cvTest1==1)|(cvTest2==1))

clonevar result=hecvtstb
replace result=1 if ((cvTest1==1)|(cvTest2==1))  // positive test in substudy
mvdecode result,mv(-9/-1,3,4)                    // set to missing
tab result
svy:tab result

*************************************************************************************************************
*3. % having any symptoms (Denominator: those having a +ve test)
*IF confirmed that had COVID in COVID surveys or had a positive test since│ [HeCVtstb = 1 or HECvconf=1]
**************************************************************************************************************

tab1 hecvsym  // any symptoms when have COVID (1857)
count if (hecvsym!=-1)                             // valid cases 
count if (hecvtstb==1)|(hecvconf==1)

clonevar symptoms=hecvsym
mvdecode symptoms,mv(-9/-1)          // set to missing
tab symptoms
svy:tab symptoms


*********************************************************************************.
*4. % having symptoms at least 4 weeks (Denominator: those having any symptoms)
*IF confirmed that had symptoms│ [HECvSym = 1]
*********************************************************************************.

count if hecvsyme!=-1
count if hecvsym==1
tab1 hecvsyme if hecvsym==1  

clonevar exp_symptoms=hecvsyme
mvdecode exp_symptoms,mv(-8/-1,8)   // spontaneous (n=15) set to missing
tab exp_symptoms
svy:tab exp_symptoms                // 4+ weeks = last 2 categories
di (.0459 + .025)

***********************************************************************
*5. % diagnosed long-standing illness due to COVID (Denominator: all)
***********************************************************************

tab1 hecvlong  // any long-standing illness/disability due to covid

clonevar covid_LSI=hecvlong
mvdecode covid_LSI,mv(-8)                        // set to missing
tab covid_LSI
svy:tab covid_LSI 

*********************************************************************************************************************************
*6. % with new diagnosed conditions linked to COVID (Denominator: those with any long-standing illness/disability due to covid)
*********************************************************************************************************************************

count if hecvlconfa!=-1  // [(HECvLong=1)]
tab1 hecvlconfa-hecvlconot if hecvlong==1    // new diagnosed conditions linked to COVID-19

foreach var of varlist hecvlconfa hecvlconcg hecvlconsb  {
svy:tab `var' if hecvlong==1 
}

***************************************************
*7. % received booster vaccine (Denominator: all)
***************************************************

tab1 hecvbvac 
tab1 hecvvac 
*131 not applicable for hecvbvac  (not vaccinated): set these to not having received a booster vaccine


generate booster=-2
replace booster=. if inlist(hecvbvac,-8,-9)|inlist(hecvvac,-9)    // set to missing
replace booster=0 if inlist(hecvvac,2,3,4)                        // not vaccinated
replace booster=0 if inlist(hecvbvac,2)                           // vaccinated, but no booster
replace booster=1 if inlist(hecvbvac,1)                           // vaccinated, received booster
tab1 booster
svy:tab booster





























 