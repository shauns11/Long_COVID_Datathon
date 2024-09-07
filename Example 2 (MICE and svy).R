#Example 2: UEA COVID datathon.
#Single-level model of health outcomes (here we focus on an outcome at COVID wave 2).
#Some illustrative code for:
#Mental health, financial, and social outcomes among older adults with probable COVID-19 infection: A longitudinal cohort study
#PNAS 2022 Vol. 119 No. 27 e2200816119.
#Note that Eleanor's full code can be accessed on GitHub 
#(https://github.com/Ellie25moon/ELSA-COVID-19-Infection).

#I have used some of that code in this illustrative example.
#The work also builds on the R code in the excellent paper mentioned in the Lecture:
#MatchThem:: Matching and Weighting after Multiple Imputation
#by Farhad Pishgar, Noah Greifer, Clémence Leyrat and Elizabeth Stuart
#The R Journal Vol. 13/2, December 2021
#https://journal.r-project.org/archive/2021/RJ-2021-073/RJ-2021-073.pdf

#This file also includes a function found online to pool the results of survey weighted multiple imputation datasets.
#https://gist.github.com/AaronGullickson/3ccb3fdd1778b32fc46df40d78faf5d3

#The PNAS paper details many analyses; and has lengthy code.
#I only provide an illustration of these analyses as a starting point for your own efforts.

#In this example:
#Outcome measure: (Binary CESD depression score): at the second wave of the COVID study.
#Key exposure is probable COVID-19 infection (measured at the first wave of the COVID study).
#I illustrate two different approaches to examine associations between probable infection and depression.

# (1) Regression model estimated on multiply-imputed data, combining regression estimates using Rubin's rules, using survey weight.
# Implemented in Stata.
# Implemented in this R script (MICE and svy)

# (2) IPTW using multiply imputed datasets.
# Implemented in R (MICE and IPTW)

#########################################################
#Datasets:
#SN 5050: Main ELSA study: wave_9_elsa_data_eul_v1.dta
#SN 8688 COVID wave 1: elsa_covid_w1_eulv2.dta
#SN 8688 COVID wave 2: elsa_covid_w2_eulv2.dta
##########################################################

#############################################
#(SN 5050): Wave 9 main ELSA study.
#Pre-pandemic mental health
#############################################

#install and load libraries.

#install.packages(
# c("dplyr","tidyr","haven")
#)

library(dplyr)
library(tidyr)
library(haven)
library(data.table) 
library(cobalt)
library(MatchThem)
library(mice)
library(survey)
library(tidyverse)
library(dbarts)

# Load wave 9 data
elsaw9 <- read_dta("C:/Users/rmjdshc/OneDrive - University College London/ELSA/UKDS/ELSA-5050/stata/wave_9_elsa_data_eul_v1.dta")
# Keep only the necessary columns
elsaw9 <- elsaw9 %>% select(idauniq, finstat, psceda, pscedb, pscedc, pscedd, pscede, pscedf, 
                            pscedg, pscedh)

# Filter rows based on finstat (core members only: cohorts 1,3,4)
elsaw9 <- elsaw9 %>% filter(finstat %in% c(1, 7, 14))

# Handle missing values on the CESD variables (-9 to -1) by setting them to NA
elsaw9 <- elsaw9 %>% mutate(across(starts_with("psced"), ~ na_if(., -9))) %>% 
  mutate(across(starts_with("psced"), ~ na_if(., -1))) %>% 
  mutate(across(starts_with("psced"), ~ na_if(., -8)))

# Count the number of missing values for each row.
# we will exclude from the dataset those missing 3+ of the 8 items.
elsaw9 <- elsaw9 %>% mutate(s = rowSums(is.na(select(., starts_with("psced")))))
table(elsaw9$s)

# Filter rows based on the number of missing values
elsaw9 <- elsaw9 %>% filter(s %in% c(0, 1, 2))
print(n_obs <- nrow(elsaw9))  # 5377

# summary measure of CESD.
# Generate f1 to f8 columns with initial value -2
elsaw9$f1<-2
elsaw9$f2<-2
elsaw9$f3<-2
elsaw9$f4<-2
elsaw9$f5<-2
elsaw9$f6<-2
elsaw9$f7<-2
elsaw9$f8<-2

# Replace values according to the conditions (0=not depressed; 1=depressed)
# taking care of the items we need to reverse code (items d and f)
elsaw9 <- elsaw9 %>% 
  mutate(f1 = if_else(psceda == 2, 0, if_else(psceda == 1, 1, f1)),
         f2 = if_else(pscedb == 2, 0, if_else(pscedb == 1, 1, f2)),
         f3 = if_else(pscedc == 2, 0, if_else(pscedc == 1, 1, f3)),
         f4 = if_else(pscedd == 1, 0, if_else(pscedd == 2, 1, f4)),
         f5 = if_else(pscede == 2, 0, if_else(pscede == 1, 1, f5)),
         f6 = if_else(pscedf == 1, 0, if_else(pscedf == 2, 1, f6)),
         f7 = if_else(pscedg == 2, 0, if_else(pscedg == 1, 1, f7)),
         f8 = if_else(pscedh == 2, 0, if_else(pscedh == 1, 1, f8)))

# Sum the f1 to f8 values (coded 0/1) for CESD score (range 0-8)
elsaw9 <- elsaw9 %>% mutate(cesd_w9 = f1+f2+f3+f4+f5+f6+f7+f8)
table(elsaw9$cesd_w9)

# set score to -9 for those with at least 1 missing item (n=34.
elsaw9 <- elsaw9 %>%
  mutate(cesd_w9 = if_else(s != 0, -9, cesd_w9))
table(elsaw9$cesd_w9)

# Generate binary CESD score (0-3; 4+).
elsaw9 <- elsaw9 %>%
  mutate(cesdbinw9 = ifelse(cesd_w9>=0 & cesd_w9<=3,0, 
                             ifelse(cesd_w9>=4 & cesd_w9<=8,1,
                            ifelse(cesd_w9==-9,-9,-9))))
table(elsaw9$cesdbinw9)
elsaw9$cesdbinw9[elsaw9$cesdbinw9 == -9] <- NA
#we will impute these missing depression scores later using mice.

# Sort by idauniq for later merging.
elsaw9 <- elsaw9 %>% arrange(idauniq)
# Keep only idauniq and cesd_binw9
elsaw9 <- elsaw9 %>% select(idauniq, cesdbinw9)
# Save the data for use below.
readr::write_rds(elsaw9,"C:/Users/rmjdshc/OneDrive - University College London/elsaw9.rds")

#############
#COVID Wave 1
##############

# Load the covid wave 1 data
covidw1 <- read_dta("C:/Users/rmjdshc/OneDrive - University College London/ELSA/UKDS/Covid-8688/stata/elsa_covid_w1_eulv2.dta")
# remove capital letters from column names.
names(covidw1) <- tolower(names(covidw1))

# Keep only the necessary columns
covidw1 <- covidw1 %>% select(idauniq,corepartner,finstat,
cvsymp01, cvsymp02, cvsymp05, cvtestb, cvhosp,
cvmhced_cvmhced1_q,cvmhced_cvmhced2_q, cvmhced_cvmhced3_q, 
cvmhced_cvmhced4_q, cvmhced_cvmhced5_q, cvmhced_cvmhced6_q, cvmhced_cvmhced7_q,
cvmhced_cvmhced8_q_final) 
        
# Filter rows based on finstat (core members only) using the corepartner variable.
covidw1 <- covidw1 %>% filter(corepartner %in% c(1)) 
nrow(covidw1)   # 5825

# rename columns to use code developed above.
covidw1 <- covidw1 %>%
rename(psceda = cvmhced_cvmhced1_q,
pscedb = cvmhced_cvmhced2_q,
pscedc = cvmhced_cvmhced3_q, 
pscedd = cvmhced_cvmhced4_q,
pscede = cvmhced_cvmhced5_q, 
pscedf = cvmhced_cvmhced6_q, 
pscedh = cvmhced_cvmhced7_q,          
pscedg = cvmhced_cvmhced8_q_final)
# NOTE: this item has a lot of missing values (~75%) due to error in survey administration.
# Indicated by code (-6)
table(covidw1$pscedg)

#####################################################
#Key exposure:
#Definition of "probable COVID-19 infection" 
#This is Ellie's code: we will use definition 1.
#a key symptom; or positive test or hospitalisation.
#####################################################

#set refusals, dont knows, not applicable to missing.
#symptoms: whether had one/two of the three key symptoms of covid.
covidw1$cvsymp01[covidw1$cvsymp01 %in% c(-9,-8,-1) ] <- NA
covidw1$cvsymp02[covidw1$cvsymp02 %in% c(-9,-8,-1) ] <- NA
covidw1$cvsymp05[covidw1$cvsymp05 %in% c(-9,-8,-1) ] <- NA
#whether test positive
covidw1$cvtestb[covidw1$cvtestb %in% c(-9,-8,-1) ] <- NA
#any hospitalisation
covidw1$cvhosp[covidw1$cvhosp %in% c(-9,-8,-1) ] <- NA

#(dimension 1: presence of any of the 3 key symptoms): high temperature; cough; loss of smell/taste.
covidw1 <- covidw1 %>%
  mutate(keycovsymsum = rowSums(select(., cvsymp01, cvsymp02, cvsymp05), na.rm = TRUE)) %>%
  mutate(keycovsymp1 = ifelse(!is.na(keycovsymsum) & keycovsymsum >= 1, 1, 0))
# Display frequency tables
table(covidw1$keycovsymp1) 

#(dimension 2: had a positive test): setting relevant cases to zero.
covidw1 <- covidw1 %>%
  mutate(cvtestb = ifelse(cvtestb > 1 | is.na(cvtestb), 0, cvtestb))
table(covidw1$cvtestb)

#(3: hospitalisation): : setting relevant cases to zero.
covidw1 <- covidw1 %>%
  mutate(cvhosp = ifelse(cvhosp == 2 | is.na(cvhosp), 0, cvhosp))
table(covidw1$cvhosp)

#our key exposure (covcase1): a key symptom; or positive test or hospitalisation.
covidw1 <- covidw1 %>%
  mutate(covcase1 = ifelse(
    !is.na(cvtestb) | !is.na(cvhosp) | !is.na(keycovsymp1),
    (cvtestb == 1 | cvhosp == 1 | keycovsymp1 == 1),
    NA
  ))

#from logical vector to numeric.
covidw1 <- covidw1 %>% mutate(covcase1 = as.numeric(covcase1))
table(covidw1$covcase1)  # n=471 with 'probable covid infection'
class(covidw1$covcase1)  

#Mental health outcome at time point 2 (COVID wave 1).
#Binary depression score.
#Similar code as used above.
#Handle missing values (-9 to -1) by setting them to NA
#Note the inclusion of the special value (-6).
covidw1 <- covidw1 %>% mutate(across(starts_with("psced"), ~ na_if(., -9))) %>% 
  mutate(across(starts_with("psced"), ~ na_if(., -1))) %>% 
  mutate(across(starts_with("psced"), ~ na_if(., -8))) %>%
  mutate(across(starts_with("psced"), ~ na_if(., -6)))

# Count the number of missing values for each row
covidw1 <- covidw1 %>% mutate(s = rowSums(is.na(select(., starts_with("psced")))))
table(covidw1$s)

# Filter rows based on the number of missing values
covidw1 <- covidw1 %>% filter(s %in% c(0, 1, 2))
nrow(covidw1)   # 5797

# Generate f1 to f8 columns with initial value -2
covidw1$f1<-2
covidw1$f2<-2
covidw1$f3<-2
covidw1$f4<-2
covidw1$f5<-2
covidw1$f6<-2
covidw1$f7<-2
covidw1$f8<-2

# Replace values according to the conditions (0=not depressed; 1=depressed).
# taking care with items d and f.
covidw1 <- covidw1 %>% 
  mutate(f1 = if_else(psceda == 2, 0, if_else(psceda == 1, 1, f1)),
         f2 = if_else(pscedb == 2, 0, if_else(pscedb == 1, 1, f2)),
         f3 = if_else(pscedc == 2, 0, if_else(pscedc == 1, 1, f3)),
         f4 = if_else(pscedd == 1, 0, if_else(pscedd == 2, 1, f4)),
         f5 = if_else(pscede == 2, 0, if_else(pscede == 1, 1, f5)),
         f6 = if_else(pscedf == 1, 0, if_else(pscedf == 2, 1, f6)),
         f7 = if_else(pscedg == 2, 0, if_else(pscedg == 1, 1, f7)),
         f8 = if_else(pscedh == 2, 0, if_else(pscedh == 1, 1, f8)))
# Sum the f1 to f8 values (0/1) for CESD score (range 0-8)
covidw1 <- covidw1 %>% mutate(cesd_covidw1 = f1+f2+f3+f4+f5+f6+f7+f8)
table(covidw1$cesd_covidw1)

# set to missing if missed at least 1 item (majority because of pscedg)
covidw1 <- covidw1 %>%
  mutate(cesd_covidw1 = if_else(s != 0, -9, cesd_covidw1))
table(covidw1$cesd_covidw1)

# Generate binary CESD score (0-3 not depressed; 4+ depressed)
covidw1 <- covidw1 %>%
  mutate(cesd_bincovid1 = ifelse(cesd_covidw1>=0 & cesd_covidw1<=3,0, 
                            ifelse(cesd_covidw1>=4 & cesd_covidw1<=8,1,
                                   ifelse(cesd_covidw1==-9,-9,-9))))
table(covidw1$cesd_bincovid1)
covidw1$cesd_bincovid1[covidw1$cesd_bincovid1 == -9] <- NA
# we will impute these n=4447 missing values later using mice.

# Sort by idauniq
covidw1 <- covidw1 %>% arrange(idauniq)

#Keep only idauniq, covcase1 (probable infection) and cesd_bincovid1
covidw1 <- covidw1 %>% select(idauniq,covcase1,cesd_bincovid1)

# Save the data for merging later
readr::write_rds(covidw1,"C:/Users/rmjdshc/OneDrive - University College London/elsa_covidw1.rds")

#######################################################################################################################
#COVID Wave 2
#Data set for the outcome (depression) and the set of confounders:
#(i) we wish to adjust for in a regression model (MICE and svy); OR
#(ii) use in a model for the exposure (probable covid infection) to estimate a propensity score and estimate IPTW.
########################################################################################################################

# Load covid wave 2 data
covidw2 <- read_dta("C:/Users/rmjdshc/OneDrive - University College London/ELSA/UKDS/Covid-8688/stata/elsa_covid_w2_eulv2.dta")
# remove capital letters from column names.
names(covidw2) <- tolower(names(covidw2))

# Keep only the necessary columns
covidw2 <- covidw2 %>% select(idauniq,corepartner,cov19lwgtw2b,
      cvmhced_cvmhced1_q,cvmhced_cvmhced2_q, cvmhced_cvmhced3_q, 
    cvmhced_cvmhced4_q, cvmhced_cvmhced5_q, cvmhced_cvmhced6_q, cvmhced_cvmhced7_q,
  cvmhced_cvmhced8_q, cvtestb, cvhosp, cvlongcovid,heill,cvvulnb,cvnump,
  sex, age_arch, ethnicity_arch) 

#Analytical sample: those with a positive weight (only core members receive weights).
#longitudinal weight: COV19LWGTW2, therefore, is for all respondents to wave 2 of the COVID-19 study 
#who took part in ELSA wave 9, irrespective of response to wave 1 of the substudy.
summary(covidw2$cov19lwgtw2b)
# Filter rows based on having a positive weight.
covidw2 <- covidw2 %>% filter(cov19lwgtw2b > 0)
nrow(covidw2)  # 5146

#Outcome measure at COVID wave 2.
#For simplicity I will impute the outcome as a binary measure.
#Rather than impute the 8 individual items and then create the summary variable.

# rename columns to use code developed above.
# lower proportion of missing at COVID W2
covidw2 <- covidw2 %>%
  rename(psceda = cvmhced_cvmhced1_q,
         pscedb = cvmhced_cvmhced2_q,
         pscedc = cvmhced_cvmhced3_q, 
         pscedd = cvmhced_cvmhced4_q,
         pscede = cvmhced_cvmhced5_q, 
         pscedf = cvmhced_cvmhced6_q, 
         pscedh = cvmhced_cvmhced7_q,          
         pscedg = cvmhced_cvmhced8_q)

#Mental health outcome at time point 3 (COVID wave 2).
#Binary depression score.
#Similar code as used above.
#Handle missing values (-9 to -1) by setting them to NA: (-6 not relevant here)
covidw2 <- covidw2 %>% mutate(across(starts_with("psced"), ~ na_if(., -9))) %>% 
  mutate(across(starts_with("psced"), ~ na_if(., -1))) %>% 
  mutate(across(starts_with("psced"), ~ na_if(., -8)))

# Count the number of missing values for each row
covidw2 <- covidw2 %>% mutate(s = rowSums(is.na(select(., starts_with("psced")))))
table(covidw2$s)

# Filter rows based on the number of missing values
covidw2 <- covidw2 %>% filter(s %in% c(0, 1, 2))
nrow(covidw2) # 5134

# Generate f1 to f8 columns with initial value -2
covidw2$f1<-2
covidw2$f2<-2
covidw2$f3<-2
covidw2$f4<-2
covidw2$f5<-2
covidw2$f6<-2
covidw2$f7<-2
covidw2$f8<-2

# Replace values according to the conditions (0=not depressed; 1=depressed)
# taking care with items d and f.
covidw2 <- covidw2 %>% 
  mutate(f1 = if_else(psceda == 2, 0, if_else(psceda == 1, 1, f1)),
         f2 = if_else(pscedb == 2, 0, if_else(pscedb == 1, 1, f2)),
         f3 = if_else(pscedc == 2, 0, if_else(pscedc == 1, 1, f3)),
         f4 = if_else(pscedd == 1, 0, if_else(pscedd == 2, 1, f4)),
         f5 = if_else(pscede == 2, 0, if_else(pscede == 1, 1, f5)),
         f6 = if_else(pscedf == 1, 0, if_else(pscedf == 2, 1, f6)),
         f7 = if_else(pscedg == 2, 0, if_else(pscedg == 1, 1, f7)),
         f8 = if_else(pscedh == 2, 0, if_else(pscedh == 1, 1, f8)))

# Sum the f1 to f8 values (0/1) for CESD score (range 0-8)
covidw2 <- covidw2 %>% mutate(cesd_covidw2 = f1+f2+f3+f4+f5+f6+f7+f8)
table(covidw2$cesd_covidw2)

# set to missing if missed at least 1 item 
covidw2 <- covidw2 %>%
  mutate(cesd_covidw2 = if_else(s != 0, -9, cesd_covidw2))
table(covidw2$cesd_covidw2)

#Generate binary CESD score (0-3 not depressed; 4+ depressed)
covidw2 <- covidw2 %>%
  mutate(cesd_bincovid2 = ifelse(cesd_covidw2>=0 & cesd_covidw2<=3,0, 
                                 ifelse(cesd_covidw2>=4 & cesd_covidw2<=8,1,
                                        ifelse(cesd_covidw2==-9,-9,-9))))
table(covidw2$cesd_bincovid2)
covidw2$cesd_bincovid2[covidw2$cesd_bincovid2 == -9] <- NA
# we will impute the n=54 with missing values later using mice.

############################################
#Now the confounders we wish to adjust for.
############################################.

#Covid infection at wave 2.
#Derived using similar strategy as above for COVID wave 1.
# but using CvLongCovid
#Have you been told by a doctor that you have any long-standing illness or disability caused by coronavirus (COVID-19)?
table(covidw2$cvlongcovid)
#note large number of -1s (not applicable): see questionnaire for routing.
#{IF FFCvTestB = 1 OR CvTestB = 1 OR FFCvHosp = 1 OR CvHosp = 1 OR ((FFCvSymp01+FFCvSymp02+FFCvSymp05) >1)}

#Had a positive test: set relevant cases to zero.
table(covidw2$cvtestb)
covidw2 <- covidw2 %>% 
  mutate(cvtestb = if_else(cvtestb > 1, 0, 
                  if_else(cvtestb == -1, 0,
                  if_else(cvtestb < -1, -8,cvtestb))))
covidw2$cvtestb[covidw2$cvtestb == -8] <- NA
table(covidw2$cvtestb,exclude=NULL)

#covd hospitalisation: set relevant cases to zero.
table(covidw2$cvhosp)
covidw2 <- covidw2 %>%
  mutate(cvhosp = ifelse(cvhosp == 2 | is.na(cvhosp), 0, cvhosp))
covidw2$cvhosp[covidw2$cvhosp == -8] <- NA
table(covidw2$cvhosp,exclude=NULL)

#long covid: set relevant cases to zero. 
table(covidw2$cvlongcovid)
covidw2 <- covidw2 %>% 
  mutate(cvlongcovid = if_else(cvlongcovid > 1, 0, 
                           if_else(cvlongcovid == -1, 0,cvlongcovid)))
covidw2$cvlongcovid[covidw2$cvlongcovid == -8] <- NA
table(covidw2$cvlongcovid,exclude=NULL)

#COVID infection status at COVID wave 2 (longcovid; positive test or hospitalisation)
#any valid value
covidw2 <- covidw2 %>%
  mutate(
    cov_w11 = ifelse(cvtestb == 1,1,
                   ifelse(cvhosp==1,1,
                  ifelse(cvlongcovid==1,1,0))))
table(covidw2$cov_w11)
#need to deal with 4 more cases (set to zero)

covidw2 <- covidw2 %>%
  mutate(
    cov_w11 = ifelse(is.na(cvtestb) & cvhosp==0 & cvlongcovid==0,0,cov_w11))
table(covidw2$cov_w11)

#limiting illness
covidw2$heill[covidw2$heill %in% c(-9,-8,-1) ] <- NA
covidw2 <- covidw2 %>%
  mutate(heill = case_when(
    heill == 2 ~ 0,
    heill == 1 ~ 1,
    TRUE ~ heill
  ))
table(covidw2$heill)

#vulnerability to covid (-1s set to zero not as NA)
covidw2$cvvulnb[covidw2$cvvulnb %in% c(-9,-8) ] <- NA
covidw2 <- covidw2 %>%
  mutate(cvvulnb = case_when(
    cvvulnb == -1 | cvvulnb == 2 ~ 0,
    TRUE ~ cvvulnb
  ))
table(covidw2$cvvulnb)

#living alone

covidw2$cvnump[covidw2$cvnump >= -9 & covidw2$cvnump <= -1] <- NA
table(covidw2$cvnump)
covidw2$cvalone <- ifelse(!is.na(covidw2$cvnump) & covidw2$cvnump < 2, 1, 
                  ifelse(!is.na(covidw2$cvnump) & covidw2$cvnump >= 2, 0,
                      ifelse(is.na(covidw2$cvnump),NA,NA)))
table(covidw2$cvalone)

# other confounders
table(covidw2$sex)
table(covidw2$age_arch)
table(covidw2$ethnicity_arch)

# Sort by idauniq
covidw2 <- covidw2 %>% arrange(idauniq)

#Keep only idauniq, longitudinal weight, cov_w11 (probable infection); cesd_bincovid2; and confounders
covidw2 <- covidw2 %>% select(idauniq,cesd_bincovid2,
          cov19lwgtw2b,cov_w11,heill,cvvulnb,cvalone,sex,age_arch,ethnicity_arch)

# Save the data 
readr::write_rds(covidw2,"C:/Users/rmjdshc/OneDrive - University College London/elsa_covidw2.rds")

############################
#Put the datasets together
############################

rm()
#readr::read_rds("Data1.rds")
readr::read_rds("C:/Users/rmjdshc/OneDrive - University College London/elsaw9.rds")
readr::read_rds("C:/Users/rmjdshc/OneDrive - University College London/elsa_covidw1.rds")
readr::read_rds("C:/Users/rmjdshc/OneDrive - University College London/elsa_covidw2.rds")

#set as datatables to facilitate merging.
setDT(elsaw9)
setDT(covidw1)
setDT(covidw2)
# do not add to the observations in the covid wave 2 dataset (N=5134)

# First merge (covid wave2 and wave9)
df1 <- merge(covidw2,elsaw9, by = "idauniq", all.x = TRUE, suffixes = c("", ".y"))
nrow(df1)

# Second merge (bring in covid wave1 data)
df1 <- merge(df1, covidw1, by = "idauniq", all.x = TRUE, suffixes = c("", ".y"))
nrow(df1)
ncol(df1)
# 5134 observations (13 variables).
names(df1)

#order columns for mice (cesd_bincovid1 not used here)
df1 <- df1 %>%
  dplyr::select(cesd_bincovid2,covcase1,cesdbinw9,cvalone,cvvulnb,heill,
                idauniq,cov_w11,sex,age_arch,ethnicity_arch,cov19lwgtw2b)
ncol(df1)

set.seed(45673953)

#The six variables we wish to impute.
table(df1$cesd_bincovid2,exclude=NULL)
table(df1$covcase1,exclude=NULL)
table(df1$cesdbinw9,exclude=NULL)
table(df1$cvalone,exclude=NULL)
table(df1$cvvulnb,exclude=NULL)
table(df1$heill,exclude=NULL)

#set these binary variables to factors.
df1$cesd_bincovid2<-factor(df1$cesd_bincovid2)
df1$covcase1<-factor(df1$covcase1)
df1$cesdbinw9<-factor(df1$cesdbinw9)
df1$cvalone<-factor(df1$cvalone)
df1$cvvulnb<-factor(df1$cvvulnb)
df1$heill<-factor(df1$heill)

##########################################
#imputing the missing data in the dataset.
#using mice for the multiple imputation.
# 5 datasets created
##########################################

imputations <- mice(df1,
                         m = 5, maxit = 10,
                         method = c("logreg","logreg","logreg","logreg","logreg","logreg",
                                    "","","","","",""))

##function (lm_svy_mi): to use need to know the identifier (idauniq); weight (cov19lwgtw2b); and the name of the imputed datasets (here "imputations")
#take each imputed dataset
#run a survey weighted glm: adjusting for confounders
#pool the results

#https://gist.github.com/AaronGullickson/3ccb3fdd1778b32fc46df40d78faf5d3
#### Doing Two Things at the Same Time ####

#you can't use the pool function to create the model if you want to apply
#the design effects. You will need to loop through the complete datasets 
#in our imputations object, create a design effect dataset, run the model,
#and collect the results. To make this more flexible, we can put the 
#whole thing into a function that just needs a model formula and the
#imputations object.
lm_svy_mi <- function(formula, imputations) {
  
  #setting up null objects allows us to easily add results
  #later
  b <- se <- R2 <- NULL
  
  #now loop through our imputations and run the model
  for(i in 1:imputations$m) {
    #grab the complete dataset
    imputation <- complete(imputations, i)
    #create the design effect object
    imputation.svy <- svydesign(ids=~idauniq, weight=~cov19lwgtw2b,
                                data=imputation) 
    #run the model
    model <- svyglm(formula, design=imputation.svy,family=quasibinomial)
    #collect the results
    b <- cbind(b, coef(model))
    se <- cbind(se, summary(model)$coef[,2])
    #We should get R squared too. Sadly, svyglm won't give
    #it to us by default, but we can get it from some of the 
    #slots in the model output
    SSR <- sum((model$residuals)^2)
    SSY <- sum((model$y-mean(model$y))^2)
    R2 <- c(R2,1-SSR/SSY)
  }
  
  #now pool the results
  b.pool <- apply(b, 1, mean)
  between.var <- apply(b, 1, var)
  within.var <- apply(se^2, 1, mean)
  se.pool <- sqrt(within.var+between.var+between.var/imputations$m) 
  t.pool <- b.pool/se.pool 
  pvalue.pool <- (1-pnorm(abs(t.pool)))*2 
  coefficients <- data.frame(b.pool, se.pool, t.pool, pvalue.pool)
  
  #lets take the mean R2 value
  r.squared <- mean(R2)
  #we can also grap n and p from the last model since 
  #they should be the same across all iterations
  n <- nobs(model)
  p <- length(model$coefficients)-1
  #go ahead and calculate BIC.null
  bic.null <- n*log(1-r.squared)+p*log(n)
  
  #return everything in a list
  return(list(coef=coefficients,
              n=n,
              r.squared=r.squared,
              bic.null=bic.null))
}

#now lets try the model out using this function.
lm_svy_mi(cesd_bincovid2 ~ covcase1 +
            cesdbinw9 + cvalone + cov_w11 + cvvulnb + heill + sex + age_arch 
          + ethnicity_arch, imputations)

#exponentiate estimates for ORs (exposure = covcase1)
exp(0.4294)                        ## OR
exp(0.4294 - (1.96*0.1587))        ## LL
exp(0.4294 + (1.96*0.1587))        ## UL
#(OR=1.53; 95% CI: 1.12-2.10)

##############
#FINISHED
##############
























                             
                           




