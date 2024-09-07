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

#The PNAS paper details many analyses; and has lengthy code.
#I only provide an illustration of these analyses as a starting point for your own efforts.

#In this example:
#Outcome measure: (Binary CESD depression score): at the second wave of the COVID study.
#Key exposure is probable COVID-19 infection (measured at the first wave of the COVID study).
#I illustrate two different approaches to examine associations between probable infection and depression.

# (1) Regression model estimated on multiply-imputed data, combining regression estimates using Rubin's rules, using survey weight.
# Implemented in Stata.
# Implemented in R script (MICE and svy)

# (2) IPTW using multiply imputed datasets.
# Implemented in this R script. (MICE and IPTW)

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

#############################################
#(SN 5050): Wave 9 main ELSA study.
#Pre-pandemic mental health
#############################################

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
# we will exclude those missing 3+ items.
elsaw9 <- elsaw9 %>% mutate(s = rowSums(is.na(select(., starts_with("psced")))))
table(elsaw9$s)

# Filter rows based on the number of missing values
elsaw9 <- elsaw9 %>% filter(s %in% c(0, 1, 2))
print(n_obs <- nrow(elsaw9))

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
# taking care of the items we need to reverse code (items 4 and 6)
elsaw9 <- elsaw9 %>% 
  mutate(f1 = if_else(psceda == 2, 0, if_else(psceda == 1, 1, f1)),
         f2 = if_else(pscedb == 2, 0, if_else(pscedb == 1, 1, f2)),
         f3 = if_else(pscedc == 2, 0, if_else(pscedc == 1, 1, f3)),
         f4 = if_else(pscedd == 1, 0, if_else(pscedd == 2, 1, f4)),
         f5 = if_else(pscede == 2, 0, if_else(pscede == 1, 1, f5)),
         f6 = if_else(pscedf == 1, 0, if_else(pscedf == 2, 1, f6)),
         f7 = if_else(pscedg == 2, 0, if_else(pscedg == 1, 1, f7)),
         f8 = if_else(pscedh == 2, 0, if_else(pscedh == 1, 1, f8)))

# Sum the f1 to f8 values (coded 0/1) for CESD score
elsaw9 <- elsaw9 %>% mutate(cesd_w9 = f1+f2+f3+f4+f5+f6+f7+f8)
table(elsaw9$cesd_w9)

# set to missing for those with at least 1 missing item.
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
#we will impute these missing later.

# Sort by idauniq for later merging.
elsaw9 <- elsaw9 %>% arrange(idauniq)
# Keep only idauniq and cesd_binw9
elsaw9 <- elsaw9 %>% select(idauniq, cesdbinw9)
# Save the data for use below.
readr::write_rds(elsaw9,"C:/Users/rmjdshc/OneDrive - University College London/elsaw9.rds")

#############
#COVID Wave 1
##############

# Load covid wave 1 data
covidw1 <- read_dta("C:/Users/rmjdshc/OneDrive - University College London/ELSA/UKDS/Covid-8688/stata/elsa_covid_w1_eulv2.dta")
# remove capital letters from column names.
names(covidw1) <- tolower(names(covidw1))

# Keep only the necessary columns
covidw1 <- covidw1 %>% select(idauniq,corepartner,finstat,
cvsymp01, cvsymp02, cvsymp05, cvtestb, cvhosp,
cvmhced_cvmhced1_q,cvmhced_cvmhced2_q, cvmhced_cvmhced3_q, 
cvmhced_cvmhced4_q, cvmhced_cvmhced5_q, cvmhced_cvmhced6_q, cvmhced_cvmhced7_q,
cvmhced_cvmhced8_q_final) 
        
# Filter rows based on finstat (core members only)
covidw1 <- covidw1 %>% filter(corepartner %in% c(1)) 

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
# note that this item has a lot of missing values.
table(covidw1$pscedg)

#####################################################
#Key exposure:
#Definition of "probable COVID-19 infection" 
#This is Ellie's code: we will use definition 1.
#a key symptom; or positive test or hospitalisation.
#####################################################

#set refusals, dont knows, not applicable to missing.
covidw1$cvsymp01[covidw1$cvsymp01 %in% c(-9,-8,-1) ] <- NA
covidw1$cvsymp02[covidw1$cvsymp02 %in% c(-9,-8,-1) ] <- NA
covidw1$cvsymp05[covidw1$cvsymp05 %in% c(-9,-8,-1) ] <- NA
#whether test positive
covidw1$cvtestb[covidw1$cvtestb %in% c(-9,-8,-1) ] <- NA
#any hospitalisation
covidw1$cvhosp[covidw1$cvhosp %in% c(-9,-8,-1) ] <- NA

#symptoms: whether had one/two of the three key symptoms of covid.
#(1: presence of symptoms).
covidw1 <- covidw1 %>%
  mutate(keycovsymsum = rowSums(select(., cvsymp01, cvsymp02, cvsymp05), na.rm = TRUE)) %>%
  mutate(keycovsymp1 = ifelse(!is.na(keycovsymsum) & keycovsymsum >= 1, 1, 0))
table(covidw1$keycovsymp1) 

#(2: had a positive test).
covidw1 <- covidw1 %>%
  mutate(cvtestb = ifelse(cvtestb > 1 | is.na(cvtestb), 0, cvtestb))
table(covidw1$cvtestb)

#(3: hospitalisation).
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
table(covidw1$covcase1)  # n=471 with 'probable covid19 infection'
class(covidw1$covcase1)  

#Mental health outcome at time point 2 (COVID wave 1).
#Binary depression score at covid wave 1.
#Similar code as used above.
#Handle missing values (-9 to -1) by setting them to NA
covidw1 <- covidw1 %>% mutate(across(starts_with("psced"), ~ na_if(., -9))) %>% 
  mutate(across(starts_with("psced"), ~ na_if(., -1))) %>% 
  mutate(across(starts_with("psced"), ~ na_if(., -8))) %>%
  mutate(across(starts_with("psced"), ~ na_if(., -6)))
  
# Count the number of missing values for each row
covidw1 <- covidw1 %>% mutate(s = rowSums(is.na(select(., starts_with("psced")))))
table(covidw1$s)

# Filter rows based on the number of missing values
covidw1 <- covidw1 %>% filter(s %in% c(0, 1, 2))
nrow(covidw1)

# Generate f1 to f8 columns with initial value -2
covidw1$f1<-2
covidw1$f2<-2
covidw1$f3<-2
covidw1$f4<-2
covidw1$f5<-2
covidw1$f6<-2
covidw1$f7<-2
covidw1$f8<-2

# Replace values according to the conditions (0=not depressed; 1=depressed)
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

# Sort by idauniq
covidw1 <- covidw1 %>% arrange(idauniq)

#Keep only idauniq, covcase1 (probable infection) and cesd_bincovid1
covidw1 <- covidw1 %>% select(idauniq,covcase1,cesd_bincovid1)

# Save the data for merging later
readr::write_rds(covidw1,"C:/Users/rmjdshc/OneDrive - University College London/elsa_covidw1.rds")

###################################################################################################################
#COVID Wave 2
#Dataset for the outcome (depression) and confounders.
#Here we use the confounders in a model for the exposure to estimate a propensity score and compute an IPTW
#But note: the weights are created using a multiply imputed dataset
###################################################################################################################

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

#Analytical sample: those with a positive weight (N=5146)
summary(covidw2$cov19lwgtw2b)
# Filter rows based on having a positive weight.
covidw2 <- covidw2 %>% filter(cov19lwgtw2b > 0)
nrow(covidw2)

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
#Binary depression score at covid wave 2.
#Similar code as used above.
#Handle missing values (-9 to -1) by setting them to NA
covidw2 <- covidw2 %>% mutate(across(starts_with("psced"), ~ na_if(., -9))) %>% 
  mutate(across(starts_with("psced"), ~ na_if(., -1))) %>% 
  mutate(across(starts_with("psced"), ~ na_if(., -8)))

# Count the number of missing values for each row
covidw2 <- covidw2 %>% mutate(s = rowSums(is.na(select(., starts_with("psced")))))
table(covidw2$s)

# Filter rows based on the number of missing values
covidw2 <- covidw2 %>% filter(s %in% c(0, 1, 2))
nrow(covidw2)

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
# we will impute the missing later.

############################################
#Now the confounders we wish to use.
############################################.

#Covid infection at wave 2.
#Derived using similar strategy as above for covid wave 1.

#Had a positive test.
table(covidw2$cvtestb)
covidw2 <- covidw2 %>% 
  mutate(cvtestb = if_else(cvtestb > 1, 0, 
                  if_else(cvtestb == -1, 0,
                  if_else(cvtestb < -1, -8,cvtestb))))
covidw2$cvtestb[covidw2$cvtestb == -8] <- NA
table(covidw2$cvtestb,exclude=NULL)


#covd hospitalisation 
table(covidw2$cvhosp)
covidw2 <- covidw2 %>%
  mutate(cvhosp = ifelse(cvhosp == 2 | is.na(cvhosp), 0, cvhosp))
covidw2$cvhosp[covidw2$cvhosp == -8] <- NA
table(covidw2$cvhosp,exclude=NULL)

#long covid 
table(covidw2$cvlongcovid)
covidw2 <- covidw2 %>% 
  mutate(cvlongcovid = if_else(cvlongcovid > 1, 0, 
                           if_else(cvlongcovid == -1, 0,cvlongcovid)))
covidw2$cvlongcovid[covidw2$cvlongcovid == -8] <- NA
table(covidw2$cvlongcovid,exclude=NULL)

#COVID infection status at COVID wave 2 (longcovid; positive test or hospitalisation)
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
nrow(covidw2)

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
                          
# Sort by idauniq
covidw2 <- covidw2 %>% arrange(idauniq)

#Keep only idauniq, longitudinal weight, cov_w11 (probable infection); cesd_bincovid2 (outcome); and confounders
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
#do not add to the observations in the covid wave 2 dataset (N=5134)


# First merge (covidw2 and w9)
df1 <- merge(covidw2,elsaw9, by = "idauniq", all.x = TRUE, suffixes = c("", ".y"))
nrow(df1)

# Second merge (bring in covidw1)
df1 <- merge(df1, covidw1, by = "idauniq", all.x = TRUE, suffixes = c("", ".y"))
nrow(df1)
ncol(df1)
# 5134 observations (13 variables).
names(df1)

#order for mice
df1 <- df1 %>%
  dplyr::select(cesd_bincovid2,covcase1,cesdbinw9,cvalone,cvvulnb,heill,
                idauniq,cov_w11,sex,age_arch,ethnicity_arch,cov19lwgtw2b)
ncol(df1)

##########################################################################
##Confounder balance by IPTW.
##Code explained in the paper:
##MatchThem:: Matching andWeighting after Multiple Imputation
##by Farhad Pishgar, Noah Greifer, Clémence Leyrat and Elizabeth Stuart
##The R Journal Vol. 13/2, December 2021
###########################################################################.

set.seed(583854324)

###################################
#The six variables we wish to impute.
###################################

table(df1$cesd_bincovid2,exclude=NULL)
table(df1$covcase1,exclude=NULL)
table(df1$cesdbinw9,exclude=NULL)
table(df1$cvalone,exclude=NULL)
table(df1$cvvulnb,exclude=NULL)
table(df1$heill,exclude=NULL)

#set binary variables to factors.
df1$cesd_bincovid2<-factor(df1$cesd_bincovid2)
df1$covcase1<-factor(df1$covcase1)
df1$cesdbinw9<-factor(df1$cesdbinw9)
df1$cvalone<-factor(df1$cvalone)
df1$cvvulnb<-factor(df1$cvvulnb)
df1$heill<-factor(df1$heill)

#SEE WORKFLOW IN THE LECTURE.

##########################################
#imputing the missing data in the dataset.
#using mice for the multiple imputation.
##########################################

#The code produces 5 imputed datasets and saves them in the imputed.datasets object.

imputed.datasets <- mice(df1,
                         m = 5, maxit = 10,
                         method = c("logreg","logreg","logreg","logreg","logreg","logreg",
                                    "","","","","",""))


##########################################################
#weighting the imputed datasets
#regression model for the treatment (our measure of COVID-19 infection)
#include the confounders and the longitudinal weight
###########################################################.

#logistic regression propensity score weighting ("ps")

weighted.datasets <- MatchThem::weightthem(covcase1 ~ sex + age_arch + cesdbinw9 + 
                                             cvalone + cov19lwgtw2b + heill + cvvulnb,
                                           imputed.datasets,
                                           approach = 'within', method = 'ps',
                                           estimand = 'ATM')
summary(weighted.datasets, n=1)

#Assessing Balance on the Weighted Datasets.
#absolute standardized mean differences (ASMDs) 
cobalt::bal.tab(weighted.datasets, abs = TRUE)

#create plot 
plot.dep<- love.plot(weighted.datasets, title = "Probable infection") 
plot.dep


#model (svyglm) for the outcome on each imputed and weighted dataset: just containing the treatment
#Using with() function from MatchThem package, causal effects and their standard errors are estimated in each of the
#weighted imputed datasets.
#The exposure effect within each imputed dataset can be estimated using with(), which applies the
#supplied outcome model to each of the  weighted datasets.

weighted.models <- with(data = weighted.datasets,
                        expr = svyglm(cesd_bincovid2 ~ covcase1, 
                                      family=quasibinomial),cluster=TRUE)

#Pooling the Causal Effect Estimates: The pool() function from the package is used to pool the
#obtained causal effect estimates and standard errors from each dataset using Rubin’s rules.
#In order to arrive at a single set of coefficient and standard error estimates from the imputed datasets,
#we must pool the estimated models using pool().

weighted.results <- pool(weighted.models)

#run summary to arrive at a final set of estimates for the weighted data:
  
summary(weighted.results, conf.int = TRUE)

#The displayed results show that there is evidence of an association between 
#probable covid-19 infection and depression (beta = 0.34 [0.13 – 0.55],
#odds ratio = 1.41 [1.14 – 1.74]).

#exp(0.34)   ## estimate
#exp(0.13)   ## 95% LL
#exp(0.55)   ## 95% UL

#regression approach 1 (MI & survey) in R: OR = 1.53; (95% CI: 1.12-2.10)
#regression approach 2 (MI & IPTW) in R: OR = 1.41; (95% CI: 1.14-1.74).

################
#FINISHED
################
























                             
                           




