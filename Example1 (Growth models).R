#Example 1: UEA COVID datathon
#Growth model of depression
#Some illustrative code for:
#Depression and anxiety in people with cognitive impairment and dementia during the COVID-19 pandemic: Analysis of the English Longitudinal Study of Ageing. 
#PLoS Med 20(4): e1004162.

#Datasets used:
#SN 5050: Main ELSA study (Harmonized version): h_elsa_g3.dta
#SN 5050: Main ELSA study: wave_9_elsa_data_eul_v1.dta
#SN 8688 COVID wave 1: elsa_covid_w1_eulv2.dta
#SN 8688 COVID wave 2: elsa_covid_w2_eulv2.dta

#Use of a growth model to examine whether changes in depression were associated with cognitive impairment.
#Dependent variable: depression (CESD) score at 3 time points (pre-pandemic wave 9, and both waves of the COVID-19 sub-study)
#Main Independent variables: measurement wave and cognitive function status (assessed in the paper using an algorithm, including HCAP data).
#(Pre-pandemic) confounders: demographics, SES, health, geography (e.g. region)
#Statistical analysis:
#Here we provide code to estimate a multilevel / mixed effects linear regression model 
#to estimate change in depression across 3 cognitive function groups (no impairment; mild impairment; dementia).
#We also include code to demonstrate use of multiple imputation to fill in missing values on 1 or 2 depression items 
#(especially important for COVID 1 data, where 1 item is missing for 75%+)
#Note that here we do not use the exact dementia algorithm used in this paper. It is not replicable using archived data.
#So we illustrate here using a combination of Wave 9 cognitive function (CF) data (memory scores); and doctor-diagnosed dementia. 
#Therefore please note: this is just an illustration.

#install and load libraries.

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
library(lme4)

rm()
#Load Harmonized data from SN 5050.
h_elsa_g3<-read_dta("C:/Users/rmjdshc/OneDrive - University College London/ELSA/UKDS/ELSA-5050/stata/h_elsa_g3.dta")

# Keep only the necessary columns (diagnosed dementia/alzheimers)
h_elsa_g3 <- h_elsa_g3 %>% select(idauniq,r9demene,r9alzhe)
ncol(h_elsa_g3)

#Load data (ELSA wave9)
elsaw9 <- read_dta("C:/Users/rmjdshc/OneDrive - University College London/ELSA/UKDS/ELSA-5050/stata/wave_9_elsa_data_eul_v1.dta")

# Keep only the necessary columns
elsaw9 <- elsaw9 %>% select(idauniq, finstat, psceda, pscedb, pscedc, pscedd, pscede, pscedf, 
                            pscedg, pscedh, indager, indsex, GOR, fqethnmr, couple, cflisen, cflisd)
nrow(elsaw9)

#Cognitive function status at wave9.
#Match in variables for doctor-diagnosed dementia and alzheimer's disease from the Harmonized ELSA data.
#which uses information from previous waves.

setDT(elsaw9)
setDT(h_elsa_g3)

#Merge data sets without adding observations
df1 <- merge(elsaw9,h_elsa_g3, by = "idauniq", all.x = TRUE, suffixes = c("", ".y"))
nrow(df1)
table(df1$r9demene)
table(df1$r9alzhe)

#Memory scores (here as sum of immediate- and delayed-word recall).
#higher scores = better memory.
#Handle missing values  (-9 to -1) by setting them to NA
df1 <- df1 %>% mutate(across(starts_with("cflis"), ~ na_if(., -9))) %>% 
  mutate(across(starts_with("cflis"), ~ na_if(., -1))) %>% 
  mutate(across(starts_with("cflis"), ~ na_if(., -8)))
#compute the sum.
df1 <- df1 %>%
  mutate(cfw9 = cflisen + cflisd)
table(df1$cfw9)

#Group the memory score into high; medium; low (arbitrary cut-offs).
df1 <- df1 %>%
  mutate(
    cf_g3w9 = case_when(
      cfw9 >= 8 & cfw9 <= 20 ~ 1,       # no impairment
      cfw9 >= 6 & cfw9 <= 7 ~ 2,        # mild impairment
      cfw9 >= 0 & cfw9 <= 5 ~ 3,       # dementia
      TRUE ~ NA_real_  
    )
  )

#Assign those with diagnosed condition into the third group (irrespective of memory score)
df1 <- df1 %>%
  mutate(cf_g3w9 = case_when(
    (r9demene == 1) | (r9alzhe == 1) ~ 3,
    TRUE ~ cf_g3w9  # Retain the existing value if conditions are not met
  ))
table(df1$cf_g3w9)

#Apply labels using factor levels
df1$cf_g3w9 <- factor(df1$cf_g3w9,
                     levels = c(1, 2, 3),
                     labels = c("no impairment", "mild-impairment", "dementia"))
table(df1$cf_g3w9)

#Numeric variable for region to match paper.
df1 <- df1 %>%
  mutate(region = case_when(
    GOR %in% c("E12000001", "E12000002", "E12000003") ~ 1,
    GOR %in% c("E12000004", "E12000005") ~ 2,
    GOR %in% c("E12000006", "E12000007") ~ 3,
    GOR %in% c("E12000008", "E12000009") ~ 4,
    TRUE ~ NA_real_  # Handle cases where GOR does not match any specified value
  ))
table(df1$region)
df1$region <- factor(df1$region,
                      levels = c(1, 2, 3,4),
                      labels = c("North", "Midlands", "London and East", "South"))
table(df1$region)

#Keep core members only (decided here to use Cohorts 1, 3 and 4).
# Filter rows based on finstat (core members only: cohorts 1,3,4)
df1 <- df1 %>% filter(finstat %in% c(1, 7, 14))
nrow(df1)    # 5655

#CESD scale.
#Deriving a depression score is not a simple task due to need for reverse coding.
#high score to indicate more depressive symptoms
#items d (happy) and f (enjoyed life) coded differently to others.

# Handle missing values on the CESD variables (-9 to -1) by setting them to NA
df1 <- df1 %>% mutate(across(starts_with("psced"), ~ na_if(., -9))) %>% 
  mutate(across(starts_with("psced"), ~ na_if(., -1))) %>% 
  mutate(across(starts_with("psced"), ~ na_if(., -8)))

#Count the number of missing values for each row.
#we will exclude from the dataset those missing 3+ items.
df1 <- df1 %>% mutate(s = rowSums(is.na(select(., starts_with("psced")))))
table(df1$s)

# Filter rows based on the number of missing values
df1 <- df1 %>% filter(s %in% c(0, 1, 2))
print(n_obs <- nrow(df1))   # 5377

# summary measure of CESD.
# Generate f1 to f8 columns with initial value -2
df1$f1<-2
df1$f2<-2
df1$f3<-2
df1$f4<-2
df1$f5<-2
df1$f6<-2
df1$f7<-2
df1$f8<-2

# Replace values according to the conditions (0=not depressed; 1=depressed)
# taking care of the items we need to reverse code (items 4 and 6)
df1 <- df1 %>% 
  mutate(f1 = if_else(psceda == 2, 0, if_else(psceda == 1, 1, f1)),
         f2 = if_else(pscedb == 2, 0, if_else(pscedb == 1, 1, f2)),
         f3 = if_else(pscedc == 2, 0, if_else(pscedc == 1, 1, f3)),
         f4 = if_else(pscedd == 1, 0, if_else(pscedd == 2, 1, f4)),
         f5 = if_else(pscede == 2, 0, if_else(pscede == 1, 1, f5)),
         f6 = if_else(pscedf == 1, 0, if_else(pscedf == 2, 1, f6)),
         f7 = if_else(pscedg == 2, 0, if_else(pscedg == 1, 1, f7)),
         f8 = if_else(pscedh == 2, 0, if_else(pscedh == 1, 1, f8)))

# Sum the f1 to f8 values (coded 0/1) for CESD score: (range: 0 to 8)
df1 <- df1 %>% mutate(cesd_w9 = f1+f2+f3+f4+f5+f6+f7+f8)
table(df1$cesd_w9)

# set score to missing for those with at least 1 missing item.
df1 <- df1 %>%
  mutate(cesd_w9 = if_else(s != 0, -9, cesd_w9))
df1$cesd_w9[df1$cesd_w9 == -9] <- NA
table(df1$cesd_w9,exclude=NULL) # 34 missing: we will impute those using mice.

# Drop columns to facilitate mice
df1 <- df1 %>%
  select(-psceda,-pscedb,-pscedc,-pscedd,-pscede,-pscedf,-pscedg,-pscedh,-finstat,-GOR,-s,-cflisen,-cflisd,-f1,-f2,
         -f3,-f4,-f5,-f6,-f7,-f8,-cfw9,-r9demene,-r9alzhe)
nrow(df1)
ncol(df1)

#impute the missing w9 variables here (depression; Cf status and region)
table(df1$cesd_w9,exclude=NULL)
table(df1$region,exclude=NULL)
table(df1$cf_g3w9,exclude=NULL)

w9data_imp <- complete(mice(df1,    # Predictive mean matching imputation (1 dataset)
                          m = 1,
            method = "pmm"))

#now create binary depression score (0-3 not depressed; 4+depressed)
w9data_imp <- w9data_imp %>%
  mutate(cesdbinw9 = ifelse(cesd_w9>=0 & cesd_w9<=3,0, 
                            ifelse(cesd_w9>=4 & cesd_w9<=8,1,-2)))
table(w9data_imp$cesdbinw9,exclude=NULL)  # different result to Stata as expect.    
                            
# Sort by idauniq for later merging.
w9data_imp <- w9data_imp %>% arrange(idauniq)
# Save the data for use below.
readr::write_rds(w9data_imp,"C:/Users/rmjdshc/OneDrive - University College London/Temp/w9data_imp.rds") 


##########################
#Wave 1 COVID: (SN 8688).
##########################

# Load data
covidw1 <- read_dta("C:/Users/rmjdshc/OneDrive - University College London/ELSA/UKDS/Covid-8688/stata/elsa_covid_w1_eulv2.dta")
# remove capital letters from column names.
names(covidw1) <- tolower(names(covidw1))

# Keep only the necessary columns
covidw1 <- covidw1 %>% select(idauniq,finstat,cvmhced_cvmhced1_q,cvmhced_cvmhced2_q, cvmhced_cvmhced3_q, 
                              cvmhced_cvmhced4_q, cvmhced_cvmhced5_q, cvmhced_cvmhced6_q, cvmhced_cvmhced7_q,
                              cvmhced_cvmhced8_q_final) 


#Keep core members only (decided here to use Cohorts 1, 3 and 4).
# Filter rows based on finstat (core members only: cohorts 1,3,4)
covidw1 <- covidw1 %>% filter(finstat %in% c(1, 7, 14))
nrow(covidw1)   ## 4558

#CESD at COVID wave 1.
#Error in survey administration:
#Eight depression item not asked to around 75% of respondents (CvMhCed_CvMhCed8_q_final: felt sad): 
#making imputation necessary rather than optional.

#rename columns to use code developed above.
covidw1 <- covidw1 %>%
  rename(psceda = cvmhced_cvmhced1_q,
         pscedb = cvmhced_cvmhced2_q,
         pscedc = cvmhced_cvmhced3_q, 
         pscedd = cvmhced_cvmhced4_q,
         pscede = cvmhced_cvmhced5_q, 
         pscedf = cvmhced_cvmhced6_q, 
         pscedh = cvmhced_cvmhced7_q,          
         pscedg = cvmhced_cvmhced8_q_final)
# this item has a lot of missing values (special code:-6)
table(covidw1$pscedg)

#Similar code as used above.
#Handle missing values (-9 to -1) by setting them to NA
covidw1 <- covidw1 %>% mutate(across(starts_with("psced"), ~ na_if(., -9))) %>% 
  mutate(across(starts_with("psced"), ~ na_if(., -1))) %>% 
  mutate(across(starts_with("psced"), ~ na_if(., -8))) %>%
  mutate(across(starts_with("psced"), ~ na_if(., -6)))

# Count the number of missing values for each row.
# Note the large number missing 1 item.
covidw1 <- covidw1 %>% mutate(s = rowSums(is.na(select(., starts_with("psced")))))
table(covidw1$s)

# Filter rows based on the number of missing values
covidw1 <- covidw1 %>% filter(s %in% c(0, 1, 2))
nrow(covidw1)  # 4535

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
covidw1$cesd_covidw1[covidw1$cesd_covidw1 == -9] <- NA
table(covidw1$cesd_covidw1,exclude=NULL)

# Drop columns to facilitate mice
covidw1 <- covidw1 %>%
  select(-psceda,-pscedb,-pscedc,-pscedd,-pscede,-pscedf,-pscedg,-pscedh,-finstat,-f1,-f2,
         -f3,-f4,-f5,-f6,-f7,-f8,-s)
nrow(covidw1)
ncol(covidw1)

#impute the missing here using mice (depression; Cf status and region)
table(covidw1$cesd_covidw1,exclude=NULL)
covid1data_imp <- complete(mice(covidw1,    # Predictive mean matching imputation
                            m = 1,
                            method = "pmm"))
table(covid1data_imp$cesd_covidw1,exclude=NULL)

#now create binary depression score (0-3 not depressed; 4+depressed)
covid1data_imp <- covid1data_imp %>%
  mutate(cesdbin_covid1 = ifelse(cesd_covidw1>=0 & cesd_covidw1<=3,0, 
                            ifelse(cesd_covidw1>=4 & cesd_covidw1<=8,1,-2)))
table(covid1data_imp$cesdbin_covid1,exclude=NULL) # different to Stata as expect

# Sort by idauniq for later merging.
covid1data_imp <- covid1data_imp %>% arrange(idauniq)

# Save the data for use below.
readr::write_rds(covid1data_imp,"C:/Users/rmjdshc/OneDrive - University College London/Temp/covid1data_imp.rds")  


##############################################################################
#COVID Wave 2
##############################################################################

# Load data
covidw2 <- read_dta("C:/Users/rmjdshc/OneDrive - University College London/ELSA/UKDS/Covid-8688/stata/elsa_covid_w2_eulv2.dta")
# remove capital letters from column names.
names(covidw2) <- tolower(names(covidw2))

# Keep only the necessary columns
covidw2 <- covidw2 %>% select(idauniq,finstat_w1,
                              cvmhced_cvmhced1_q,cvmhced_cvmhced2_q, cvmhced_cvmhced3_q, 
                              cvmhced_cvmhced4_q, cvmhced_cvmhced5_q, cvmhced_cvmhced6_q, cvmhced_cvmhced7_q,
                              cvmhced_cvmhced8_q)

#Keep core members only (decided here to use Cohorts 1, 3 and 4).
# Filter rows based on finstat (core members only: cohorts 1,3,4)
covidw2 <- covidw2 %>% filter(finstat_w1 %in% c(1, 7, 14))
nrow(covidw2)   # 4365

#CESD at COVID wave 2.
# rename columns to use code developed above.
covidw2 <- covidw2 %>%
  rename(psceda = cvmhced_cvmhced1_q,
         pscedb = cvmhced_cvmhced2_q,
         pscedc = cvmhced_cvmhced3_q, 
         pscedd = cvmhced_cvmhced4_q,
         pscede = cvmhced_cvmhced5_q, 
         pscedf = cvmhced_cvmhced6_q, 
         pscedh = cvmhced_cvmhced7_q,          
         pscedg = cvmhced_cvmhced8_q)

#Similar code as used above.
#Handle missing values (-9 to -1) by setting them to NA
covidw2 <- covidw2 %>% mutate(across(starts_with("psced"), ~ na_if(., -9))) %>% 
  mutate(across(starts_with("psced"), ~ na_if(., -1))) %>% 
  mutate(across(starts_with("psced"), ~ na_if(., -8))) %>%
  mutate(across(starts_with("psced"), ~ na_if(., -6)))

# Count the number of missing values for each row
covidw2 <- covidw2 %>% mutate(s = rowSums(is.na(select(., starts_with("psced")))))
table(covidw2$s)

# Filter rows based on the number of missing values
covidw2 <- covidw2 %>% filter(s %in% c(0, 1, 2))
nrow(covidw2)   # 4351

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
covidw2$cesd_covidw2[covidw2$cesd_covidw2 == -9] <- NA
table(covidw2$cesd_covidw2,exclude=NULL)
# impute the 56 below using mice.

# Drop columns to facilitate mice
covidw2 <- covidw2 %>%
  select(-psceda,-pscedb,-pscedc,-pscedd,-pscede,-pscedf,-pscedg,-pscedh,-finstat_w1,-f1,-f2,
         -f3,-f4,-f5,-f6,-f7,-f8,-s)
nrow(covidw2)
ncol(covidw2)

#impute the missing here (depression)
table(covidw2$cesd_covidw2,exclude=NULL)
covid2data_imp <- complete(mice(covidw2,    # Predictive mean matching imputation
                                m = 1,
                                method = "pmm"))
table(covid2data_imp$cesd_covidw2,exclude=NULL)

#now create binary depression score (0-3 not depressed; 4+depressed)
covid2data_imp <- covid2data_imp %>%
  mutate(cesdbin_covid2 = ifelse(cesd_covidw2>=0 & cesd_covidw2<=3,0, 
                                ifelse(cesd_covidw2>=4 & cesd_covidw2<=8,1,-2)))
table(covid2data_imp$cesdbin_covid2,exclude=NULL) # different to stata as expect

# Sort by idauniq for later merging.
covid2data_imp <- covid2data_imp %>% arrange(idauniq)

# Save the data for use below.
readr::write_rds(covid2data_imp,"C:/Users/rmjdshc/OneDrive - University College London/Temp/covid2data_imp.rds") 

############################
#Put the datasets together
############################

rm()
w9data_imp<-readr::read_rds("C:/Users/rmjdshc/OneDrive - University College London/Temp/w9data_imp.rds") 
covid1data_imp<-readr::read_rds("C:/Users/rmjdshc/OneDrive - University College London/Temp/covid1data_imp.rds")
covid2data_imp<-readr::read_rds("C:/Users/rmjdshc/OneDrive - University College London/Temp/covid2data_imp.rds")

#use data tables to facilitate merging.
#using left-join: add no more observations to those in ELSA wave 9.
setDT(w9data_imp)
setDT(covid1data_imp)
setDT(covid2data_imp)

# First merge (w9 and covid1)
df1 <- merge(w9data_imp,covid1data_imp, by = "idauniq", all.x = TRUE, suffixes = c("", ".y"))
nrow(df1)

# Second merge (bring in covidw2)
df1 <- merge(df1, covid2data_imp, by = "idauniq", all.x = TRUE, suffixes = c("", ".y"))
nrow(df1)
ncol(df1)
# 5377 observations (13 variables).
names(df1)

#rename variables to facilitate reshape data from wide to long format.
df1 <- df1 %>%
  rename(cesd0 = cesd_w9,
         cesd1 = cesd_covidw1,
         cesd2 = cesd_covidw2, 
         cesdbin0 = cesdbinw9,
         cesdbin1 = cesdbin_covid1, 
         cesdbin2 = cesdbin_covid2)

#focus on continuous outcome: so drop binary variables.
df1 <- df1 %>%
  select(-cesdbin0,-cesdbin1,-cesdbin2)

# Reshape from wide to long format
df_long <- df1 %>%
  pivot_longer(
    cols = starts_with("cesd"),  # Select columns to reshape
    names_to = "wave",           # New variable for the original column names
    values_to = "cesd"           # Variable for the values
  ) %>%
  mutate(
    wave = sub("cesd", "", wave)  # Optionally, remove prefix from 'wave' variable
  )

#drop rows with missing CESD score (i.e at wave 9 but not covid substudy)
df_long <- df_long[!is.na(df_long$cesd), ]
nrow(df_long)   # 13810 observations to use in mixed-effects model 

# Fit the mixed-effects model
# Outcome : continuous (cesd)
# Key predictors: time (wave) & CF status.
# Confounders: region; ethnicity; couple.

model <- lmer(
  cesd ~ as.factor(wave) * as.factor(cf_g3w9) + region + fqethnmr + as.factor(couple) + 
    (1 | idauniq), 
  data = df_long,
  REML = FALSE              # ML estimation
)

summary(model)
#nice to use an R equivalent of marginsplot.



######################################################.
#logistic mixed effects model
#see:
#https://stats.oarc.ucla.edu/r/dae/mixed-effects-logistic-regression/#:~:text=Mixed%20effects%20logistic%20regression%20is,both%20fixed%20and%20random%20effects.
######################################################.

#Put the datasets together

rm()
w9data_imp<-readr::read_rds("C:/Users/rmjdshc/OneDrive - University College London/Temp/w9data_imp.rds") 
covid1data_imp<-readr::read_rds("C:/Users/rmjdshc/OneDrive - University College London/Temp/covid1data_imp.rds")
covid2data_imp<-readr::read_rds("C:/Users/rmjdshc/OneDrive - University College London/Temp/covid2data_imp.rds")

#use data tables to facilitate merging.
#using left-join: add no more observations to those in ELSA wave 9.
setDT(w9data_imp)
setDT(covid1data_imp)
setDT(covid2data_imp)

# First merge (w9 and covid1)
df1 <- merge(w9data_imp,covid1data_imp, by = "idauniq", all.x = TRUE, suffixes = c("", ".y"))
nrow(df1)

# Second merge (bring in covidw2)
df1 <- merge(df1, covid2data_imp, by = "idauniq", all.x = TRUE, suffixes = c("", ".y"))
nrow(df1)
ncol(df1)
# 5377 observations (13 variables).
names(df1)

#rename variables to facilitate reshape data from wide to long format.
df1 <- df1 %>%
  rename(cesd0 = cesd_w9,
         cesd1 = cesd_covidw1,
         cesd2 = cesd_covidw2, 
         cesdbin0 = cesdbinw9,
         cesdbin1 = cesdbin_covid1, 
         cesdbin2 = cesdbin_covid2)

#focus now on binary outcomes
df1 <- df1 %>%
  select(-cesd0,-cesd1,-cesd2)

# Reshape from wide to long format
df_long <- df1 %>%
  pivot_longer(
    cols = starts_with("cesd"),  # Select columns to reshape
    names_to = "wave",           # New variable for the original column names
    values_to = "cesd"           # Variable for the values
  ) %>%
  mutate(
    wave = sub("cesd", "", wave)  # Optionally, remove prefix from 'wave' variable
  )

#drop rows with missing CESD score (i.e at wave 9 but not covid substudy)
df_long <- df_long[!is.na(df_long$cesd), ]
nrow(df_long)   # 13810 observations to use in mixed-effects model 

# Fit the logistic mixed-effects model
# Outcome : continuous (cesd)
# Key predictors: time (wave) & CF status.
# Confounders: region; ethnicity; couple.


# estimate the model and store results in m
m <- glmer(cesd ~ wave * cf_g3w9  + region + fqethnmr + as.factor(couple) + 
             (1 | idauniq), data = df_long, family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)

# print the mod results without correlations among fixed effects
print(m, corr = FALSE)
se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 *
                se))
exp(tab)  # odds ratios


##################
#FINISHED.
##################























