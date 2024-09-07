########################################
#Descriptive estimates using wave10 data.
#########################################.

library(dplyr)
library(tidyr)
library(haven)
library(data.table) 
library(survey)
library(srvyr)
library(tidyverse)


#############
#COVID Wave 1
##############

# Load covid wave 1 data
covidw1 <- read_dta("C:/Users/rmjdshc/OneDrive - University College London/ELSA/UKDS/Covid-8688/stata/elsa_covid_w1_eulv2.dta")
# remove capital letters from column names.
names(covidw1) <- tolower(names(covidw1))

# Keep only the necessary columns
covidw1 <- covidw1 %>% select(idauniq,cvtestb) 
table(covidw1$cvtestb)  ## result of test
covidw1 <- covidw1 %>%
rename(cvTest1 = cvtestb) 
# Sort by idauniq
covidw1 <- covidw1 %>% arrange(idauniq)
# Save the data 
readr::write_rds(covidw1,"C:/Users/rmjdshc/OneDrive - University College London/temp_covidw1.rds") 

# Load covid wave 2 data
covidw2 <- read_dta("C:/Users/rmjdshc/OneDrive - University College London/ELSA/UKDS/Covid-8688/stata/elsa_covid_w2_eulv2.dta")
# remove capital letters from column names.
names(covidw2) <- tolower(names(covidw2))

# Keep only the necessary columns
covidw2 <- covidw2 %>% select(idauniq,cvtestb) 
table(covidw2$cvtestb)  ## result of test
covidw2 <- covidw2 %>%
  rename(cvTest2 = cvtestb) 
# Sort by idauniq
covidw2 <- covidw2 %>% arrange(idauniq)
# Save the data 
readr::write_rds(covidw2,"C:/Users/rmjdshc/OneDrive - University College London/temp_covidw2.rds") 


# Load wave 10 data
elsaw10 <- read_dta("C:/Users/rmjdshc/OneDrive - University College London/ELSA/UKDS/ELSA-5050/stata/wave_10_elsa_data_eul_v3.dta")
# remove capital letters from column names.
names(elsaw10) <- tolower(names(elsaw10))

# Keep only the necessary columns
elsaw10 <- elsaw10 %>% select(idauniq,mode,askpx,w10xwgt,corepartner,
                              hecvtsta,hecvtstb,hecvconf,hecvsym,hecvsyme,
                              hecvlong,hecvlconfa,hecvlconcg,hecvlconsb,hecvlconot,hecvllim,hecvbvac,hecvvac)


#set as datatables to facilitate merging.
setDT(elsaw10)
setDT(covidw1)
setDT(covidw2)
#do not add to the observations to the wave 10 data (n=7589)

# First merge (wave10 and covidw1)
df1 <- merge(elsaw10,covidw1, by = "idauniq", all.x = TRUE, suffixes = c("", ".y"))
nrow(df1)

# Second merge (bring in covidw2)
df1 <- merge(df1, covidw2, by = "idauniq", all.x = TRUE, suffixes = c("", ".y"))
nrow(df1)
ncol(df1)
# 7589 observations 
names(df1)
table(df1$mode)  # mode

#Analytical sample (n=6041): 
#(i) non-proxies; 
#(ii) cross-sectional weight (ELSA core members aged 50+.

df1 <- df1 %>%
   filter(askpx != 1) %>%
  filter(w10xwgt > 0, !is.na(w10xwgt))  
nrow(df1)  ## n=6041
table(df1$corepartner)  ## all core members

#Wave 10 variables:
#hecvtsta                 // any test
#hecvtstb                 // result of test
#hecvconf                 // whether confirms previous
#hecvsym                  // any symptoms when have COVID
#hecvsyme                 // respondent experience
#hecvlong                 // any long-standing illness/disability
#hecvlconfa-hecvlconot   // new diagnosed conditions linked to COVID-19
#hecvllim                // whether limits activities in any way
#hecvvac                 // booster vaccine 

                 
#1. % having a COVID test (Denominator: all)
table(df1$hecvtsta)  ## 56 not applicable (positive test in substudy.
# Clone the variable 'hecvtsta' to 'anytest'
df1$anytest <- df1$hecvtsta
# Replace values of 'anytest' with 1 where either 'cvTest1' or 'cvTest2' equals 1
df1$anytest[df1$cvTest1 == 1 | df1$cvTest2 == 1] <- 1
df1$anytest[df1$anytest %in% c(-9,-8,-1) ] <- NA
#convert to 0/1 for means.
df1 <- df1 %>%
  mutate(anytest = case_when(
    anytest == 2 ~ 0,
   anytest == 1 ~ 1,
    TRUE ~ anytest
  ))
table(df1$anytest)

#2.% Having a +ve test (Denominator: those having a COVID test)
table(df1$hecvtstb)  ##result of test

# Clone the variable 
df1$result <- df1$hecvtstb
df1$result[df1$cvTest1 == 1 | df1$cvTest2 == 1] <- 1 ## positive test in substudy
df1$result[df1$result %in% c(-9,-8,-1,3,4) ] <- NA
table(df1$result)
#convert to 0/1 for means.
df1 <- df1 %>%
  mutate(result = case_when(
    result == 2 ~ 0,
    result == 1 ~ 1,
    TRUE ~ result
  ))
table(df1$result)

#3. % having any symptoms (Denominator: those having a +ve test)
#IF confirmed that had COVID in COVID surveys or had a positive test since│ [HeCVtstb = 1 or HECvconf=1]

table(df1$hecvsym) ## any symptoms when have COVID 
# Clone the variable 
df1$symptoms <- df1$hecvsym
df1$symptoms[df1$symptoms %in% c(-9,-8,-1) ] <- NA
table(df1$symptoms)
#convert to 0/1 for means.
df1 <- df1 %>%
  mutate(symptoms = case_when(
    symptoms == 2 ~ 0,
    symptoms == 1 ~ 1,
    TRUE ~ symptoms
  ))
table(df1$symptoms)

#4. COVID-19 symptoms: lasted at least 4 weeks
table(df1$hecvsyme) ## experienced any symptoms when have COVID 
# Clone the variable 
df1$exp_symptoms <- df1$hecvsyme
df1$exp_symptoms[df1$exp_symptoms %in% c(-9,-8,-1,8) ] <- NA    ## spontaneous (n=15) set to missing
table(df1$exp_symptoms)
#convert to 0/1 for means (1= lasted 4 weeks or more)
df1 <- df1 %>%
  mutate(exp_symptoms = case_when(
    exp_symptoms %in% c(1,2,3,4,5) ~ 0,
    exp_symptoms %in% c(6,7) ~ 1,
    TRUE ~ exp_symptoms
  ))
table(df1$exp_symptoms)

#5. Diagnosed long-standing illness caused by COVID
table(df1$hecvlong) ## any long-standing illness/disability due to covid
# Clone the variable 
df1$covid_LSI <- df1$hecvlong
df1$covid_LSI[df1$covid_LSI %in% c(-8) ] <- NA 
table(df1$covid_LSI)
#convert to 0/1 for means 
df1 <- df1 %>%
  mutate(covid_LSI = case_when(
    covid_LSI %in% c(2) ~ 0,
    covid_LSI %in% c(1) ~ 1,
    TRUE ~ covid_LSI
  ))
table(df1$covid_LSI)

#6.New diagnosed illness/condition linked to COVID.
#base is hecvlong==1 (n=95)
table(df1$hecvlconfa)        ## fatigue
table(df1$hecvlconcg)        ## cough
table(df1$hecvlconsb)       ## shortness of breath
#all we need to is move -1 to missing

# List of variables to process
variables <- c("hecvlconfa", "hecvlconcg", "hecvlconsb")

# Use mutate with across to replace -1 with NA
df1 <- df1 %>%
  mutate(across(all_of(variables), ~ na_if(.x, -1)))
table(df1$hecvlconfa,exclude=NULL)       
table(df1$hecvlconcg,exclude=NULL)       
table(df1$hecvlconsb,exclude=NULL)  

#7. Received booster vaccine
table(df1$hecvbvac)  #131 not applicable for hecvbvac  (not vaccinated): set these to not having received a booster vaccine
table(df1$hecvvac) 

df1 <- df1 %>%
  # Step 1: Generate the variable 'booster' with initial value -2
  mutate(booster = -2) %>%
  # Step 2: Replace 'booster' with NA where 'hecvbvac' is -8 or -9, or 'hecvvac' is -9
  mutate(booster = case_when(
    hecvbvac %in% c(-8, -9) | hecvvac == -9 ~ NA_real_,          ## set to missing
    hecvvac %in% c(2, 3, 4) ~ 0,                                 ## not vaccinated
    hecvbvac == 2 ~ 0,                                           ## vaccinated, but no booster
    hecvbvac == 1 ~ 1,                                           ## vaccinated, received booster
    TRUE ~ booster
  ))
table(df1$booster)


################################################
#set the survey design.
#after all the variables have been derived
################################################

survey_design <- df1 %>%
  srvyr::as_survey(ids=idauniq,
                   weights = w10xwgt)
summary(survey_design)

#any test (89.9%).
survey_design %>%
  summarize(Result = srvyr::survey_mean(anytest, 
                                        na.rm = TRUE, 
                                        proportions = TRUE, 
                                        vartype = "ci"))
#+test (37.5%).
survey_design %>%
  summarize(Result = srvyr::survey_mean(result, 
                                        na.rm = TRUE, 
                                        proportions = TRUE, 
                                        vartype = "ci"))

#symptoms (85.3%).
survey_design %>%
  summarize(Result = srvyr::survey_mean(symptoms, 
                                        na.rm = TRUE, 
                                        proportions = TRUE, 
                                        vartype = "ci"))

#symptoms 4 weeks+ (7.1%).
survey_design %>%
  summarize(Result = srvyr::survey_mean(exp_symptoms, 
                                        na.rm = TRUE, 
                                        proportions = TRUE, 
                                        vartype = "ci"))


#Diagnosed long-standing illness caused by COVID (2.1%).
survey_design %>%
  summarize(Result = srvyr::survey_mean(covid_LSI, 
                                        na.rm = TRUE, 
                                        proportions = TRUE, 
                                        vartype = "ci"))


#new diagnosed/conditions linked to covid: 
#fatigue (52.7)
survey_design %>%
  summarize(Result = srvyr::survey_mean(hecvlconfa, 
                                        na.rm = TRUE, 
                                        proportions = TRUE, 
                                        vartype = "ci"))
#cough (26.7)
survey_design %>%
  summarize(Result = srvyr::survey_mean(hecvlconcg, 
                                        na.rm = TRUE, 
                                        proportions = TRUE, 
                                        vartype = "ci"))

#shortness of breath (55.7)
survey_design %>%
  summarize(Result = srvyr::survey_mean(hecvlconsb, 
                                        na.rm = TRUE, 
                                        proportions = TRUE, 
                                        vartype = "ci"))



# received a booster vaccine (90.8)
survey_design %>%
  summarize(Result = srvyr::survey_mean(booster, 
                                        na.rm = TRUE, 
                                        proportions = TRUE, 
                                        vartype = "ci"))
                                                                          
                             




























