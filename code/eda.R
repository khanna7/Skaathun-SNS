
rm(list=ls())

# Libraries and packages ----------------

library(tidyverse)
library(readxl)


# Read data ----------------

sns <- read_excel("../data/SNS_Complete_Demos_Risk.xlsx")
View(sns)
dim(sns)

n <- nrow(sns)


# Demographic characteristics ----------------

#age
year_of_survey <- 2021
age <- year_of_survey - sns$dob_year
summary(age)
sd(age)
sns$age <- age

#race
sum(sns$race___1)#white
sum(sns$race___7)#Black
sum(sns$race___4)#Asian
sum_other <- nrow(sns)-sum(sns$race___1)+sum(sns$race___7)+sum(sns$race___4)
sum_other

#ethnicity (hisp vs not)
table(sns$hispanic, exclude = NULL) #1=hispanic; 2=non-hispanic; 4=not want to report

#sex at  birth
table(sns$sex_birth, exclude = NULL)

#gender identity
table(sns$gender, exclude = NULL)
  # 1 Male
  # 2 Female
  # 3 Trans Male
  # 4 Trans Female
  # 5 Do not identify as female, male, or transgender
  # 6 Non-binary /genderqueer /gender 

#homeless
table(sns$homeless, exclude = NULL) #1=yes, 0=no

#insurance
sum(sns$insurance___1) #no insurance
sum(sns$insurance___2) #private insurance
sum(sns$insurance___3) #medicaid
sum(sns$insurance___4) #medicare
sum(sns$insurance___5)#military
sum(sns$insurance___6) #other
sum(sns$insurance___7) #don't know
sum(sns$insurance___8) #declines to answer

# sns svy compleete
table(sns$svy_complt, exclude = NULL) #0=no, 1=yes


# self-reports
table(sns$std_3mo___1, exclude = NULL) #....

# test results 
# chlamydia 1st and 2nd test (combine throat, rectal, urine)
table(sns$rectal_ct1, exclude = NULL) #1st chlamydua test
  # 0 Not Detected
  # 1 Detected 
  # 3 Invalid
table(sns$rectal_ct2, exclude = NULL) #1st chlamydua test
# 0 Not Detected
# 1 Detected 
# 3 Invalid

# Outcomes GC/CT (combine throat, rectal, urine) ----------------
# See here: https://docs.google.com/document/d/18yTkd10ehanokf_pQ798YjQy-kc19mJbWdeLHDsdzg4/edit#bookmark=id.lzk9r1ehjhh6

## CT
table(sns$rectal_ct1, exclude = NULL)
table(sns$rectal_ct2, exclude = NULL)
table(sns$throat_ct1, exclude = NULL)
table(sns$throat_ct2, exclude = NULL)
table(sns$urine_ct1, exclude = NULL)
table(sns$urine_ct2, exclude = NULL)

rectal_ct <- 
  union(
    which(sns$rectal_ct1 == 1), which(sns$rectal_ct2 == 1)
  )

throat_ct <- 
  union(
    which(sns$throat_ct1 == 1), which(sns$throat_ct2 == 1)   
  )

urine_ct <- 
  union(
    which(sns$urine_ct1 == 1), which(sns$urine_ct2 == 1)   
  )

ct <- unique(c(rectal_ct, throat_ct, urine_ct))
length(ct)

## GC
table(sns$rectal_gc1, exclude = NULL)
table(sns$rectal_gc2, exclude = NULL)
table(sns$throat_gc1, exclude = NULL)
table(sns$throat_gc2, exclude = NULL)
table(sns$urine_gc1, exclude = NULL)
table(sns$urine_gc2, exclude = NULL)

rectal_gc <- 
  union(
    which(sns$rectal_gc1 == 1), which(sns$rectal_gc2 == 1)
  )

throat_gc <- 
  union(
    which(sns$throat_gc1 == 1), which(sns$throat_gc2 == 1)   
  )

urine_gc <- 
  union(
    which(sns$urine_gc1 == 1), which(sns$urine_gc2 == 1)   
  )

gc <- unique(c(rectal_gc, throat_gc, urine_gc))
length(gc)

gc_or_ct <- sort(unique(union(gc, ct)))
length(gc_or_ct)

gc_and_ct <- sort(unique(intersect(gc, ct)))
length(gc_and_ct)


# Covariates on network members ----------------
