#--------------------------
# Analysis to produce Output 2 for NDL with Liverpool CCG Linked data
# Pre-existing conditions of the Shileded Patients
# From the shielded patient list (SPL) linked with SUS tables
# To the aggregated outputs provided in file: Liverpool Output 2 - v2.xlsx"      
#--------------------------
# Author: Roberta Piroddi
# Date: January 2021
#--------------------------

library(data.table)
library(stringr)
library(dtplyr)
library(dplyr)
library(tidyr)


source("setup2.R")
# this file defines the path to the data as datapath <- "..."
# and the input data file name as datafile <- "*.csv"


dat <-fread(normalizePath(file.path(datapath,datafile)))

names(dat)[68] <- "# cardiac arrhythmias episodes" # correct spelling mistake


#----------------------------------------------------------------------------
cond_lu <- fread("lookup_conditions.csv") # as defined in metadata for output one

cond_lu[, description:= str_replace_all(description,"[^[:alnum:]]","")]
#------------------------------------------------------------------------------

#----------------------------------------------
# the input data dat has this structure
# one row per patient
# columns 6:35 contain an indicator variable
# which had value (1/0) whether the patient has one of 30 conditions as in lookup table
# columns 36:65 contain an integer
# with the van walraven score associated to each of the 30 conditions
# columns 66:95 contain an integer
# the number of admissions for patient and condition in the 2 years 2018-2019



dat1 <- dat[,6:35]

pat1 <- colSums(dat1)

pat.tab1 <- data.table(description = names(pat1), number.patients = pat1)

pat.tab1[, description:= str_replace_all(description,"[^[:alnum:]]","")]


dat3 <- dat[,36:65]

dat2 <- dat[,66:95]
adm1 <- colSums(dat2)


adm.tab1 <- data.table(description = names(adm1), number.admissions = adm1)

adm.tab1[, description:= str_replace_all(description,"episodes","")]

adm.tab1[, description:= str_replace_all(description,"[^[:alnum:]]","")]

table2.1.1 <- adm.tab1

table2.1.1 <- table2.1.1[pat.tab1, on='description']

table2.1 <- table2.1.1[cond_lu, on='description']

table2.1[, description:=NULL]

setcolorder(table2.1,c("condition","number.patients","number.admissions"))

table2.1$total.patients = nrow(dat)

fwrite(table2.1,normalizePath(file.path(datapath,"output2_tab2-1.csv")))
# this is the table 2-1 as it appears in the Output 2 template

#-----------------------------------------------------------------------

conditions_aggr <- rowSums(dat1)

dat$conditions_total <- conditions_aggr


aggrcond <- data.frame( conditions_sums = conditions_aggr)

aggrcond<- aggrcond %>% 
       mutate(
         conditions_aggregated = case_when(
           conditions_sums == 0 ~ "0",
           conditions_sums == 1 ~ "1",
           conditions_sums > 1 ~ "2 or more",
           TRUE ~ NA_character_
         )
       )


dat$conditions_aggregcat <- aggrcond$conditions_aggregated

tab2.2 <- as.data.table(table(aggrcond$conditions_aggregated))


setnames(tab2.2, c("V1","N"), c("conditions_aggregated","number.patients"))

fwrite(tab2.2,normalizePath(file.path(datapath,"output2_tab2-2.csv")))

#-------------------------------------------------------------------------------------

vwscores <- rowSums(dat3)

vwscoresum <- data.frame( vw_sums = vwscores)

vwscoresum<- vwscoresum %>% 
  mutate(
      vws = case_when(
      vw_sums < 0 ~ "<0",
      vw_sums == 0 ~ "0",
      vw_sums > 0 & vw_sums <5 ~ as.character("1 to 4"),
      vw_sums > 4 ~ ">= 5",
      TRUE ~ NA_character_
    )
  )


dat$windex_vw <- vwscoresum$vws

tab2.3 <- as.data.table(table(vwscoresum$vws))

setnames(tab2.3, c("V1","N"), c("van_walraven_score","number.patients"))

fwrite(tab2.3,normalizePath(file.path(datapath,"output2_tab2-3.csv")))

#-------------------------------------------------------------------------

dat1f <- dat[Gender=="Female", 6:35]
dat1m <- dat[Gender=="Male", 6:35]
dat1xu <- dat[!(Gender=="Male") & !(Gender=="Female"), 6:35]

dat3f <- dat[Gender=="Female",36:65]
dat3m <- dat[Gender=="Male",36:65]
dat3xu <- dat[!(Gender=="Female") & !(Gender=="Male"),36:65]

dat2f <- dat[Gender=="Female",66:95]
dat2m <- dat[Gender=="Male",66:95]
dat2xu <- dat[!(Gender=="Female") & !(Gender=="Male"),66:95]


pat1f <- colSums(dat1f)
pat.tab1f <- data.table(description = names(pat1f), number.patients.sex.female = pat1f)
pat.tab1f[, description:= str_replace_all(description,"[^[:alnum:]]","")]


pat1m <- colSums(dat1m)
pat.tab1m <- data.table(description = names(pat1m), number.patients.sex.male = pat1m)
pat.tab1m[, description:= str_replace_all(description,"[^[:alnum:]]","")]


pat1xu <- colSums(dat1xu)
pat.tab1xu <- data.table(description = names(pat1xu), number.patients.sex.unknown = pat1xu)
pat.tab1xu[, description:= str_replace_all(description,"[^[:alnum:]]","")]




adm1f <- colSums(dat2f)
adm1m <- colSums(dat2m)
adm1xu <- colSums(dat2xu)




adm.tab1f <- data.table(description = names(adm1f), number.admissions.sex.female = adm1f)
adm.tab1m <- data.table(description = names(adm1m), number.admissions.sex.male = adm1m)
adm.tab1xu <- data.table(description = names(adm1xu), number.admissions.sex.unknown = adm1xu)



adm.tab1f[, description:= str_replace_all(description,"episodes","")]
adm.tab1f[, description:= str_replace_all(description,"[^[:alnum:]]","")]

adm.tab1m[, description:= str_replace_all(description,"episodes","")]
adm.tab1m[, description:= str_replace_all(description,"[^[:alnum:]]","")]

adm.tab1xu[, description:= str_replace_all(description,"episodes","")]
adm.tab1xu[, description:= str_replace_all(description,"[^[:alnum:]]","")]





table2.2.s <- adm.tab1f

table2.2.s <- table2.2.s[pat.tab1f, on='description']

table2.2.s <- table2.2.s[pat.tab1m, on='description']

table2.2.s <- table2.2.s[pat.tab1xu, on='description']


table2.2.s <- table2.2.s[adm.tab1m, on='description']

table2.2.s <- table2.2.s[adm.tab1xu, on='description']


table2.2.sex <- table2.2.s[cond_lu, on='description']


table2.2.sex[, description:=NULL]

setcolorder(table2.2.sex,c("condition","number.patients.sex.female","number.patients.sex.male","number.patients.sex.unknown","number.admissions.sex.female","number.admissions.sex.male","number.admissions.sex.unknown"))

table2.2.sex$total.patients.sex.female = nrow(dat1f)
table2.2.sex$total.patients.sex.male = nrow(dat1m)
table2.2.sex$total.patients.sex.unknown = nrow(dat1xu)


fwrite(table2.2.sex,normalizePath(file.path(datapath,"output2_tab2-1-sex.csv")))


#------------------------------------------------------------------------------------
# Aggregation by age groups

dat <- dat %>% mutate(
  age_group = case_when(
    Age_at_01_03_2020 <30 ~ "<30",
    Age_at_01_03_2020 >29 & Age_at_01_03_2020 <50 ~ "30 to 49",
    Age_at_01_03_2020 > 49 & Age_at_01_03_2020 <70 ~ "50 to 69",
    Age_at_01_03_2020 >69 ~ "70 or older",
    TRUE ~ "Unknown"
  )
)


dat1a1 <- dat[Age_at_01_03_2020 <30, 6:35]
dat1a2 <- dat[age_group == "30 to 49", 6:35]
dat1a3 <- dat[age_group == "50 to 69", 6:35]
dat1a4 <- dat[age_group == "70 or older", 6:35]


dat2a1 <- dat[age_group == "<30", 66:95]
dat2a2 <- dat[age_group == "30 to 49", 66:95]
dat2a3 <- dat[age_group == "50 to 69", 66:95]
dat2a4 <- dat[age_group == "70 or older", 66:95]

datt_cond <- gather(dat, condition, condition_flag, 6:35, factor_key = TRUE)

pat1a1 <- colSums(dat1a1)
pat1a2 <- colSums(dat1a2)
pat1a3 <- colSums(dat1a3)
pat1a4 <- colSums(dat1a4)


pat.tab1a1 <- data.table(description = names(pat1a1), number.patients.age.younger30 = pat1a1)
pat.tab1a2 <- data.table(description = names(pat1a2), number.patients.age.30to49 = pat1a2)
pat.tab1a3 <- data.table(description = names(pat1a3), number.patients.age.50to69 = pat1a3)
pat.tab1a4 <- data.table(description = names(pat1a4), number.patients.age.70andolder = pat1a4)

pat.tab1a1[, description:= str_replace_all(description,"[^[:alnum:]]","")]
pat.tab1a2[, description:= str_replace_all(description,"[^[:alnum:]]","")]
pat.tab1a3[, description:= str_replace_all(description,"[^[:alnum:]]","")]
pat.tab1a4[, description:= str_replace_all(description,"[^[:alnum:]]","")]


adm1a1 <- colSums(dat2a1)
adm1a2 <- colSums(dat2a2)
adm1a3 <- colSums(dat2a3)
adm1a4 <- colSums(dat2a4)



adm.tab1a1 <- data.table(description = names(adm1a1), number.admissions.age.younger30 = adm1a1)
adm.tab1a2 <- data.table(description = names(adm1a2), number.admissions.age.30to49 = adm1a2)
adm.tab1a3 <- data.table(description = names(adm1a3), number.admissions.age.50to69 = adm1a3)
adm.tab1a4 <- data.table(description = names(adm1a4), number.admissions.age.70andolder = adm1a4)



adm.tab1a1[, description:= str_replace_all(description,"episodes","")]
adm.tab1a1[, description:= str_replace_all(description,"[^[:alnum:]]","")]

adm.tab1a2[, description:= str_replace_all(description,"episodes","")]
adm.tab1a2[, description:= str_replace_all(description,"[^[:alnum:]]","")]

adm.tab1a3[, description:= str_replace_all(description,"episodes","")]
adm.tab1a3[, description:= str_replace_all(description,"[^[:alnum:]]","")]

adm.tab1a4[, description:= str_replace_all(description,"episodes","")]
adm.tab1a4[, description:= str_replace_all(description,"[^[:alnum:]]","")]



table2.2.age <- adm.tab1a1

table2.2.age <- table2.2.age[pat.tab1a1, on='description']
table2.2.age <- table2.2.age[pat.tab1a2, on='description']
table2.2.age <- table2.2.age[pat.tab1a3, on='description']
table2.2.age <- table2.2.age[pat.tab1a4, on='description']
table2.2.age <- table2.2.age[adm.tab1a2, on='description']
table2.2.age <- table2.2.age[adm.tab1a3, on='description']
table2.2.age <- table2.2.age[adm.tab1a4, on='description']


table2.1_age <- table2.2.age[cond_lu, on='description']


table2.1_age[, description:=NULL]

setcolorder(table2.1_age,c("condition","number.patients.age.younger30","number.patients.age.30to49","number.patients.age.50to69","number.patients.age.70andolder","number.admissions.age.younger30","number.admissions.age.30to49","number.admissions.age.50to69","number.admissions.age.70andolder"))

table2.1_age$total.patients.age.younger30 = nrow(dat1a1)
table2.1_age$total.patients.age.30to49 = nrow(dat1a2)
table2.1_age$total.patients.age.50to69 = nrow(dat1a3)
table2.1_age$total.patients.age.70andolder = nrow(dat1a4)


fwrite(table2.1_age,normalizePath(file.path(datapath,"output2_tab2-1-age.csv")))



#--------------------------------------------------------------------------------
# Table 2-1 Deprivation


dat1d1 <- dat[Deprivation == 1, 6:35]
dat1d2 <- dat[Deprivation == 2, 6:35]
dat1d3 <- dat[Deprivation == 3, 6:35]
dat1d4 <- dat[Deprivation == 4, 6:35]
dat1d5 <- dat[Deprivation == 5, 6:35]

dat2d1 <- dat[Deprivation == 1, 66:95]
dat2d2 <- dat[Deprivation == 2, 66:95]
dat2d3 <- dat[Deprivation == 3, 66:95]
dat2d4 <- dat[Deprivation == 4, 66:95]
dat2d5 <- dat[Deprivation == 4, 66:95]


pat1d1 <- colSums(dat1d1)
pat1d2 <- colSums(dat1d2)
pat1d3 <- colSums(dat1d3)
pat1d4 <- colSums(dat1d4)
pat1d5 <- colSums(dat1d5)






pat.tab1d1 <- data.table(description = names(pat1d1), number.patients.dep.1 = pat1d1)
pat.tab1d2 <- data.table(description = names(pat1d2), number.patients.dep.2 = pat1d2)
pat.tab1d3 <- data.table(description = names(pat1d3), number.patients.dep.3 = pat1d3)
pat.tab1d4 <- data.table(description = names(pat1d4), number.patients.dep.4 = pat1d4)
pat.tab1d5 <- data.table(description = names(pat1d5), number.patients.dep.5 = pat1d5)


pat.tab1d1[, description:= str_replace_all(description,"[^[:alnum:]]","")]
pat.tab1d2[, description:= str_replace_all(description,"[^[:alnum:]]","")]
pat.tab1d3[, description:= str_replace_all(description,"[^[:alnum:]]","")]
pat.tab1d4[, description:= str_replace_all(description,"[^[:alnum:]]","")]
pat.tab1d5[, description:= str_replace_all(description,"[^[:alnum:]]","")]

adm1d1 <- colSums(dat2d1)
adm1d2 <- colSums(dat2d2)
adm1d3 <- colSums(dat2d3)
adm1d4 <- colSums(dat2d4)
adm1d5 <- colSums(dat2d5)



adm.tab1d1 <- data.table(description = names(adm1d1), number.admissions.dep.1 = adm1d1)
adm.tab1d2 <- data.table(description = names(adm1d2), number.admissions.dep.2 = adm1d2)
adm.tab1d3 <- data.table(description = names(adm1d3), number.admissions.dep.3 = adm1d3)
adm.tab1d4 <- data.table(description = names(adm1d4), number.admissions.dep.4 = adm1d4)
adm.tab1d5 <- data.table(description = names(adm1d5), number.admissions.dep.5 = adm1d5)


adm.tab1d1[, description:= str_replace_all(description,"episodes","")]
adm.tab1d1[, description:= str_replace_all(description,"[^[:alnum:]]","")]

adm.tab1d2[, description:= str_replace_all(description,"episodes","")]
adm.tab1d2[, description:= str_replace_all(description,"[^[:alnum:]]","")]

adm.tab1d3[, description:= str_replace_all(description,"episodes","")]
adm.tab1d3[, description:= str_replace_all(description,"[^[:alnum:]]","")]

adm.tab1d4[, description:= str_replace_all(description,"episodes","")]
adm.tab1d4[, description:= str_replace_all(description,"[^[:alnum:]]","")]


adm.tab1d5[, description:= str_replace_all(description,"episodes","")]
adm.tab1d5[, description:= str_replace_all(description,"[^[:alnum:]]","")]


table2.1.dep <- adm.tab1d1

table2.1.dep <- table2.1.dep[pat.tab1d1, on='description']
table2.1.dep <- table2.1.dep[pat.tab1d2, on='description']
table2.1.dep <- table2.1.dep[pat.tab1d3, on='description']
table2.1.dep <- table2.1.dep[pat.tab1d4, on='description']
table2.1.dep <- table2.1.dep[pat.tab1d5, on='description']
table2.1.dep <- table2.1.dep[adm.tab1d2, on='description']
table2.1.dep <- table2.1.dep[adm.tab1d3, on='description']
table2.1.dep <- table2.1.dep[adm.tab1d4, on='description']
table2.1.dep <- table2.1.dep[adm.tab1d5, on='description']


table2.1.dep <- table2.1.dep[cond_lu, on='description']


table2.1.dep[, description:=NULL]

setcolorder(table2.1.dep,c("condition","number.patients.dep.1","number.patients.dep.2","number.patients.dep.3","number.patients.dep.4", "number.patients.dep.5", "number.admissions.dep.1" , "number.admissions.dep.2", "number.admissions.dep.3", "number.admissions.dep.4", "number.admissions.dep.5"))

table2.1.dep$total.patients.dep.1 = nrow(dat1d1)
table2.1.dep$total.patients.dep.2 = nrow(dat1d2)
table2.1.dep$total.patients.dep.3 = nrow(dat1d3)
table2.1.dep$total.patients.dep.4 = nrow(dat1d4)
table2.1.dep$total.patients.dep.5 = nrow(dat1d5)


fwrite(table2.1.dep,normalizePath(file.path(datapath,"output2_tab2-1-deprivation.csv")))

#-------------------------------------------------------------------------------------
# Table 2-1 Reason for shielding


dat1d1 <- dat[reason_respiratory == 1, 6:35]
dat1d2 <- dat[reason_rare == 1, 6:35]
dat1d3 <- dat[reason_transplant == 1, 6:35]
dat1d4 <- dat[reason_cancer == 1, 6:35]
dat1d5 <- dat[reason_other == 1, 6:35]
dat1d6 <- dat[reason_unknown == 1, 6:35]

dat2d1 <- dat[reason_respiratory == 1, 66:95]
dat2d2 <- dat[reason_rare == 1, 66:95]
dat2d3 <- dat[reason_transplant == 1, 66:95]
dat2d4 <- dat[reason_cancer == 1, 66:95]
dat2d5 <- dat[reason_other == 1, 66:95]
dat2d6 <- dat[reason_unknown == 1, 66:95]


pat1d1 <- colSums(dat1d1)
pat1d2 <- colSums(dat1d2)
pat1d3 <- colSums(dat1d3)
pat1d4 <- colSums(dat1d4)
pat1d5 <- colSums(dat1d5)
pat1d6 <- colSums(dat1d6)





pat.tab1d1 <- data.table(description = names(pat1d1), number.patients.reason.respiratory = pat1d1)
pat.tab1d2 <- data.table(description = names(pat1d2), number.patients.reason.rare.genetic.metabolic.autoimmune = pat1d2)
pat.tab1d3 <- data.table(description = names(pat1d3), number.patients.reason.transplants = pat1d3)
pat.tab1d4 <- data.table(description = names(pat1d4), number.patients.reason.cancer = pat1d4)
pat.tab1d5 <- data.table(description = names(pat1d5), number.patients.reason.other = pat1d5)
pat.tab1d6 <- data.table(description = names(pat1d6), number.patients.reason.unknown = pat1d6)


pat.tab1d1[, description:= str_replace_all(description,"[^[:alnum:]]","")]
pat.tab1d2[, description:= str_replace_all(description,"[^[:alnum:]]","")]
pat.tab1d3[, description:= str_replace_all(description,"[^[:alnum:]]","")]
pat.tab1d4[, description:= str_replace_all(description,"[^[:alnum:]]","")]
pat.tab1d5[, description:= str_replace_all(description,"[^[:alnum:]]","")]
pat.tab1d6[, description:= str_replace_all(description,"[^[:alnum:]]","")]

adm1d1 <- colSums(dat2d1)
adm1d2 <- colSums(dat2d2)
adm1d3 <- colSums(dat2d3)
adm1d4 <- colSums(dat2d4)
adm1d5 <- colSums(dat2d5)
adm1d6 <- colSums(dat2d6)


adm.tab1d1 <- data.table(description = names(adm1d1), number.admissions.reason.respiratory = adm1d1)
adm.tab1d2 <- data.table(description = names(adm1d2), number.admissions.reason.rare.genetic.metabolic.autoimmune = adm1d2)
adm.tab1d3 <- data.table(description = names(adm1d3), number.admissions.reason.transplants = adm1d3)
adm.tab1d4 <- data.table(description = names(adm1d4), number.admissions.reason.cancer = adm1d4)
adm.tab1d5 <- data.table(description = names(adm1d5), number.admissions.reason.other = adm1d5)
adm.tab1d6 <- data.table(description = names(adm1d6), number.admissions.reason.unknown = adm1d6)


adm.tab1d1[, description:= str_replace_all(description,"episodes","")]
adm.tab1d1[, description:= str_replace_all(description,"[^[:alnum:]]","")]

adm.tab1d2[, description:= str_replace_all(description,"episodes","")]
adm.tab1d2[, description:= str_replace_all(description,"[^[:alnum:]]","")]

adm.tab1d3[, description:= str_replace_all(description,"episodes","")]
adm.tab1d3[, description:= str_replace_all(description,"[^[:alnum:]]","")]

adm.tab1d4[, description:= str_replace_all(description,"episodes","")]
adm.tab1d4[, description:= str_replace_all(description,"[^[:alnum:]]","")]


adm.tab1d5[, description:= str_replace_all(description,"episodes","")]
adm.tab1d5[, description:= str_replace_all(description,"[^[:alnum:]]","")]

adm.tab1d6[, description:= str_replace_all(description,"episodes","")]
adm.tab1d6[, description:= str_replace_all(description,"[^[:alnum:]]","")]


table2.1.dep <- adm.tab1d1

table2.1.dep <- table2.1.dep[pat.tab1d1, on='description']
table2.1.dep <- table2.1.dep[pat.tab1d2, on='description']
table2.1.dep <- table2.1.dep[pat.tab1d3, on='description']
table2.1.dep <- table2.1.dep[pat.tab1d4, on='description']
table2.1.dep <- table2.1.dep[pat.tab1d5, on='description']
table2.1.dep <- table2.1.dep[pat.tab1d6, on='description']

table2.1.dep <- table2.1.dep[adm.tab1d2, on='description']
table2.1.dep <- table2.1.dep[adm.tab1d3, on='description']
table2.1.dep <- table2.1.dep[adm.tab1d4, on='description']
table2.1.dep <- table2.1.dep[adm.tab1d5, on='description']
table2.1.dep <- table2.1.dep[adm.tab1d6, on='description']

table2.1.dep <- table2.1.dep[cond_lu, on='description']


table2.1.dep[, description:=NULL]

setcolorder(table2.1.dep,c("condition","number.patients.reason.respiratory","number.patients.reason.rare.genetic.metabolic.autoimmune","number.patients.reason.cancer","number.patients.reason.transplants", "number.patients.reason.other", "number.patients.reason.unknown","number.admissions.reason.respiratory" , "number.admissions.reason.rare.genetic.metabolic.autoimmune", "number.admissions.reason.cancer", "number.admissions.reason.transplants", "number.admissions.reason.other","number.admissions.reason.unknown"))

table2.1.dep$total.patients.reason.respiratory = nrow(dat1d1)
table2.1.dep$total.patients.reason.rare.genetic.metabolic.autoimmune = nrow(dat1d2)
table2.1.dep$total.patients.reason.cancer = nrow(dat1d4)
table2.1.dep$total.patients.reason.tranplant = nrow(dat1d3)
table2.1.dep$total.patients.reason.other = nrow(dat1d5)
table2.1.dep$total.patients.reason.unknown = nrow(dat1d6)

fwrite(table2.1.dep,normalizePath(file.path(datapath,"output2_tab2-1-reasons.csv")))






#-------------------------------------------------------------------------------------

aggrcond$Gender <- dat$Gender


table2.2_sex <- as.data.table(table(aggrcond$Gender,aggrcond$conditions_aggregated))

setnames(table2.2_sex, c("V1", "V2", "N"), c("sex", "conditions_aggregated", "number.patients"))

fwrite(table2.2_sex,normalizePath(file.path(datapath,"output2_tab2-2-sex.csv")))


aggrcond$age_group <- dat$age_group

table2.2_age <- as.data.table(table(aggrcond$age_group,aggrcond$conditions_aggregated))

setnames(table2.2_age, c("V1", "V2", "N"), c("age_group", "conditions_aggregated", "number.patients"))

fwrite(table2.2_age,normalizePath(file.path(datapath,"output2_tab2-2-age.csv")))


aggrcond$deprivation <- dat$Deprivation

table2.2_dep <- as.data.table(table(aggrcond$deprivation,aggrcond$conditions_aggregated))

setnames(table2.2_dep, c("V1", "V2", "N"), c("deprivation", "conditions_aggregated", "number.patients"))

fwrite(table2.2_dep,normalizePath(file.path(datapath,"output2_tab2-2-dep.csv")))


aggrcond$reasofrofr <- dat$Deprivation

table2.2_dep <- as.data.table(table(aggrcond$deprivation,aggrcond$conditions_aggregated))

setnames(table2.2_dep, c("V1", "V2", "N"), c("deprivation", "conditions_aggregated", "number.patients"))

fwrite(table2.2_dep,normalizePath(file.path(datapath,"output2_tab2-2-dep.csv")))

#--------------------------------------------------------------------------------

datr <- gather(dat, reason, reason_flag, reason_respiratory:reason_unknown, factor_key = TRUE)

datr = as.data.table(datr)

datr <- datr[reason_flag == 1, ]

table2.2_reasons <- data.table(table(datr$reason,datr$conditions_aggregcat))

setnames(table2.2_reasons, c("V1", "V2", "N"), c("reason", "conditions_aggregated", "number.patients"))

fwrite(table2.2_reasons,normalizePath(file.path(datapath,"output2_tab2-2-reasons.csv")))

#--------------------------------------------------------------------------------


table2.3_sex <- data.table(table(dat$Gender,dat$windex_vw))

setnames(table2.3_sex, c("V1", "V2", "N"), c("sex", "windex_vw", "number.patients"))

fwrite(table2.3_sex,normalizePath(file.path(datapath,"output2_tab2-3-sex.csv")))




table2.3_age <- data.table(table(dat$age_group,dat$windex_vw))

setnames(table2.3_age, c("V1", "V2", "N"), c("age_group", "windex_vw", "number.patients"))

fwrite(table2.3_age,normalizePath(file.path(datapath,"output2_tab2-3-age.csv")))




table2.3_deprivation <- data.table(table(dat$Deprivation,dat$windex_vw))

setnames(table2.3_deprivation, c("V1", "V2", "N"), c("deprivation", "windex_vw", "number.patients"))

fwrite(table2.3_deprivation,normalizePath(file.path(datapath,"output2_tab2-3-deprivation.csv")))



table2.3_reasons <- data.table(table(datr$reason,datr$windex_vw))

setnames(table2.3_reasons, c("V1", "V2", "N"), c("reason", "windex_vw", "number.patients"))

fwrite(table2.3_reasons,normalizePath(file.path(datapath,"output2_tab2-3-reasons.csv")))


