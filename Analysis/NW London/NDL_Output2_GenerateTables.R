#Import libraries
library(dplyr)
library(tidyr)
library(readxl)
library(data.table)

`%nin%` <- Negate(`%in%`)

#Read Data####
adm <- fread("admissions_output2.csv")
pat <- fread("patients_output2.csv")
meta <- read_xlsx("../Output 2 Tables - machine readable.xlsx",sheet = "Metadata",skip = 17)
colnames(meta) <- c("variable_name","condition","ICD-10s","regex")
meta <- subset(meta,!is.na(meta$regex))

conditions <- meta$variable_name
morb_variables <- c("RfS_Respiratory","RfS_RareGenetic","RfS_Cancer","RfS_Cancer_CR","RfS_Cancer_Haem","RfS_Other","RfS_Unknown")

#Define variable order
SetVariableOrder <- function(data){
  data$Gender <- factor(data$Gender,levels = c("Female","Male","Unknown"))
  data$AgeBracket <- factor(data$AgeBracket,levels = c("<30","30 to 49","50 to 69","70 or older","Unknown"))
  data$IMD_Quintile <- factor(data$IMD_Quintile,levels = c("1","2","3","4","5","Unknown"))
  data$windex_vw <- factor(data$windex_vw, levels = c("<0","0","1-4",">=5"))
  return(data)
}

pat <- SetVariableOrder(pat)
adm <- SetVariableOrder(adm)

#Main function
AggregateConditions <- function(data,vars){
  dataout <- data %>%
    group_by(.dots = vars) %>%
    summarise(n = n()) %>%
    mutate(n = ifelse(n <=7, "<5", as.character(n)))
  
  return(dataout)
}


#Tables####

##Table 2-1####
##Table 2-1 Overall
TEMP <- lapply(conditions,function(x) AggregateConditions(adm,x) %>% select(Conditions = x, Admissions = n))
table2_1_Overall_adm <- bind_rows(TEMP) %>% filter(!is.na(Conditions))

TEMP <- lapply(conditions,function(x) AggregateConditions(pat,x) %>% select(Conditions = x, Patients = n))
table2_1_Overall_pat <- bind_rows(TEMP) %>% filter(!is.na(Conditions))

table2_1_Overall <- left_join(table2_1_Overall_adm,table2_1_Overall_pat,by="Conditions")
table2_1_Overall$Total_patients <- length(unique(pat$PseudoID))
rm(table2_1_Overall_adm,table2_1_Overall_pat)

##Table 2-1 Sex
TEMP <- lapply(conditions,function(x) AggregateConditions(adm,c(x,"Gender")) %>% 
                 select(Conditions = x, Sex=Gender, Admissions = n))
table2_1_Sex_adm <- bind_rows(TEMP) %>%   filter(!is.na(Conditions))
table2_1_Sex_adm <- reshape(data = data.frame(table2_1_Sex_adm),direction = "wide",idvar=c("Conditions"),timevar = "Sex")

TEMP <- lapply(conditions,function(x) AggregateConditions(pat,c(x,"Gender")) %>%
                 select(Conditions = x,  Sex=Gender, Patients = n))
table2_1_Sex_pat <- bind_rows(TEMP) %>% filter(!is.na(Conditions))
table2_1_Sex_pat <- reshape(data = data.frame(table2_1_Sex_pat),direction = "wide",idvar=c("Conditions"),timevar = "Sex")

table2_1_Sex <- left_join(table2_1_Sex_adm,table2_1_Sex_pat,by="Conditions")

total_sex <- pat %>% group_by(Gender) %>% summarise(n = length(unique(PseudoID)))

table2_1_Sex$total_patients_female <- total_sex$n[total_sex$Gender=="Female"]
table2_1_Sex$total_patients_male <- total_sex$n[total_sex$Gender=="Male"]
table2_1_Sex$total_patients_unknown <- total_sex$n[total_sex$Gender=="Unknown"]
rm(table2_1_Sex_pat,table2_1_Sex_adm,total_sex)

##Table 2-1 Age
TEMP <- lapply(conditions,function(x) AggregateConditions(adm,c(x,"AgeBracket")) %>% 
                 select(Conditions = x, Age=AgeBracket, Admissions = n))
table2_1_age_adm <- bind_rows(TEMP) %>%   filter(!is.na(Conditions))
table2_1_age_adm <- reshape(data = data.frame(table2_1_age_adm),direction = "wide",idvar=c("Conditions"),timevar = "Age")

TEMP <- lapply(conditions,function(x) AggregateConditions(pat,c(x,"AgeBracket")) %>%
                 select(Conditions = x,  Age=AgeBracket, Patients = n))
table2_1_age_pat <- bind_rows(TEMP) %>% filter(!is.na(Conditions))
table2_1_age_pat <- reshape(data = data.frame(table2_1_age_pat),direction = "wide",idvar=c("Conditions"),timevar = "Age")

table2_1_Age <- left_join(table2_1_age_adm,table2_1_age_pat,by="Conditions")

total_age <- pat %>% group_by(AgeBracket) %>% summarise(n = length(unique(PseudoID)))

table2_1_Age$`total_patients_age_<30` <- total_age$n[total_age$AgeBracket=="<30"]
table2_1_Age$total_patients_age_30_to_49 <- total_age$n[total_age$AgeBracket=="30 to 49"]
table2_1_Age$total_patients_age_50_to_69 <- total_age$n[total_age$AgeBracket=="50 to 69"]
table2_1_Age$total_patients_age_70_or_older <- total_age$n[total_age$AgeBracket=="70 or older"]
table2_1_Age$total_patients_age_unknown <- total_age$n[total_age$AgeBracket=="Unknown"]

rm(table2_1_age_pat,table2_1_age_adm,total_age)

##Table 2-1 Deprivation
TEMP <- lapply(conditions,function(x) AggregateConditions(adm,c(x,"IMD_Quintile")) %>% 
                 select(Conditions = x, IMDquintile=IMD_Quintile, Admissions = n))
table2_1_imd_adm <- bind_rows(TEMP) %>%   filter(!is.na(Conditions))
table2_1_imd_adm <- reshape(data = data.frame(table2_1_imd_adm),direction = "wide",idvar=c("Conditions"),timevar = "IMDquintile")

TEMP <- lapply(conditions,function(x) AggregateConditions(pat,c(x,"IMD_Quintile")) %>%
                 select(Conditions = x,  IMDquintile=IMD_Quintile, Patients = n))
table2_1_imd_pat <- bind_rows(TEMP) %>% filter(!is.na(Conditions))
table2_1_imd_pat <- reshape(data = data.frame(table2_1_imd_pat),direction = "wide",idvar=c("Conditions"),timevar = "IMDquintile")

table2_1_imd <- left_join(table2_1_imd_adm,table2_1_imd_pat,by="Conditions")

total_imd <- pat %>% group_by(IMD_Quintile) %>% summarise(n = length(unique(PseudoID)))

table2_1_imd$total_patients_dep_1 <- total_imd$n[total_imd$IMD_Quintile=="1"]
table2_1_imd$total_patients_dep_2 <- total_imd$n[total_imd$IMD_Quintile=="2"]
table2_1_imd$total_patients_dep_3 <- total_imd$n[total_imd$IMD_Quintile=="3"]
table2_1_imd$total_patients_dep_4 <- total_imd$n[total_imd$IMD_Quintile=="4"]
table2_1_imd$total_patients_dep_5 <- total_imd$n[total_imd$IMD_Quintile=="5"]
table2_1_imd$total_patients_age_unknown <- total_imd$n[total_imd$IMD_Quintile=="Unknown"]

rm(table2_1_imd_pat,table2_1_imd_adm,total_imd)

##Table 2-1 RfS
#admissions
cond_adm <- adm %>% 
  gather(Condition,ConditionType,conditions) %>% 
  select(AdmissionID,ConditionType) %>% 
  filter(!is.na(ConditionType))

morb_adm <- adm %>% 
  gather(RsF,RfSType,morb_variables) %>%
  select(AdmissionID,RfSType) %>% 
  filter(!is.na(RfSType))

ids <- data.frame(AdmissionID = unique(adm$AdmissionID))

cond_morb_adm <- left_join(ids,cond_adm, by = "AdmissionID")
cond_morb_adm <- left_join(cond_morb_adm,morb_adm,by=c("AdmissionID"))

cm_count_adm <- cond_morb_adm %>% 
  group_by(ConditionType,RfSType) %>% summarise(admissions=n()) %>% 
  mutate(admissions=ifelse(admissions<=7,"<5",as.character(admissions))) %>%
  filter(!is.na(ConditionType))

cm_count_adm <- reshape(data = data.frame(cm_count_adm),direction = "wide",idvar=c("ConditionType"),timevar = "RfSType")

#Patients
cond_pat <- pat %>% 
  gather(Condition,ConditionType,conditions) %>% 
  select(PseudoID,ConditionType) %>% 
  filter(!is.na(ConditionType))

morb_pat <- pat %>% 
  gather(RsF,RfSType,morb_variables) %>%
  select(PseudoID,RfSType) %>% 
  filter(!is.na(RfSType))

ids <- data.frame(PseudoID = unique(pat$PseudoID))

cond_morb_pat <- left_join(x = ids, y = cond_pat,by=c("PseudoID"))
cond_morb_pat <- left_join(cond_morb_pat,morb_pat,by=c("PseudoID"))

cm_count_pat <- cond_morb_pat %>% 
  group_by(ConditionType,RfSType) %>% summarise(patients=n()) %>% 
  mutate(patients=ifelse(patients<=7,"<5",as.character(patients))) %>%
  filter(!is.na(ConditionType))

cm_count_pat <- reshape(data = data.frame(cm_count_pat),direction = "wide",idvar=c("ConditionType"),timevar = "RfSType")

#Merge
cm_count_final <- left_join(cm_count_adm,cm_count_pat,by=c("ConditionType"))

rfs_totals <- morb_pat %>% group_by(RfSType) %>% summarise(n = length(unique(PseudoID)))

cm_count_final$total_patients_cancer <- rfs_totals$n[rfs_totals$RfSType=="Cancer"]
cm_count_final$total_patients_respiratoy <- rfs_totals$n[rfs_totals$RfSType=="Respiratory"]
cm_count_final$total_patients_raraGen <- rfs_totals$n[rfs_totals$RfSType=="Rare genetic"]
cm_count_final$total_patients_other <- rfs_totals$n[rfs_totals$RfSType=="Other"]
cm_count_final$total_patients_unknown <- rfs_totals$n[rfs_totals$RfSType=="Unknown"]

rm(morb_adm,morb_pat,cond_adm,cond_pat,cond_morb_pat,cond_morb_adm,cm_count_adm,cm_count_pat,rfs_totals)

##Table 2-2####
table2_2_Overall <- AggregateConditions(pat,"ConditionSum")
table2_2_Sex <- AggregateConditions(pat,c("Gender","ConditionSum"))
table2_2_age <- AggregateConditions(pat,c("AgeBracket","ConditionSum"))
table2_2_IMD <- AggregateConditions(pat,c("IMD_Quintile","ConditionSum"))
TEMP <- lapply(morb_variables,function(x) AggregateConditions(pat,c(x,"ConditionSum")) %>% 
                 select(Reason = x, Conditions = ConditionSum,Patients = n))
table2_2_RfS <- bind_rows(TEMP) %>% filter(!is.na(Reason))

#Table 2-3 Overall
table2_3_Overall <- AggregateConditions(pat,"windex_vw")
table2_3_Sex <- AggregateConditions(pat,c("Gender","windex_vw"))
table2_3_age <- AggregateConditions(pat,c("AgeBracket","windex_vw"))
table2_3_IMD <- AggregateConditions(pat,c("IMD_Quintile","windex_vw"))
TEMP <- lapply(morb_variables,function(x) AggregateConditions(pat,c(x,"windex_vw")) %>% 
                 select(Reason = x, windex_vw = windex_vw,Patients = n))
table2_3_RfS <- bind_rows(TEMP) %>% filter(!is.na(Reason))

#Table 2-4

table2_4 <- adm %>%
  summarise(F204 = sum(grepl("F204",DiagCodes)),
            F315 = sum(grepl("F315",DiagCodes)),
            G114 = sum(grepl("G114",DiagCodes)),
            I110 = sum(grepl("I110",DiagCodes)),
            I120 = sum(grepl("I120",DiagCodes)),
            I130 = sum(grepl("I130",DiagCodes)),
            I131 = sum(grepl("I131",DiagCodes)),
            I132 = sum(grepl("I132",DiagCodes)),
            I278 = sum(grepl("I278",DiagCodes)),
            I279 = sum(grepl("I279",DiagCodes)),
            I426 = sum(grepl("I426",DiagCodes)),
            K700 = sum(grepl("K700",DiagCodes)),
            K703 = sum(grepl("K703",DiagCodes)),
            K709 = sum(grepl("K709",DiagCodes))
            ) %>% t()
table2_4 <- data.frame(Code = rownames(table2_4),
                       n = table2_4[,1])

#Write tables
fwrite(table2_1_Overall,"2_1_overall.csv")
fwrite(table2_1_Sex,"2_1_sex.csv")
fwrite(table2_1_Age,"2_1_age.csv")
fwrite(table2_1_imd,"2_1_imd.csv")
fwrite(cm_count_final,"2_1_RfS.csv")
fwrite(table2_2_Overall,"2_2_overall.csv")
fwrite(table2_2_Sex,"2_2_sex.csv")
fwrite(table2_2_age,"2_2_age.csv")
fwrite(table2_2_IMD,"2_2_imd.csv")
fwrite(table2_2_RfS,"2_2_RfS.csv")
fwrite(table2_3_Overall,"2_3_overall.csv")
fwrite(table2_3_Sex,"2_3_sex.csv")
fwrite(table2_3_age,"2_3_age.csv")
fwrite(table2_3_IMD,"2_3_imd.csv")
fwrite(table2_3_RfS,"2_3_RfS.csv")
fwrite(table2_4,"2_4_extra.csv")
