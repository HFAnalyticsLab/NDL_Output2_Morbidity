#Library imports and extra functions####
library(dplyr)
library(readxl)
library(comorbidity)

`%nin%` <- Negate(`%in%`)

#Read data####
data <- read_excel("S:/Roberto/NDL/Central Analysis 1/Output2/NDL - Shielding Patients 20210112.xlsx", sheet = "Admissions")
data <- subset(data, !is.na(data$PseudoID))
meta <- read_xlsx("../Output 2 Tables - machine readable.xlsx",sheet = "Metadata",skip = 17)
colnames(meta) <- c("variable_name","condition","ICD-10s","regex")
meta <- subset(meta,!is.na(meta$regex))

#Fix ICD-10 codes, some have trailing 'X's, '-'s, "X-"s, or characters after spaces (i.e. "M501 D)
regexcols <- colnames(data)[grepl("DiagnosisCode",colnames(data))]
codes <- unique(unlist(c(data[,regexcols])))
#codes

data[regexcols] <- sapply(data[regexcols],function(x) stringr::str_remove_all(x,"[Xx]?-.*$"))
data$PrimaryDiagnosisInLastEpisode <- stringr::str_remove_all(data$PrimaryDiagnosisInLastEpisode,"[Xx]?-.*$")

#Filter out duplicate entries. These appear complete repetitions of the rows, or as episodes for a given patient with the same admission date and diagnosis codes, but one entry is missing the primary diagnosis code

data$DiagCodes <-  apply(X = data[,regexcols],MARGIN = 1,FUN = paste, collapse = ",")

data$duptag <- paste0(data$PseudoID,"_",data$AdmissionDate,"_",data$DiagCodes)

duplic <- duplicated(data)
data <- data[!duplic,]

test <- data
test$rep <- F

for(i in 1:(nrow(test)-1)){
  j <- i+1
  if(test$duptag[i] == test$duptag[j] & test$PrimaryDiagnosisInLastEpisode[i] != test$PrimaryDiagnosisInLastEpisode[j]){
    if(test$PrimaryDiagnosisInLastEpisode=="NULL"){
      test$rep[i] <- T
    } else {
      test$rep[j] <- T
    }
  }
}

data <- test[!test$rep,]

#Change vars that should be numeric to numeric
toNumeric <- c("Age",
               "Flag_PDSInformallyDeceased",
               "2a_Flag_Chemo/Radiotherapy","2b_Flag_HeamatologicalCancers","05_Flag_PregnantWithCongentialHeartDefect",
               "04_Flag_RareDiseases","03_Flag_Respiratory",
               "01_Flag_Transplant",
               "IMDRank")

data[toNumeric] <- sapply(data[toNumeric],as.numeric)

#Keep vars that are morbidity flags
morb_flags <- c("01_Flag_Transplant",
                "2a_Flag_Chemo/Radiotherapy",
                "2b_Flag_HeamatologicalCancers",
                "03_Flag_Respiratory",
                "04_Flag_RareDiseases",
                "05_Flag_PregnantWithCongentialHeartDefect")

#Pre process data#####
##Turn age into the pre-defined buckets####
data <- data %>%
  mutate(AgeBracket = case_when(Age<30 ~ "<30",
                                Age>= 30 & Age<50 ~ "30 to 49",
                                Age>= 50 & Age<69 ~ "50 to 69",
                                Age >=70 ~ "70 or older"))
         
data$AgeBracket[is.na(data$AgeBracket)] <- "Unknown"

#Check that new variable is OK
table(data$Age,data$AgeBracket)

##Rank IMDs into quintiles####
#Rank banding taken from https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/853811/IoD2019_FAQ_v4.pdf (p.10)
data <- data %>%
  mutate(IMD_Quintile = case_when(IMDRank <= 6568 ~ "1",
                                  IMDRank >= 6569  & IMDRank <= 13137 ~ "2",
                                  IMDRank >= 13138 & IMDRank <= 19706 ~ "3",
                                  IMDRank >= 19707 & IMDRank <= 26275 ~ "4",
                                  IMDRank >= 26276 ~ "5",
                                  is.na(IMDRank) ~ "Unknown"
                                  )
  )

##Fix Gender varaible####
data$Gender <- ifelse(data$Gender=="NULL","Unknown",data$Gender)

##Create variable describing reason for shielding####
data$RfS_Respiratory <- ifelse(data$`03_Flag_Respiratory`,"Respiratory",NA)
data$RfS_RareGenetic <- ifelse(data$`04_Flag_RareDiseases`,"Rare genetic",NA)
data$RfS_Cancer_CR <- ifelse(data$`2a_Flag_Chemo/Radiotherapy` == 1, "Chemotherapy/Radiotherapy",NA)
data$RfS_Cancer_Haem <- ifelse(data$`2b_Flag_HeamatologicalCancers` == 1, "Haemathological cancer",NA)
data$RfS_Cancer <- ifelse(data$`2b_Flag_HeamatologicalCancers` == 1 | data$`2a_Flag_Chemo/Radiotherapy` == 1, "Cancer",NA)
data$RfS_Other <- ifelse(data$`05_Flag_PregnantWithCongentialHeartDefect`|data$`01_Flag_Transplant`,"Other",NA)
data$RfS_Unknown <- ifelse((data$`01_Flag_Transplant` + data$`2a_Flag_Chemo/Radiotherapy` + data$`2b_Flag_HeamatologicalCancers` +
                              data$`03_Flag_Respiratory` + data$`04_Flag_RareDiseases` + 
                              data$`05_Flag_PregnantWithCongentialHeartDefect`)==0,"Unknown",NA)

## check conditions within each episode
#Add id for episodes
data$AdmissionID <- 1:nrow(data)

#Get comorbidity data for patients and episodes
admissions <- comorbidity(x = data,id = "AdmissionID",code = "PrimaryDiagnosisInLastEpisode", score = "elixhauser", assign0 = F)
patients <- comorbidity(x = data,id = "PseudoID",code = "DiagCodes", score = "elixhauser", assign0 = T)

#Get metadata
admissions_data <- data %>% 
  select(AdmissionID,Gender,AgeBracket,IMD_Quintile,PrimaryDiagnosisInLastEpisode,DiagCodes,contains("RfS"))
patients_data <- data %>% select(PseudoID,Gender,AgeBracket,IMD_Quintile,contains("RfS")) %>% unique()

admissions_output <- left_join(admissions_data,admissions, by = "AdmissionID")
patients_output <- left_join(patients_data,patients, by = "PseudoID")

patients_output$ConditionSum <- apply(patients_output[meta$variable_name],1,sum)
patients_output <- patients_output %>% group_by(PseudoID) %>%
  mutate(ConditionSum = as.character(sum(ConditionSum))) %>%
  mutate(ConditionSum = case_when(ConditionSum == "0" ~ "0",
                                  ConditionSum == "1" ~ "1",
                                  ConditionSum %nin% c("0","1") ~ "2 or more"))

admissions_output$ConditionSum <- apply(admissions_output[meta$variable_name],1,sum)


PopulateConditions <- function(data,vars = unique(meta$variable_name)){
  for(meta in meta$variable_name) { 
    data[,meta] = c(ifelse(data[,meta] == 1,meta,NA))
  }
  return(data)
}

admissions_output <- PopulateConditions(admissions_output)
patients_output <- PopulateConditions(patients_output)

#OUTPUT####
write.csv(admissions_output,"admissions_output2.csv",row.names = F)
write.csv(patients_output,"patients_output2.csv",row.names = F)

