##############################################
################### TO-DO ####################
##############################################

##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,readODS,
               gmodels,DescTools,data.table,
               tibble,pbapply,pbmcapply,here,
               tidyverse,readxl)

#Clean up the global environment
rm(list = ls())

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Networked Data Lab/Partner outputs/Output 2 Morbidity/"

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

####################################################
##################### Table 1  #####################
####################################################

###### Overall

#Import
table1.overall.wales <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                                   sheet = "Table 2-1 Overall")

#New variables
table1.overall.wales <- mutate(table1.overall.wales,
                                  breakdown="overall",
                                  breakdown.level="overall") %>%
  dplyr::rename(.,total.patients.adm=total.patients)

###### Age

#Import and reshape: number patients
table1.age.wales_A <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                               sheet = "Table 2-1 Age") %>%
  select(.,condition,starts_with("number.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="age",
         breakdown.level=str_replace_all(breakdown.level,"number.patients.age.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"70.or.older","70+")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".to.","-"))
  
#Import and reshape: number admissions
table1.age.wales_B <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                                 sheet = "Table 2-1 Age") %>%
  select(.,condition,starts_with("number.admissions")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.admissions="value",breakdown.level="variable") %>%
  mutate(.,breakdown="age",
         breakdown.level=str_replace_all(breakdown.level,"number.admissions.age.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"70.or.older","70+")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".to.","-"))

#Import and reshape: totals
table1.age.wales_C <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                                 sheet = "Table 2-1 Age") %>%
  select(.,condition,starts_with("total.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,total.patients.adm="value",breakdown.level="variable") %>%
  mutate(.,breakdown="age",
         breakdown.level=str_replace_all(breakdown.level,"total.patients.age.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"70.or.older","70+")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".to.","-"))

#Merge all 3 columns: patients, admissions and totals
table1.age.wales <- full_join(table1.age.wales_A,table1.age.wales_B,by=c("condition","breakdown","breakdown.level")) %>%
  full_join(.,table1.age.wales_C,by=c("condition","breakdown","breakdown.level"))
rm(table1.age.wales_A,table1.age.wales_B,table1.age.wales_C)

###### Sex

#Import and reshape: number patients
table1.sex.wales_A <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                                 sheet = "Table 2-1 Sex") %>%
  select(.,condition,starts_with("number.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="sex",
         breakdown.level=str_replace_all(breakdown.level,"number.patients.sex.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))

#Import and reshape: number admissions
table1.sex.wales_B <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                                 sheet = "Table 2-1 Sex") %>%
  select(.,condition,starts_with("number.admissions")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.admissions="value",breakdown.level="variable") %>%
  mutate(.,breakdown="sex",
         breakdown.level=str_replace_all(breakdown.level,"number.admissions.sex.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))

#Import and reshape: totals
table1.sex.wales_C <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                                 sheet = "Table 2-1 Sex") %>%
  select(.,condition,starts_with("total.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,total.patients.adm="value",breakdown.level="variable") %>%
  mutate(.,breakdown="sex",
         breakdown.level=str_replace_all(breakdown.level,"total.patients.sex.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))

#Merge all 3 columns: patients, admissions and totals
table1.sex.wales <- full_join(table1.sex.wales_A,table1.sex.wales_B,by=c("condition","breakdown","breakdown.level")) %>%
  full_join(.,table1.sex.wales_C,by=c("condition","breakdown","breakdown.level"))
rm(table1.sex.wales_A,table1.sex.wales_B,table1.sex.wales_C)

###### Deprivation

#Import and reshape: number patients
table1.dep.wales_A <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                                 sheet = "Table 2-1 Deprivation") %>%
  select(.,condition,starts_with("number.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="imd",
         breakdown.level=str_replace_all(breakdown.level,"number.patients.dep.",""))

#Import and reshape: number admissions
table1.dep.wales_B <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                                 sheet = "Table 2-1 Deprivation") %>%
  select(.,condition,starts_with("number.admissions")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.admissions="value",breakdown.level="variable") %>%
  mutate(.,breakdown="imd",
         breakdown.level=str_replace_all(breakdown.level,"number.admissions.dep.",""))

#Import and reshape: totals
table1.dep.wales_C <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                                 sheet = "Table 2-1 Deprivation") %>%
  select(.,condition,starts_with("total.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,total.patients.adm="value",breakdown.level="variable") %>%
  mutate(.,breakdown="imd",
         breakdown.level=str_replace_all(breakdown.level,"total.patients.dep.",""))

#Merge all 3 columns: patients, admissions and totals
table1.dep.wales <- full_join(table1.dep.wales_A,table1.dep.wales_B,by=c("condition","breakdown","breakdown.level")) %>%
  full_join(.,table1.dep.wales_C,by=c("condition","breakdown","breakdown.level"))
rm(table1.dep.wales_A,table1.dep.wales_B,table1.dep.wales_C)

###### Reason for shielding

#Import and reshape: number patients
table1.reason.wales_A <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                                 sheet = "Table 2-1 Reason for shielding") %>%
  select(.,condition,starts_with("number.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="reason_shielding",
         breakdown.level=str_replace_all(breakdown.level,"number.patients.reason.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare.diseases","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"immunsuppresion","immunosuppressants")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"immunosuppression","immunosuppressants")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"[.]"," "))

#Import and reshape: number admissions
table1.reason.wales_B <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                                 sheet = "Table 2-1 Reason for shielding") %>%
  select(.,condition,starts_with("number.admissions")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.admissions="value",breakdown.level="variable") %>%
  mutate(.,breakdown="reason_shielding",
         breakdown.level=str_replace_all(breakdown.level,"number.admissions.reason.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare.diseases","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"immunsuppresion","immunosuppressants")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"immunosuppression","immunosuppressants")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"[.]"," "))

#Import and reshape: totals
table1.reason.wales_C <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                                 sheet = "Table 2-1 Reason for shielding") %>%
  select(.,condition,starts_with("total.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,total.patients.adm="value",breakdown.level="variable") %>%
  mutate(.,breakdown="reason_shielding",
         breakdown.level=str_replace_all(breakdown.level,"total.patients.reason.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare.diseases","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"immunsuppresion","immunosuppressants")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"immunosuppression","immunosuppressants")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"[.]"," "))

#Merge all 3 columns: patients, admissions and totals
table1.reason.wales <- full_join(table1.reason.wales_A,table1.reason.wales_B,by=c("condition","breakdown","breakdown.level")) %>%
  full_join(.,table1.reason.wales_C,by=c("condition","breakdown","breakdown.level"))
rm(table1.reason.wales_A,table1.reason.wales_B,table1.reason.wales_C)

###### Append

table1.wales <- plyr::rbind.fill(table1.overall.wales,
                              table1.age.wales,
                              table1.sex.wales,
                              table1.dep.wales,
                              table1.reason.wales)

table1.wales <- mutate(table1.wales,partner="Wales")

rm(table1.overall.wales,
   table1.age.wales,
   table1.sex.wales,
   table1.dep.wales,
   table1.reason.wales)

fwrite(table1.wales, file = paste0(rawdatadir,"Wales/table1.wales.csv"), sep = ",")

unique(filter(table1.wales,breakdown=="reason_shielding")$breakdown.level)

####################################################
##################### Table 2  #####################
####################################################

###### Overall

#Import
table2.overall.wales <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                                   sheet = "Table 2-2 Overall")

#New names and variables
table2.overall.wales <- table2.overall.wales %>%
  mutate(.,breakdown="overall",breakdown.level="overall") %>%
  dplyr::rename(.,score_band="conditions_aggregated") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band)) %>%
  filter(.,!is.na(score_band))

#Compute totals and merge in
detach(package:plyr)
table2.total.overall.wales <- table2.overall.wales %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table2.overall.wales <- left_join(table2.overall.wales,table2.total.overall.wales,
                                 by="breakdown.level")
rm(table2.total.overall.wales)

###### Age

#Import
table2.age.wales <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                                   sheet = "Table 2-2 Age")

#New names and variables
table2.age.wales <- table2.age.wales %>%
  mutate(.,breakdown="age") %>%
  dplyr::rename(.,score_band="conditions_aggregated",breakdown.level="age_group") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band)) %>%
  filter(.,!is.na(score_band)) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"70.or.older","70+")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".to.","-"))

#Compute totals and merge in
detach(package:plyr)
table2.total.age.wales <- table2.age.wales %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table2.age.wales <- left_join(table2.age.wales,table2.total.age.wales,
                                  by="breakdown.level")
rm(table2.total.age.wales)

###### Sex

#Import
table2.sex.wales <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                               sheet = "Table 2-2 Sex")

#New names and variables
table2.sex.wales <- table2.sex.wales %>%
  mutate(.,breakdown="sex") %>%
  dplyr::rename(.,score_band="conditions_aggregated",breakdown.level="sex") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band)) %>%
  filter(.,!is.na(score_band)) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"unknown/other","unknown_other"))

#Compute totals and merge in
detach(package:plyr)
table2.total.sex.wales <- table2.sex.wales %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table2.sex.wales <- left_join(table2.sex.wales,table2.total.sex.wales,
                                  by="breakdown.level")
rm(table2.total.sex.wales)

###### Deprivation

#Import
table2.dep.wales <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                               sheet = "Table 2-2 Deprivation")

#New names and variables
table2.dep.wales <- table2.dep.wales %>%
  mutate(.,breakdown="imd") %>%
  dplyr::rename(.,score_band="conditions_aggregated",breakdown.level="deprivation") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band)) %>%
  filter(.,!is.na(score_band))

#Compute totals and merge in
detach(package:plyr)
table2.total.dep.wales <- table2.dep.wales %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table2.dep.wales <- left_join(table2.dep.wales,table2.total.dep.wales,
                                  by="breakdown.level")
rm(table2.total.dep.wales)

###### Reason for shielding

#Import
table2.reason.wales <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                               sheet = "Table 2-2 Wales")

#New names and variables
table2.reason.wales <- table2.reason.wales %>%
  mutate(.,breakdown="reason_shielding") %>%
  dplyr::rename(.,score_band="conditions_aggregated",breakdown.level="reason") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band)) %>%
  filter(.,!is.na(score_band)) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare.diseases","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"immunsuppresion","immunosuppressants")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"immunosuppression","immunosuppressants")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"[.]"," "))

#Compute totals and merge in
detach(package:plyr)
table2.total.reason.wales <- table2.reason.wales %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table2.reason.wales <- left_join(table2.reason.wales,table2.total.reason.wales,
                              by="breakdown.level")
rm(table2.total.reason.wales)

###### Append

table2.wales <- plyr::rbind.fill(table2.overall.wales,
                                    table2.age.wales,
                                    table2.sex.wales,
                                    table2.dep.wales,
                                    table2.reason.wales)

table2.wales <- mutate(table2.wales,partner="Wales")

rm(table2.overall.wales,
   table2.age.wales,
   table2.sex.wales,
   table2.dep.wales,
   table2.reason.wales)

fwrite(table2.wales, file = paste0(rawdatadir,"Wales/table2.wales.csv"), sep = ",")

####################################################
##################### Table 3  #####################
####################################################

###### Overall

#Import
table3.overall.wales <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                                   sheet = "Table 2-3 Overall")

#New names and variables
table3.overall.wales <- table3.overall.wales %>%
  mutate(.,breakdown="overall",breakdown.level="overall") %>%
  dplyr::rename(.,windex_vw=van_walraven_score) %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4")) %>%
  filter(.,!is.na(windex_vw))

#Compute totals and merge in
detach(package:plyr)
table3.total.overall.wales <- table3.overall.wales %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table3.overall.wales <- left_join(table3.overall.wales,table3.total.overall.wales,
                                  by="breakdown.level")
rm(table3.total.overall.wales)

###### Age

#Import
table3.age.wales <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                                   sheet = "Table 2-3 Age")

#New names and variables
table3.age.wales <- table3.age.wales %>%
  mutate(.,breakdown="age") %>%
  dplyr::rename(.,breakdown.level="age_group") %>%
  filter(.,!is.na(windex_vw)) %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"70.or.older","70+")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".to.","-"))

#Compute totals and merge in
detach(package:plyr)
table3.total.age.wales <- table3.age.wales %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table3.age.wales <- left_join(table3.age.wales,table3.total.age.wales,
                                  by="breakdown.level")
rm(table3.total.age.wales)

###### Sex

#Import
table3.sex.wales <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                               sheet = "Table 2-3 Sex")

#New names and variables
table3.sex.wales <- table3.sex.wales %>%
  mutate(.,breakdown="sex") %>%
  dplyr::rename(.,breakdown.level="sex") %>%
  filter(.,!is.na(windex_vw)) %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))
  
#Compute totals and merge in
detach(package:plyr)
table3.total.sex.wales <- table3.sex.wales %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table3.sex.wales <- left_join(table3.sex.wales,table3.total.sex.wales,
                              by="breakdown.level")
rm(table3.total.sex.wales)

###### Deprivation

#Import
table3.dep.wales <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                               sheet = "Table 2-3 Deprivation")

#New names and variables
table3.dep.wales <- table3.dep.wales %>%
  mutate(.,breakdown="imd") %>%
  filter(.,!is.na(windex_vw)) %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4")) %>%
  dplyr::rename(.,breakdown.level="deprivation")

#Compute totals and merge in
detach(package:plyr)
table3.total.dep.wales <- table3.dep.wales %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table3.dep.wales <- left_join(table3.dep.wales,table3.total.dep.wales,
                              by="breakdown.level")
rm(table3.total.dep.wales)

###### Reason for shielding

#Import
table3.reason.wales <- read_excel(paste0(rawdatadir,"Wales/","NDLWales Output 2.xlsx"),
                               sheet = "Table 2-3 Wales")

#New names and variables
table3.reason.wales <- table3.reason.wales %>%
  mutate(.,breakdown="reason_shielding") %>%
  dplyr::rename(.,breakdown.level="reason") %>%
  filter(.,!is.na(windex_vw)) %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare.diseases","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"immunsuppresion","immunosuppressants")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"immunosuppression","immunosuppressants")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"[.]"," "))

#Compute totals and merge in
detach(package:plyr)
table3.total.reason.wales <- table3.reason.wales %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table3.reason.wales <- left_join(table3.reason.wales,table3.total.reason.wales,
                              by="breakdown.level")
rm(table3.total.reason.wales)

###### Append

table3.wales <- plyr::rbind.fill(table3.overall.wales,
                                    table3.age.wales,
                                    table3.sex.wales,
                                    table3.dep.wales,
                                    table3.reason.wales)

table3.wales <- mutate(table3.wales,partner="Wales")

rm(table3.overall.wales,
   table3.age.wales,
   table3.sex.wales,
   table3.dep.wales,
   table3.reason.wales)

fwrite(table3.wales, file = paste0(rawdatadir,"Wales/table3.wales.csv"), sep = ",")