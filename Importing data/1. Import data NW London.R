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
table1.overall.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                                   sheet = "Table 2-1 Overall")

#New variables
table1.overall.nwlondon <- mutate(table1.overall.nwlondon,
                                  breakdown="overall",
                                  breakdown.level="overall") %>%
  dplyr::rename(.,total.patients=total.patients)

###### Age

#Import and reshape: number patients
table1.age.nwlondon_A <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                               sheet = "Table 2-1 Age") %>%
  select(.,condition,starts_with("number.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="age",
         breakdown.level=str_replace_all(breakdown.level,"number.patients.age.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".or.older","+")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".to.","-"))
  
#Import and reshape: number admissions
table1.age.nwlondon_B <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                                 sheet = "Table 2-1 Age") %>%
  select(.,condition,starts_with("number.admissions")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.admissions="value",breakdown.level="variable") %>%
  mutate(.,breakdown="age",
         breakdown.level=str_replace_all(breakdown.level,"number.admissions.age.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".or.older","+")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".to.","-"))

#Import and reshape: totals
table1.age.nwlondon_C <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                                 sheet = "Table 2-1 Age") %>%
  select(.,condition,starts_with("total.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,total.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="age",
         breakdown.level=str_replace_all(breakdown.level,"total.patients.age.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".or.older","+")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".to.","-"))

#Merge all 3 columns: patients, admissions and totals
table1.age.nwlondon <- full_join(table1.age.nwlondon_A,table1.age.nwlondon_B,by=c("condition","breakdown","breakdown.level")) %>%
  full_join(.,table1.age.nwlondon_C,by=c("condition","breakdown","breakdown.level"))
rm(table1.age.nwlondon_A,table1.age.nwlondon_B,table1.age.nwlondon_C)

###### Sex

#Import and reshape: number patients
table1.sex.nwlondon_A <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                                 sheet = "Table 2-1 Sex") %>%
  select(.,condition,starts_with("number.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="sex",
         breakdown.level=str_replace_all(breakdown.level,"number.patients.sex.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))

#Import and reshape: number admissions
table1.sex.nwlondon_B <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                                 sheet = "Table 2-1 Sex") %>%
  select(.,condition,starts_with("number.admissions")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.admissions="value",breakdown.level="variable") %>%
  mutate(.,breakdown="sex",
         breakdown.level=str_replace_all(breakdown.level,"number.admissions.sex.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))

#Import and reshape: totals
table1.sex.nwlondon_C <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                                 sheet = "Table 2-1 Sex") %>%
  select(.,condition,starts_with("total.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,total.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="sex",
         breakdown.level=str_replace_all(breakdown.level,"total.patients.sex.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))

#Merge all 3 columns: patients, admissions and totals
table1.sex.nwlondon <- full_join(table1.sex.nwlondon_A,table1.sex.nwlondon_B,by=c("condition","breakdown","breakdown.level")) %>%
  full_join(.,table1.sex.nwlondon_C,by=c("condition","breakdown","breakdown.level"))
rm(table1.sex.nwlondon_A,table1.sex.nwlondon_B,table1.sex.nwlondon_C)

###### Deprivation

#Import and reshape: number patients
table1.dep.nwlondon_A <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                                 sheet = "Table 2-1 Deprivation") %>%
  select(.,condition,starts_with("number.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="imd",
         breakdown.level=str_replace_all(breakdown.level,"number.patients.dep.",""))

#Import and reshape: number admissions
table1.dep.nwlondon_B <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                                 sheet = "Table 2-1 Deprivation") %>%
  select(.,condition,starts_with("number.admissions")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.admissions="value",breakdown.level="variable") %>%
  mutate(.,breakdown="imd",
         breakdown.level=str_replace_all(breakdown.level,"number.admissions.dep.",""))

#Import and reshape: totals
table1.dep.nwlondon_C <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                                 sheet = "Table 2-1 Deprivation") %>%
  select(.,condition,starts_with("total.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,total.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="imd",
         breakdown.level=str_replace_all(breakdown.level,"total.patients.dep.",""))

#Merge all 3 columns: patients, admissions and totals
table1.dep.nwlondon <- full_join(table1.dep.nwlondon_A,table1.dep.nwlondon_B,by=c("condition","breakdown","breakdown.level")) %>%
  full_join(.,table1.dep.nwlondon_C,by=c("condition","breakdown","breakdown.level"))
rm(table1.dep.nwlondon_A,table1.dep.nwlondon_B,table1.dep.nwlondon_C)

###### Reason for shielding

#Import and reshape: number patients
table1.reason.nwlondon_A <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                                 sheet = "Table 2-1 Reason for shielding") %>%
  select(.,condition,starts_with("number.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="reason_shielding",
         breakdown.level=str_replace_all(breakdown.level,"number.patients.reason.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare.genetic.metabolic.autoimmune","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant"))

#Import and reshape: number admissions
table1.reason.nwlondon_B <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                                 sheet = "Table 2-1 Reason for shielding") %>%
  select(.,condition,starts_with("number.admissions")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.admissions="value",breakdown.level="variable") %>%
  mutate(.,breakdown="reason_shielding",
         breakdown.level=str_replace_all(breakdown.level,"number.admissions.reason.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare.genetic.metabolic.autoimmune","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant"))

#Import and reshape: totals
table1.reason.nwlondon_C <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                                 sheet = "Table 2-1 Reason for shielding") %>%
  select(.,condition,starts_with("total.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,total.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="reason_shielding",
         breakdown.level=str_replace_all(breakdown.level,"total.patients.reason.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare.genetic.metabolic.autoimmune","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant"))

#Merge all 3 columns: patients, admissions and totals
table1.reason.nwlondon <- full_join(table1.reason.nwlondon_A,table1.reason.nwlondon_B,by=c("condition","breakdown","breakdown.level")) %>%
  full_join(.,table1.reason.nwlondon_C,by=c("condition","breakdown","breakdown.level"))
rm(table1.reason.nwlondon_A,table1.reason.nwlondon_B,table1.reason.nwlondon_C)

###### Append

table1.nwlondon <- plyr::rbind.fill(table1.overall.nwlondon,
                              table1.age.nwlondon,
                              table1.sex.nwlondon,
                              table1.dep.nwlondon,
                              table1.reason.nwlondon)

table1.nwlondon <- mutate(table1.nwlondon,partner="NW London")

rm(table1.overall.nwlondon,
   table1.age.nwlondon,
   table1.sex.nwlondon,
   table1.dep.nwlondon,
   table1.reason.nwlondon)

fwrite(table1.nwlondon, file = paste0(rawdatadir,"NW London/table1.nwlondon.csv"), sep = ",")

####################################################
##################### Table 2  #####################
####################################################

###### Overall

#Import
table2.overall.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                                   sheet = "Table 2-2 Overall")

#New names and variables
table2.overall.nwlondon <- table2.overall.nwlondon %>%
  mutate(.,breakdown="overall",breakdown.level="overall") %>%
  dplyr::rename(.,score_band="conditions_aggregated") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band))

#Compute totals and merge in
detach(package:plyr)
table2.total.overall.nwlondon <- table2.overall.nwlondon %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table2.overall.nwlondon <- left_join(table2.overall.nwlondon,table2.total.overall.nwlondon,
                                 by="breakdown.level")
rm(table2.total.overall.nwlondon)

###### Age

#Import
table2.age.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                                   sheet = "Table 2-2 Age")

#New names and variables
table2.age.nwlondon <- table2.age.nwlondon %>%
  mutate(.,breakdown="age") %>%
  dplyr::rename(.,score_band="conditions_aggregated",breakdown.level="age_group") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band)) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".or.older","+")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".to.","-"))

#Compute totals and merge in
detach(package:plyr)
table2.total.age.nwlondon <- table2.age.nwlondon %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table2.age.nwlondon <- left_join(table2.age.nwlondon,table2.total.age.nwlondon,
                                  by="breakdown.level")
rm(table2.total.age.nwlondon)

###### Sex

#Import
table2.sex.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                               sheet = "Table 2-2 Sex")

#New names and variables
table2.sex.nwlondon <- table2.sex.nwlondon %>%
  mutate(.,breakdown="sex") %>%
  dplyr::rename(.,score_band="conditions_aggregated",breakdown.level="sex") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band)) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))

#Compute totals and merge in
detach(package:plyr)
table2.total.sex.nwlondon <- table2.sex.nwlondon %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table2.sex.nwlondon <- left_join(table2.sex.nwlondon,table2.total.sex.nwlondon,
                                  by="breakdown.level")
rm(table2.total.sex.nwlondon)

###### Deprivation

#Import
table2.dep.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                               sheet = "Table 2-2 Deprivation")

#New names and variables
table2.dep.nwlondon <- table2.dep.nwlondon %>%
  mutate(.,breakdown="imd") %>%
  dplyr::rename(.,score_band="conditions_aggregated",breakdown.level="deprivation") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band))

#Compute totals and merge in
detach(package:plyr)
table2.total.dep.nwlondon <- table2.dep.nwlondon %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table2.dep.nwlondon <- left_join(table2.dep.nwlondon,table2.total.dep.nwlondon,
                                  by="breakdown.level")
rm(table2.total.dep.nwlondon)

###### Reason for shielding

#Import
table2.reason.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                               sheet = "Table 2-2 Reason for shielding")

#New names and variables
table2.reason.nwlondon <- table2.reason.nwlondon %>%
  mutate(.,breakdown="reason_shielding") %>%
  dplyr::rename(.,score_band="conditions_aggregated",breakdown.level="reason") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band)) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare.genetic.metabolic.autoimmune","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant"))

#Compute totals and merge in
detach(package:plyr)
table2.total.reason.nwlondon <- table2.reason.nwlondon %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table2.reason.nwlondon <- left_join(table2.reason.nwlondon,table2.total.reason.nwlondon,
                              by="breakdown.level")
rm(table2.total.reason.nwlondon)

###### Append

table2.nwlondon <- plyr::rbind.fill(table2.overall.nwlondon,
                                    table2.age.nwlondon,
                                    table2.sex.nwlondon,
                                    table2.dep.nwlondon,
                                    table2.reason.nwlondon)

table2.nwlondon <- mutate(table2.nwlondon,partner="NW London")

rm(table2.overall.nwlondon,
   table2.age.nwlondon,
   table2.sex.nwlondon,
   table2.dep.nwlondon,
   table2.reason.nwlondon)

fwrite(table2.nwlondon, file = paste0(rawdatadir,"NW London/table2.nwlondon.csv"), sep = ",")

####################################################
##################### Table 3  #####################
####################################################

###### Overall

#Import
table3.overall.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                                   sheet = "Table 2-3 Overall")

#New names and variables
table3.overall.nwlondon <- table3.overall.nwlondon %>%
  mutate(.,breakdown="overall",breakdown.level="overall") %>%
  dplyr::rename(.,windex_vw=van_walraven_score) %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4"))

#Compute totals and merge in
detach(package:plyr)
table3.total.overall.nwlondon <- table3.overall.nwlondon %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table3.overall.nwlondon <- left_join(table3.overall.nwlondon,table3.total.overall.nwlondon,
                                  by="breakdown.level")
rm(table3.total.overall.nwlondon)

###### Age

#Import
table3.age.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                                   sheet = "Table 2-3 Age")

#New names and variables
table3.age.nwlondon <- table3.age.nwlondon %>%
  mutate(.,breakdown="age") %>%
  dplyr::rename(.,breakdown.level="age_group") %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".or.older","+")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".to.","-")) %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4"))

#Compute totals and merge in
detach(package:plyr)
table3.total.age.nwlondon <- table3.age.nwlondon %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table3.age.nwlondon <- left_join(table3.age.nwlondon,table3.total.age.nwlondon,
                                  by="breakdown.level")
rm(table3.total.age.nwlondon)

###### Sex

#Import
table3.sex.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                               sheet = "Table 2-3 Sex")

#New names and variables
table3.sex.nwlondon <- table3.sex.nwlondon %>%
  mutate(.,breakdown="sex") %>%
  dplyr::rename(.,breakdown.level="sex") %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))

#Compute totals and merge in
detach(package:plyr)
table3.total.sex.nwlondon <- table3.sex.nwlondon %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table3.sex.nwlondon <- left_join(table3.sex.nwlondon,table3.total.sex.nwlondon,
                              by="breakdown.level")
rm(table3.total.sex.nwlondon)

###### Deprivation

#Import
table3.dep.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                               sheet = "Table 2-3 Deprivation")

#New names and variables
table3.dep.nwlondon <- table3.dep.nwlondon %>%
  mutate(.,breakdown="imd") %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4")) %>%
  dplyr::rename(.,breakdown.level="deprivation")

#Compute totals and merge in
detach(package:plyr)
table3.total.dep.nwlondon <- table3.dep.nwlondon %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table3.dep.nwlondon <- left_join(table3.dep.nwlondon,table3.total.dep.nwlondon,
                              by="breakdown.level")
rm(table3.total.dep.nwlondon)

###### Reason for shielding

#Import
table3.reason.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 2 Tables - output tableV2.xlsx"),
                               sheet = "Table 2-3 Reason for shielding")

#New names and variables
table3.reason.nwlondon <- table3.reason.nwlondon %>%
  mutate(.,breakdown="reason_shielding") %>%
  dplyr::rename(.,breakdown.level="reason") %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare.genetic.metabolic.autoimmune","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant"))

#Compute totals and merge in
detach(package:plyr)
table3.total.reason.nwlondon <- table3.reason.nwlondon %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table3.reason.nwlondon <- left_join(table3.reason.nwlondon,table3.total.reason.nwlondon,
                              by="breakdown.level")
rm(table3.total.reason.nwlondon)

###### Append

table3.nwlondon <- plyr::rbind.fill(table3.overall.nwlondon,
                                    table3.age.nwlondon,
                                    table3.sex.nwlondon,
                                    table3.dep.nwlondon,
                                    table3.reason.nwlondon)

table3.nwlondon <- mutate(table3.nwlondon,partner="NW London")

rm(table3.overall.nwlondon,
   table3.age.nwlondon,
   table3.sex.nwlondon,
   table3.dep.nwlondon,
   table3.reason.nwlondon)

fwrite(table3.nwlondon, file = paste0(rawdatadir,"NW London/table3.nwlondon.csv"), sep = ",")