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
table1.overall.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                                   sheet = "Table 2-1 Overall comb")

#New variables
table1.overall.lpoolwirral <- mutate(table1.overall.lpoolwirral,
                                  breakdown="overall",
                                  breakdown.level="overall") %>%
  dplyr::rename(.,total.patients=total.patients)

###### Age

#Import and reshape: number patients
table1.age.lpoolwirral_A <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                               sheet = "Table 2-1 Age Comb") %>%
  select(.,condition,starts_with("number.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="age",
         breakdown.level=str_replace_all(breakdown.level,"number.patients_","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29"))
  
#Import and reshape: number admissions
table1.age.lpoolwirral_B <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                                 sheet = "Table 2-1 Age Comb") %>%
  select(.,condition,starts_with("number.admissions")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.admissions="value",breakdown.level="variable") %>%
  mutate(.,breakdown="age",
         breakdown.level=str_replace_all(breakdown.level,"number.admissions_","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29"))

#Import and reshape: totals
table1.age.lpoolwirral_C <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                                 sheet = "Table 2-1 Age Comb") %>%
  select(.,condition,starts_with("total.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,total.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="age",
         breakdown.level=str_replace_all(breakdown.level,"total.patients_","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"30.to.49","30-49")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"50.to.69","50-69")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"70.or.older","70+"))

#Merge all 3 columns: patients, admissions and totals
table1.age.lpoolwirral <- full_join(table1.age.lpoolwirral_A,table1.age.lpoolwirral_B,by=c("condition","breakdown","breakdown.level")) %>%
  full_join(.,table1.age.lpoolwirral_C,by=c("condition","breakdown","breakdown.level"))
rm(table1.age.lpoolwirral_A,table1.age.lpoolwirral_B,table1.age.lpoolwirral_C)

###### Sex

#Import and reshape: number patients
table1.sex.lpoolwirral_A <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                                 sheet = "Table 2-1 Sex Comb") %>%
  select(.,condition,starts_with("number.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="sex",
         breakdown.level=str_replace_all(breakdown.level,"number.patients.sex.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))

#Import and reshape: number admissions
table1.sex.lpoolwirral_B <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                                 sheet = "Table 2-1 Sex Comb") %>%
  select(.,condition,starts_with("number.admissions")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.admissions="value",breakdown.level="variable") %>%
  mutate(.,breakdown="sex",
         breakdown.level=str_replace_all(breakdown.level,"number.admissions.sex.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))

#Import and reshape: totals
table1.sex.lpoolwirral_C <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                                 sheet = "Table 2-1 Sex Comb") %>%
  select(.,condition,starts_with("total.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,total.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="sex",
         breakdown.level=str_replace_all(breakdown.level,"total.patients.sex.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))

#Merge all 3 columns: patients, admissions and totals
table1.sex.lpoolwirral <- full_join(table1.sex.lpoolwirral_A,table1.sex.lpoolwirral_B,by=c("condition","breakdown","breakdown.level")) %>%
  full_join(.,table1.sex.lpoolwirral_C,by=c("condition","breakdown","breakdown.level"))
rm(table1.sex.lpoolwirral_A,table1.sex.lpoolwirral_B,table1.sex.lpoolwirral_C)

###### Deprivation

#Import and reshape: number patients
table1.dep.lpoolwirral_A <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                                 sheet = "Table 2-1 Dep Comb") %>%
  select(.,condition,starts_with("number.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="imd",
         breakdown.level=str_replace_all(breakdown.level,"number.patients.dep.",""))

#Import and reshape: number admissions
table1.dep.lpoolwirral_B <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                                 sheet = "Table 2-1 Dep Comb") %>%
  select(.,condition,starts_with("number.admissions")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.admissions="value",breakdown.level="variable") %>%
  mutate(.,breakdown="imd",
         breakdown.level=str_replace_all(breakdown.level,"number.admissions.dep.",""))

#Import and reshape: totals
table1.dep.lpoolwirral_C <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                                 sheet = "Table 2-1 Dep Comb") %>%
  select(.,condition,starts_with("total.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,total.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="imd",
         breakdown.level=str_replace_all(breakdown.level,"total.patients.dep.",""))

#Merge all 3 columns: patients, admissions and totals
table1.dep.lpoolwirral <- full_join(table1.dep.lpoolwirral_A,table1.dep.lpoolwirral_B,by=c("condition","breakdown","breakdown.level")) %>%
  full_join(.,table1.dep.lpoolwirral_C,by=c("condition","breakdown","breakdown.level"))
rm(table1.dep.lpoolwirral_A,table1.dep.lpoolwirral_B,table1.dep.lpoolwirral_C)

###### Reason for shielding

#Import and reshape: number patients
table1.reason.lpoolwirral_A <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                                 sheet = "Table 2-1 RfS Comb") %>%
  select(.,condition,starts_with("number.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="reason_shielding",
         breakdown.level=str_replace_all(breakdown.level,"number.patients.reason.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare.genetic.metabolic.autoimmune","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant"))

#Import and reshape: number admissions
table1.reason.lpoolwirral_B <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                                 sheet = "Table 2-1 RfS Comb") %>%
  select(.,condition,starts_with("number.admissions")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.admissions="value",breakdown.level="variable") %>%
  mutate(.,breakdown="reason_shielding",
         breakdown.level=str_replace_all(breakdown.level,"number.admissions.reason.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare.genetic.metabolic.autoimmune","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant"))

#Import and reshape: totals
table1.reason.lpoolwirral_C <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                                 sheet = "Table 2-1 RfS Comb") %>%
  select(.,condition,starts_with("total.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,total.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="reason_shielding",
         breakdown.level=str_replace_all(breakdown.level,"total.patients.reason.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare.genetic.metabolic.autoimmune","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant"))

#Merge all 3 columns: patients, admissions and totals
table1.reason.lpoolwirral <- full_join(table1.reason.lpoolwirral_A,table1.reason.lpoolwirral_B,by=c("condition","breakdown","breakdown.level")) %>%
  full_join(.,table1.reason.lpoolwirral_C,by=c("condition","breakdown","breakdown.level"))
rm(table1.reason.lpoolwirral_A,table1.reason.lpoolwirral_B,table1.reason.lpoolwirral_C)

###### Append

table1.lpoolwirral <- plyr::rbind.fill(table1.overall.lpoolwirral,
                              table1.age.lpoolwirral,
                              table1.sex.lpoolwirral,
                              table1.dep.lpoolwirral,
                              table1.reason.lpoolwirral)

table1.lpoolwirral <- mutate(table1.lpoolwirral,partner="Liverpool and Wirral")

rm(table1.overall.lpoolwirral,
   table1.age.lpoolwirral,
   table1.sex.lpoolwirral,
   table1.dep.lpoolwirral,
   table1.reason.lpoolwirral)

fwrite(table1.lpoolwirral, file = paste0(rawdatadir,"Liverpool Wirral/table1.lpoolwirral.csv"), sep = ",")

####################################################
##################### Table 2  #####################
####################################################

###### Overall

#Import
table2.overall.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                                   sheet = "Table 2-2 Overall comb")

#New names and variables
table2.overall.lpoolwirral <- table2.overall.lpoolwirral %>%
  mutate(.,breakdown="overall",breakdown.level="overall") %>%
  dplyr::rename(.,score_band="conditions_aggregated") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band))

#Compute totals and merge in
detach(package:plyr)
table2.total.overall.lpoolwirral <- table2.overall.lpoolwirral %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table2.overall.lpoolwirral <- left_join(table2.overall.lpoolwirral,table2.total.overall.lpoolwirral,
                                 by="breakdown.level")
rm(table2.total.overall.lpoolwirral)

###### Age

#Import
table2.age.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                                   sheet = "Table 2-2 Age Comb")

#New names and variables
table2.age.lpoolwirral <- table2.age.lpoolwirral %>%
  mutate(.,breakdown="age") %>%
  dplyr::rename(.,score_band="conditions_aggregated",breakdown.level="age_group") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band)) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29"))

#Compute totals and merge in
detach(package:plyr)
table2.total.age.lpoolwirral <- table2.age.lpoolwirral %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table2.age.lpoolwirral <- left_join(table2.age.lpoolwirral,table2.total.age.lpoolwirral,
                                  by="breakdown.level")
rm(table2.total.age.lpoolwirral)

###### Sex

#Import
table2.sex.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                               sheet = "Table 2-2 Sex comb")

#New names and variables
table2.sex.lpoolwirral <- table2.sex.lpoolwirral %>%
  mutate(.,breakdown="sex") %>%
  dplyr::rename(.,score_band="conditions_aggregated",breakdown.level="sex") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band)) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"unknown/other","unknown_other"))

#Compute totals and merge in
detach(package:plyr)
table2.total.sex.lpoolwirral <- table2.sex.lpoolwirral %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table2.sex.lpoolwirral <- left_join(table2.sex.lpoolwirral,table2.total.sex.lpoolwirral,
                                  by="breakdown.level")
rm(table2.total.sex.lpoolwirral)

###### Deprivation

#Import
table2.dep.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                               sheet = "Table 2-2 Dep Comb")

#New names and variables
table2.dep.lpoolwirral <- table2.dep.lpoolwirral %>%
  mutate(.,breakdown="imd") %>%
  dplyr::rename(.,score_band="conditions_aggregated",breakdown.level="deprivation") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band))

#Compute totals and merge in
detach(package:plyr)
table2.total.dep.lpoolwirral <- table2.dep.lpoolwirral %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table2.dep.lpoolwirral <- left_join(table2.dep.lpoolwirral,table2.total.dep.lpoolwirral,
                                  by="breakdown.level")
rm(table2.total.dep.lpoolwirral)

###### Reason for shielding

#Import
table2.reason.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                               sheet = "Table 2-2 RfS Comb")

#New names and variables
table2.reason.lpoolwirral <- table2.reason.lpoolwirral %>%
  mutate(.,breakdown="reason_shielding") %>%
  dplyr::rename(.,score_band="conditions_aggregated",breakdown.level="reason") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band)) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare.genetic.metabolic.autoimmune","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant"))

#Compute totals and merge in
detach(package:plyr)
table2.total.reason.lpoolwirral <- table2.reason.lpoolwirral %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table2.reason.lpoolwirral <- left_join(table2.reason.lpoolwirral,table2.total.reason.lpoolwirral,
                              by="breakdown.level")
rm(table2.total.reason.lpoolwirral)

###### Append

table2.lpoolwirral <- plyr::rbind.fill(table2.overall.lpoolwirral,
                                    table2.age.lpoolwirral,
                                    table2.sex.lpoolwirral,
                                    table2.dep.lpoolwirral,
                                    table2.reason.lpoolwirral)

table2.lpoolwirral <- mutate(table2.lpoolwirral,partner="Liverpool and Wirral")

rm(table2.overall.lpoolwirral,
   table2.age.lpoolwirral,
   table2.sex.lpoolwirral,
   table2.dep.lpoolwirral,
   table2.reason.lpoolwirral)

fwrite(table2.lpoolwirral, file = paste0(rawdatadir,"Liverpool Wirral/table2.lpoolwirral.csv"), sep = ",")

####################################################
##################### Table 3  #####################
####################################################

###### Overall

#Import
table3.overall.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                                   sheet = "Table 2-3 Overall comb")

#New names and variables
table3.overall.lpoolwirral <- table3.overall.lpoolwirral %>%
  mutate(.,breakdown="overall",breakdown.level="overall") %>%
  dplyr::rename(.,windex_vw=van_walraven_score) %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4"))

#Compute totals and merge in
detach(package:plyr)
table3.total.overall.lpoolwirral <- table3.overall.lpoolwirral %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table3.overall.lpoolwirral <- left_join(table3.overall.lpoolwirral,table3.total.overall.lpoolwirral,
                                  by="breakdown.level")
rm(table3.total.overall.lpoolwirral)

###### Age

#Import
table3.age.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                                   sheet = "Table 2-3 Age Comb")

#New names and variables
table3.age.lpoolwirral <- table3.age.lpoolwirral %>%
  mutate(.,breakdown="age") %>%
  dplyr::rename(.,breakdown.level="age_group") %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4"))

#Compute totals and merge in
detach(package:plyr)
table3.total.age.lpoolwirral <- table3.age.lpoolwirral %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table3.age.lpoolwirral <- left_join(table3.age.lpoolwirral,table3.total.age.lpoolwirral,
                                  by="breakdown.level")
rm(table3.total.age.lpoolwirral)

###### Sex

#Import
table3.sex.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                               sheet = "Table 2-3 Sex Comb")

#New names and variables
table3.sex.lpoolwirral <- table3.sex.lpoolwirral %>%
  mutate(.,breakdown="sex") %>%
  dplyr::rename(.,breakdown.level="sex") %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"unknown/other","unknown_other"))

#Compute totals and merge in
detach(package:plyr)
table3.total.sex.lpoolwirral <- table3.sex.lpoolwirral %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table3.sex.lpoolwirral <- left_join(table3.sex.lpoolwirral,table3.total.sex.lpoolwirral,
                              by="breakdown.level")
rm(table3.total.sex.lpoolwirral)

###### Deprivation

#Import
table3.dep.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                               sheet = "Table 2-3 Dep Comb")

#New names and variables
table3.dep.lpoolwirral <- table3.dep.lpoolwirral %>%
  mutate(.,breakdown="imd") %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4")) %>%
  dplyr::rename(.,breakdown.level="deprivation")

#Compute totals and merge in
detach(package:plyr)
table3.total.dep.lpoolwirral <- table3.dep.lpoolwirral %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table3.dep.lpoolwirral <- left_join(table3.dep.lpoolwirral,table3.total.dep.lpoolwirral,
                              by="breakdown.level")
rm(table3.total.dep.lpoolwirral)

###### Reason for shielding

#Import
table3.reason.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool Wirral/","Output 2 Tables Liverpool and Wirral combined 29012021.xlsx"),
                               sheet = "Table 2-3 RfS Comb")

#New names and variables
table3.reason.lpoolwirral <- table3.reason.lpoolwirral %>%
  mutate(.,breakdown="reason_shielding") %>%
  dplyr::rename(.,breakdown.level="reason") %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare.genetic.metabolic.autoimmune","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant"))

#Compute totals and merge in
detach(package:plyr)
table3.total.reason.lpoolwirral <- table3.reason.lpoolwirral %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients = sum(number.patients)) %>%
  ungroup()
table3.reason.lpoolwirral <- left_join(table3.reason.lpoolwirral,table3.total.reason.lpoolwirral,
                              by="breakdown.level")
rm(table3.total.reason.lpoolwirral)

###### Append

table3.lpoolwirral <- plyr::rbind.fill(table3.overall.lpoolwirral,
                                    table3.age.lpoolwirral,
                                    table3.sex.lpoolwirral,
                                    table3.dep.lpoolwirral,
                                    table3.reason.lpoolwirral)

table3.lpoolwirral <- mutate(table3.lpoolwirral,partner="Liverpool and Wirral")

rm(table3.overall.lpoolwirral,
   table3.age.lpoolwirral,
   table3.sex.lpoolwirral,
   table3.dep.lpoolwirral,
   table3.reason.lpoolwirral)

fwrite(table3.lpoolwirral, file = paste0(rawdatadir,"Liverpool Wirral/table3.lpoolwirral.csv"), sep = ",")