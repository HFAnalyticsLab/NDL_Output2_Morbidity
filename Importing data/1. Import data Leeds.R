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
table1.overall.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                                   sheet = "Table 2-1 Overall")

#New variables
table1.overall.leeds <- mutate(table1.overall.leeds,
                                  breakdown="overall",
                                  breakdown.level="overall") %>%
  dplyr::rename(.,total.patients.adm=total.patients)

###### Age

#Import and reshape: number patients
table1.age.leeds_A <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                               sheet = "Table 2-1 Age") %>%
  select(.,condition,starts_with("number.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="age",
         breakdown.level=str_replace_all(breakdown.level,"number.patients.age.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29"))
  
#Import and reshape: number admissions
table1.age.leeds_B <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                                 sheet = "Table 2-1 Age") %>%
  select(.,condition,starts_with("number.admissions")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.admissions="value",breakdown.level="variable") %>%
  mutate(.,breakdown="age",
         breakdown.level=str_replace_all(breakdown.level,"number.admissions.age.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29"))

#Import and reshape: totals
table1.age.leeds_C <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                                 sheet = "Table 2-1 Age") %>%
  select(.,condition,starts_with("total.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,total.patients.adm="value",breakdown.level="variable") %>%
  mutate(.,breakdown="age",
         breakdown.level=str_replace_all(breakdown.level,"total.patients.age.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"less.than.30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"30.to.49","30-49")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"50.to.69","50-69")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"70.or.older","70+"))

#Merge all 3 columns: patients, admissions and totals
table1.age.leeds <- full_join(table1.age.leeds_A,table1.age.leeds_B,by=c("condition","breakdown","breakdown.level")) %>%
  full_join(.,table1.age.leeds_C,by=c("condition","breakdown","breakdown.level"))
rm(table1.age.leeds_A,table1.age.leeds_B,table1.age.leeds_C)

###### Sex

#Import and reshape: number patients
table1.sex.leeds_A <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                                 sheet = "Table 2-1 Sex") %>%
  select(.,condition,starts_with("number.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="sex",
         breakdown.level=str_replace_all(breakdown.level,"number.patients.sex.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))

#Import and reshape: number admissions
table1.sex.leeds_B <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                                 sheet = "Table 2-1 Sex") %>%
  select(.,condition,starts_with("number.admissions")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.admissions="value",breakdown.level="variable") %>%
  mutate(.,breakdown="sex",
         breakdown.level=str_replace_all(breakdown.level,"number.admissions.sex.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))

#Import and reshape: totals
table1.sex.leeds_C <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                                 sheet = "Table 2-1 Sex") %>%
  select(.,condition,starts_with("total.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,total.patients.adm="value",breakdown.level="variable") %>%
  mutate(.,breakdown="sex",
         breakdown.level=str_replace_all(breakdown.level,"total.patients.sex.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))

#Merge all 3 columns: patients, admissions and totals
table1.sex.leeds <- full_join(table1.sex.leeds_A,table1.sex.leeds_B,by=c("condition","breakdown","breakdown.level")) %>%
  full_join(.,table1.sex.leeds_C,by=c("condition","breakdown","breakdown.level"))
rm(table1.sex.leeds_A,table1.sex.leeds_B,table1.sex.leeds_C)

###### Deprivation

#Import and reshape: number patients
table1.dep.leeds_A <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                                 sheet = "Table 2-1 Deprivation") %>%
  select(.,condition,starts_with("number.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="imd",
         breakdown.level=str_replace_all(breakdown.level,"number.patients.dep.",""))

#Import and reshape: number admissions
table1.dep.leeds_B <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                                 sheet = "Table 2-1 Deprivation") %>%
  select(.,condition,starts_with("number.admissions")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.admissions="value",breakdown.level="variable") %>%
  mutate(.,breakdown="imd",
         breakdown.level=str_replace_all(breakdown.level,"number.admissions.dep.",""))

#Import and reshape: totals
table1.dep.leeds_C <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                                 sheet = "Table 2-1 Deprivation") %>%
  select(.,condition,starts_with("total.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,total.patients.adm="value",breakdown.level="variable") %>%
  mutate(.,breakdown="imd",
         breakdown.level=str_replace_all(breakdown.level,"total.patients.dep.",""))

#Merge all 3 columns: patients, admissions and totals
table1.dep.leeds <- full_join(table1.dep.leeds_A,table1.dep.leeds_B,by=c("condition","breakdown","breakdown.level")) %>%
  full_join(.,table1.dep.leeds_C,by=c("condition","breakdown","breakdown.level"))
rm(table1.dep.leeds_A,table1.dep.leeds_B,table1.dep.leeds_C)

###### Reason for shielding

#Import and reshape: number patients
table1.reason.leeds_A <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                                 sheet = "Table 2-1 Reasons for Shielding") %>%
  select(.,condition,starts_with("number.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.patients="value",breakdown.level="variable") %>%
  mutate(.,breakdown="reason_shielding",
         breakdown.level=str_replace_all(breakdown.level,"number.patients.reason.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare_genetic_metabolic_autoimmune","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant"))

#Import and reshape: number admissions
table1.reason.leeds_B <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                                 sheet = "Table 2-1 Reasons for Shielding") %>%
  select(.,condition,starts_with("number.admissions")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,number.admissions="value",breakdown.level="variable") %>%
  mutate(.,breakdown="reason_shielding",
         breakdown.level=str_replace_all(breakdown.level,"number.admissions.reason.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare_genetic_metabolic_autoimmune","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant"))

#Import and reshape: totals
table1.reason.leeds_C <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                                 sheet = "Table 2-1 Reasons for Shielding") %>%
  select(.,condition,starts_with("total.patients")) %>%
  reshape2::melt(., id.vars=c("condition")) %>%
  dplyr::rename(.,total.patients.adm="value",breakdown.level="variable") %>%
  mutate(.,breakdown="reason_shielding",
         breakdown.level=str_replace_all(breakdown.level,"total.patients.reason.","")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare_genetic_metabolic_autoimmune","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant"))

#Merge all 3 columns: patients, admissions and totals
table1.reason.leeds <- full_join(table1.reason.leeds_A,table1.reason.leeds_B,by=c("condition","breakdown","breakdown.level")) %>%
  full_join(.,table1.reason.leeds_C,by=c("condition","breakdown","breakdown.level"))
rm(table1.reason.leeds_A,table1.reason.leeds_B,table1.reason.leeds_C)

###### Append

table1.leeds <- plyr::rbind.fill(table1.overall.leeds,
                              table1.age.leeds,
                              table1.sex.leeds,
                              table1.dep.leeds,
                              table1.reason.leeds)

table1.leeds <- mutate(table1.leeds,partner="Leeds")

rm(table1.overall.leeds,
   table1.age.leeds,
   table1.sex.leeds,
   table1.dep.leeds,
   table1.reason.leeds)

fwrite(table1.leeds, file = paste0(rawdatadir,"Leeds/table1.leeds.csv"), sep = ",")

####################################################
##################### Table 2  #####################
####################################################

###### Overall

#Import
table2.overall.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                                   sheet = "Table 2-2 Overall")

#New names and variables
table2.overall.leeds <- table2.overall.leeds %>%
  mutate(.,breakdown="overall",breakdown.level="overall") %>%
  dplyr::rename(.,score_band="conditions_aggregated") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band))

#Compute totals and merge in
detach(package:plyr)
table2.total.overall.leeds <- table2.overall.leeds %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table2.overall.leeds <- left_join(table2.overall.leeds,table2.total.overall.leeds,
                                 by="breakdown.level")
rm(table2.total.overall.leeds)

###### Age

#Import
table2.age.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                                   sheet = "Table 2-2 Age")

#New names and variables
table2.age.leeds <- table2.age.leeds %>%
  mutate(.,breakdown="age") %>%
  dplyr::rename(.,score_band="conditions_aggregated",breakdown.level="Age_Group") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band)) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29"))

#Compute totals and merge in
detach(package:plyr)
table2.total.age.leeds <- table2.age.leeds %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table2.age.leeds <- left_join(table2.age.leeds,table2.total.age.leeds,
                                  by="breakdown.level")
rm(table2.total.age.leeds)

###### Sex

#Import
table2.sex.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                               sheet = "Table 2-2 Sex")

#New names and variables
table2.sex.leeds <- table2.sex.leeds %>%
  mutate(.,breakdown="sex") %>%
  dplyr::rename(.,score_band="conditions_aggregated",breakdown.level="Sex") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band)) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))

#Compute totals and merge in
detach(package:plyr)
table2.total.sex.leeds <- table2.sex.leeds %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table2.sex.leeds <- left_join(table2.sex.leeds,table2.total.sex.leeds,
                                  by="breakdown.level")
rm(table2.total.sex.leeds)

###### Deprivation

#Import
table2.dep.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                               sheet = "Table 2-2 Deprivation")

#New names and variables
table2.dep.leeds <- table2.dep.leeds %>%
  mutate(.,breakdown="imd") %>%
  dplyr::rename(.,score_band="conditions_aggregated",breakdown.level="IMD_Quintile") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band))

#Compute totals and merge in
detach(package:plyr)
table2.total.dep.leeds <- table2.dep.leeds %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table2.dep.leeds <- left_join(table2.dep.leeds,table2.total.dep.leeds,
                                  by="breakdown.level")
rm(table2.total.dep.leeds)

###### Reason for shielding

#Import
table2.reason.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                               sheet = "Table 2-2 Reasons for Shielding")

#New names and variables
table2.reason.leeds <- table2.reason.leeds %>%
  mutate(.,breakdown="reason_shielding") %>%
  dplyr::rename(.,score_band="conditions_aggregated",breakdown.level="reason") %>%
  mutate(.,score_band=ifelse(score_band=="2 or more",2,score_band)) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare_genetic_metabolic_autoimmune","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant"))

#Compute totals and merge in
detach(package:plyr)
table2.total.reason.leeds <- table2.reason.leeds %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = NA) %>%
  ungroup()
table2.reason.leeds <- left_join(table2.reason.leeds,table2.total.reason.leeds,
                              by="breakdown.level")
rm(table2.total.reason.leeds)

###### Append

table2.leeds <- plyr::rbind.fill(table2.overall.leeds,
                                    table2.age.leeds,
                                    table2.sex.leeds,
                                    table2.dep.leeds,
                                    table2.reason.leeds)

table2.leeds <- mutate(table2.leeds,partner="Leeds")

rm(table2.overall.leeds,
   table2.age.leeds,
   table2.sex.leeds,
   table2.dep.leeds,
   table2.reason.leeds)

fwrite(table2.leeds, file = paste0(rawdatadir,"Leeds/table2.leeds.csv"), sep = ",")

####################################################
##################### Table 3  #####################
####################################################

###### Overall

#Import
table3.overall.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                                   sheet = "Table 2-3 Overall")

#New names and variables
table3.overall.leeds <- table3.overall.leeds %>%
  mutate(.,breakdown="overall",breakdown.level="overall") %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4"))

#Compute totals and merge in
detach(package:plyr)
table3.total.overall.leeds <- table3.overall.leeds %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table3.overall.leeds <- left_join(table3.overall.leeds,table3.total.overall.leeds,
                                  by="breakdown.level")
rm(table3.total.overall.leeds)

###### Age

#Import
table3.age.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                                   sheet = "Table 2-3 Age")

#New names and variables
table3.age.leeds <- table3.age.leeds %>%
  mutate(.,breakdown="age") %>%
  dplyr::rename(.,breakdown.level="Age_Group") %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4"))

#Compute totals and merge in
detach(package:plyr)
table3.total.age.leeds <- table3.age.leeds %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table3.age.leeds <- left_join(table3.age.leeds,table3.total.age.leeds,
                                  by="breakdown.level")
rm(table3.total.age.leeds)

###### Sex

#Import
table3.sex.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                               sheet = "Table 2-3 Sex")

#New names and variables
table3.sex.leeds <- table3.sex.leeds %>%
  mutate(.,breakdown="sex") %>%
  dplyr::rename(.,breakdown.level="Sex") %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))
  
#Compute totals and merge in
detach(package:plyr)
table3.total.sex.leeds <- table3.sex.leeds %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table3.sex.leeds <- left_join(table3.sex.leeds,table3.total.sex.leeds,
                              by="breakdown.level")
rm(table3.total.sex.leeds)

###### Deprivation

#Import
table3.dep.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                               sheet = "Table 2-3 Deprivation")

#New names and variables
table3.dep.leeds <- table3.dep.leeds %>%
  mutate(.,breakdown="imd") %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4")) %>%
  dplyr::rename(.,breakdown.level="IMD_Quintile")

#Compute totals and merge in
detach(package:plyr)
table3.total.dep.leeds <- table3.dep.leeds %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table3.dep.leeds <- left_join(table3.dep.leeds,table3.total.dep.leeds,
                              by="breakdown.level")
rm(table3.total.dep.leeds)

###### Reason for shielding

#Import
table3.reason.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_2B_Tables_Leeds5.xlsx"),
                               sheet = "Table 2-3 Reasons for Shielding")

#New names and variables
table3.reason.leeds <- table3.reason.leeds %>%
  mutate(.,breakdown="reason_shielding") %>%
  dplyr::rename(.,breakdown.level="reason") %>%
  mutate(.,windex_vw=str_replace_all(windex_vw,"1-4","1 to 4")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"rare_genetic_metabolic_autoimmune","rare disease")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"transplants","transplant"))

#Compute totals and merge in
detach(package:plyr)
table3.total.reason.leeds <- table3.reason.leeds %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = NA) %>%
  ungroup()
table3.reason.leeds <- left_join(table3.reason.leeds,table3.total.reason.leeds,
                              by="breakdown.level")
rm(table3.total.reason.leeds)

###### Append

table3.leeds <- plyr::rbind.fill(table3.overall.leeds,
                                    table3.age.leeds,
                                    table3.sex.leeds,
                                    table3.dep.leeds,
                                    table3.reason.leeds)

table3.leeds <- mutate(table3.leeds,partner="Leeds")

rm(table3.overall.leeds,
   table3.age.leeds,
   table3.sex.leeds,
   table3.dep.leeds,
   table3.reason.leeds)

fwrite(table3.leeds, file = paste0(rawdatadir,"Leeds/table3.leeds.csv"), sep = ",")