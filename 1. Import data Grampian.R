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
table1.overall.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.1_overall.csv"),
                                 header=TRUE, sep=",", check.names=T) %>%
  t(.) %>% as.data.frame()

#Clean up structure and variable names
colnames(table1.overall.grampian) <- table1.overall.grampian[1,]
table1.overall.grampian <- table1.overall.grampian[-1,]
table1.overall.grampian$condition <- rownames(table1.overall.grampian)
rownames(table1.overall.grampian) <- c()

#New variables
total.patients.grampian <- filter(table1.overall.grampian,condition=="n_people") %>%
  select(.,`any diagnosis`) %>% as.numeric()
table1.overall.grampian <- mutate(table1.overall.grampian,
                                  breakdown="overall",
                                  breakdown.level="overall",
                                  total.patients.adm=total.patients.grampian,
                                  `any diagnosis`=as.numeric(`any diagnosis`),
                                  `last main condition`=as.numeric(`last main condition`)) %>%
  filter(.,condition!="n_people")
rm(total.patients.grampian)

#Cleaning
table1.overall.grampian$`any diagnosis`[which(table1.overall.grampian$condition=="total_diagnoses")] <- NA

#New names
table1.overall.grampian <- dplyr::rename(table1.overall.grampian,
                                         number.patients=`any diagnosis`,
                                         number.admissions=`last main condition`)

###### Age

#Import patients and reshape
table1.age.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.1_age.csv"),
                             header=TRUE, sep=",", check.names=T)
table1.age.grampian <- gather(table1.age.grampian, condition, count, n_people:depre, factor_key=TRUE)

#Import admissions and reshape
table1.age.adm.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.1_age_admissions.csv"),
                             header=TRUE, sep=",", check.names=T)
table1.age.adm.grampian <- gather(table1.age.adm.grampian, condition, count, n_people:depre, factor_key=TRUE) %>%
  filter(.,!(condition %in% c("n_people","n_admissions"))) %>%
  dplyr::rename(.,number.admissions=count)
table1.age.grampian <- left_join(table1.age.grampian,table1.age.adm.grampian,by=c("age_band","condition"))
rm(table1.age.adm.grampian)

#Merge in totals
table1.age.grampian.totals <- filter(table1.age.grampian,condition=="n_people") %>%
  dplyr::rename(.,total.patients.adm=count) %>%
  select(.,-c("condition","number.admissions"))
table1.age.grampian <- filter(table1.age.grampian,condition!="n_people")
table1.age.grampian <- left_join(table1.age.grampian,table1.age.grampian.totals,
                                 by="age_band")
rm(table1.age.grampian.totals)

#New names
table1.age.grampian <- table1.age.grampian %>%
  dplyr::rename(., number.patients=count,breakdown.level=age_band) %>%
  mutate(.,breakdown="age")

###### Sex

#Import and reshape
table1.sex.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.1_sex.csv"),
                             header=TRUE, sep=",", check.names=T)
table1.sex.grampian <- gather(table1.sex.grampian, condition, count, n_people:depre, factor_key=TRUE)

#Import admissions and reshape
table1.sex.adm.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.1_sex_admissions.csv"),
                                 header=TRUE, sep=",", check.names=T)
table1.sex.adm.grampian <- gather(table1.sex.adm.grampian, condition, count, n_people:depre, factor_key=TRUE) %>%
  filter(.,!(condition %in% c("n_people","n_admissions"))) %>%
  dplyr::rename(.,number.admissions=count)
table1.sex.grampian <- left_join(table1.sex.grampian,table1.sex.adm.grampian,by=c("sex","condition"))
rm(table1.sex.adm.grampian)

#Merge in totals
table1.sex.grampian.totals <- filter(table1.sex.grampian,condition=="n_people") %>%
  dplyr::rename(.,total.patients.adm=count) %>%
  select(.,-c("condition","number.admissions"))
table1.sex.grampian <- filter(table1.sex.grampian,condition!="n_people")
table1.sex.grampian <- left_join(table1.sex.grampian,table1.sex.grampian.totals,
                                 by="sex")
rm(table1.sex.grampian.totals)

#New names
table1.sex.grampian <- table1.sex.grampian %>%
  dplyr::rename(., number.patients=count,breakdown.level=sex) %>%
  mutate(.,breakdown="sex")

###### Deprivation

#Import and reshape
table1.dep.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.1_deprivation.csv"),
                             header=TRUE, sep=",", check.names=T)
table1.dep.grampian <- gather(table1.dep.grampian, condition, count, n_people:depre, factor_key=TRUE)

#Import admissions and reshape
table1.dep.adm.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.1_simd_admissions.csv"),
                                 header=TRUE, sep=",", check.names=T)
table1.dep.adm.grampian <- gather(table1.dep.adm.grampian, condition, count, n_people:depre, factor_key=TRUE) %>%
  filter(.,!(condition %in% c("n_people","n_admissions"))) %>%
  dplyr::rename(.,number.admissions=count)
table1.dep.grampian <- left_join(table1.dep.grampian,table1.dep.adm.grampian,by=c("simd_quintile","condition"))
rm(table1.dep.adm.grampian)

#Merge in totals
table1.dep.grampian.totals <- filter(table1.dep.grampian,condition=="n_people") %>%
  dplyr::rename(.,total.patients.adm=count) %>%
  select(.,-c("condition","number.admissions"))
table1.dep.grampian <- filter(table1.dep.grampian,condition!="n_people")
table1.dep.grampian <- left_join(table1.dep.grampian,table1.dep.grampian.totals,
                                 by="simd_quintile")
rm(table1.dep.grampian.totals)

#New names
table1.dep.grampian <- table1.dep.grampian %>%
  dplyr::rename(., number.patients=count,breakdown.level=simd_quintile) %>%
  mutate(.,breakdown="imd")

###### Reason for shielding

#Import and reshape
table1.reason.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.1_reasons.csv"),
                                header=TRUE, sep=",", check.names=T)
table1.reason.grampian <- gather(table1.reason.grampian, condition, count, n_people:depre, factor_key=TRUE)

#Import admissions and reshape
table1.reason.adm.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.1_reasons_admissions.csv"),
                                 header=TRUE, sep=",", check.names=T)
table1.reason.adm.grampian <- gather(table1.reason.adm.grampian, condition, count, n_people:depre, factor_key=TRUE) %>%
  filter(.,!(condition %in% c("n_people","n_admissions"))) %>%
  dplyr::rename(.,number.admissions=count)
table1.reason.grampian <- left_join(table1.reason.grampian,table1.reason.adm.grampian,by=c("shielding_group_description","condition"))
rm(table1.reason.adm.grampian)

#Merge in totals
table1.dep.grampian.totals <- filter(table1.reason.grampian,condition=="n_people") %>%
  dplyr::rename(.,total.patients.adm=count) %>%
  select(.,-c("condition","number.admissions"))
table1.reason.grampian <- filter(table1.reason.grampian,condition!="n_people")
table1.reason.grampian <- left_join(table1.reason.grampian,table1.dep.grampian.totals,
                                    by="shielding_group_description")
rm(table1.dep.grampian.totals)

#New names
table1.reason.grampian <- table1.reason.grampian %>%
  dplyr::rename(., number.patients=count,breakdown.level=shielding_group_description) %>%
  mutate(.,breakdown="reason_shielding")

###### Append

table1.grampian <- rbind.fill(table1.overall.grampian,
                              table1.age.grampian,
                              table1.sex.grampian,
                              table1.dep.grampian,
                              table1.reason.grampian)

table1.grampian <- mutate(table1.grampian,partner="Grampian")

rm(table1.overall.grampian,
   table1.age.grampian,
   table1.sex.grampian,
   table1.dep.grampian,
   table1.reason.grampian)

fwrite(table1.grampian, file = paste0(rawdatadir,"Grampian-Aberdeen/table1.grampian.csv"), sep = ",")

####################################################
##################### Table 2  #####################
####################################################

###### Overall

#Import
table2.overall.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.2_overall.csv"),
                                 header=TRUE, sep=",", check.names=T)

#New names and variables
table2.overall.grampian <- table2.overall.grampian %>%
  dplyr::rename(.,number.patients=`n_people`) %>%
  mutate(.,breakdown="overall",breakdown.level="overall")

#Merge in totals
table2.totals.overall.grampian <- filter(table2.overall.grampian,score_band=="Total") %>%
  select(.,number.patients) %>% as.numeric()
table2.overall.grampian <- mutate(table2.overall.grampian,total.patients.adm=table2.totals.overall.grampian)
rm(table2.totals.overall.grampian)
table2.overall.grampian <- filter(table2.overall.grampian,score_band!="Total")

###### Age

#Import
table2.age.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.2_age.csv"),
                             header=TRUE, sep=",", check.names=T) %>%
  filter(.,age_band!="Total")

#New names and variables
table2.age.grampian <- table2.age.grampian %>%
  dplyr::rename(.,number.patients=`n_people`,breakdown.level=age_band) %>%
  mutate(.,breakdown="age")

#Compute totals and merge in
detach(package:plyr)
table2.totals.age.grampian <- table2.age.grampian %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table2.age.grampian <- left_join(table2.age.grampian,table2.totals.age.grampian,
                                 by="breakdown.level")
rm(table2.totals.age.grampian)

###### Sex

#Import
table2.sex.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.2_sex.csv"),
                             header=TRUE, sep=",", check.names=T) %>%
  filter(.,sex!="Total")

#New names and variables
table2.sex.grampian <- table2.sex.grampian %>%
  dplyr::rename(.,number.patients=`n_people`,breakdown.level=sex) %>%
  mutate(.,breakdown="sex")

#Compute totals and merge in
detach(package:plyr)
table2.totals.sex.grampian <- table2.sex.grampian %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table2.sex.grampian <- left_join(table2.sex.grampian,table2.totals.sex.grampian,
                                 by="breakdown.level")
rm(table2.totals.sex.grampian)

###### Deprivation

#Import
table2.dep.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.2_deprivation.csv"),
                             header=TRUE, sep=",", check.names=T) %>%
  filter(.,simd_quintile!="Total")

#New names and variables
table2.dep.grampian <- table2.dep.grampian %>%
  dplyr::rename(.,number.patients=`n_people`,breakdown.level=simd_quintile) %>%
  mutate(.,breakdown="imd")

#Compute totals and merge in
detach(package:plyr)
table2.totals.dep.grampian <- table2.dep.grampian %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table2.dep.grampian <- left_join(table2.dep.grampian,table2.totals.dep.grampian,
                                 by="breakdown.level")
rm(table2.totals.dep.grampian)

###### Reason for shielding

#Import
table2.reason.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.2_reasons.csv"),
                                header=TRUE, sep=",", check.names=T) %>%
  filter(.,shielding_group_description!="Total")

#New names and variables
table2.reason.grampian <- table2.reason.grampian %>%
  dplyr::rename(.,number.patients=`n_people`,breakdown.level=shielding_group_description) %>%
  mutate(.,breakdown="reason_shielding")

#Compute totals and merge in
detach(package:plyr)
table2.totals.reason.grampian <- table2.reason.grampian %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table2.reason.grampian <- left_join(table2.reason.grampian,table2.totals.reason.grampian,
                                    by="breakdown.level")
rm(table2.totals.reason.grampian)

###### Append

table2.grampian <- plyr::rbind.fill(table2.overall.grampian,
                                    table2.age.grampian,
                                    table2.sex.grampian,
                                    table2.dep.grampian,
                                    table2.reason.grampian)

table2.grampian <- mutate(table2.grampian,partner="Grampian")

rm(table2.overall.grampian,
   table2.age.grampian,
   table2.sex.grampian,
   table2.dep.grampian,
   table2.reason.grampian)

fwrite(table2.grampian, file = paste0(rawdatadir,"Grampian-Aberdeen/table2.grampian.csv"), sep = ",")

####################################################
##################### Table 3  #####################
####################################################

###### Overall

#Import
table3.overall.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.3_overall.csv"),
                                 header=TRUE, sep=",", check.names=T)

#New names and variables
table3.overall.grampian <- table3.overall.grampian %>%
  dplyr::rename(.,number.patients=`n_people`) %>%
  mutate(.,breakdown="overall",breakdown.level="overall")

#Merge in totals
table3.totals.overall.grampian <- filter(table3.overall.grampian,windex_vw=="Total") %>%
  select(.,number.patients) %>% as.numeric()
table3.overall.grampian <- mutate(table3.overall.grampian,total.patients.adm=table3.totals.overall.grampian)
rm(table3.totals.overall.grampian)
table3.overall.grampian <- filter(table3.overall.grampian,windex_vw!="Total")

###### Age

#Import
table3.age.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.3_age.csv"),
                             header=TRUE, sep=",", check.names=T) %>%
  filter(.,age_band!="Total")

#New names and variables
table3.age.grampian <- table3.age.grampian %>%
  dplyr::rename(.,number.patients=`n_people`,breakdown.level=age_band) %>%
  mutate(.,breakdown="age")

#Compute totals and merge in
detach(package:plyr)
table3.totals.age.grampian <- table3.age.grampian %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table3.age.grampian <- left_join(table3.age.grampian,table3.totals.age.grampian,
                                 by="breakdown.level")
rm(table3.totals.age.grampian)

###### Sex

#Import
table3.sex.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.3_sex.csv"),
                             header=TRUE, sep=",", check.names=T) %>%
  filter(.,sex!="Total")

#New names and variables
table3.sex.grampian <- table3.sex.grampian %>%
  dplyr::rename(.,number.patients=`n_people`,breakdown.level=sex) %>%
  mutate(.,breakdown="sex")

#Compute totals and merge in
detach(package:plyr)
table3.totals.sex.grampian <- table3.sex.grampian %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table3.sex.grampian <- left_join(table3.sex.grampian,table3.totals.sex.grampian,
                                 by="breakdown.level")
rm(table3.totals.sex.grampian)

###### Deprivation

#Import
table3.dep.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.3_deprivation.csv"),
                             header=TRUE, sep=",", check.names=T) %>%
  filter(.,simd_quintile!="Total")

#New names and variables
table3.dep.grampian <- table3.dep.grampian %>%
  dplyr::rename(.,number.patients=`n_people`,breakdown.level=simd_quintile) %>%
  mutate(.,breakdown="imd")

#Compute totals and merge in
detach(package:plyr)
table3.totals.dep.grampian <- table3.dep.grampian %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table3.dep.grampian <- left_join(table3.dep.grampian,table3.totals.dep.grampian,
                                 by="breakdown.level")
rm(table3.totals.dep.grampian)

###### Reason for shielding

#Import
table3.reason.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/Grampian_morbidity/","table_2.3_reasons.csv"),
                                header=TRUE, sep=",", check.names=T) %>%
  filter(.,shielding_group_description!="Total") %>%
  select(.,-V4)

#New names and variables
table3.reason.grampian <- table3.reason.grampian %>%
  dplyr::rename(.,number.patients=`n_people`,breakdown.level=shielding_group_description) %>%
  mutate(.,breakdown="reason_shielding")

#Compute totals and merge in
detach(package:plyr)
table3.totals.reason.grampian <- table3.reason.grampian %>%
  group_by(breakdown.level) %>% 
  summarise(total.patients.adm = sum(number.patients)) %>%
  ungroup()
table3.reason.grampian <- left_join(table3.reason.grampian,table3.totals.reason.grampian,
                                    by="breakdown.level")
rm(table3.totals.reason.grampian)

###### Append

table3.grampian <- plyr::rbind.fill(table3.overall.grampian,
                                    table3.age.grampian,
                                    table3.sex.grampian,
                                    table3.dep.grampian,
                                    table3.reason.grampian)

table3.grampian <- mutate(table3.grampian,partner="Grampian")

rm(table3.overall.grampian,
   table3.age.grampian,
   table3.sex.grampian,
   table3.dep.grampian,
   table3.reason.grampian)

fwrite(table3.grampian, file = paste0(rawdatadir,"Grampian-Aberdeen/table3.grampian.csv"), sep = ",")