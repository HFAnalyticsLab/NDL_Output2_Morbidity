##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,readODS,
               gmodels,Rmisc,DescTools,data.table,readxl,
               Hmisc,tibble,leaflet,rgeos,raster,plotly,
               pbapply,pbmcapply,here,rgdal,RColorBrewer,ggthemes,
               ggchicklet,tidyverse,showtext,ggchicklet,viridis,hrbrthemes)

#Clean up the global environment
rm(list = ls())

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Networked Data Lab/Partner outputs/Output 2 Morbidity/"
graphsdir <- "C:/Users/SebastienP/OneDrive - The Health Foundation/Output 2/Charts/"

##################################################
################### Load clean data ##############
##################################################

table1.all.partners <- fread(paste0(rawdatadir,"table1.all.partners.csv"),
                             header=TRUE, sep=",", check.names=T)

table2.all.partners <- fread(paste0(rawdatadir,"table2.all.partners.csv"),
                             header=TRUE, sep=",", check.names=T)

table3.all.partners <- fread(paste0(rawdatadir,"table3.all.partners.csv"),
                             header=TRUE, sep=",", check.names=T)

##Small number of conditions

small.elix <- filter(table1.all.partners,breakdown=="overall") %>%
  filter(.,pct.people>5) %>%
  select(.,condition) %>%
  unlist(.) %>%
  unique(.) %>%
  c(.,"diabc","hypc")

elix.order <- data.frame(order=as.numeric(1:14),
                    condition=c("cpd","solidtum","metacanc","lymph","hypc","hypunc",
                      "diabc","diabunc","rf","rheumd","depre","fed",
                      "carit","ond"))

small.elix.bis <- c("solidtum","metacanc","lymph","cpd","rf","chf","rheumd")

elix.order.bis <- data.frame(order=as.numeric(7:1),
                         condition=
                           c("solidtum","metacanc","lymph",
                             "cpd","rf","chf","rheumd"))

##Heatmap data, by reason

# table1.all.partners %>%
#   filter(.,partner=="NW London",breakdown=="reason_shielding",condition=="chf") %>%
#   select(.,breakdown,breakdown.level,condition,number.patients,total.patients,pct.people)

pct.people.reason <- filter(table1.all.partners,breakdown=="reason_shielding") %>%
  filter(.,!(breakdown.level %in% c("unknown","pregnancy", "GP referred"))) %>%
  filter(.,condition %in% small.elix) %>%
  left_join(.,elix.order,by="condition") %>%
  arrange(.,partner,breakdown.level,order) %>%
  select(.,partner,breakdown.level,condition,condition.desc.short,pct.people,order) %>%
  mutate(.,breakdown.level=str_to_title(breakdown.level)) %>%
  dplyr::rename(.,`Prelavence (%)`=pct.people,
                `Disease group`=breakdown.level) %>%
  filter(.,!is.na(`Prelavence (%)`))

fwrite(pct.people.reason, file = paste0(graphsdir,"pct.people.reason.csv"), sep = ",")

##Heatmap data, by age group

pct.people.age <- filter(table1.all.partners,breakdown=="age") %>%
  filter(.,breakdown.level!="unknown") %>%
  filter(.,condition %in% small.elix) %>%
  left_join(.,elix.order,by="condition") %>%
  arrange(.,partner,breakdown.level,order) %>%
  select(.,partner,breakdown.level,condition,condition.desc.short,pct.people,order) %>%
  mutate(.,breakdown.level=str_to_title(breakdown.level)) %>%
  dplyr::rename(.,`Prelavence (%)`=pct.people,`Age group`=breakdown.level) %>%
  filter(.,!is.na(`Prelavence (%)`))

fwrite(pct.people.age, file = paste0(graphsdir,"pct.people.age.csv"), sep = ",")

##Heatmap data, by deprivation

pct.people.dep <- filter(table1.all.partners,breakdown=="imd") %>%
  filter(.,breakdown.level!="unknown") %>%
  filter(.,condition %in% small.elix) %>%
  left_join(.,elix.order,by="condition") %>%
  arrange(.,partner,breakdown.level,order) %>%
  select(.,partner,breakdown.level,condition,condition.desc.short,pct.people,order) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"1","1 (most deprived)")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"5","5 (least deprived)")) %>%
  dplyr::rename(.,`Prelavence (%)`=pct.people) %>%
  dplyr::rename(.,`IMD quintile`=breakdown.level) %>%
  filter(.,!is.na(`Prelavence (%)`))

fwrite(pct.people.dep, file = paste0(graphsdir,"pct.people.dep.csv"), sep = ",")

##Heatmap data, by sex

pct.people.sex <- filter(table1.all.partners,breakdown=="sex") %>%
  filter(.,breakdown.level %in% c("M","F")) %>%
  filter(.,condition %in% small.elix) %>%
  left_join(.,elix.order,by="condition") %>%
  arrange(.,partner,breakdown.level,order) %>%
  select(.,partner,breakdown.level,condition,condition.desc.short,pct.people,order) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"M","Men")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"F","Women")) %>%
  dplyr::rename(.,`Prelavence (%)`=pct.people) %>%
  dplyr::rename(.,`Sex`=breakdown.level) %>%
  filter(.,!is.na(`Prelavence (%)`))

fwrite(pct.people.sex, file = paste0(graphsdir,"pct.people.sexp.csv"), sep = ",")

##Admissions bar chart

admission.rate <- filter(table1.all.partners,breakdown=="overall") %>%
  filter(.,condition %in% small.elix) %>%
  left_join(.,elix.order,by="condition") %>%
  arrange(.,partner,order) %>%
  select(.,partner,breakdown,condition,condition.desc.short,yearly.admissions.per.person,order) %>%
  mutate(.,yearly.admissions.per.person=100*yearly.admissions.per.person) %>%
  dplyr::rename(.,`Admissions per 100 people (yearly)`=yearly.admissions.per.person) %>%
  filter(.,!is.na(`Admissions per 100 people (yearly)`))

filter(admission.rate,partner=="Leeds")

admission.rate.reason.wide <- spread(admission.rate,breakdown, `Admissions per 100 people (yearly)`) %>%
  arrange(.,partner,order)

fwrite(admission.rate.reason.wide, file = paste0(graphsdir,"admissions.csv"), sep = ",")

##Admissions bar chart, by reason

admission.rate.reason <- filter(table1.all.partners,breakdown=="reason_shielding") %>%
  filter(.,(breakdown.level %in% c("cancer","respiratory","rare disease","transplant"))) %>%
  filter(.,condition %in% small.elix.bis) %>%
  left_join(.,elix.order.bis,by="condition") %>%
  arrange(.,partner,breakdown.level,order) %>%
  select(.,partner,breakdown.level,condition,condition.desc.short,yearly.admissions.per.person,order) %>%
  mutate(.,breakdown.level=str_to_title(breakdown.level)) %>%
  mutate(.,yearly.admissions.per.person=100*yearly.admissions.per.person) %>%
  dplyr::rename(.,`Admissions per 100 people (yearly)`=yearly.admissions.per.person) %>%
  filter(.,!is.na(`Admissions per 100 people (yearly)`))

#filter(admission.rate.reason,partner=="NW London")

admission.rate.reason.wide <- spread(admission.rate.reason,breakdown.level, `Admissions per 100 people (yearly)`) %>%
  arrange(.,partner,order)

fwrite(admission.rate.reason.wide, file = paste0(graphsdir,"admissions_dgroup.csv"), sep = ",")

##Donuts

pct.multimorbidity <- filter(table2.all.partners,breakdown=="overall") %>%
  filter(.,breakdown.level!="unknown") %>%
  mutate(.,score_band_rc=ifelse((score_band==0|score_band=="no admission"),0,score_band)) %>%
  group_by(partner,score_band_rc) %>%
  summarise(number.patients = sum(number.patients),
            total.patients = first(total.patients)) %>%
  ungroup()

pct.multimorbidity <- pct.multimorbidity %>%
  within(.,
         {group_totals = ave(number.patients,partner,FUN=sum)}) %>%
  mutate(.,pct_group=number.patients/group_totals*100,
         score_band_rc=str_replace_all(score_band_rc,"2","2+")) %>%
  select(.,partner,score_band_rc,pct_group) %>%
  dplyr::rename(`Number of conditions`=score_band_rc) %>%
  spread(.,`Number of conditions`, pct_group)

fwrite(pct.multimorbidity, file = paste0(graphsdir,"mm.csv"), sep = ",")

##Donuts, by reason

pct.multimorbidity.reason <- filter(table2.all.partners,breakdown=="reason_shielding") %>%
  filter(.,breakdown.level!="unknown") %>%
  mutate(.,score_band_rc=ifelse((score_band==0|score_band=="no admission"),0,score_band)) %>%
  group_by(partner,breakdown.level,score_band_rc) %>%
  summarise(number.patients = sum(number.patients),
            total.patients = first(total.patients)) %>%
  ungroup()

pct.multimorbidity.reason <- pct.multimorbidity.reason %>%
  within(.,
  {group_totals = ave(number.patients,partner,breakdown.level,FUN=sum)}) %>%
  mutate(.,pct_group=number.patients/group_totals*100,
         score_band_rc=str_replace_all(score_band_rc,"2","2+")) %>%
  select(.,partner,breakdown.level,score_band_rc,pct_group) %>%
  dplyr::rename(`Number of conditions`=score_band_rc) %>%
  mutate(.,breakdown.level=str_to_title(breakdown.level)) %>%
  spread(.,`Number of conditions`, pct_group)

fwrite(pct.multimorbidity.reason, file = paste0(graphsdir,"mm_bydgroup.csv"), sep = ",")