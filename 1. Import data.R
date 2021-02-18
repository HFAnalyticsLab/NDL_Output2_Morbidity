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

###########################################################
##################### Import Table 1  #####################
###########################################################

#Grampian
table1.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/table1.grampian.csv"),
                               header=TRUE, sep=",", check.names=T)

#Leeds
table1.leeds <- fread(paste0(rawdatadir,"Leeds/table1.leeds.csv"),
                         header=TRUE, sep=",", check.names=T)

#Liverpool Wirral
table1.liverpoolwirral <- fread(paste0(rawdatadir,"Liverpool Wirral/table1.lpoolwirral.csv"),
                      header=TRUE, sep=",", check.names=T)

#Wales
table1.wales <- fread(paste0(rawdatadir,"Wales/table1.wales.csv"),
                                header=TRUE, sep=",", check.names=T)

#NW London
table1.nwlondon <- fread(paste0(rawdatadir,"NW London/table1.nwlondon.csv"),
                      header=TRUE, sep=",", check.names=T)

#Merge all
table1 <- plyr::rbind.fill(table1.grampian,table1.leeds,table1.liverpoolwirral,table1.wales,table1.nwlondon)
rm(table1.grampian,table1.leeds,table1.liverpoolwirral,table1.wales,table1.nwlondon)

###### Import disease descriptions

elixhauser_conditions <- fread(paste0(rawdatadir,"elixhauser_conditions.csv"),
                               header=TRUE, sep=",", check.names=T)

table1 <- left_join(table1,elixhauser_conditions,by="condition")

rm(elixhauser_conditions)

###########################################################
##################### Import Table 2  #####################
###########################################################

#Grampian
table2.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/table2.grampian.csv"),
                         header=TRUE, sep=",", check.names=T)

#Leeds
table2.leeds <- fread(paste0(rawdatadir,"Leeds/table2.leeds.csv"),
                      header=TRUE, sep=",", check.names=T)

#Liverpool Wirral
table2.liverpoolwirral <- fread(paste0(rawdatadir,"Liverpool Wirral/table2.lpoolwirral.csv"),
                                header=TRUE, sep=",", check.names=T)

#Wales
table2.wales <- fread(paste0(rawdatadir,"Wales/table2.wales.csv"),
                                header=TRUE, sep=",", check.names=T)

#NW London
table2.nwlondon <- fread(paste0(rawdatadir,"NW London/table2.nwlondon.csv"),
                      header=TRUE, sep=",", check.names=T)

#Merge all
table2 <- plyr::rbind.fill(table2.grampian,table2.leeds,table2.liverpoolwirral,table2.wales,table2.nwlondon)
rm(table2.grampian,table2.leeds,table2.liverpoolwirral,table2.wales)

###########################################################
##################### Import Table 3  #####################
###########################################################

#Grampian
table3.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/table3.grampian.csv"),
                         header=TRUE, sep=",", check.names=T)

#Leeds
table3.leeds <- fread(paste0(rawdatadir,"Leeds/table3.leeds.csv"),
                      header=TRUE, sep=",", check.names=T)

#Liverpool Wirral
table3.liverpoolwirral <- fread(paste0(rawdatadir,"Liverpool Wirral/table3.lpoolwirral.csv"),
                      header=TRUE, sep=",", check.names=T)

#Wales
table3.wales <- fread(paste0(rawdatadir,"Wales/table3.wales.csv"),
                                header=TRUE, sep=",", check.names=T)

#NW London
table3.nwlondon <- fread(paste0(rawdatadir,"NW London/table3.nwlondon.csv"),
                      header=TRUE, sep=",", check.names=T)

#Merge all
table3 <- plyr::rbind.fill(table3.grampian,table3.leeds,table3.liverpoolwirral,table3.wales,table3.nwlondon)
rm(table3.grampian,table3.leeds,table3.liverpoolwirral,table3.wales)

###############################################################################
##################### Merge in totals (for some partners) #####################
###############################################################################

totals <- fread(paste0(rawdatadir,"totals.csv"),
                               header=TRUE, sep=",", check.names=T) %>%
  mutate(.,total.patients=as.numeric(total.patients))

#Table 1
names(table1)
table1 <- left_join(table1,totals,
                             by=c("partner","breakdown","breakdown.level"))
#Remove duplicate column
table1$total.patients.x[which(is.na(table1$total.patients.x)==T&is.na(table1$total.patients.y)==F)] <- table1$total.patients.y[which(is.na(table1$total.patients.x)==T&is.na(table1$total.patients.y)==F)]
table1$total.patients.y <- NULL
table1 <- dplyr::rename(table1,total.patients=total.patients.x)

#Table 2
names(table2)
table2 <- left_join(table2,totals,
                             by=c("partner","breakdown","breakdown.level"))
#Remove duplicate column
table2$total.patients.x[which(is.na(table2$total.patients.x)==T&is.na(table2$total.patients.y)==F)] <- table2$total.patients.y[which(is.na(table2$total.patients.x)==T&is.na(table2$total.patients.y)==F)]
table2$total.patients.y <- NULL
table2 <- dplyr::rename(table2,total.patients=total.patients.x)

#Table 3
names(table3)
table3 <- left_join(table3,totals,
                             by=c("partner","breakdown","breakdown.level"))
#Remove duplicate column
table3$total.patients.x[which(is.na(table3$total.patients.x)==T&is.na(table3$total.patients.y)==F)] <- table3$total.patients.y[which(is.na(table3$total.patients.x)==T&is.na(table3$total.patients.y)==F)]
table3$total.patients.y <- NULL
table3 <- dplyr::rename(table3,total.patients=total.patients.x)

#Remove totals data
rm(totals)

##################################################
##################### Rates  #####################
##################################################

#Table 1

table1 <- table1 %>%
  mutate(.,number.patients=ifelse(grepl("<",number.patients,fixed=TRUE),NA,number.patients),
         number.admissions=ifelse(grepl("<",number.admissions,fixed=TRUE),NA,number.admissions)) %>%
  mutate(.,number.patients=as.numeric(number.patients),
         number.admissions=as.numeric(number.admissions))

table1 <- mutate(table1,
                          pct.people=(number.patients)/(total.patients)*100,
                          yearly.admissions.per.person=((number.admissions)/(total.patients))/2,
                          yearly.admissions.per.user=((number.admissions)/(number.patients))/2)

#Table 2

table2 <- table2 %>%
  mutate(.,number.patients=ifelse(grepl("<",number.patients,fixed=TRUE),NA,number.patients)) %>%
  mutate(.,number.patients=as.numeric(number.patients))

table2 <- mutate(table2,
                          pct.people=(number.patients)/(total.patients)*100)

#Table 3

table3 <- table3 %>%
  mutate(.,number.patients=ifelse(grepl("<",number.patients,fixed=TRUE),NA,number.patients)) %>%
  mutate(.,number.patients=as.numeric(number.patients))

table3 <- mutate(table3,
                          pct.people=(number.patients)/(total.patients)*100)

#########################################################
##################### Save results  #####################
#########################################################

fwrite(table1, file = paste0(rawdatadir,"table1.all.partners.csv"), sep = ",")
fwrite(table2, file = paste0(rawdatadir,"table2.all.partners.csv"), sep = ",")
fwrite(table3, file = paste0(rawdatadir,"table3.all.partners.csv"), sep = ",")