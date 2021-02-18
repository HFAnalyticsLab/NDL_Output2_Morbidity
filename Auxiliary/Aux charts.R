##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

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
graphsdir <- "M:/Analytics/Networked Data Lab/Partner outputs/Output 2 Morbidity/Charts/"

##################################################
################### Load clean data ##############
##################################################

table1.all.partners <- fread(paste0(rawdatadir,"table1.all.partners.csv"),
                                 header=TRUE, sep=",", check.names=T)

table2.all.partners <- fread(paste0(rawdatadir,"table2.all.partners.csv"),
                             header=TRUE, sep=",", check.names=T)

table3.all.partners <- fread(paste0(rawdatadir,"table3.all.partners.csv"),
                             header=TRUE, sep=",", check.names=T)

##################################################
################### Conditions ###################
##################################################

############### % with admission, overall

pct.with.admission <- table2.all.partners %>% filter(.,breakdown.level=="overall") %>%
  mutate(.,admission=ifelse(score_band=="no admission","no","yes")) %>%
  group_by(.,admission) %>%
  summarise(.,number.patients = sum(number.patients),
            pct.people = sum(pct.people),
            breakdown.level = first(breakdown.level)) %>%
  ungroup(.)

pct.with.admission.plot <- ggplot(pct.with.admission,
  aes(fill=factor(admission) , y = pct.people, x=factor(breakdown.level))) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("Admissions rate") +
  theme_ipsum()

ggplotly(pct.with.admission.plot)

pct.vwscore.overall <- filter(table3.all.partners,breakdown.level=="overall") %>%
  mutate(.,windex_vw=factor(windex_vw,levels = c("no admission","<0","0","1 to 4",">=5")))

plot.pct.vwscore.overall <- ggplot(pct.vwscore.overall, aes(fill=windex_vw , y=pct.people, x=breakdown.level)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("Multimorbidity score") +
  theme_ipsum() 

############### % Patients

pct.people.overall <- filter(table1.all.partners,breakdown.level=="overall") %>%
  filter(.,condition!="total_diagnoses") %>%
  arrange(.,-pct.people) %>%
  filter(.,(pct.people>5)|(yearly.admissions.per.person>0.01))

people.and.admissions.overall <- ggplot(pct.people.overall.small) + 
  geom_bar(aes(x = reorder(condition, -pct.people), y = pct.people), stat="identity",position="dodge") +
  geom_line(aes(x = reorder(condition, -pct.people), y = yearly.admissions.per.person*100), size = 1.5, color="red", group = 1) + 
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Yearly admissions per 100 people"),
                     name = "Percentage of people") +
  scale_x_discrete(label=pct.people.overall$condition.desc.short,name="Conditions") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(people.and.admissions.overall)

plot_ly(pct.people.overall, x = ~reorder(condition.desc.short,-pct.people), y = ~pct.people, type = "bar", name = "% patients with condition") %>%
  add_lines(x = ~reorder(condition.desc.short,-pct.people), y = ~yearly.admissions.per.person, mode = "lines", yaxis = "y2", name = "Number admissions/CEV person per year") %>%
  facet_wrap(~ partner) +
  layout(xaxis = list(title = "Condition"),
         yaxis = list(showgrid=FALSE, title = "% with condition"),
         yaxis2 = list(overlaying = "y", side = "right",showgrid=FALSE, title = "Admissions/CEV person\n(per year)"),
         legend= list(itemsizing='constant'))

############### Number of admissions

#bar with numbers of admissions
#line with yearly admissions by 100 CEV people
#line with yearly admissions by 100 people CEV people with condtions
#(provided they are ALREADY being treated in hospital)
#(therefore more likely for conditions that need inpatient care)

number.admissions.overall <- filter(table1.all.partners,breakdown.level=="overall") %>%
  filter(.,condition!="total_diagnoses") %>%
  arrange(.,-pct.people) %>%
  filter(.,(pct.people>5)|(yearly.admissions.per.person>0.01))

people.and.admissions.overall <- ggplot(pct.people.overall) + 
  geom_bar(aes(x = reorder(condition, -pct.people), y = pct.people), stat="identity",position="dodge") +
  geom_line(aes(x = reorder(condition, -pct.people), y = yearly.admissions.per.person*100), size = 1.5, color="red", group = 1) + 
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Yearly admissions per 100 people"),
                     name = "Percentage of people") +
  scale_x_discrete(label=pct.people.overall$condition.desc.short,name="Conditions") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(people.and.admissions.overall)

############### % Patients, by sex

pct.people.sex <- filter(table1.all.partners,breakdown=="sex") %>%
  filter(.,condition!="total_diagnoses") %>%
  arrange(.,-pct.people) %>%
  filter(.,(pct.people>5)|(yearly.admissions.per.person>0.01))

people.and.admissions.sex <- ggplot(pct.people.sex) + 
  geom_bar(aes(x = breakdown.level, y = pct.people,fill=reorder(condition,-pct.people)),
           stat="identity",position="dodge") +
  scale_y_continuous(name = "Percentage of people") +
  scale_x_discrete(name="Conditions") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(people.and.admissions.sex)

############### % Patients, by age

pct.people.age <- filter(table1.all.partners,breakdown=="age") %>%
  filter(.,condition!="total_diagnoses"&breakdown.level!="unknown") %>%
  mutate(.,breakdown.level=factor(breakdown.level,
                                  levels = c("0-29","30-49","50-69","70+"))) %>%
  arrange(.,-pct.people) %>%
  filter(.,(pct.people>5)|(yearly.admissions.per.person>0.01))

people.and.admissions.age <- ggplot(pct.people.age) + 
  geom_bar(aes(x = breakdown.level, y = pct.people,fill=reorder(condition,-pct.people)),
           stat="identity",position="dodge") +
  scale_y_continuous(name = "Percentage of people") +
  scale_x_discrete(name="Conditions") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(people.and.admissions.age)

############### Multimorbidity

pct.multimbrbidity.overall <- filter(table2.all.partners,breakdown.level=="overall")

plot.pct.multimbrbidity.overall <- ggplot(pct.multimbrbidity.overall, aes(fill=score_band, y=pct.people, x=breakdown.level)) + 
  geom_bar(position="fill", stat="identity") +
  viridis::scale_fill_viridis(discrete = T) +
  ggtitle("Multimorbidity") +
  theme_ipsum()

ggplotly(plot.pct.multimbrbidity.overall)

############### Multimorbidity, by sex

pct.multimbrbidity.sex <- filter(table2.all.partners,breakdown=="sex")

plot.pct.multimbrbidity.overall <- ggplot(pct.multimbrbidity.sex,
                                          aes(fill=score_band, y=pct.people,
                                              x=breakdown.level)) + 
  facet_wrap(~ partner) +
  geom_bar(position="fill", stat="identity") +
  viridis::scale_fill_viridis(discrete = T) +
  ggtitle("Multimorbidity") +
  theme_ipsum()

ggplotly(plot.pct.multimbrbidity.overall)

############### Multimorbidity, by reason for shielding

pct.multimorbidity.reason <- filter(table2.all.partners,breakdown=="reason_shielding") %>%
  filter(.,breakdown.level!="unknown")

plot.pct.multimorbidity.reason <- ggplot(pct.multimorbidity.reason,
                                         aes(fill=score_band, y=pct.people,
                                             x=breakdown.level)) + 
  facet_wrap(~ partner) +
  geom_bar(position="fill", stat="identity") +
  viridis::scale_fill_viridis(discrete = T) +
  ggtitle("Multimorbidity") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############### van Walraven score

pct.vwscore.overall <- filter(table3.all.partners,breakdown.level=="overall") %>%
  mutate(.,windex_vw=factor(windex_vw,levels = c("no admission","<0","0","1 to 4",">=5")))

myColors <- c("#bdbdbd",brewer.pal(n = 4, name = "BuPu"))
names(myColors) <- levels(pct.vwscore.overall$windex_vw)
FillScale <- scale_fill_manual(name = "grp",values = myColors)

plot.pct.vwscore.overall <- ggplot(pct.vwscore.overall, aes(fill=windex_vw , y=pct.people, x=breakdown.level)) + 
  geom_bar(position="fill", stat="identity") +
  FillScale +
  ggtitle("Multimorbidity score") +
  theme_ipsum() 

ggplotly(plot.pct.vwscore.overall)

############### van Walraven score, by sex

pct.vwscore.sex <- filter(table3.all.partners,breakdown=="sex") %>%
  mutate(.,windex_vw=factor(windex_vw,levels = c("no admissions","<0","0","1 to 4",">=5")))

############### % with conditions

pct.people.overall <- table1.all.partners %>% filter(.,breakdown=="overall") %>%
  filter(.,condition %in% c("cpd","hypc","diabc","rf","metacanc"))

people.overall <- ggplot(pct.people.overall) +
  geom_bar(aes(x = reorder(condition.desc.short, -pct.people),
               y = pct.people, fill = partner), stat="identity",position="dodge") +
  scale_x_discrete(name="Conditions") +
  scale_y_continuous(name="% of CEV people") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        legend.key.size = unit(0.2, "cm"),
        legend.text=element_text(size=7))

ggsave("people.overall.png",width=14,height=6,units="cm")

