library(pander)
library(knitr)
library(xtable)
library(dplyr)
library(tables)
library(tidyr)
library(sjstats)
#library(ExcelPvtTbls)
library(car) # for VIF
library(pivottabler)

#source: wd = paste("C:/LEI/Reports/", year, sep="")
setwd(wd)

### Section 1
tbl1.1<-data.frame(excelPivot2Sum("AgeGroup", "Race", "COUNT", demogTbl))
tbl1.2<-data.frame(excelPivot1("Committed Offense Type", arrests))
tbl1.3<-data.frame(excelPivot1("Suspected", contacts))

### Law Enforcement Data
tbl2.1<-data.frame(excelPivot1("MainCat", arrests))
tbl2.2<-data.frame(excelPivot1("Race", arrests))
temp<-subset(arrests, ArrestType=="Traffic Stops")
tbl2.3<-data.frame(excelPivot2("AgeCat", "MainCat", temp))
temp<-subset(arrests, ArrestType=="warrant")
tbl2.4<-data.frame(excelPivot2("AgeCat", "MainCat", temp))
temp<-subset(arrests, ArrestType=="Summons")
tbl2.5<-data.frame(excelPivot2("AgeCat", "MainCat", temp))

temp<-subset(arrests, ArrestType=="On-view/probable cause" & AgeCat=="Adult")
tbl2.6<-data.frame(excelPivot2("MainCat", "Race", temp))
temp<-subset(arrests, ArrestType=="On-view/probable cause" & AgeCat=="Juvenile")
tbl2.7<-data.frame(excelPivot2("MainCat", "Race", temp))

temp<-subset(arrests, ArrestType=="warrant" & AgeCat=="Adult")
tbl2.8<-data.frame(excelPivot2("MainCat", "Race", temp))
temp<-subset(arrests, ArrestType=="warrant" & AgeCat=="Juvenile")
tbl2.9<-data.frame(excelPivot2("MainCat", "Race", temp))

temp<-subset(arrests, ArrestType=="Summons" & AgeCat =="Adult")
tbl2.10<-data.frame(excelPivot2("MainCat", "Race", temp))
temp<-subset(arrests, ArrestType=="Summons" & AgeCat =="Juvenile")
tbl2.11<-data.frame(excelPivot2("MainCat", "Race", temp))

temp<-subset(arrests, ArrestType=="On-view/probable cause")
tbl2.12<-data.frame(excelPivot2("MainCat", "Gender", temp))
temp<-subset(arrests, ArrestType=="warrant")
tbl2.13<-data.frame(excelPivot2("MainCat", "Gender", temp))
temp<-subset(arrests, ArrestType=="Summons")
tbl2.14<-data.frame(excelPivot2("MainCat", "Gender", temp))

### Suspected offense contacts
tbl3.1<-data.frame(excelPivot1("Race", contacts))
tb1<-arrange(contacts,MainCat,Race)
tbl3.2<-data.frame(excelPivot2("MainCat","Race",tb1))
tb1<-arrange(contacts,Gender,MainCat)
tbl3.3<-data.frame(excelPivot2("Gender","MainCat",tb1))
tb1<-arrange(contacts,Suspected,Race)
tbl3.4<-data.frame(excelPivot2("Suspected","Race",tb1))
tb1<-arrange(contacts,Suspected,MainCat)
tbl3.5<-data.frame(excelPivot2("Suspected","MainCat",tb1))
tb1<-arrange(contacts,Suspected,Gender)
tbl3.6<-data.frame(excelPivot2("Suspected","Gender",tb1))
tb1<-arrange(contacts,Suspected,Outcome)
tbl3.7<-data.frame(excelPivot2("Suspected","Outcome",tb1))
tb1<-arrange(contacts,MainCat,Outcome)
tbl3.8<-data.frame(excelPivot2("MainCat","Outcome",tb1))
#### Case Outcomes
tb1<-arrange(contacts,Race,MainCat,Outcome)
temp<-subset(tb1, Suspected=="Suspected Offense")
tbl3.9<-data.frame(excelPivot3("Race","MainCat","Outcome", temp))
tb1<-arrange(contacts,Race,MainCat,Outcome)
temp<-subset(tb1, Suspected=="Suspected Offense")
tbl3.10<-data.frame(excelPivot3("Race","MainCat","Outcome", temp))
tb1<-arrange(contacts,Race,MainCat,Outcome)
temp<-subset(tb1, Suspected=="Juvenile")
tbl3.11<-data.frame(excelPivot3("Race","MainCat","Outcome", temp))

#### action taken
temp<-subset(offense, Suspected=="Suspected Offense")
temp$reason<-factor(temp$reason) # remove unused factors
tbl3.12<-data.frame(excelPivot2a("reason","MainCat", temp))
tbl3.13<-data.frame(excelPivot2a("reason","Gender", temp))
tbl3.12N<-nrow(temp)
tbl3.13N<-tbl3.12N
tbl3.14<-data.frame(excelPivot2a("reason","Race",temp))
temp<-subset(offense, Suspected=="Suspected Offense" & MainCat=="Traffic")
temp$reason<-factor(temp$reason) # remove unused factors
tbl3.15<-data.frame(excelPivot2a("reason","Race",temp))
temp<-subset(offense, Suspected=="Suspected Offense" & MainCat=="Other")
temp$reason<-factor(temp$reason) # remove unused factors
tbl3.16<-data.frame(excelPivot2a("reason","Race",temp))
temp<-subset(offense, Suspected=="Suspected Offense" & MainCat=="Property")
temp$reason<-factor(temp$reason) # remove unused factors
tbl3.17<-data.frame(excelPivot2a("reason","Race",temp))
temp<-subset(offense, Suspected=="Suspected Offense" & MainCat=="")
temp$reason<-factor(temp$reason) # remove unused factors
tbl3.18<-data.frame(excelPivot2a("reason","Race",temp))

temp<-subset(offense, Suspected=="Suspected Offense")
temp$reason<-factor(temp$reason) # remove unused factors
tbl3.19<-data.frame(excelPivot2a("reason","MainCat", temp))
tbl3.20<-data.frame(excelPivot2a("reason","Gender", temp))
tbl3.21<-data.frame(excelPivot2a("reason","Race",temp))
temp<-subset(offense, Suspected=="Suspected Offense" & MainCat=="Person")
temp$reason<-factor(temp$reason) # remove unused factors
#tbl3.22<-data.frame(excelPivot2a("reason","Race",temp))
#temp<-subset(offense, Suspected=="Suspected Offense" & MainCat=="Other")
temp$reason<-factor(temp$reason) # remove unused factors
tbl3.23<-data.frame(excelPivot2a("reason","Race",temp))
temp<-subset(offense, Suspected=="Suspected Offense" & MainCat=="Property")
temp$reason<-factor(temp$reason) # remove unused factors
tbl3.24<-data.frame(excelPivot2a("reason","Race",temp))
temp<-subset(offense, Suspected=="Suspected Offense" & MainCat=="Property")
temp$reason<-factor(temp$reason) # remove unused factors
tbl3.25<-data.frame(excelPivot2a("reason","Race",temp))


#### action taken
temp<-subset(offense, (Suspected=="Suspected Offense" & (reason=="No action" | reason=="Warning" | reason=="Arrest")))
tbl3.33<-data.frame(excelPivot3("Race","MainCat","action taken", temp))
tbl3.34<-data.frame(excelPivot3("Gender","MainCat","action taken", temp))
temp<-subset(offense, (Suspected=="Suspected Offense" & (reason=="No action" | reason=="Warning" | reason=="Arrest")))
tbl3.35<-data.frame(excelPivot3("Race","MainCat","action taken", temp))
tbl3.36<-data.frame(excelPivot3("Gender","MainCat","action taken", temp))
temp<-subset(offense, (Suspected=="Juvenile" & (reason=="No action" | reason=="Warning" | reason=="Arrest")))
tbl3.37<-data.frame(excelPivot3("Race","MainCat","action taken", temp))
tbl3.38<-data.frame(excelPivot3("Gender","MainCat","action taken", temp))

### Additional Info
##### exploring contact reasons
options(scipen=0)
crs<-offense %>% filter(Cascls=="CR" & grepl("F",TopReasons))
crs<-within(crs,Race<-relevel(Race,ref=4))
crs<-within(crs,MainCat<-relevel(MainCat,ref=2))
tb1<-arrange(crs,Race,isDOC)
tbl5.1<-data.frame(excelPivot2("Race","isDOC",tb1))
#crs$isDOC<-ifelse(crs$isDOC=="Yes",1,0)
lg1<-glm(Duration~Suspected+Age+Race+Gender+MainCat+Other+IndexIsCV, data=crs, family=binomial("logit"))
# Null Hypothesis - model isnt any better than the null model. need to play around with this more
1- pchisq(lg1$null.deviance - lg1$deviance, lg1$df.null- lg1$df.residual)

#lg1<-glm(Month~FC, data=crs, family=binomial("logit"))
# Pseudo R2
ps1<-PseudoR2(lg1)

##### Warning offense for Adults in District Suspected
crs<-offense %>% filter(Cascls=="CR")
crs<-within(crs,Race<-relevel(Race,ref=4)) # Whites
crs<-within(crs,MainCat<-relevel(MainCat,ref=2))
tb1<-arrange(crs,Race,isDefer)
tbl5.2<-data.frame(excelPivot2("Race","isDefer",tb1))
lg2<-glm(UoF~Suspected+Age+Race+Gender+MainCat+Other+IndexIsCV, data=crs, family=binomial("logit"))
ps2<-PseudoR2(lg2)

##### Warning offense for Juveniles
crs<-offense %>% filter(Cascls=="JD")
crs<-within(crs,Race<-relevel(Race,ref=4)) # Whites
crs<-within(crs,MainCat<-relevel(MainCat,ref=2))
tb1<-arrange(crs,Race,isAT)
tbl5.3<-data.frame(excelPivot2("Race","isDefer",tb1))
lg3<-glm(AT~Suspected+Age+Race+Gender+MainCat+Other+IndexIsCV, data=crs, family=binomial("logit"))
ps3<-PseudoR2(lg3)

##### reasons for Juveniles
#crs<-offense %>% filter(Cascls=="JD" & grepl("F",TopReasons))
crs<-offense %>% filter(Cascls=="JD")
#crs$isarrest<-as.factor(ifelse(grepl("F",crs$arrest)==T,"Yes","No"))<-remove for 2022
crs<-within(crs,Race<-relevel(Race,ref=4)) # whites
crs<-within(crs,MainCat<-relevel(MainCat,ref=2))
tb1<-arrange(crs,Race,isSO)
tbl5.4<-data.frame(excelPivot2("Race","isDYS",tb1))
lg4<-glm(isSO~Suspected+Race+Gender+MainCat+Other+IndexIsCV+Outcome, data=crs, family=binomial("logit"))
ps4<-PseudoR2(lg4)
summary(lg4)
ps4
exp(cbind(coef(lg4), confint(lg4)))


# look at percentages
contacts %>%
  group_by(RaceCode) %>%
  summarise(N=n()) %>%
  mutate(Percent=(N/sum(N))*100)


#unknown genders removed
table(contactsTemp$Sex)
table(offenseTemp$Sex)

#additional data
offense %>%
  group_by(Cascls,Action) %>%
  summarise(N=n()) %>%
  mutate(Percent = (N/sum(N))) %>%
  mutate(HadOthers=(1-Percent)*100)

# another look at offense outcome and sex of citizen
temp<-subset(offense, ((reason=="No action" | reason=="Warning" | reason=="Arrest")))
temp %>%
  filter(offense != "Offense Committed") %>%
  group_by(Sex) %>%
  summarise(N=n()) %>%
  mutate(Percent = (N/sum(N))*100) 

