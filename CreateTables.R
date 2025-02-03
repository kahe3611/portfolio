library(pander)
library(knitr)
library(xtable)
library(dplyr)
library(BaylorEdPsych) # Install archived version: 0.5
library(tables)
library(tidyr)
library(sjstats)
library(ExcelPvtTbls)
library(car) # for VIF
source("O:/CLEAR Act SB15-185/R Code/DataSources.R")

# Set Working Directory to grab .Rdata files
year = "2023"
wd = paste("O:/CLEAR Act SB15-185/Reports/", year, sep="")
setwd(wd)

load("arrests.RData")
load("charges.RData")
load("sentences.RData")
load("demogTbl.RData")

# # Consider moving this into ExcelPvtTbls
# fixTable<-function(my_tbl){
#   cols <- colnames(my_tbl)
#   for (col in cols) {
#     my_tbl[col] <- (unlist(my_tbl[col]) %>% as.character)
#   }
#   return(my_tbl)
# }

### Section 1
tbl1.1<-excelPivot2Sum("AgeGroup", "RaceEthnicity", "COUNT", demogTbl) %>%
  data.frame() %>% fixTable()
colnames(tbl1.1) <- c("Age Group", "Race/Ethnicity", "Percent", "Total")

tbl1.1 <- demogTbl %>% group_by(RaceEthnicity) %>%
  summarise(OverallPopulation = sum(COUNT)) %>%
  mutate(OverallPercentage = round(OverallPopulation*100/sum(OverallPopulation))) %>%
  cbind(., AdultPopulation = filter(demogTbl, AgeGroup=='Adult')$COUNT)

tbl1.1 <- demogTbl %>% group_by(RaceEthnicity) %>%
  summarise(OverallPopulation = sum(COUNT)) %>%
  mutate(AdultPopulation = filter(demogTbl, AgeGroup=='Adult')$COUNT,
         JuvenilePopulation = filter(demogTbl, AgeGroup=='Juvenile')$COUNT) %>%
  mutate(OverallPercentage = OverallPopulation*100/sum(OverallPopulation),
         AdultPercentage = AdultPopulation*100/sum(AdultPopulation),
         JuvenilePercentage = JuvenilePopulation*100/sum(JuvenilePopulation)) %>% data.frame() %>%
  rbind(list('Total', sum(.$OverallPopulation), sum(.$AdultPopulation), sum(.$JuvenilePopulation), 100.0, 100.0, 100.0), .) %>%
  mutate_if(is.integer, format, big.mark=',') %>%
  mutate(OverallPercentage = paste(round(OverallPercentage), '%', sep=''),
         AdultPercentage = paste(round(AdultPercentage), '%', sep=''),
         JuvenilePercentage = paste(round(JuvenilePercentage), '%', sep='')) %>%
  select(RaceEthnicity, OverallPopulation, OverallPercentage, AdultPopulation, AdultPercentage, JuvenilePopulation, JuvenilePercentage)
colnames(tbl1.1) <- c('Race/\nEthnicity', 'Overall\nPopulation', 'Overall\nPercent',
                      'Adult\nPopulation', 'Adult\nPercent', 'Juvenile\nPopulation', 'Juvenile\nPercent')

tbl1.2<-excelPivot1("ArrestType", arrests) %>%
  data.frame() %>% fixTable()
colnames(tbl1.2) <- c("Arrest Type", "Percent", "Total")

tbl1.3<-excelPivot1("Court", charges) %>%
  data.frame() %>% fixTable()

### Law Enforcement Data
tbl2.1<-excelPivot1("SuperCat", arrests) %>%
  data.frame() %>% fixTable()
colnames(tbl2.1) <- c("Offense", "Percent", "Total")

tbl2.2<-excelPivot1("RaceEthnicity", arrests) %>%
  data.frame() %>% fixTable()
colnames(tbl2.2) <- c("Race/Ethnicity", "Percent", "Total")

temp<-subset(arrests, ArrestType=="On-view/probable cause")
tbl2.3<-excelPivot2("AgeCat", "SuperCat", temp) %>%
  data.frame() %>% fixTable()
colnames(tbl2.3) <- c("Age Group", "Offense", "Percent", "Total")

temp<-subset(arrests, ArrestType=="Custody/warrant")
tbl2.4<-excelPivot2("AgeCat", "SuperCat", temp) %>%
  data.frame() %>% fixTable()
colnames(tbl2.4) <- c("Age Group", "Offense", "Percent", "Total")

temp<-subset(arrests, ArrestType=="Summons")
tbl2.5<-excelPivot2("AgeCat", "SuperCat", temp) %>%
  data.frame() %>% fixTable()
colnames(tbl2.5) <- c("Age Group", "Offense", "Percent", "Total")

temp<-subset(arrests, ArrestType=="On-view/probable cause" & AgeCat=="Adult")
tbl2.6<-excelPivot2("SuperCat", "RaceEthnicity", temp) %>%
  data.frame() %>% fixTable()
colnames(tbl2.6) <- c("Offense", "Race/Ethnicity", "Percent", "Total")

temp<-subset(arrests, ArrestType=="Custody/warrant" & AgeCat=="Adult")
tbl2.7<-excelPivot2("SuperCat", "RaceEthnicity", temp) %>%
  data.frame() %>% fixTable()
colnames(tbl2.7) <- c("Offense", "Race/Ethnicity", "Percent", "Total")

temp<-subset(arrests, ArrestType=="Summons" & AgeCat =="Adult")
tbl2.8<-excelPivot2("SuperCat", "RaceEthnicity", temp) %>%
  data.frame() %>% fixTable()
colnames(tbl2.8) <- c("Offense", "Race/Ethnicity", "Percent", "Total")

temp<-subset(arrests, ArrestType=="On-view/probable cause" & AgeCat=="Juvenile")
tbl2.9<-excelPivot2("SuperCat", "RaceEthnicity", temp) %>%
  data.frame() %>% fixTable()
colnames(tbl2.9) <- c("Offense", "Race/Ethnicity", "Percent", "Total")

temp<-subset(arrests, ArrestType=="Custody/warrant" & AgeCat=="Juvenile")
tbl2.10<-excelPivot2("SuperCat", "RaceEthnicity", temp) %>%
  data.frame() %>% fixTable()
colnames(tbl2.10) <- c("Offense", "Race/Ethnicity", "Percent", "Total")

temp<-subset(arrests, ArrestType=="Summons" & AgeCat =="Juvenile")
tbl2.11<-excelPivot2("SuperCat", "RaceEthnicity", temp) %>%
  data.frame() %>% fixTable()
colnames(tbl2.11) <- c("Offense", "Race/Ethnicity", "Percent", "Total")

temp<-subset(arrests, ArrestType=="On-view/probable cause")
tbl2.12<-excelPivot2("SuperCat", "Gender", temp) %>%
  data.frame() %>% fixTable()
colnames(tbl2.12) <- c("Offense", "Gender", "Percent", "Total")

temp<-subset(arrests, ArrestType=="Custody/warrant")
tbl2.13<-excelPivot2("SuperCat", "Gender", temp) %>%
  data.frame() %>% fixTable()
colnames(tbl2.13) <- c("Offense", "Gender", "Percent", "Total")

temp<-subset(arrests, ArrestType=="Summons")
tbl2.14<-excelPivot2("SuperCat", "Gender", temp) %>%
  data.frame() %>% fixTable()
colnames(tbl2.14) <- c("Offense", "Gender", "Percent", "Total")


### Court Case Processing
tbl3.1<-(excelPivot1("RaceEthnicity", charges)) %>%
  data.frame() %>% fixTable()
colnames(tbl3.1) <- c("Race/Ethnicity", "Percent", "Total")

tb1<-arrange(charges,SuperCat,RaceEthnicity)
tbl3.2<-(excelPivot2("SuperCat","RaceEthnicity",tb1)) %>%
  data.frame() %>% fixTable()
colnames(tbl3.2) <- c("Offense", "Race/Ethnicity", "Percent", "Total")

tb1<-arrange(charges,Gender,SuperCat)
tbl3.3<-(excelPivot2("Gender","SuperCat",tb1)) %>%
  data.frame() %>% fixTable()
colnames(tbl3.3) <- c("Gender", "Offense", "Percent", "Total")

tb1<-arrange(charges,Court,RaceEthnicity)
tbl3.4<-(excelPivot2("Court","RaceEthnicity",tb1)) %>%
  data.frame() %>% fixTable()
colnames(tbl3.4) <- c("Court", "Race/Ethnicity", "Percent", "Total")

tb1<-arrange(charges,Court,SuperCat)
tbl3.5<-(excelPivot2("Court","SuperCat",tb1)) %>%
  data.frame() %>% fixTable()
colnames(tbl3.5) <- c("Court", "Offense", "Percent", "Total")

tb1<-arrange(charges,Court,Gender)
tbl3.6<-(excelPivot2("Court","Gender",tb1)) %>%
  data.frame() %>% fixTable()

tb1<-arrange(charges,Court,CompletedTrial)
tbl3.7<-(excelPivot2("Court","CompletedTrial",tb1)) %>%
  data.frame() %>% fixTable()
colnames(tbl3.7) <- c("Court", "Completed Trial", "Percent", "Total")

tb1<-arrange(charges,SuperCat,CompletedTrial)
tbl3.8<-(excelPivot2("SuperCat","CompletedTrial",tb1)) %>%
  data.frame() %>% fixTable()
colnames(tbl3.8) <- c("Offense", "Completed Trial", "Percent", "Total")

#### Case Outcomes - The following tables have very long column names, so we insert \n to break them up
tb1<-arrange(charges,RaceEthnicity,SuperCat,Outcome)
temp<-subset(tb1, Court=="County")
tbl3.9<-(excelPivot3("RaceEthnicity","SuperCat","Outcome", temp)) %>%
  data.frame() %>% fixTable() %>% select(-contains(".1"))
colnames(tbl3.9) <- c("Race/\nEthnicity", "Offense", "Convicted\nas charged",
                      "Convicted\nother crime",
                      "Dismissed/\nCase Closed",
                      "Not yet\nresolved/\nCase Closed",
                      "Percent", "Total")
tbl3.9 <- tbl3.9[,names(tbl3.9) != 'Percent']

tb1<-arrange(charges,RaceEthnicity,SuperCat,Outcome)
temp<-subset(tb1, Court=="Adult District")
tbl3.10<-(excelPivot3("RaceEthnicity","SuperCat","Outcome", temp)) %>%
  data.frame() %>% fixTable() %>% select(-contains(".1"))
colnames(tbl3.10) <- c("Race/\nEthnicity", "Offense", "Convicted\nas charged",
                       "Convicted\nother crime",
                       "Dismissed/\nCase Closed",
                       "Not yet\nresolved/\nCase Closed",
                       "Percent", "Total")
tbl3.10 <- tbl3.10[,names(tbl3.10) != 'Percent']

tb1<-arrange(charges,RaceEthnicity,SuperCat,Outcome)
temp<-subset(tb1, Court=="Juvenile")
tbl3.11<-(excelPivot3("RaceEthnicity","SuperCat","Outcome", temp)) %>%
  data.frame() %>% fixTable() %>% select(-contains(".1"))
colnames(tbl3.11) <- c("Race/\nEthnicity", "Offense", "Convicted\nas charged",
                       "Convicted\nother crime",
                       "Dismissed/\nCase Closed",
                       "Not yet\nresolved/\nCase Closed",
                       "Percent", "Total")
tbl3.11 <- tbl3.11[,names(tbl3.11) != 'Percent']

#### Initial Sentences
temp<-subset(sentences, Court=="County")
temp$Sentence<-factor(temp$Sentence) # remove unused factors
tbl3.12<-(excelPivot2a("Sentence","SuperCat", temp)) %>%
            data.frame() %>% fixTable()

tbl3.13<-(excelPivot2a("Sentence","Gender", temp)) %>%
  data.frame() %>% fixTable()

tbl3.12N<-nrow(temp)
tbl3.13N<-tbl3.12N

tbl3.14<-(excelPivot2a("Sentence","RaceEthnicity",temp))
cNames <- colnames(tbl3.14)
tbl3.14 <- data.frame(tbl3.14) %>% fixTable()
colnames(tbl3.14) <- cNames

temp<-subset(sentences, Court=="County" & SuperCat=="Drugs")
temp$Sentence<-factor(temp$Sentence) # remove unused factors
tbl3.15<-(excelPivot2a("Sentence","RaceEthnicity",temp))
cNames <- colnames(tbl3.15)
tbl3.15 <- data.frame(tbl3.15) %>% fixTable()
colnames(tbl3.15) <- cNames

temp<-subset(sentences, Court=="County" & SuperCat=="Other")
temp$Sentence<-factor(temp$Sentence) # remove unused factors
tbl3.16<-(excelPivot2a("Sentence","RaceEthnicity",temp))
cNames <- colnames(tbl3.16)
tbl3.16 <- data.frame(tbl3.16) %>% fixTable()
colnames(tbl3.16) <- cNames

temp<-subset(sentences, Court=="County" & SuperCat=="Property")
temp$Sentence<-factor(temp$Sentence) # remove unused factors
tbl3.17<-(excelPivot2a("Sentence","RaceEthnicity",temp))
cNames <- colnames(tbl3.17)
tbl3.17 <- data.frame(tbl3.17) %>% fixTable()
colnames(tbl3.17) <- cNames

temp<-subset(sentences, Court=="County" & SuperCat=="Violent")
temp$Sentence<-factor(temp$Sentence) # remove unused factors
tbl3.18<-(excelPivot2a("Sentence","RaceEthnicity",temp))
cNames <- colnames(tbl3.18)
tbl3.18 <- data.frame(tbl3.18) %>% fixTable()
colnames(tbl3.18) <- cNames

temp<-subset(sentences, Court=="Adult District")
temp$Sentence<-factor(temp$Sentence) # remove unused factors
tbl3.19<-(excelPivot2a("Sentence","SuperCat", temp)) %>%
  data.frame() %>% fixTable()

tbl3.20<-(excelPivot2a("Sentence","Gender", temp)) %>%
  data.frame() %>% fixTable()

tbl3.21<-(excelPivot2a("Sentence","RaceEthnicity",temp))
cNames <- colnames(tbl3.21)
tbl3.21 <- data.frame(tbl3.21) %>% fixTable()
colnames(tbl3.21) <- cNames

temp<-subset(sentences, Court=="Adult District" & SuperCat=="Drugs")
temp$Sentence<-factor(temp$Sentence) # remove unused factors
tbl3.22<-(excelPivot2a("Sentence","RaceEthnicity",temp))
cNames <- colnames(tbl3.22)
tbl3.22 <- data.frame(tbl3.22) %>% fixTable()
colnames(tbl3.22) <- cNames

temp<-subset(sentences, Court=="Adult District" & SuperCat=="Other")
temp$Sentence<-factor(temp$Sentence) # remove unused factors
tbl3.23<-(excelPivot2a("Sentence","RaceEthnicity",temp))
cNames <- colnames(tbl3.23)
tbl3.23 <- data.frame(tbl3.23) %>% fixTable()
colnames(tbl3.23) <- cNames

temp<-subset(sentences, Court=="Adult District" & SuperCat=="Property")
temp$Sentence<-factor(temp$Sentence) # remove unused factors
tbl3.24<-(excelPivot2a("Sentence","RaceEthnicity",temp))
cNames <- colnames(tbl3.24)
tbl3.24 <- data.frame(tbl3.24) %>% fixTable()
colnames(tbl3.24) <- cNames

temp<-subset(sentences, Court=="Adult District" & SuperCat=="Violent")
temp$Sentence<-factor(temp$Sentence) # remove unused factors
tbl3.25<-(excelPivot2a("Sentence","RaceEthnicity",temp))
cNames <- colnames(tbl3.25)
tbl3.25 <- data.frame(tbl3.25) %>% fixTable()
colnames(tbl3.25) <- cNames

# Juvenile
temp<-subset(sentences, Court=="Juvenile")
temp$Sentence<-factor(temp$Sentence) # remove unused factors
tbl3.26<-(excelPivot2a("Sentence","SuperCat", temp)) %>%
  data.frame() %>% fixTable()

tbl3.27<-(excelPivot2a("Sentence","Gender", temp)) %>%
  data.frame() %>% fixTable()

tbl3.28<-(excelPivot2a("Sentence","RaceEthnicity",temp))
cNames <- colnames(tbl3.28)
tbl3.28 <- data.frame(tbl3.28) %>% fixTable()
colnames(tbl3.28) <- cNames

temp<-subset(sentences, Court=="Juvenile" & SuperCat=="Drugs")
temp$Sentence<-factor(temp$Sentence) # remove unused factors
tbl3.29<-(excelPivot2a("Sentence","RaceEthnicity",temp))
cNames <- colnames(tbl3.29)
tbl3.29 <- data.frame(tbl3.29) %>% fixTable()
colnames(tbl3.29) <- cNames

temp<-subset(sentences, Court=="Juvenile" & SuperCat=="Other")
temp$Sentence<-factor(temp$Sentence) # remove unused factors
tbl3.30<-(excelPivot2a("Sentence","RaceEthnicity",temp))
cNames <- colnames(tbl3.30)
tbl3.30 <- data.frame(tbl3.30) %>% fixTable()
colnames(tbl3.30) <- cNames

temp<-subset(sentences, Court=="Juvenile" & SuperCat=="Property")
temp$Sentence<-factor(temp$Sentence) # remove unused factors
tbl3.31<-(excelPivot2a("Sentence","RaceEthnicity",temp))
cNames <- colnames(tbl3.31)
tbl3.31 <- data.frame(tbl3.31) %>% fixTable()
colnames(tbl3.31) <- cNames

temp<-subset(sentences, Court=="Juvenile" & SuperCat=="Violent")
temp$Sentence<-factor(temp$Sentence) # remove unused factors
tbl3.32<-(excelPivot2a("Sentence","RaceEthnicity",temp))
cNames <- colnames(tbl3.32)
tbl3.32 <- data.frame(tbl3.32) %>% fixTable()
colnames(tbl3.32) <- cNames

#### Revocations
temp<-subset(sentences, (Court=="County" & (Sentence=="Probation/Intensive Supervision" | Sentence=="Deferred" | Sentence=="Unsupervised Probation")))
tbl3.33<-(excelPivot3("RaceEthnicity","SuperCat","WasRevoked", temp)) %>%
  data.frame() %>% fixTable()
tbl3.33 <- tbl3.33[,!(names(tbl3.33) %in% c("No.1", "Yes.1"))]
colnames(tbl3.33) <- c("Race/Ethnicity", "Offense", "No", "Yes", "Percent", "Total")
tbl3.33 <- tbl3.33[,names(tbl3.33) != 'Percent']

tbl3.34<-(excelPivot3("Gender","SuperCat","WasRevoked", temp)) %>%
  data.frame() %>% fixTable()
tbl3.34 <- tbl3.34[,!(names(tbl3.34) %in% c("No.1", "Yes.1"))]
colnames(tbl3.34) <- c("Gender", "Offense", "No", "Yes", "Percent", "Total")
tbl3.34 <- tbl3.34[,names(tbl3.34) != 'Percent']

temp<-subset(sentences, (Court=="Adult District" & (Sentence=="Probation/Intensive Supervision" | Sentence=="Deferred" | Sentence=="Unsupervised Probation")))
tbl3.35<-(excelPivot3("RaceEthnicity","SuperCat","WasRevoked", temp)) %>%
  data.frame() %>% fixTable()
tbl3.35 <- tbl3.35[,!(names(tbl3.35) %in% c("No.1", "Yes.1"))]
colnames(tbl3.35) <- c("Race/Ethnicity", "Offense", "No", "Yes", "Percent", "Total")
tbl3.35 <- tbl3.35[,names(tbl3.35) != 'Percent']

tbl3.36<-(excelPivot3("Gender","SuperCat","WasRevoked", temp)) %>%
  data.frame() %>% fixTable()
tbl3.36 <- tbl3.36[,!(names(tbl3.36) %in% c("No.1", "Yes.1"))]
colnames(tbl3.36) <- c("Gender", "Offense", "No", "Yes", "Percent", "Total")
tbl3.36 <- tbl3.36[,names(tbl3.36) != 'Percent']

temp<-subset(sentences, (Court=="Juvenile" & (Sentence=="Probation/Intensive Supervision" | Sentence=="Deferred" | Sentence=="Unsupervised Probation")))
tbl3.37<-(excelPivot3("RaceEthnicity","SuperCat","WasRevoked", temp)) %>%
  data.frame() %>% fixTable()
tbl3.37 <- tbl3.37[,!(names(tbl3.37) %in% c("No.1", "Yes.1"))]
colnames(tbl3.37) <- c("Race/Ethnicity", "Offense", "No", "Yes", "Percent", "Total")
tbl3.37 <- tbl3.37[,names(tbl3.37) != 'Percent']

tbl3.38<-(excelPivot3("Gender","SuperCat","WasRevoked", temp)) %>%
  data.frame() %>% fixTable()
tbl3.38 <- tbl3.38[,!(names(tbl3.38) %in% c("No.1", "Yes.1"))]
colnames(tbl3.38) <- c("Gender", "Offense", "No", "Yes", "Percent", "Total")
tbl3.38 <- tbl3.38[,names(tbl3.38) != 'Percent']

### Section 4: Colorado State Parole Board Decisions
# Acquire data/tables from Annie Andrews: anne.andrews@state.co.us


### Section 5: Additional Information
##### DOC Sentences for Adults in District Court
options(scipen=0)
jdM<-sentences %>% filter(Cascls=="CR" & grepl("F",TopConvictionLawcls))
jdM<-within(jdM,RaceEthnicity<-relevel(RaceEthnicity,ref=4))
jdM<-within(jdM,SuperCat<-relevel(SuperCat,ref=2))
lg1<-glm(isDOC~PriorCases+hasCOVStrict+RaceEthnicity+Gender+SuperCat+OtherCount+ConvictionLevel+IndexIsCOV, data=jdM, family='binomial')
# Null Hypothesis - model isnt any better than the null model
1- pchisq(lg1$null.deviance - lg1$deviance, lg1$df.null- lg1$df.residual)
# Pseudo R2
ps1<-PseudoR2(lg1)
# Confidence Intervals
ci = exp(confint(lg1)) %>% data.frame() %>%
  .[c('RaceEthnicityBlack', 'RaceEthnicityHispanic*', 'RaceEthnicityOther'), ]
row.names(ci) <- c('Black', 'Hispanic*', 'Other')
colnames(ci) <- c('Lower', 'Upper')
ci = rbind('White' = c(1,1), ci, 'Total' = c(1,1))
# Constructing Table
tbl5.1 <- arrange(jdM, RaceEthnicity, isDOC) %>% select(RaceEthnicity, isDOC) %>% group_by(RaceEthnicity) %>%
  summarise(N=n(),
            isNotDOC=sum(isDOC == 'No'),
            isDOC=sum(isDOC == 'Yes')) %>%
  mutate(RaceEthnicity = as.character(RaceEthnicity),
         Percent = round(N*100/sum(N)),
         isNotDOC = round(isNotDOC*100/N),
         isDOC = round(isDOC*100/N)) %>%
  select(RaceEthnicity, N, Percent, isNotDOC, isDOC) %>%
  rbind(., c('Total', sum(.$N), 100, round(sum(jdM$isDOC == 'No')*100/sum(.$N)), round(sum(jdM$isDOC == 'Yes')*100/sum(.$N)))) %>%
  mutate(N = paste(format(as.numeric(N), big.mark=','), ' (', Percent, '%)', sep=''),
         isNotDOC = paste(isNotDOC, '%', sep=''),
         isDOC = paste(isDOC, '%', sep='')) %>%
  cbind(., ci) %>%
  mutate(Lower = Lower - 1,
         Upper = Upper - 1,
         SigDiff = ifelse(Lower*Upper <= 0, 'No', 'Yes')) %>%
  select(RaceEthnicity, N, isNotDOC, isDOC, SigDiff) %>% data.frame() %>% fixTable()
tbl5.1$SigDiff[1] <- 'Reference'
tbl5.1$SigDiff[5] <- '--'
rownames(tbl5.1) <- NULL
colnames(tbl5.1) <- c("Race/Ethnicity",
                      "Total N\n(Percent of N)",
                      "No DOC\nSentence",
                      "DOC Sentence",
                      "Significant\nDifference from\nReference")

##### Deferred Sentences for Adults in District Court
jdM<-sentences %>% filter(Cascls=="CR")
jdM<-within(jdM,RaceEthnicity<-relevel(RaceEthnicity,ref=4)) # Whites
jdM<-within(jdM,SuperCat<-relevel(SuperCat,ref=2))
lg2<-glm(isDefer~PriorCases+RaceEthnicity+Gender+SuperCat+OtherCount+IndexIsCOV, data=jdM, family=binomial("logit"))
ps2<-PseudoR2(lg2)
# Confidence Intervals
ci = exp(confint(lg2)) %>% data.frame() %>%
  .[c('RaceEthnicityBlack', 'RaceEthnicityHispanic*', 'RaceEthnicityOther'), ]
row.names(ci) <- c('Black', 'Hispanic*', 'Other')
colnames(ci) <- c('Lower', 'Upper')
ci = rbind('White' = c(1,1), ci, 'Total' = c(1,1))
# Constructing Table
tbl5.2 <- arrange(jdM, RaceEthnicity, isDefer) %>% select(RaceEthnicity, isDefer) %>% group_by(RaceEthnicity) %>%
  summarise(N=n(),
            isNotDefer=sum(isDefer == 'No'),
            isDefer=sum(isDefer == 'Yes')) %>%
  mutate(RaceEthnicity = as.character(RaceEthnicity),
         Percent = round(N*100/sum(N)),
         isNotDefer = round(isNotDefer*100/N),
         isDefer = round(isDefer*100/N)) %>%
  select(RaceEthnicity, N, Percent, isNotDefer, isDefer) %>%
  rbind(., c('Total', sum(.$N), 100, round(sum(jdM$isDefer == 'No')*100/sum(.$N)), round(sum(jdM$isDefer == 'Yes')*100/sum(.$N)))) %>%
  mutate(N = paste(format(as.numeric(N), big.mark=','), ' (', Percent, '%)', sep=''),
         isNotDefer = paste(isNotDefer, '%', sep=''),
         isDefer = paste(isDefer, '%', sep='')) %>%
  cbind(., ci) %>%
  mutate(Lower = Lower - 1,
         Upper = Upper - 1,
         SigDiff = ifelse(Lower*Upper <= 0, 'No', 'Yes')) %>%
  select(RaceEthnicity, N, isNotDefer, isDefer, SigDiff) %>% data.frame() %>% fixTable()
tbl5.2$SigDiff[1] <- 'Reference'
tbl5.2$SigDiff[5] <- '--'
rownames(tbl5.2) <- NULL
colnames(tbl5.2) <- c("Race/Ethnicity",
                      "Total N\n(Percent of N)",
                      "No Deferred\nSentence",
                      "Deferred\nSentence",
                      "Significant\nDifference from\nReference")

##### Deferred Sentences for Juveniles
jdM<-sentences %>% filter(Cascls=="JD")
jdM<-within(jdM,RaceEthnicity<-relevel(RaceEthnicity,ref=4)) # Whites
jdM<-within(jdM,SuperCat<-relevel(SuperCat,ref=2))
lg3<-glm(isDefer~PriorCases+RaceEthnicity+Gender+SuperCat+OtherCount+IndexIsCOV, data=jdM, family=binomial("logit"))
ps3<-PseudoR2(lg3)
# Confidence Intervals
ci = exp(confint(lg3)) %>% data.frame() %>%
  .[c('RaceEthnicityBlack', 'RaceEthnicityHispanic*', 'RaceEthnicityOther'), ]
row.names(ci) <- c('Black', 'Hispanic*', 'Other')
colnames(ci) <- c('Lower', 'Upper')
ci = rbind('White' = c(1,1), ci, 'Total' = c(1,1))
# Constructing Table
tbl5.3 <- arrange(jdM, RaceEthnicity, isDefer) %>% select(RaceEthnicity, isDefer) %>% group_by(RaceEthnicity) %>%
  summarise(N=n(),
            isNotDefer=sum(isDefer == 'No'),
            isDefer=sum(isDefer == 'Yes')) %>%
  mutate(RaceEthnicity = as.character(RaceEthnicity),
         Percent = round(N*100/sum(N)),
         isNotDefer = round(isNotDefer*100/N),
         isDefer = round(isDefer*100/N)) %>%
  select(RaceEthnicity, N, Percent, isNotDefer, isDefer) %>%
  rbind(., c('Total', sum(.$N), 100, round(sum(jdM$isDefer == 'No')*100/sum(.$N)), round(sum(jdM$isDefer == 'Yes')*100/sum(.$N)))) %>%
  mutate(N = paste(format(as.numeric(N), big.mark=','), ' (', Percent, '%)', sep=''),
         isNotDefer = paste(isNotDefer, '%', sep=''),
         isDefer = paste(isDefer, '%', sep='')) %>%
  cbind(., ci) %>%
  mutate(Lower = Lower - 1,
         Upper = Upper - 1,
         SigDiff = ifelse(Lower*Upper <= 0, 'No', 'Yes')) %>%
  select(RaceEthnicity, N, isNotDefer, isDefer, SigDiff) %>% data.frame() %>% fixTable()
tbl5.3$SigDiff[1] <- 'Reference'
tbl5.3$SigDiff[5] <- '--'
rownames(tbl5.3) <- NULL
colnames(tbl5.3) <- c("Race/Ethnicity",
                      "Total N\n(Percent of N)",
                      "No Deferred\nSentence",
                      "Deferred\nSentence",
                      "Significant\nDifference from\nReference")


##### DYS Sentences for Juveniles
#jdM<-sentences %>% filter(Cascls=="JD" & grepl("F",TopConvictionLawcls))
jdM<-sentences %>% filter(Cascls=="JD")
jdM<-within(jdM,RaceEthnicity<-relevel(RaceEthnicity,ref=4)) # whites
jdM<-within(jdM,SuperCat<-relevel(SuperCat,ref=2))
lg4<-glm(isDYS~PriorCases+RaceEthnicity+Gender+SuperCat+OtherCount+IndexIsCOV+ConvictionLevel, data=jdM, family=binomial("logit"))
ps4<-PseudoR2(lg4)
# Confidence Intervals
ci = exp(confint(lg4)) %>% data.frame() %>%
  .[c('RaceEthnicityBlack', 'RaceEthnicityHispanic*', 'RaceEthnicityOther'), ]
row.names(ci) <- c('Black', 'Hispanic*', 'Other')
colnames(ci) <- c('Lower', 'Upper')
ci = rbind('White' = c(1,1), ci, 'Total' = c(1,1))
# Constructing Table
tbl5.4 <- arrange(jdM, RaceEthnicity, isDYS) %>% select(RaceEthnicity, isDYS) %>% group_by(RaceEthnicity) %>%
  summarise(N=n(),
            isNotDYS=sum(isDYS == 'No'),
            isDYS=sum(isDYS == 'Yes')) %>%
  mutate(RaceEthnicity = as.character(RaceEthnicity),
         Percent = round(N*100/sum(N)),
         isNotDYS = round(isNotDYS*100/N),
         isDYS = round(isDYS*100/N)) %>%
  select(RaceEthnicity, N, Percent, isNotDYS, isDYS) %>%
  rbind(., c('Total', sum(.$N), 100, round(sum(jdM$isDYS == 'No')*100/sum(.$N)), round(sum(jdM$isDYS == 'Yes')*100/sum(.$N)))) %>%
  mutate(N = paste(format(as.numeric(N), big.mark=','), ' (', Percent, '%)', sep=''),
         isNotDYS = paste(isNotDYS, '%', sep=''),
         isDYS = paste(isDYS, '%', sep='')) %>%
  cbind(., ci) %>%
  mutate(Lower = Lower - 1,
         Upper = Upper - 1,
         SigDiff = ifelse(Lower*Upper <= 0, 'No', 'Yes')) %>%
  select(RaceEthnicity, N, isNotDYS, isDYS, SigDiff) %>% data.frame() %>% fixTable()
tbl5.4$SigDiff[1] <- 'Reference'
tbl5.4$SigDiff[5] <- '--'
rownames(tbl5.4) <- NULL
colnames(tbl5.4) <- c("Race/Ethnicity",
                      "Total N\n(Percent of N)",
                      "No DYS\nSentence",
                      "DYS\nSentence",
                      "Significant\nDifference from\nReference")

# check for VIF - higher than 4 is bad

vif(lg1)
vif(lg2)
vif(lg3)
vif(lg4)

# Other values for the report

# Percent with H code
charges %>%
  group_by(RaceCode) %>%
  summarise(N=n()) %>%
  mutate(Percent=(N/sum(N))*100)


#unknown genders removed
chargesTemp<-read.csv(paste("SB15-185Charges_", year, ".csv", sep=""), stringsAsFactors=FALSE)
sentencesTemp<-read.csv(paste("SB15-185Sentences_", year, ".csv", sep=""), stringsAsFactors=FALSE)
table(chargesTemp$Sex)
table(sentencesTemp$Sex)

#other cases
sentences %>%
  group_by(Cascls,Concurrents) %>%
  summarise(N=n()) %>%
  mutate(Percent = (N/sum(N))) %>%
  mutate(HadOthers=(1-Percent)*100)

# revos reinstated
temp<-subset(sentences, ((Sentence=="Probation/Intensive Supervision" | Sentence=="Deferred" | Sentence=="Unsupervised Probation")))
temp %>%
  filter(RevoCat != "Not revoked") %>%
  group_by(RevoCat) %>%
  summarise(N=n()) %>%
  mutate(Percent = (N/sum(N))*100)

