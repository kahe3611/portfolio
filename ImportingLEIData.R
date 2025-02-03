require(stringi)
library(readxl)
library(dplyr)

year = "2023"
wd = paste("C:/LEI/Reports/", year, sep="")
setwd(wd)

# contacts
contactsTemp<-read.csv(paste("LEIcontacts_", year,".csv", sep=""), stringsAsFactors=FALSE)
table(contactsTemp$Sex)
contacts<-subset(contactsTemp,Sex!="Unknown")
table(contacts$reason)
# create factors
contacts$Outcome<-as.factor(contacts$Outcome)
contacts$Ethnicity<-ifelse(contacts$Ethnicity=="Hispanic","Hispanic*",contacts$Ethnicity)
contacts$Race<-as.factor(contacts$Race)
contacts$Age<-as.factor(contacts$Age)
contacts$Reason<-as.factor(contacts$Reason)
contacts$Gender<-as.factor(contacts$Sex)
contacts$gender<-as.factor(ifelse(contacts$Sex=="Female","F","M", "N")) # temp
contacts$Agency<-as.character(contacts$Agency)
table(contacts$Outcome)
contacts$OutcomeCat<-ifelse(contacts$Outcome=="arrest" | contacts$Outcome=="arrest","citation","warning")
save(contacts, file="contacts.RData")

# search basis - evaluating data structure and trying out code and see what I get
search_basisTemp<-read.csv(paste("LEIsearch_basis_", year,".csv", sep=""), stringsAsFactors=FALSE)
search_basis<-subset(search_basisTemp,Sex!="Unknown")
search_basis$Ethnicity<-ifelse(search_basis$Ethnicity=="Hispanic","Hispanic*",search_basis$Ethnicity)
search_basis$Race<-as.factor(search_basis$Race)
search_basis$Reason<-as.factor(search_basis$Reason)
search_basis$Initiation<-as.factor(search_basis$Initiation)
search_basis$Sentence<-as.factor(search_basis$Sentence)
search_basis$Gender<-as.factor(search_basis$Sex)
search_basis$CitizenInjury<-as.factor(search_basis$CitizenInjury)
search_basis$warrant<-as.factor(ifelse(search_basis$warrant== "Yes","No"))
search_basis$isAmbulance <- as.factor(ifelse(grepl("Defer",search_basis$Ambulance),"Yes","No"))
search_basis$UOF <- as.factor(ifelse(search_basis$UOF=="Force Used","Yes","No"))
search_basis$Contraband<-ifelse(search_basis$Contraband == "Contraband Found","Yes","No")
search_basis$hasCOVStrict<-as.factor(search_basis$hasCOVStrict)
search_basis$Age<-as.factor(search_basis$Age)

search_basis$Agency<-as.character(search_basis$Agency)

search_basis$ConvictionLevel<-sapply(search_basis$TopConvictionLawcls,FUN=function(x) {ifelse(x %in% lawclsOrders,
                                                                                        which(lawclsOrders==x),
                                                                                        which(lawclsOrders==''))})
search_basis$IndexIsCOV<-as.factor(search_basis$IndexIsCOV)
save(search_basis, file="search_basis.RData")


# Arrests
arrests<-read.csv(paste("LEIArrests_", year,".csv", sep=""), stringsAsFactors=FALSE)
arrests$Gender<-as.factor(arrests$Sex)
arrests$AgeCat<-as.factor(arrests$AgeCat)
arrests$Race<-as.factor(arrests$Race)
arrests$Reason<-as.factor(arrests$Reason)
arrests$Offense<-as.factor(arrests$Offense)
save(arrests, file="arrests.RData")
table(arrests$ArrestType)

# demographics overview
library(openxlsx)
library(pivottabler)
temp<-read.xlsx("C:/SB20-217 LEI/Demographics/DemographicsDOLA.xlsx", sheet=year)
temp <- temp[, c('SEX', 'AGE', 'Agency', 'White', 'Black', 'Asian', 'Native American', 'Native Hawaiian/Other Pacific Islander',
                'Multi-racial', 'Other')]
demog<-subset(temp, AGE > 9)
demog$AgeGroup<-ifelse(demog$AGE<18,"Juvenile","Adult")

county <- read.xlsx("C:/SB20-217 LEI/Demographics/DemographicsDOLA.xlsx", sheet="County")
county <- as.character(county)
demog = merge(demog, county, by.x='COUNTY', by.y='County')
demog$White <- as.integer(demog$White)
demog$Black <- as.integer(demog$Black)
demog$NAmerican <- as.integer(demog$NAmerican)
demog$Hawaiian <- as.integer(demog$Hawaiian)
demog$Multiracial <- as.integer(demog$multiracial)
demog$Asian <- as.integer(demog$asian)
demog$Hispanic <- as.integer(demog$Hispanic)
demog$Other <- as.integer(demog$Other)

# You should be able to load a tibble and just input the current year's data into the appropriate locations
load("C:/SB20-217 LEI/Demographics/demo_tibble.RData")

#Should be able to do this in the ORS shared drive
#example: load"O:/Law Enforcement Integrity/Data/demo_tibble.RData")

x = demog %>%
  select(AgeGroup, White, Black, Hispanic, Other) %>%
  group_by(AgeGroup) %>%
  summarise(White=sum(White), Black=sum(Black), Hispanic=sum(Hispanic), Other=sum(Other))
COUNT = c(as.integer(x[x$AgeGroup=='Adult', 'Black']),
          as.integer(x[x$AgeGroup=='Adult', 'Hispanic']),
          as.integer(x[x$AgeGroup=='Adult', 'Other']),
          as.integer(x[x$AgeGroup=='Adult', 'White']),
          as.integer(x[x$AgeGroup=='Juvenile', 'Black']),
          as.integer(x[x$AgeGroup=='Juvenile', 'Hispanic']),
          as.integer(x[x$AgeGroup=='Juvenile', 'Other']),
          as.integer(x[x$AgeGroup=='Juvenile', 'White']))

tbl$COUNT <- COUNT

demogTbl <- tbl
save(demogTbl, file="demogTbl.RData")
