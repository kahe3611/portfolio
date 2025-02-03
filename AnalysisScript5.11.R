setwd("C:/Users/khodson/Desktop/2015.2020/Data/2015.2020/Data")
list.files()

### 2015 data
# Arrests
ar2015<-read.csv("2015/SB15-185Arrests2015.csv")
colnames(ar2015)
# Charges Filed
fil2015<-read.csv("2015/SB15-185Charges2015.csv")
colnames(fil2015)
# Sentences
sen2015<-read.csv("2015/SB15-185Sentences2015.csv")
colnames(sen2015)


### 2016 data
# Arrests
ar2016<-read.csv("2016/SB15-185Arrests2016.csv")
colnames(ar2016)
# Charges Filed
fil2016<-read.csv("2016/SB15-185Charges2016.csv")
colnames(fil2016)
# Sentences
sen2016<-read.csv("2016/SB15-185Sentences2016.csv")
colnames(sen2016)


### 2017 data
# Arrests
ar2017<-read.csv("2017/SB15-185Arrests2017.csv")
colnames(ar2017)
# Charges Filed
fil2017<-read.csv("2017/SB15-185Charges2017.csv")
colnames(fil2017)
# Sentences
sen2017<-read.csv("2017/SB15-185Sentences2017.csv")
colnames(sen2017)


### 2018 data
# Arrests
ar2018<-read.csv("2018/SB15-185Arrests2018.csv")
colnames(ar2018)
# Charges Filed
fil2018<-read.csv("2018/SB15-185Charges2018.csv")
colnames(fil2018)
# Sentences
sen2018<-read.csv("2018/SB15-185Sentences2018.csv")
colnames(sen2018)


### 2019 data
# Arrests
ar2019<-read.csv("2019/SB15-185Arrests2019.csv")
colnames(ar2019)
# Charges Filed
fil2019<-read.csv("2019/SB15-185Charges2019.csv")
colnames(fil2019)
# Sentences
sen2019<-read.csv("2019/SB15-185Sentences2019.csv")
colnames(sen2019)




#### Juvenile Arrest Data

juvar2015<-ar2015[ar2015$AgeCat=="Juvenile",]
juvar2016<-ar2016[ar2016$AgeCat=="Juvenile",]
juvar2017<-ar2017[ar2017$AgeCat=="Juvenile",]
juvar2018<-ar2018[ar2018$AgeCat=="Juvenile",]
juvar2019<-ar2019[ar2019$AgeCat=="Juvenile",]


#### Juvenile Filling Data

juvfil2015<-fil2015[fil2015$AgeCat=="Juvenile",]
juvfil2016<-fil2016[fil2016$AgeCat=="Juvenile",]
juvfil2017<-fil2017[fil2017$AgeCat=="Juvenile",]
juvfil2018<-fil2018[fil2018$AgeCat=="Juvenile",]
juvfil2019<-fil2019[fil2019$AgeCat=="Juvenile",]


#### Juvenile Sentencing Data

juvsen2015<-sen2015[sen2015$AgeCat=="Juvenile",]
juvsen2016<-sen2016[sen2016$AgeCat=="Juvenile",]
juvsen2017<-sen2017[sen2017$AgeCat=="Juvenile",]
juvsen2018<-sen2018[sen2018$AgeCat=="Juvenile",]
juvsen2019<-sen2019[sen2019$AgeCat=="Juvenile",]




#### Adult Arrest Data

aduar2015<-ar2015[ar2015$AgeCat=="Adult",]
aduar2016<-ar2016[ar2016$AgeCat=="Adult",]
aduar2017<-ar2017[ar2017$AgeCat=="Adult",]
aduar2018<-ar2018[ar2018$AgeCat=="Adult",]
aduar2019<-ar2019[ar2019$AgeCat=="Adult",]


#### Adult Filling Data

adufil2015<-fil2015[fil2015$AgeCat=="Adult",]
adufil2016<-fil2016[fil2016$AgeCat=="Adult",]
adufil2017<-fil2017[fil2017$AgeCat=="Adult",]
adufil2018<-fil2018[fil2018$AgeCat=="Adult",]
adufil2019<-fil2019[fil2019$AgeCat=="Adult",]


#### Adult Sentencing Data

adusen2015<-sen2015[sen2015$AgeCat=="Adult",]
adusen2016<-sen2016[sen2016$AgeCat=="Adult",]
adusen2017<-sen2017[sen2017$AgeCat=="Adult",]
adusen2018<-sen2018[sen2018$AgeCat=="Adult",]
adusen2019<-sen2019[sen2019$AgeCat=="Adult",]




#### County Court Filing Data

cocfil2015<-fil2015[fil2015$Court=="County",]
cocfil2016<-fil2016[fil2016$Court=="County",]
cocfil2017<-fil2017[fil2017$Court=="County",]
cocfil2018<-fil2018[fil2018$Court=="County",]
cocfil2019<-fil2019[fil2019$Court=="County",]


#### County Court Sentencing Data

cocsen2015<-sen2015[sen2015$Court=="County",]
cocsen2016<-sen2016[sen2016$Court=="County",]
cocsen2017<-sen2017[sen2017$Court=="County",]
cocsen2018<-sen2018[sen2018$Court=="County",]
cocsen2019<-sen2019[sen2019$Court=="County",]




#### County Court Juvenile Filing Data

cocjuvfil2015<-cocfil2015[cocfil2015$AgeCat=="Juvenile",]
cocjuvfil2016<-cocfil2016[cocfil2016$AgeCat=="Juvenile",]
cocjuvfil2017<-cocfil2017[cocfil2017$AgeCat=="Juvenile",]
cocjuvfil2018<-cocfil2018[cocfil2018$AgeCat=="Juvenile",]
cocjuvfil2019<-cocfil2019[cocfil2019$AgeCat=="Juvenile",]


#### County Court Juvenile Sentencing Data

cocjuvsen2015<-cocsen2015[cocsen2015$AgeCat=="Juvenile",]
cocjuvsen2016<-cocsen2016[cocsen2016$AgeCat=="Juvenile",]
cocjuvsen2017<-cocsen2017[cocsen2017$AgeCat=="Juvenile",]
cocjuvsen2018<-cocsen2018[cocsen2018$AgeCat=="Juvenile",]
cocjuvsen2019<-cocsen2019[cocsen2019$AgeCat=="Juvenile",]




#### County Court Adult Filing Data

cocadufil2015<-cocfil2015[cocfil2015$AgeCat=="Adult",]
cocadufil2016<-cocfil2016[cocfil2016$AgeCat=="Adult",]
cocadufil2017<-cocfil2017[cocfil2017$AgeCat=="Adult",]
cocadufil2018<-cocfil2018[cocfil2018$AgeCat=="Adult",]
cocadufil2019<-cocfil2019[cocfil2019$AgeCat=="Adult",]


#### County Court Adult Sentencing Data

cocadusen2015<-cocsen2015[cocsen2015$AgeCat=="Adult",]
cocadusen2016<-cocsen2016[cocsen2016$AgeCat=="Adult",]
cocadusen2017<-cocsen2017[cocsen2017$AgeCat=="Adult",]
cocadusen2018<-cocsen2018[cocsen2018$AgeCat=="Adult",]
cocadusen2019<-cocsen2019[cocsen2019$AgeCat=="Adult",]




#####################################################################




#### District Court Filing Data

dicfil2015<-fil2015[fil2015$Court=="Adult District",]
dicfil2016<-fil2016[fil2016$Court=="Adult District",]
dicfil2017<-fil2017[fil2017$Court=="Adult District",]
dicfil2018<-fil2018[fil2018$Court=="Adult District",]
dicfil2019<-fil2019[fil2019$Court=="Adult District",]


#### District Court Sentencing Data

dicsen2015<-sen2015[sen2015$Court=="Adult District",]
dicsen2016<-sen2016[sen2016$Court=="Adult District",]
dicsen2017<-sen2017[sen2017$Court=="Adult District",]
dicsen2018<-sen2018[sen2018$Court=="Adult District",]
dicsen2019<-sen2019[sen2019$Court=="Adult District",]




#### District Court Juvenile Filing Data

dicjuvfil2015<-dicfil2015[dicfil2015$AgeCat=="Juvenile",]
dicjuvfil2016<-dicfil2016[dicfil2016$AgeCat=="Juvenile",]
dicjuvfil2017<-dicfil2017[dicfil2017$AgeCat=="Juvenile",]
dicjuvfil2018<-dicfil2018[dicfil2018$AgeCat=="Juvenile",]
dicjuvfil2019<-dicfil2019[dicfil2019$AgeCat=="Juvenile",]


#### District Court Juvenile Sentencing Data

dicjuvsen2015<-dicsen2015[dicsen2015$AgeCat=="Juvenile",]
dicjuvsen2016<-dicsen2016[dicsen2016$AgeCat=="Juvenile",]
dicjuvsen2017<-dicsen2017[dicsen2017$AgeCat=="Juvenile",]
dicjuvsen2018<-dicsen2018[dicsen2018$AgeCat=="Juvenile",]
dicjuvsen2019<-dicsen2019[dicsen2019$AgeCat=="Juvenile",]




#### District Court Adult Filing Data

dicadufil2015<-dicfil2015[dicfil2015$AgeCat=="Adult",]
dicadufil2016<-dicfil2016[dicfil2016$AgeCat=="Adult",]
dicadufil2017<-dicfil2017[dicfil2017$AgeCat=="Adult",]
dicadufil2018<-dicfil2018[dicfil2018$AgeCat=="Adult",]
dicadufil2019<-dicfil2019[dicfil2019$AgeCat=="Adult",]


#### District Court Adult Sentencing Data

dicadusen2015<-dicsen2015[dicsen2015$AgeCat=="Adult",]
dicadusen2016<-dicsen2016[dicsen2016$AgeCat=="Adult",]
dicadusen2017<-dicsen2017[dicsen2017$AgeCat=="Adult",]
dicadusen2018<-dicsen2018[dicsen2018$AgeCat=="Adult",]
dicadusen2019<-dicsen2019[dicsen2019$AgeCat=="Adult",]




#### District Court Adult Felony Filing Data

dicadufelfil2015<-dicfil2015[grep("F",dicfil2015$TopFilingLawcls),]
dicadufelfil2016<-dicfil2016[grep("F",dicfil2016$TopFilingLawcls),]
dicadufelfil2017<-dicfil2017[grep("F",dicfil2017$TopFilingLawcls),]
dicadufelfil2018<-dicfil2018[grep("F",dicfil2018$TopFilingLawcls),]
dicadufelfil2019<-dicfil2019[grep("F",dicfil2019$TopFilingLawcls),]


#### District Court Adult Felony Sentencing Data

dicadufelsen2015<-dicsen2015[grep("F",dicsen2015$TopConvictionLawcls),]
dicadufelsen2016<-dicsen2016[grep("F",dicsen2016$TopConvictionLawcls),]
dicadufelsen2017<-dicsen2017[grep("F",dicsen2017$TopConvictionLawcls),]
dicadufelsen2018<-dicsen2018[grep("F",dicsen2018$TopConvictionLawcls),]
dicadufelsen2019<-dicsen2019[grep("F",dicsen2019$TopConvictionLawcls),]




#### District Court Adult Misdemeanor + Petty Offense Filing Data

dicadumpofil2015<-dicadufil2015[grep("M|PO",dicadufil2015$TopFilingLawcls),]
dicadumpofil2016<-dicadufil2016[grep("M|PO",dicadufil2016$TopFilingLawcls),]
dicadumpofil2017<-dicadufil2017[grep("M|PO",dicadufil2017$TopFilingLawcls),]
dicadumpofil2018<-dicadufil2018[grep("M|PO",dicadufil2018$TopFilingLawcls),]
dicadumpofil2019<-dicadufil2019[grep("M|PO",dicadufil2019$TopFilingLawcls),]


#### District Court Adult Misdemeanor + Petty Offense Sentencing Data

dicadumposen2015<-dicadusen2015[grep("M|PO",dicadusen2015$TopConvictionLawcls),]
dicadumposen2016<-dicadusen2016[grep("M|PO",dicadusen2016$TopConvictionLawcls),]
dicadumposen2017<-dicadusen2017[grep("M|PO",dicadusen2017$TopConvictionLawcls),]
dicadumposen2018<-dicadusen2018[grep("M|PO",dicadusen2018$TopConvictionLawcls),]
dicadumposen2019<-dicadusen2019[grep("M|PO",dicadusen2019$TopConvictionLawcls),]






#####################################################################




#### Juvenile Court Filing Data

jucfil2015<-fil2015[fil2015$Court=="Juvenile District",]
jucfil2016<-fil2016[fil2016$Court=="Juvenile",]
jucfil2017<-fil2017[fil2017$Court=="Juvenile",]
jucfil2018<-fil2018[fil2018$Court=="Juvenile",]
jucfil2019<-fil2019[fil2019$Court=="Juvenile",]


#### Juvenile Court Sentencing Data

jucsen2015<-sen2015[sen2015$Court=="Juvenile District",]
jucsen2016<-sen2016[sen2016$Court=="Juvenile",]
jucsen2017<-sen2017[sen2017$Court=="Juvenile",]
jucsen2018<-sen2018[sen2018$Court=="Juvenile",]
jucsen2019<-sen2019[sen2019$Court=="Juvenile",]




#### Juvenile Court Juvenile Filing Data

jucjuvfil2015<-jucfil2015[jucfil2015$AgeCat=="Juvenile",]
jucjuvfil2016<-jucfil2016[jucfil2016$AgeCat=="Juvenile",]
jucjuvfil2017<-jucfil2017[jucfil2017$AgeCat=="Juvenile",]
jucjuvfil2018<-jucfil2018[jucfil2018$AgeCat=="Juvenile",]
jucjuvfil2019<-jucfil2019[jucfil2019$AgeCat=="Juvenile",]


#### Juvenile Court Juvenile Sentencing Data

jucjuvsen2015<-jucsen2015[jucsen2015$AgeCat=="Juvenile",]
jucjuvsen2016<-jucsen2016[jucsen2016$AgeCat=="Juvenile",]
jucjuvsen2017<-jucsen2017[jucsen2017$AgeCat=="Juvenile",]
jucjuvsen2018<-jucsen2018[jucsen2018$AgeCat=="Juvenile",]
jucjuvsen2019<-jucsen2019[jucsen2019$AgeCat=="Juvenile",]




#### Juvenile Court Adult Filing Data

jucadufil2015<-jucfil2015[jucfil2015$AgeCat=="Adult",]
jucadufil2016<-jucfil2016[jucfil2016$AgeCat=="Adult",]
jucadufil2017<-jucfil2017[jucfil2017$AgeCat=="Adult",]
jucadufil2018<-jucfil2018[jucfil2018$AgeCat=="Adult",]
jucadufil2019<-jucfil2019[jucfil2019$AgeCat=="Adult",]


#### Juvenile Court Adult Sentencing Data

jucadusen2015<-jucsen2015[jucsen2015$AgeCat=="Adult",]
jucadusen2016<-jucsen2016[jucsen2016$AgeCat=="Adult",]
jucadusen2017<-jucsen2017[jucsen2017$AgeCat=="Adult",]
jucadusen2018<-jucsen2018[jucsen2018$AgeCat=="Adult",]
jucadusen2019<-jucsen2019[jucsen2019$AgeCat=="Adult",]




#### Juvenile Court Juvenile Felony Filing Data

jucjuvfelfil2015<-jucjuvfil2015[grep("F",jucjuvfil2015$TopFilingLawcls),]
jucjuvfelfil2016<-jucjuvfil2016[grep("F",jucjuvfil2016$TopFilingLawcls),]
jucjuvfelfil2017<-jucjuvfil2017[grep("F",jucjuvfil2017$TopFilingLawcls),]
jucjuvfelfil2018<-jucjuvfil2018[grep("F",jucjuvfil2018$TopFilingLawcls),]
jucjuvfelfil2019<-jucjuvfil2019[grep("F",jucjuvfil2019$TopFilingLawcls),]


#### Juvenile Court Juvenile Felony Sentencing Data

jucjuvfelsen2015<-jucjuvsen2015[grep("F",jucjuvsen2015$TopConvictionLawcls),]
jucjuvfelsen2016<-jucjuvsen2016[grep("F",jucjuvsen2016$TopConvictionLawcls),]
jucjuvfelsen2017<-jucjuvsen2017[grep("F",jucjuvsen2017$TopConvictionLawcls),]
jucjuvfelsen2018<-jucjuvsen2018[grep("F",jucjuvsen2018$TopConvictionLawcls),]
jucjuvfelsen2019<-jucjuvsen2019[grep("F",jucjuvsen2019$TopConvictionLawcls),]




#### Juvenile Court Juvenile Misdemeanor + Petty Offense Filing Data

jucjuvmpofil2015<-jucjuvfil2015[grep("M|PO",jucjuvfil2015$TopFilingLawcls),]
jucjuvmpofil2016<-jucjuvfil2016[grep("M|PO",jucjuvfil2016$TopFilingLawcls),]
jucjuvmpofil2017<-jucjuvfil2017[grep("M|PO",jucjuvfil2017$TopFilingLawcls),]
jucjuvmpofil2018<-jucjuvfil2018[grep("M|PO",jucjuvfil2018$TopFilingLawcls),]
jucjuvmpofil2019<-jucjuvfil2019[grep("M|PO",jucjuvfil2019$TopFilingLawcls),]


#### Juvenile Court Juvenile Misdemeanor + Petty Offense Sentencing Data

jucjuvmposen2015<-jucjuvsen2015[grep("M|PO",jucjuvsen2015$TopConvictionLawcls),]
jucjuvmposen2016<-jucjuvsen2016[grep("M|PO",jucjuvsen2016$TopConvictionLawcls),]
jucjuvmposen2017<-jucjuvsen2017[grep("M|PO",jucjuvsen2017$TopConvictionLawcls),]
jucjuvmposen2018<-jucjuvsen2018[grep("M|PO",jucjuvsen2018$TopConvictionLawcls),]
jucjuvmposen2019<-jucjuvsen2019[grep("M|PO",jucjuvsen2019$TopConvictionLawcls),]


