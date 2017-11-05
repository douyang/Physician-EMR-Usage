############ GET RELATIONSHIP BETWEEN HOURS AND PATIENT OUTCOMES#######

library(ggplot2)

data <- read.csv("UniquePatientDateTeamMedicineOnlyWithMedicineLOSAndAverageTimes.csv", stringsAsFactors = FALSE)
str(data)

summary(lm(data = data, medicineLOS~averageResidentTime))
qplot(data = data, averageResidentTime, medicineLOS)

summary(lm(data = data, medicineLOS~averageInternTime))
qplot(data = data, averageInternTime, medicineLOS)

data2 <- read.csv("PatientToDeceasedStatus.csv")

data3 <- merge(data, data2, by = "DE_PAT_ID", all.x = TRUE)
summary(lm(data = data3, died~averageResidentTime))
summary(lm(data = data3, died~averageInternTime))

summary(lm(data = data3, X30dayReadmission ~averageResidentTime))
summary(lm(data = data3, X30dayReadmission ~averageInternTime))

#### WEEKLYHOURS FROM TEAM ANALYSIS 3-5 ####
data3$week <- format(strptime(data3$ADMSN_DATE, "%m/%d/%Y"), format="%Y-%U")
data3$team <- "NA"
data3[str_detect(data$PROV_NAME, "TT MED UNIV A"),]$team <- "A"
data3[str_detect(data$PROV_NAME, "TT MED UNIV B"),]$team <- "B"
data3[str_detect(data$PROV_NAME, "TT MED UNIV C"),]$team <- "C"
data3[str_detect(data$PROV_NAME, "TT MED UNIV D"),]$team <- "D"
data3[str_detect(data$PROV_NAME, "TT MED UNIV E"),]$team <- "E"
data3$merger <- paste(data3$team, data3$week)
str(data3)

weeklyhours$over80 <- weeklyhours$totalhours>80
weeklyhours$weekDate <-  strptime(weeklyhours$week, "%Y-%U") 
weeklyhours$team2 <- "NA"
weeklyhours[(weeklyhours$team) =="Stanford Team A",]$team2 <- "A"
weeklyhours[(weeklyhours$team) =="Stanford Team B",]$team2 <- "B"
weeklyhours[(weeklyhours$team) =="Stanford Team C",]$team2 <- "C"
weeklyhours[(weeklyhours$team) =="Stanford Team D",]$team2 <- "D"
weeklyhours[(weeklyhours$team) =="Stanford Team E",]$team2 <- "E"
weeklyhours$merger <- paste(weeklyhours$team2, weeklyhours$week)
str(weeklyhours)

combineData <- merge(data3, weeklyhours, by = "merger", all.x = TRUE)
str(combineData)
summary(combineData)

summary(lm(data = combineData, died ~ over80 + medicineLOS + averageResidentTime))
summary(lm(data = combineData, died ~ over80 ))
summary(lm(data = combineData, X30dayReadmission ~ over80 + medicineLOS + averageResidentTime))
summary(lm(data = combineData, X30dayReadmission ~ over80 ))
summary(lm(data = combineData, medicineLOS ~ over80 + averageResidentTime))
summary(lm(data = combineData, medicineLOS ~ over80 ))

t.test(combineData[combineData$over80,]$died, combineData[!combineData$over80,]$died) 
t.test(combineData[combineData$over80,]$X30dayReadmission , combineData[!combineData$over80,]$X30dayReadmission ) 
t.test(combineData[combineData$over80,]$medicineLOS , combineData[!combineData$over80,]$medicineLOS ) 




#### patientDataWithInHospitalDeath from TeamAnalysis5(TeamMortality) #####
combineData$merger2 <- paste(combineData$PROV_NAME, combineData$HOSP_ADMSN_TIME)

#write.csv(combineData, "allPatientInformation.csv")
#write.csv(patientDataWithInHospitalDeath2 , "allPatientInformation2(death).csv")

### Garbage Collection ####
#combineData2 <- read.csv("allPatientInformation.csv")

patientDataWithInHospitalDeath2 <- read.csv("allPatientInformation2(death).csv")

combineDataWDeath <- merge(combineData, patientDataWithInHospitalDeath2, "merger2", all.x = TRUE)

str(combineDataWDeath[-36] )
summary(combineDataWDeath)

t.test(combineDataWDeath[combineDataWDeath$over80,]$inhospitalMortality, combineDataWDeath[!combineDataWDeath$over80,]$inhospitalMortality)

#unique(combineDataWDeath$merger2)
#duplicated(!combineDataWDeath$merger2)
#qplot(

dedupdata <- ddply(combineDataWDeath[-36], c("merger2"), summarize, DE_PAT_ID = DE_PAT_ID[1], HOSP_ADMSN_TIME = HOSP_ADMSN_TIME[1],
											over80 = sum(over80, na.rm = TRUE) >= 1, los = los[1], died = sum( inhospitalMortality, na.rm = TRUE) >= 1,
											readmission = sum(X30dayReadmission, na.rm = TRUE) >= 1)

summary(dedupdata)

t.test(dedupdata[dedupdata$over80,]$los, dedupdata[!dedupdata$over80,]$los)
t.test(dedupdata[dedupdata$over80,]$died , dedupdata[!dedupdata$over80,]$died )
t.test(dedupdata[dedupdata$over80,]$readmission , dedupdata[!dedupdata$over80,]$readmission )

str(dedupdata[dedupdata$over80,])
str(dedupdata[!dedupdata$over80,])

757/5573
# 0.4947066