# TEAM ANALYSIS 8 - PUBLISHABLE FIGURES 



setwd("C:\\Users\\David\\Dropbox\\Stanford\\Epic Research\\AccessLog\\Census3.0")

library(ggplot2)
library(gridExtra)
library(plyr)
library(reshape2)
library(strptime)
library(stringr)
library(scales)
library(ggthemes)



data <- read.csv("StanfordWardResidentsWithTeamCleaned-WithInnerRange.csv", stringsAsFactors = FALSE)
data2 <- read.csv("medicineTeamCensusTotal-FromPython.csv", stringsAsFactors = FALSE)

data$merger <- paste(str_sub(data$team, 15, 15), format(strptime(data$date2, "%m/%d/%Y"), "%Y-%m-%d"))
data2$merger <- paste(data2$teamTotal, data2$ACCESS_DATE)

allData <- merge(data,data2, all.x = TRUE, by = "merger")
str(allData)

#allData$firstActionTime <- strptime(allData$X5thPercentile, "%H:%M:%OS")
allData$firstActionTime <- strptime(allData$firstAction , "%H:%M:%OS")
allData$lastActionTime <- strptime(allData$X95thPercentile, "%H:%M:%OS")
allData$difference2 <- as.numeric( allData$lastActionTime - allData$firstActionTime )/60/60
allData$date <- strptime(allData$ACCESS_DATE, "%Y-%m-%d")
allData$month <- format(strptime(allData$ACCESS_DATE, "%Y-%m-%d"), "%Y-%b")

data3 <- read.csv("ResidentToID.csv", stringsAsFactors = FALSE)
data3$resident <- data3$ID
allData2 <- merge(allData, data3, by = "resident", all.x = TRUE)
str(allData2)

#Will retry with resident ID

Insert A1A2 data

meltInterns <- read.csv("meltInternsForEachDay-WithID.csv", stringsAsFactors = FALSE)
meltInterns$resident <- as.numeric(meltInterns$resident)

meltResidents <- read.csv("meltResidentsForEachDay-WithID.csv", stringsAsFactors = FALSE)
meltResidents$resident <- as.numeric(meltResidents$resident)

allData2Interns <- allData2[allData2$pgy == 1,]
allData2Residents <- allData2[allData2$pgy != 1,]

str(meltInterns)
str(allData2Interns)

allData2Interns$mergerbyIDandDate <- paste(allData2Interns$date2, allData2Interns$resident)
meltInterns$mergerbyIDandDate <- paste(meltInterns$date, meltInterns$resident)
meltInterns <- data.frame(meltInterns[6], meltInterns[3])
allData2Interns <- merge(allData2Interns, meltInterns, by = "mergerbyIDandDate", all.x = TRUE)
str(allData2Interns)


allData2Residents$mergerbyIDandDate <- paste(allData2Residents$date2, allData2Residents$resident)
meltResidents$mergerbyIDandDate <- paste(meltResidents$date, meltResidents$resident)
meltResidents <- data.frame(meltResidents[6], meltResidents[3])
allData2Residents <- merge(allData2Residents, meltResidents, by = "mergerbyIDandDate", all.x = TRUE)
str(allData2Residents)

allData2 <- rbind(allData2Interns, allData2Residents)

###########

data4 <- read.csv("medicineTeamCensusSpecific-FromPython.csv")
subIdatesA <- data4[str_detect(data4$PROV_NAME, "TT MED UNIV A3"),]$ACCESS_DATE
subIdatesB <- data4[str_detect(data4$PROV_NAME, "TT MED UNIV B3"),]$ACCESS_DATE
subIdatesC <- data4[str_detect(data4$PROV_NAME, "TT MED UNIV C3"),]$ACCESS_DATE
subIdatesD <- data4[str_detect(data4$PROV_NAME, "TT MED UNIV D3"),]$ACCESS_DATE
subIdatesE <- data4[str_detect(data4$PROV_NAME, "TT MED UNIV E3"),]$ACCESS_DATE
allData2$hasSubI <- NA
allData2[!is.na(allData2$teamTotal) & allData2$teamTotal == "A",]$hasSubI <- allData2[!is.na(allData2$teamTotal) & allData2$teamTotal == "A",]$ACCESS_DATE %in% subIdatesA
allData2[!is.na(allData2$teamTotal) & allData2$teamTotal == "B",]$hasSubI <- allData2[!is.na(allData2$teamTotal) & allData2$teamTotal == "B",]$ACCESS_DATE %in% subIdatesB
allData2[!is.na(allData2$teamTotal) & allData2$teamTotal == "C",]$hasSubI <- allData2[!is.na(allData2$teamTotal) & allData2$teamTotal == "C",]$ACCESS_DATE %in% subIdatesC
allData2[!is.na(allData2$teamTotal) & allData2$teamTotal == "D",]$hasSubI <- allData2[!is.na(allData2$teamTotal) & allData2$teamTotal == "D",]$ACCESS_DATE %in% subIdatesD
allData2[!is.na(allData2$teamTotal) & allData2$teamTotal == "E",]$hasSubI <- allData2[!is.na(allData2$teamTotal) & allData2$teamTotal == "E",]$ACCESS_DATE %in% subIdatesE

############## weekly duty hours ##########

allData2$week <- format(strptime(allData2$date2, "%m/%d/%Y"), format="%Y-%U")
allData2$counter <- 1
allData2$sorter <- paste(allData2$resident, "    ", allData2$week)
allData2$firstActionTime <- as.POSIXct(allData2$firstActionTime)
allData2$lastActionTime <- as.POSIXct(allData2$lastActionTime)
allData2$date <- as.POSIXct(allData2$date)

allData3 <- allData2[allData2$difference2 > 10000/60/60,]
allData3 <- allData3[allData3 $difference2 < 70000/60/60,]

str(allData3 )
weeklyhours <- ddply(allData3[-c(23:24, 26) ], .(sorter), summarize,
		resident = resident[1],
		week = week[1],
		totalhours = sum(difference2), total = sum(counter),
		pgy = pgy[1], 
		hasSubI = hasSubI[1],
		averageTeamSize = sum(team_size)/sum(counter),
		team = team[1],
		counter = counter[1],
		variable = variable[1]) 


str(weeklyhours)
weeklyhours$weekAsNumber <- as.numeric(strptime(weeklyhours$week, "%Y-%U"))

weeklyhours$over80 <- weeklyhours$totalhours > 80
weeklyhours$residentOver80 <- weeklyhours$totalhours > 80 & (weeklyhours$pgy != 1)
weeklyhours$internOver80 <- weeklyhours$totalhours > 80 & (weeklyhours$pgy == 1)
weeklyhours$weekDate <-  strptime(weeklyhours$week, "%Y-%U") 

sum(weeklyhours$residentOver80)
sum(weeklyhours$internOver80)

sumMedicineTeams<- read.csv("medicineTeamCensusSpecific-FromPython.csv")
#sumMedicineTeams<- read.csv("medicineTeamCensusTotal-FromPython.csv")
str(sumMedicineTeams)
sumMedicineTeams$census <- sumMedicineTeams$team_size

sumMedicineTeams$date <- strptime(sumMedicineTeams$ACCESS_DATE, "%Y-%m-%d")

data <- read.csv("UniquePatientDateTeamMedicineOnlyWithMedicineLOSAndFirstDayAnd30DayReadmission.csv", stringsAsFactors = FALSE)
data$causedReadmission <- data$X30dayReadmission == 1
data$causedReadmission <- !data$causedReadmission

data <- read.csv("JChi_DOuyang_all_tteamv6.csv", stringsAsFactors = FALSE)
str(data)

summarizedData <- ddply(data, .(DE_PAT_ID), summarize, deathTimeFromLastAdmission = DAYS_LOG_ENC_DISCH_DEATH[1], 
				ACCESS_DATE = ACCESS_DATE[1], ADMSN_TIME = HOSP_ADMSN_DT_TM[1], DISCH_TIME = HOSP_DISCH_DT_TM[1])
str(summarizedData)

data2 <- read.csv("PatientToDeceasedStatus.csv", stringsAsFactors = FALSE)
str(data2)

summarizedData2 <- merge(summarizedData, data2, by = "DE_PAT_ID")
summarizedData2$inhospitalMortality <- 0 
summarizedData2[!is.na(summarizedData2$deathTimeFromLastAdmission) & summarizedData2$deathTimeFromLastAdmission <= 0,]$inhospitalMortality <- 1

data3 <- read.csv("UniquePatientDateTeamMedicineOnlyWithMedicineLOSAndFirstDayAnd30DayReadmission.csv")
str(data3)

allData2 <- merge(data3, summarizedData2, "DE_PAT_ID", all.x = TRUE)
str(allData2)
allData2 <- ddply(allData2, .(DE_PAT_ID), transform, numHospitalizations = sum(count))
allData2$diedInHosp <- allData2$inhospitalMortality == 1


allData2$diedInHosp <- !allData2$diedInHosp 
allData2[is.na(allData2$diedInHosp ),]$diedInHosp <- TRUE

data <- read.csv("UniquePatientDateTeamMedicineOnlyWithMedicineLOSAndAverageTimes.csv", stringsAsFactors = FALSE)
data2 <- read.csv("PatientToDeceasedStatus.csv")
data3 <- merge(data, data2, by = "DE_PAT_ID", all.x = TRUE)

#### WEEKLYHOURS FROM TEAM ANALYSIS 3-5 ####
data3$week <- format(strptime(data3$ADMSN_DATE, "%m/%d/%Y"), format="%Y-%U")
data3$team <- "NA"
data3[str_detect(data3$PROV_NAME, "TT MED UNIV A"),]$team <- "A"
data3[str_detect(data3$PROV_NAME, "TT MED UNIV B"),]$team <- "B"
data3[str_detect(data3$PROV_NAME, "TT MED UNIV C"),]$team <- "C"
data3[str_detect(data3$PROV_NAME, "TT MED UNIV D"),]$team <- "D"
data3[str_detect(data3$PROV_NAME, "TT MED UNIV E"),]$team <- "E"
data3$variable <- "NA"
data3[str_detect(data3$PROV_NAME, "TT MED UNIV A1"),]$variable <- "A1"
data3[str_detect(data3$PROV_NAME, "TT MED UNIV B1"),]$variable <- "B1"
data3[str_detect(data3$PROV_NAME, "TT MED UNIV C1"),]$variable <- "C1"
data3[str_detect(data3$PROV_NAME, "TT MED UNIV D1"),]$variable <- "D1"
data3[str_detect(data3$PROV_NAME, "TT MED UNIV E1"),]$variable <- "E1"
data3[str_detect(data3$PROV_NAME, "TT MED UNIV A2"),]$variable <- "A2"
data3[str_detect(data3$PROV_NAME, "TT MED UNIV B2"),]$variable <- "B2"
data3[str_detect(data3$PROV_NAME, "TT MED UNIV C2"),]$variable <- "C2"
data3[str_detect(data3$PROV_NAME, "TT MED UNIV D2"),]$variable <- "D2"
data3[str_detect(data3$PROV_NAME, "TT MED UNIV E2"),]$variable <- "E2"

data3$merger <- paste(data3$team, data3$week)
data3$mergerIntern <- paste(data3$variable, data3$week)
str(data3)
table(data3$variable)
head(data3[data3$variable == "NA",])

weeklyhours$weekDate <-  strptime(weeklyhours$week, "%Y-%U") 
weeklyhours$team2 <- "NA"
weeklyhours[(weeklyhours$team) =="Stanford Team A",]$team2 <- "A"
weeklyhours[(weeklyhours$team) =="Stanford Team B",]$team2 <- "B"
weeklyhours[(weeklyhours$team) =="Stanford Team C",]$team2 <- "C"
weeklyhours[(weeklyhours$team) =="Stanford Team D",]$team2 <- "D"
weeklyhours[(weeklyhours$team) =="Stanford Team E",]$team2 <- "E"
weeklyhours$InternAndWeek <- paste(weeklyhours$variable, weeklyhours$week)
str(weeklyhours)

weeklyhoursSummarized <- ddply(weeklyhours[-16], .(InternAndWeek), summarize,
					week = week[1],
					variable = variable[1],
					residentOver80 = mean(residentOver80) > 0,
					internOver80 = mean(internOver80) > 0)

table(weeklyhoursSummarized$internOver80)
table(weeklyhoursSummarized$residentOver80)

weeklyhoursSummarizedResidents <- weeklyhoursSummarized[nchar(weeklyhoursSummarized$variable)==1,]
weeklyhoursSummarizedInterns <- weeklyhoursSummarized[nchar(weeklyhoursSummarized$variable)==2,]

str(weeklyhoursSummarizedResidents )
str(weeklyhoursSummarizedInterns )

weeklyhoursSummarizedResidents  <- data.frame(weeklyhoursSummarizedResidents[1], weeklyhoursSummarizedResidents[4])
weeklyhoursSummarizedInterns <- data.frame(weeklyhoursSummarizedInterns[1], weeklyhoursSummarizedInterns [5])

str(weeklyhoursSummarizedResidents )
str(weeklyhoursSummarizedInterns )


combineData <- merge(data3, weeklyhoursSummarizedInterns, by.x = "mergerIntern", by.y = "InternAndWeek", all.x = TRUE)
combineData <- merge(combineData , weeklyhoursSummarizedResidents , by.x = "merger", by.y = "InternAndWeek", all.x = TRUE)
combineData$over80 <- str_detect(paste(combineData$internOver80,combineData$residentOver80 ), "TRUE")

str(combineData)
summary(combineData)
head(combineData)
combineData$merger2 <- paste(combineData$PROV_NAME, combineData$HOSP_ADMSN_TIME)


patientDataWithInHospitalDeath2 <- read.csv("allPatientInformation2(death).csv")
combineDataWDeath <- merge(combineData, patientDataWithInHospitalDeath2, "merger2", all.x = TRUE)

dedupdata <- ddply(combineDataWDeath[-36], c("merger2"), summarize, DE_PAT_ID = DE_PAT_ID[1], HOSP_ADMSN_TIME = HOSP_ADMSN_TIME[1],
											over80 = sum(over80, na.rm = TRUE) >= 1, los = los[1], died = sum( inhospitalMortality, na.rm = TRUE) >= 1,
											readmission = sum(X30dayReadmission, na.rm = TRUE) >= 1,
											week = week[1],
											averageTeamCensus = mean(averageTeamCensus),
											ADMSN_DATE = ADMSN_DATE[1] )

dedupdata$uniqueHosp <- paste(dedupdata$ADMSN_DATE, dedupdata$DE_PAT_ID)

dedupdataFinal <- ddply(dedupdata,.(uniqueHosp), summarize, DE_PAT_ID = DE_PAT_ID[1], 
								HOSP_ADMSN_TIME = HOSP_ADMSN_TIME[1],
								over80 = sum(over80, na.rm = TRUE) >= 1, 
								los = los[1], 
								died = sum(died) > 0,
								readmission = sum(readmission) > 0,
								week = week[1],
								averageTeamCensus = mean(averageTeamCensus),
								ADMSN_DATE = ADMSN_DATE[1] )

str(dedupdataFinal )
summary(dedupdataFinal )

				
######### ADD WHETHER WENT TO THE ICU #########

data <- read.csv("JChi_DOuyang_all_tteamv6-Sorted.csv", stringsAsFactors = FALSE)
str(data)
table(data[str_detect(data$PROV_NAME, "MICU"),]$PROV_NAME)


#data <- data[str_detect(data$PROV_NAME, "MICU") | str_detect(data$PROV_NAME, "TT MED UNIV "),]

summarizedData <-  ddply(data, .(DE_PAT_ID, HOSP_ADMSN_DT_TM), summarize, deathTimeFromLastAdmission = DAYS_LOG_ENC_DISCH_DEATH[1], 
				ACCESS_DATE = ACCESS_DATE[1], ADMSN_TIME = HOSP_ADMSN_DT_TM[1], DISCH_TIME = HOSP_DISCH_DT_TM[1], 
				ICU_Care = sum(str_detect(PROV_NAME, "MICU"))>0, Med_Care = sum(str_detect(PROV_NAME, "TT MED UNIV "))>0,
				firstTeam = PROV_NAME[1])

table(summarizedData$firstTeam)
summary(summarizedData$ICU_Care)

#   Mode   FALSE    TRUE    NA's 
#logical   17682    1804       0 

#1804 /(17682 + 1804 ) # About 9.3% of patients went to ICU

# WEED OUT ALL MICU -> MEDICINE TRANSFERS, INSTEAD OF MEDICINE TO ICU TRANSFERS ###
summarizedData[!str_detect(summarizedData$firstTeam, "TT MED UNIV "),]$ICU_Care <- FALSE

summary(summarizedData$ICU_Care)

str(summarizedData)
str(summarizedData[summarizedData$Med_Care == TRUE,])

summarizedData$date2 <- strptime(substr(summarizedData$ADMSN_TIME,1,9),"%m/%d/%Y" )
summarizedData$mergerByPatientHospitalization <- paste(summarizedData$DE_PAT_ID, summarizedData$date2)
str(summarizedData)

dedupdataFinal$date2 <- strptime(dedupdataFinal$ADMSN_DATE, "%m/%d/%Y")
dedupdataFinal$mergerByPatientHospitalization <- paste(dedupdataFinal$DE_PAT_ID, dedupdataFinal$date2)
str(dedupdataFinal)


dedupdata2 <- merge(dedupdataFinal, summarizedData, by = "mergerByPatientHospitalization", all.x = TRUE)
a <- dedupdata2[-22]
str(dedupdata2)
dedupdataFINAL <- ddply(a[-12], c("uniqueHosp"), summarize, DE_PAT_ID = DE_PAT_ID.x[1], HOSP_ADMSN_TIME = HOSP_ADMSN_TIME[1],
											over80 = sum(over80, na.rm = TRUE) >= 1, 
											los = los[1], 
											died = sum(died) > 0,
											readmission = sum(readmission) > 0,
											week = week[1],
											averageTeamCensus = mean(averageTeamCensus),
											ADMSN_DATE = ADMSN_DATE[1],
											ICU_Care = sum(ICU_Care) > 0,
											Med_care = sum(Med_Care) > 0 )

str(dedupdataFINAL)
summary(dedupdataFINAL)
dedupdataFINAL[is.na(dedupdataFINAL$ICU_Care),]$ICU_Care <- FALSE
dedupdataFINAL[is.na(dedupdataFINAL$Med_care),]$Med_care <- TRUE



str(dedupdataFINAL[dedupdataFINAL$over80,])
str(dedupdataFINAL[!dedupdataFINAL$over80,])


t.test(dedupdataFINAL[dedupdataFINAL$over80,]$los, dedupdataFINAL[!dedupdataFINAL$over80,]$los)
sd(dedupdataFINAL[dedupdataFINAL$over80,]$los)
sd(dedupdataFINAL[!dedupdataFINAL$over80,]$los)


t.test(dedupdataFINAL[dedupdataFINAL$over80,]$died , dedupdataFINAL[!dedupdataFINAL$over80,]$died )
table(dedupdataFINAL[dedupdataFINAL$over80,]$died)
table(dedupdataFINAL[!dedupdataFINAL$over80,]$died)
#chisq.test(matrix(c(62,1890,67,2748), ncol = 2))
chisq.test(matrix(c(41,1133,88,3505), ncol = 2))
41/(41+1133)
88/(88+3505)

chisq.test(matrix(c(46,1237,83,3401), ncol = 2))
46/(46+1237)
83/(83+3401)


t.test(dedupdataFINAL[dedupdataFINAL$over80,]$readmission , dedupdataFINAL[!dedupdataFINAL$over80,]$readmission )
table(dedupdataFINAL[dedupdataFINAL$over80,]$readmission)
table(dedupdataFINAL[!dedupdataFINAL$over80,]$readmission)
#chisq.test(matrix(c(267,1685,360,2455), ncol = 2))

chisq.test(matrix(c(161,1013,466,3127), ncol = 2))
161/(41+1133)
466/(88+3505)

chisq.test(matrix(c(185,1098,442,3042), ncol = 2))
185/(41+1133)
442/(88+3505)

t.test(dedupdataFINAL[dedupdataFINAL$over80,]$ICU_Care, dedupdataFINAL[!dedupdataFINAL$over80,]$ICU_Care)
table(dedupdataFINAL[dedupdataFINAL$over80,]$ICU_Care)
table(dedupdataFINAL[!dedupdataFINAL$over80,]$ICU_Care)
#chisq.test(matrix(c(69,1883,68,2747), ncol = 2))
chisq.test(matrix(c(47,1127,90,3503), ncol = 2))
47/(41+1133)
90/(88+3505)

chisq.test(matrix(c(48,1235,89,3395), ncol = 2))
48/(41+1133)
89/(88+3505)

dedupdataFINAL$ICUorDEATH <- dedupdataFINAL$died | dedupdataFINAL$ICU_Care 
t.test(dedupdataFINAL[dedupdataFINAL$over80,]$ICUorDEATH , dedupdataFINAL[!dedupdataFINAL$over80,]$ICUorDEATH )
table(dedupdataFINAL[dedupdataFINAL$over80,]$ICUorDEATH)
table(dedupdataFINAL[!dedupdataFINAL$over80,]$ICUorDEATH)

dedupdataFINAL$composite <- dedupdataFINAL$died | dedupdataFINAL$ICU_Care | dedupdataFINAL$readmission
t.test(dedupdataFINAL[dedupdataFINAL$over80,]$composite , dedupdataFINAL[!dedupdataFINAL$over80,]$composite )
table(dedupdataFINAL[dedupdataFINAL$over80,]$composite )
table(dedupdataFINAL[!dedupdataFINAL$over80,]$composite )
#chisq.test(matrix(c(374,1578,470,2345), ncol = 2))

chisq.test(matrix(c(233,941,611,2982), ncol = 2))
233/(41+1133)
611/(88+3505)

chisq.test(matrix(c(261,1022,583,2901), ncol = 2))
261/(41+1133)
583/(88+3505)




summary(dedupdataFINAL)

mean(dedupdataFINAL$los)
sd(dedupdataFINAL$los)
mean(dedupdataFINAL$died)
mean(dedupdataFINAL$readmission)
length(unique(dedupdata$DE_PAT_ID))
str(dedupdataFINAL)

1952 / 4767 

write.csv(dedupdataFINAL, "patientsDeduplicated.csv")
write.csv(dedupdata, "patientsNOTDeduplicated.csv")

length(unique(paste(dedupdata$ADMSN_DATE, dedupdata$DE_PAT_ID))) #4767