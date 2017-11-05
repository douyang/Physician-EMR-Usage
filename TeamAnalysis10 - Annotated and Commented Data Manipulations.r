###############################################
#                                             #
#  David Ouyang, Stanford Internal Medicine   #
#  2015, Ouyangd@stanford.edu                 #
#                                             #
###############################################



# All Data Manipulations with Comments and Improved Variable Naming

# Working Directory
setwd("C:\\Users\\David\\Dropbox\\Stanford\\Epic Research\\AccessLog\\Census3.0")


# Dependent Libraries
library(ggplot2)
library(gridExtra)
library(plyr)
library(reshape2)
library(stringr)
library(scales)
library(ggthemes)



residentEMRUseData <- read.csv("StanfordWardResidentsWithTeamCleaned.csv")
teamCensusDaily <- read.csv("medicineTeamCensusTotal-FromPython.csv")
str(residentEMRUseData )
str(teamCensusDaily)

# Create a  dummy variable that links these two data sets together by which team and what date, so have team census data in the residentEMRUseData dataset
residentEMRUseData$merger <- paste(str_sub(residentEMRUseData$team, 15, 15), format(strptime(residentEMRUseData$date2, "%m/%d/%Y"), "%Y-%m-%d"))
teamCensusDaily$merger <- paste(teamCensusDaily$teamTotal, teamCensusDaily$ACCESS_DATE)

# Combine two datasets into new "residentEMRUseDatawithCensus" data frame
residentEMRUseDatawithCensus <- merge(residentEMRUseData,teamCensusDaily, all.x = TRUE, by = "merger")
str(residentEMRUseDatawithCensus)


# Cast String Variables into Data Variables in R for easier manipulation in the future
residentEMRUseDatawithCensus$firstActionTime <- strptime(residentEMRUseDatawithCensus$firstAction, "%H:%M:%OS")
residentEMRUseDatawithCensus$lastActionTime <- strptime(residentEMRUseDatawithCensus$lastAction, "%H:%M:%OS")
residentEMRUseDatawithCensus$difference2 <- as.numeric( residentEMRUseDatawithCensus$lastActionTime  - residentEMRUseDatawithCensus$firstActionTime )/60/60


residentEMRUseDatawithCensus$date <- strptime(residentEMRUseDatawithCensus$ACCESS_DATE, "%Y-%m-%d")
residentEMRUseDatawithCensus$month <- format(strptime(residentEMRUseDatawithCensus$ACCESS_DATE, "%Y-%m-%d"), "%Y-%b")


# Match resident ID (a number) to resident name
residentToID <- read.csv("ResidentToID.csv", stringsAsFactors = FALSE)
residentToID$resident <- residentToID$ID
residentEMRUseDatawithCensusAndName <- merge(residentEMRUseDatawithCensus, residentToID, by = "resident", all.x = TRUE)
str(residentEMRUseDatawithCensusAndName)



############################################################################################################################################

# Map each Resident and Date to which team they are. Specifically, already have team ABCDE, but need to match interns to A1 or A2

# Import files matching each intern/resident to 
meltInterns <- read.csv("meltInternsForEachDay-WithID.csv", stringsAsFactors = FALSE)
meltInterns$resident <- as.numeric(meltInterns$resident)

meltResidents <- read.csv("meltResidentsForEachDay-WithID.csv", stringsAsFactors = FALSE)
meltResidents$resident <- as.numeric(meltResidents$resident)

residentEMRUseDatawithCensusAndNameJustInterns <- residentEMRUseDatawithCensusAndName[residentEMRUseDatawithCensusAndName$pgy == 1,]
residentEMRUseDatawithCensusAndNameJustResidents<- residentEMRUseDatawithCensusAndName[residentEMRUseDatawithCensusAndName$pgy != 1,]

str(meltInterns)
str(residentEMRUseDatawithCensusAndNameJustInterns )

# Match by Intern ID and Date
residentEMRUseDatawithCensusAndNameJustInterns$mergerbyIDandDate <- paste(residentEMRUseDatawithCensusAndNameJustInterns$date2, residentEMRUseDatawithCensusAndNameJustInterns$resident)
meltInterns$mergerbyIDandDate <- paste(meltInterns$date, meltInterns$resident)
meltInterns <- data.frame(meltInterns[6], meltInterns[3])
residentEMRUseDatawithCensusAndNameJustInterns <- merge(residentEMRUseDatawithCensusAndNameJustInterns, meltInterns, by = "mergerbyIDandDate", all.x = TRUE)
str(residentEMRUseDatawithCensusAndNameJustInterns)

# Match by Resident ID and Date
residentEMRUseDatawithCensusAndNameJustResidents$mergerbyIDandDate <- paste(residentEMRUseDatawithCensusAndNameJustResidents$date2, residentEMRUseDatawithCensusAndNameJustResidents$resident)
meltResidents$mergerbyIDandDate <- paste(meltResidents$date, meltResidents$resident)
meltResidents <- data.frame(meltResidents[6], meltResidents[3])
residentEMRUseDatawithCensusAndNameJustResidents<- merge(residentEMRUseDatawithCensusAndNameJustResidents, meltResidents, by = "mergerbyIDandDate", all.x = TRUE)
str(residentEMRUseDatawithCensusAndNameJustResidents)


# Merge Residents and Interns together again
residentEMRUseDatawithCensusAndName <- rbind(residentEMRUseDatawithCensusAndNameJustInterns , residentEMRUseDatawithCensusAndNameJustResidents)


################################################################################################################################################
# Break out team census by A1, A2, A3 to identify when there is a subintern

teamCensusDailyMoreSpecific  <- read.csv("medicineTeamCensusSpecific-FromPython.csv")
subIdatesA <- teamCensusDailyMoreSpecific[str_detect(teamCensusDailyMoreSpecific$PROV_NAME, "TT MED UNIV A3"),]$ACCESS_DATE
subIdatesB <- teamCensusDailyMoreSpecific[str_detect(teamCensusDailyMoreSpecific$PROV_NAME, "TT MED UNIV B3"),]$ACCESS_DATE
subIdatesC <- teamCensusDailyMoreSpecific[str_detect(teamCensusDailyMoreSpecific$PROV_NAME, "TT MED UNIV C3"),]$ACCESS_DATE
subIdatesD <- teamCensusDailyMoreSpecific[str_detect(teamCensusDailyMoreSpecific$PROV_NAME, "TT MED UNIV D3"),]$ACCESS_DATE
subIdatesE <- teamCensusDailyMoreSpecific[str_detect(teamCensusDailyMoreSpecific$PROV_NAME, "TT MED UNIV E3"),]$ACCESS_DATE
residentEMRUseDatawithCensusAndName$hasSubI <- NA
residentEMRUseDatawithCensusAndName[!is.na(residentEMRUseDatawithCensusAndName$teamTotal) & residentEMRUseDatawithCensusAndName$teamTotal == "A",]$hasSubI <- residentEMRUseDatawithCensusAndName[!is.na(residentEMRUseDatawithCensusAndName$teamTotal) & residentEMRUseDatawithCensusAndName$teamTotal == "A",]$ACCESS_DATE %in% subIdatesA
residentEMRUseDatawithCensusAndName[!is.na(residentEMRUseDatawithCensusAndName$teamTotal) & residentEMRUseDatawithCensusAndName$teamTotal == "B",]$hasSubI <- residentEMRUseDatawithCensusAndName[!is.na(residentEMRUseDatawithCensusAndName$teamTotal) & residentEMRUseDatawithCensusAndName$teamTotal == "B",]$ACCESS_DATE %in% subIdatesB
residentEMRUseDatawithCensusAndName[!is.na(residentEMRUseDatawithCensusAndName$teamTotal) & residentEMRUseDatawithCensusAndName$teamTotal == "C",]$hasSubI <- residentEMRUseDatawithCensusAndName[!is.na(residentEMRUseDatawithCensusAndName$teamTotal) & residentEMRUseDatawithCensusAndName$teamTotal == "C",]$ACCESS_DATE %in% subIdatesC
residentEMRUseDatawithCensusAndName[!is.na(residentEMRUseDatawithCensusAndName$teamTotal) & residentEMRUseDatawithCensusAndName$teamTotal == "D",]$hasSubI <- residentEMRUseDatawithCensusAndName[!is.na(residentEMRUseDatawithCensusAndName$teamTotal) & residentEMRUseDatawithCensusAndName$teamTotal == "D",]$ACCESS_DATE %in% subIdatesD
residentEMRUseDatawithCensusAndName[!is.na(residentEMRUseDatawithCensusAndName$teamTotal) & residentEMRUseDatawithCensusAndName$teamTotal == "E",]$hasSubI <- residentEMRUseDatawithCensusAndName[!is.na(residentEMRUseDatawithCensusAndName$teamTotal) & residentEMRUseDatawithCensusAndName$teamTotal == "E",]$ACCESS_DATE %in% subIdatesE




#################################################################################################################################################
############## Calculate weekly duty hours by summing weekly from Sunday to Saturday ##########

residentEMRUseDatawithCensusAndName$week <- format(strptime(residentEMRUseDatawithCensusAndName$date2, "%m/%d/%Y"), format="%Y-%U")
residentEMRUseDatawithCensusAndName$counter <- 1
residentEMRUseDatawithCensusAndName$sorter <- paste(residentEMRUseDatawithCensusAndName$resident, "    ", residentEMRUseDatawithCensusAndName$week)
residentEMRUseDatawithCensusAndName$firstActionTime <- as.POSIXct(residentEMRUseDatawithCensusAndName$firstActionTime)
residentEMRUseDatawithCensusAndName$lastActionTime <- as.POSIXct(residentEMRUseDatawithCensusAndName$lastActionTime)
residentEMRUseDatawithCensusAndName$date <- as.POSIXct(residentEMRUseDatawithCensusAndName$date)

# Remove outliers
# Remove days when resident worked less than 2 hours - Assumption, they were checking the computer on thier day off
# Remove days when resident worked more than 20 hours - Assumption, very small number of cases when there was computer activity only a little after midnight (throwing off difference calculation for the next day)
residentEMRUseDatawithCensusAndName <- residentEMRUseDatawithCensusAndName[residentEMRUseDatawithCensusAndName$difference2 > 10000/60/60,] 
residentEMRUseDatawithCensusAndName<- residentEMRUseDatawithCensusAndName[residentEMRUseDatawithCensusAndName$difference2 < 70000/60/60,]


weeklyhours <- ddply(residentEMRUseDatawithCensusAndName[-c(19:20, 22) ], .(sorter), summarize,
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


# Create binary variables based on weeklyhour calculations
weeklyhours$over80 <- weeklyhours$totalhours > 80
weeklyhours$residentOver80 <- weeklyhours$totalhours > 80 & (weeklyhours$pgy != 1)
weeklyhours$internOver80 <- weeklyhours$totalhours > 80 & (weeklyhours$pgy == 1)
weeklyhours$weekDate <-  strptime(weeklyhours$week, "%Y-%U") 

sum(weeklyhours$residentOver80)
sum(weeklyhours$internOver80)

#191/(191+574)





############################################################################################################
# Patient characteristics, hospital outcomes, LOS, death, and etc


patientDataWithOnePatientPerLine <- read.csv("UniquePatientDateTeamMedicineOnlyWithMedicineLOSAndAverageTimes.csv", stringsAsFactors = FALSE)

#Identify which team and whether team A1, A2, A3 etc
patientDataWithOnePatientPerLine$week <- format(strptime(patientDataWithOnePatientPerLine$ADMSN_DATE, "%m/%d/%Y"), format="%Y-%U")
patientDataWithOnePatientPerLine$team <- "NA"
patientDataWithOnePatientPerLine[str_detect(patientDataWithOnePatientPerLine$PROV_NAME, "TT MED UNIV A"),]$team <- "A"
patientDataWithOnePatientPerLine[str_detect(patientDataWithOnePatientPerLine$PROV_NAME, "TT MED UNIV B"),]$team <- "B"
patientDataWithOnePatientPerLine[str_detect(patientDataWithOnePatientPerLine$PROV_NAME, "TT MED UNIV C"),]$team <- "C"
patientDataWithOnePatientPerLine[str_detect(patientDataWithOnePatientPerLine$PROV_NAME, "TT MED UNIV D"),]$team <- "D"
patientDataWithOnePatientPerLine[str_detect(patientDataWithOnePatientPerLine$PROV_NAME, "TT MED UNIV E"),]$team <- "E"
patientDataWithOnePatientPerLine$variable <- "NA"
patientDataWithOnePatientPerLine[str_detect(patientDataWithOnePatientPerLine$PROV_NAME, "TT MED UNIV A1"),]$variable <- "A1"
patientDataWithOnePatientPerLine[str_detect(patientDataWithOnePatientPerLine$PROV_NAME, "TT MED UNIV B1"),]$variable <- "B1"
patientDataWithOnePatientPerLine[str_detect(patientDataWithOnePatientPerLine$PROV_NAME, "TT MED UNIV C1"),]$variable <- "C1"
patientDataWithOnePatientPerLine[str_detect(patientDataWithOnePatientPerLine$PROV_NAME, "TT MED UNIV D1"),]$variable <- "D1"
patientDataWithOnePatientPerLine[str_detect(patientDataWithOnePatientPerLine$PROV_NAME, "TT MED UNIV E1"),]$variable <- "E1"
patientDataWithOnePatientPerLine[str_detect(patientDataWithOnePatientPerLine$PROV_NAME, "TT MED UNIV A2"),]$variable <- "A2"
patientDataWithOnePatientPerLine[str_detect(patientDataWithOnePatientPerLine$PROV_NAME, "TT MED UNIV B2"),]$variable <- "B2"
patientDataWithOnePatientPerLine[str_detect(patientDataWithOnePatientPerLine$PROV_NAME, "TT MED UNIV C2"),]$variable <- "C2"
patientDataWithOnePatientPerLine[str_detect(patientDataWithOnePatientPerLine$PROV_NAME, "TT MED UNIV D2"),]$variable <- "D2"
patientDataWithOnePatientPerLine[str_detect(patientDataWithOnePatientPerLine$PROV_NAME, "TT MED UNIV E2"),]$variable <- "E2"


#Merge by week and team or intern
patientDataWithOnePatientPerLine$merger <- paste(patientDataWithOnePatientPerLine$team, patientDataWithOnePatientPerLine$week)
patientDataWithOnePatientPerLine$mergerIntern <- paste(patientDataWithOnePatientPerLine$variable, patientDataWithOnePatientPerLine$week)
str(patientDataWithOnePatientPerLine)
table(patientDataWithOnePatientPerLine$variable)

# There are a bunch of subi assignments which will not be matched and throw NAs, this is to verify and identify them
head(patientDataWithOnePatientPerLine[patientDataWithOnePatientPerLine$variable == "NA",])


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



# Combine Patient Characteristics with Resident EMR use characteristics (know which people are working more than 80 hours a week)
combineData <- merge(patientDataWithOnePatientPerLine, weeklyhoursSummarizedInterns, by.x = "mergerIntern", by.y = "InternAndWeek", all.x = TRUE)
combineData <- merge(combineData , weeklyhoursSummarizedResidents , by.x = "merger", by.y = "InternAndWeek", all.x = TRUE)
combineData$over80 <- str_detect(paste(combineData$internOver80,combineData$residentOver80 ), "TRUE") # PRESUMPTION IS THAT LESS THEN 80 UNLESS TRIGGER OVER 80 AND THEN BECOMES TRUE

str(combineData)
summary(combineData)
head(combineData)
combineData$merger2 <- paste(combineData$PROV_NAME, combineData$HOSP_ADMSN_TIME)


patientDataWithInHospitalDeath2 <- read.csv("PatientInHospitalDeathAndHospitalizationStatus.csv")
combineDataWDeath <- merge(combineData, patientDataWithInHospitalDeath2, "merger2", all.x = TRUE)



# Team level data, there is one row for each team-patient assignment. IE if patient admitted and went from A2 to B1 because of bounceback, listed twice
teamLevelData <- ddply(combineDataWDeath[-36], c("merger2"), summarize, DE_PAT_ID = DE_PAT_ID[1], HOSP_ADMSN_TIME = HOSP_ADMSN_TIME[1],
											over80 = sum(over80, na.rm = TRUE) >= 1, los = los[1], died = sum( inhospitalMortality, na.rm = TRUE) >= 1,
											readmission = sum(X30dayReadmission, na.rm = TRUE) >= 1,
											week = week[1],
											averageTeamCensus = mean(averageTeamCensus),
											ADMSN_DATE = ADMSN_DATE[1] )


# Patient level data, there is one row for each team-patient assignment. IE if patient admitted and went from A2 to B1 because of bounceback, listed only once
teamLevelData$uniqueHosp <- paste(teamLevelData$ADMSN_DATE, teamLevelData$DE_PAT_ID)
patientLevelData <- ddply(teamLevelData,.(uniqueHosp), summarize, DE_PAT_ID = DE_PAT_ID[1], 
								HOSP_ADMSN_TIME = HOSP_ADMSN_TIME[1],
								over80 = sum(over80, na.rm = TRUE) >= 1, 
								los = los[1], 
								died = sum(died) > 0,
								readmission = sum(readmission) > 0,
								week = week[1],
								averageTeamCensus = mean(averageTeamCensus),
								ADMSN_DATE = ADMSN_DATE[1] )

str(patientLevelData )
summary(patientLevelData )

				
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

# WEED OUT ALL OTHER -> MICU -> MEDICINE TRANSFERS and MICU -> MEDICINE TRANSFERS, KEEP MEDICINE -> ICU TRANSFERS ###
summarizedData[!str_detect(summarizedData$firstTeam, "TT MED UNIV "),]$ICU_Care <- FALSE

summary(summarizedData$ICU_Care)

str(summarizedData)
str(summarizedData[summarizedData$Med_Care == TRUE,])

summarizedData$date2 <- strptime(substr(summarizedData$ADMSN_TIME,1,9),"%m/%d/%Y" )
summarizedData$mergerByPatientHospitalization <- paste(summarizedData$DE_PAT_ID, summarizedData$date2)
str(summarizedData)

patientLevelData$date2 <- strptime(patientLevelData$ADMSN_DATE, "%m/%d/%Y")
patientLevelData$mergerByPatientHospitalization <- paste(patientLevelData$DE_PAT_ID, patientLevelData$date2)
str(patientLevelData)


dedupdata2 <- merge(patientLevelData, summarizedData, by = "mergerByPatientHospitalization", all.x = TRUE)
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

t.test(dedupdataFINAL[dedupdataFINAL$over80,]$los, dedupdataFINAL[!dedupdataFINAL$over80,]$los)
sd(dedupdataFINAL[dedupdataFINAL$over80,]$los)
sd(dedupdataFINAL[!dedupdataFINAL$over80,]$los)


#t.test(dedupdataFINAL[dedupdataFINAL$over80,]$died , dedupdataFINAL[!dedupdataFINAL$over80,]$died )
table(dedupdataFINAL[dedupdataFINAL$over80,]$died)
table(dedupdataFINAL[!dedupdataFINAL$over80,]$died)
chisq.test(matrix(c(62,1890,67,2748), ncol = 2))
#chisq.test(matrix(c(67,2240,62,2398), ncol = 2))  # Not Removing Outliers


#t.test(dedupdataFINAL[dedupdataFINAL$over80,]$readmission , dedupdataFINAL[!dedupdataFINAL$over80,]$readmission )
table(dedupdataFINAL[dedupdataFINAL$over80,]$readmission)
table(dedupdataFINAL[!dedupdataFINAL$over80,]$readmission)
chisq.test(matrix(c(267,1685,360,2455), ncol = 2))
#chisq.test(matrix(c(313,1994,314,2146), ncol = 2))  # Not Removing Outliers

#t.test(dedupdataFINAL[dedupdataFINAL$over80,]$ICU_Care, dedupdataFINAL[!dedupdataFINAL$over80,]$ICU_Care)
table(dedupdataFINAL[dedupdataFINAL$over80,]$ICU_Care)
table(dedupdataFINAL[!dedupdataFINAL$over80,]$ICU_Care)
chisq.test(matrix(c(69,1883,68,2747), ncol = 2))
#chisq.test(matrix(c(82,2225,55,2405), ncol = 2))  # Not Removing Outliers

dedupdataFINAL$composite <- dedupdataFINAL$died | dedupdataFINAL$ICU_Care | dedupdataFINAL$readmission
t.test(dedupdataFINAL[dedupdataFINAL$over80,]$composite , dedupdataFINAL[!dedupdataFINAL$over80,]$composite )
table(dedupdataFINAL[dedupdataFINAL$over80,]$composite )
table(dedupdataFINAL[!dedupdataFINAL$over80,]$composite )
chisq.test(matrix(c(374,1578,470,2345), ncol = 2))
#chisq.test(matrix(c(436,1871,408, 2052), ncol = 2))  # Not Removing Outliers


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






















# OTHER STUFF NOT AS RELEVANT, FIRST TRY/DEADEND














length(unique(dedupdataFINAL$DE_PAT_ID))
length(unique(dedupdataFINAL$uniqueHosp))

dedupdataFINAL$date <- strptime(dedupdataFINAL$ADMSN_DATE, format = "%m/%d/%Y")
dedupdataFINAL$uniqHosp <- paste(dedupdataFINAL$date, dedupdataFINAL$DE_PAT_ID)
head(dedupdataFINAL$uniqueHosp)
str(dedupdataFINAL)


#incomeAndRace <- read.csv("JChi_DOuyang_incom_race_v7.csv", stringsAsFactors = TRUE)
incomeAndRace <- read.csv("DOuyang_Table1_ms_drg_list.csv", stringsAsFactors = TRUE)

length(unique(incomeAndRace$DE_PAT_ID))
str(incomeAndRace)

incomeAndRace$date <- strptime(incomeAndRace$ENC_ADMISSION_DATE, format = "%m/%d/%y")

incomeAndRace$uniqueHosp <- paste(incomeAndRace$date , incomeAndRace$DE_PAT_ID)
length(unique(incomeAndRace$uniqueHosp ))
str(incomeAndRace)
head(incomeAndRace$uniqueHosp)
str(incomeAndRace)



incomeAndRaceDedup <- ddply(incomeAndRace, .(uniqueHosp ), summarize,
								DE_PAT_ID = DE_PAT_ID[1],
								ADMISSION_DATE = ENC_ADMISSION_DATE[1],
								sex = GENDER[1],
								DRG_WEIGHT = ENC_MS_DRG_WEIGHT[1],
								race = PAT_RACE[1],
								Income = INCOME[1],
								numDiags = str_count(ICD_ACCOUNT_LEVEL[1], '/') + 1,
								age = mean(AGE_AT_ADMIT))

str(incomeAndRaceDedup)
summary(incomeAndRaceDedup)

#dedupdataFINALWithDemo <- merge(dedupdataFINAL, incomeAndRaceDedup, by = "DE_PAT_ID", all.x = TRUE)
dedupdataFINALWithDemo <- merge(dedupdataFINAL, incomeAndRaceDedup, by = "uniqueHosp", all.x = TRUE)


str(dedupdataFINALWithDemo )
summary(dedupdataFINALWithDemo )


summary(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,])
summary(dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,])

t.test(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$age, dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$age)
sd(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$age, na.rm = TRUE)
sd(dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$age, na.rm = TRUE)


t.test(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$numDiags , dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$numDiags )
sd(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$numDiags , na.rm = TRUE)
sd(dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$numDiags , na.rm = TRUE)


t.test(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$sex , dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$sex )
table(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$sex)
table(dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$sex)
chisq.test(matrix(c(700,699,1050,1000), ncol = 2))

700/(700+699)
1050/(1050 + 1000)

t.test(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$race, dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$race)
table(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$race)
table(dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$race)
chisq.test(matrix(c(1,3,183,144,61,338,2,16,651,2,9,233,218,83,490,3,16,995), ncol = 2))#chisq.test(matrix(c(436,1871,408, 2052), ncol = 2))  # Not Removing Outliers


t.test(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$Income , dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$Income )
sd(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$Income , na.rm = TRUE)
sd(dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$Income , na.rm = TRUE)

dedupdataFINALWithDemo[is.na(dedupdataFINALWithDemo$sex),]$DE_PAT_ID
length(unique(dedupdataFINALWithDemo[is.na(dedupdataFINALWithDemo$sex),]$DE_PAT_ID))

write.csv(dedupdataFINALWithDemo[is.na(dedupdataFINALWithDemo$sex),]$DE_PAT_ID, "missingPatients.csv")

