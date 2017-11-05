setwd("C:\\Users\\David\\Dropbox\\Stanford\\Epic Research\\AccessLog\\Census3.0")

library(ggplot2)
library(plyr)
library(stringr)
library(reshape2)

data <- read.csv("jchi_douyang_acclog_tteamv4.csv", stringsAsFactors = FALSE)
str(data)

data$died <- NA
data[data$DEAD_YN == "N",]$died <- 0
data[data$DEAD_YN == "Y",]$died <- 1
data$count <- 1

summarizedData <- ddply(data, .(DE_PAT_ID), summarize, died = sum(died)/sum(count))
write.csv(summarizedData , "PatientToDeceasedStatus.csv")

data2 <- read.csv("UniquePatientDateTeamMedicineOnlyWith30DayReadmission.csv")
str(data2)

data2$uniqueness <- paste(data2$DE_PAT_ID, data2$HOSP_ADMSN_TIME)
length(unique(data2$uniqueness))


allData <- merge(data2, summarizedData, by = "DE_PAT_ID", all.x = TRUE)
str(allData)

table(allData$died)
summarizedData2 <- ddply(allData, .(DE_PAT_ID), summarize, died = died[1])
table(summarizedData2$died)
 438/(438+2745)
#0.137606

allData2 <- ddply(allData, .(DE_PAT_ID), transform, numHospitalizations = sum(count))

t.test(allData[allData$died == 0,]$medicineLOS, allData[allData$died == 1,]$medicineLOS)
t.test(allData[allData$died == 0,]$averageTeamCensus, allData[allData$died == 1,]$averageTeamCensus)
t.test(allData2[allData2$died == 0,]$numHospitalizations , allData2[allData2$died == 1,]$numHospitalizations )

table(allData2$numHospitalizations)
summary(allData2$numHospitalizations)

length(unique(allData2$DE_PAT_ID))
3450-2347
(3450-2347)/3450

sum(allData2[allData2$numHospitalizations == 24,]$medicineLOS)
sum(allData2[allData2$numHospitalizations >= 10,]$medicineLOS)

allData2PerPatient <- ddply(allData2, .(DE_PAT_ID), summarize, numHospitalizations = numHospitalizations[1], 
					totalHospitalDays = sum(los), totalMedicineLOS = sum(medicineLOS), died = died[1])

summary(allData2PerPatient$numHospitalizations)
summary(allData2PerPatient$totalHospitalDays)
summary(allData2PerPatient$totalMedicineLOS)
summary(allData2PerPatient$died)

########## HAVING DEATH DATE, CAN IDENTIFY IN HOSPITAL MORTALITY ######

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

#write.csv(summarizedData2, "PatientToDeceasedStatusWithInHouse.csv")

data3 <- read.csv("UniquePatientDateTeamMedicineOnlyWith30DayReadmission.csv")
str(data3)

allData2 <- merge(data3, summarizedData2, "DE_PAT_ID", all.x = TRUE)
str(allData2)
allData2 <- ddply(allData2, .(DE_PAT_ID), transform, numHospitalizations = sum(count))

summary(allData2$inhospitalMortality)
t.test(allData2[allData2$inhospitalMortality == 0,]$medicineLOS, allData2[allData2$inhospitalMortality == 1,]$medicineLOS)
t.test(allData2[allData2$inhospitalMortality == 0,]$averageTeamCensus, allData2[allData2$inhospitalMortality == 1,]$averageTeamCensus)
t.test(allData2[allData2$inhospitalMortality == 0,]$numHospitalizations , allData2[allData2$inhospitalMortality == 1,]$numHospitalizations )


##### CHECK IF IT IS DURING >80 WORKWEEK ####

patientDataWithInHospitalDeath <- allData2
patientDataWithInHospitalDeath$merger2 <- paste(patientDataWithInHospitalDeath$PROV_NAME, patientDataWithInHospitalDeath$HOSP_ADMSN_TIME)

patientDataWithInHospitalDeath2  <- patientDataWithInHospitalDeath[24:26]