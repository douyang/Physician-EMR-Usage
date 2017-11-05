setwd("C:\\Users\\David\\Dropbox\\Stanford\\Epic Research\\AccessLog\\Census3.0")

library(ggplot2)
library(plyr)
library(stringr)
library(reshape2)

data <- read.csv("jchi_douyang_acclog_tteamv3.csv", stringsAsFactors = FALSE)
data$count <- 1
data$hosp_admission <- strptime(data$HOSP_ADMSN_TIME, "%d-%B-%y %H:%M:%OS")

str(data)

summarizedData <- ddply(data, .(DE_PAT_ID,PROV_NAME, ACCESS_DATE), summarize, HOSP_DISCH_TIME = HOSP_DISCH_TIME[1], HOSP_ADMSN_TIME = HOSP_ADMSN_TIME[1],
				TRTMNT_TM_BEGIN_DT = TRTMNT_TM_BEGIN_DT[1], TRTMNT_TM_END_DT = TRTMNT_TM_END_DT[1], count = 1)


summarizedData$timeAdmittedInHours <-  as.numeric(strptime(summarizedData$HOSP_DISCH_TIME, "%d-%B-%y %H:%M:%OS") - strptime(summarizedData$HOSP_ADMSN_TIME, "%d-%B-%y %H:%M:%OS")) /60 /60
summary(summarizedData$timeAdmittedInHours)
str(summarizedData[summarizedData$timeAdmittedInHours <= 12,])

summarizedData <- summarizedData[summarizedData$timeAdmittedInHours >= 12,]
str(summarizedData)
summarizedData$ADMSN_DATE <- format(strptime(summarizedData$HOSP_ADMSN_TIME, "%d-%B-%y %H:%M:%OS"), "%m/%d/%Y")
summarizedData$DISCH_DATE <- format(strptime(summarizedData$HOSP_DISCH_TIME, "%d-%B-%y %H:%M:%OS"), "%m/%d/%Y")
#summarizedDataFinal <- ddply(summarizedData, .(PROV_NAME, ACCESS_DATE), summarize, team_size = sum(count), teamlist = list(DE_PAT_ID)) # CANT WRITE LIST
summarizedDataFinal <- ddply(summarizedData, .(PROV_NAME, ACCESS_DATE), summarize, team_size = sum(count))
str(summarizedDataFinal )

#write.csv(summarizedData, "UniquePatientDateTeam.csv")
#write.csv(summarizedDataFinal , "TeamCensus-WithPatients.csv") 
#write.csv(summarizedDataFinal , "TeamCensus.csv") 

#medicineTeams <- data.frame(date = unique(summarizedDataFinal$ACCESS_DATE))
#medicineTeams$date <- unique(summarizedDataFinal$ACCESS_DATE)

medicineTeams <- summarizedDataFinal[str_detect(summarizedDataFinal$PROV_NAME, "TT MED UNIV "),]

medicineTeams$team <- "NA"
medicineTeams[str_detect(medicineTeams$PROV_NAME, "TT MED UNIV A"),]$team <- "A"
medicineTeams[str_detect(medicineTeams$PROV_NAME, "TT MED UNIV B"),]$team <- "B"
medicineTeams[str_detect(medicineTeams$PROV_NAME, "TT MED UNIV C"),]$team <- "C"
medicineTeams[str_detect(medicineTeams$PROV_NAME, "TT MED UNIV D"),]$team <- "D"
medicineTeams[str_detect(medicineTeams$PROV_NAME, "TT MED UNIV E"),]$team <- "E"
table(medicineTeams$team)

write.csv(medicineTeams,"medicineTeamCensusSpecific.csv")



#sumMedicineTeams <- ddply(medicineTeams, .(team, ACCESS_DATE), summarize, teamlist = unique(teamlist), census = length(unique(teamlist)))

sumMedicineTeams <- ddply(medicineTeams, .(team, ACCESS_DATE), summarize, census = sum(team_size))


##### UPLOAD NEW FILE SO FIGURES ARE CORRECT ####
sumMedicineTeams<- read.csv("medicineTeamCensusSpecific-FromPython.csv")
#sumMedicineTeams<- read.csv("medicineTeamCensusTotal-FromPython.csv")
str(sumMedicineTeams)
sumMedicineTeams$census <- sumMedicineTeams$team_size

sumMedicineTeams$date <- strptime(sumMedicineTeams$ACCESS_DATE, "%Y-%m-%d")

qplot(data = sumMedicineTeams, date, census, color = team, geom = c("point", "line")) + geom_smooth() +facet_wrap(~team)
ggsave("CensusOverTime.png")

ggplot(data = sumMedicineTeams[sumMedicineTeams$team != 'N',], aes(group = team, x = team, y = census, color = team))  + geom_jitter() + geom_boxplot()
ggsave("CensusBoxPlot.png")

#ggplot(data = sumMedicineTeams[sumMedicineTeams$teamTotal != 'N',], aes(group = teamTotal, x = teamTotal, y = census, color = teamTotal))  + geom_jitter() + geom_boxplot()
#ggsave("CensusBoxPlot-TotalTeam.png")

qplot(medicineTeams$team_size)
ggsave("IndividualCensus.png")

qplot(sumMedicineTeams$census, binwidth = 1)
ggsave("TeamCensus.png")


######## WITHOUT SUBIs, no A3,B3,C3,D3,E3 etc #########

medicineTeams <- summarizedDataFinal[str_detect(summarizedDataFinal$PROV_NAME, "TT MED UNIV "),]

medicineTeams$team <- "NA"
medicineTeams[str_detect(medicineTeams$PROV_NAME, "TT MED UNIV A[12]"),]$team <- "A"
medicineTeams[str_detect(medicineTeams$PROV_NAME, "TT MED UNIV B[12]"),]$team <- "B"
medicineTeams[str_detect(medicineTeams$PROV_NAME, "TT MED UNIV C[12]"),]$team <- "C"
medicineTeams[str_detect(medicineTeams$PROV_NAME, "TT MED UNIV D[12]"),]$team <- "D"
medicineTeams[str_detect(medicineTeams$PROV_NAME, "TT MED UNIV E[12]"),]$team <- "E"
table(medicineTeams$team)

sumMedicineTeamsNoSubI <- ddply(medicineTeams, .(team, ACCESS_DATE), summarize, census = sum(team_size))
sumMedicineTeamsNoSubI[sumMedicineTeamsNoSubI$team == 'NA',]$team = "Sub Intern"
sumMedicineTeamsNoSubI$date <- strptime(sumMedicineTeamsNoSubI$ACCESS_DATE, "%d-%B-%y")
str(sumMedicineTeamsNoSubI)

qplot(data = sumMedicineTeamsNoSubI, date, census, color = team, geom = c("point", "line")) + geom_smooth() +facet_wrap(~team)
ggsave("CensusOverTime-SubIApart.png")

ggplot(data = sumMedicineTeamsNoSubI, aes(group = team, x = team, y = census, color = team))  + geom_jitter() + geom_boxplot()
ggsave("CensusBoxPlot-SubIApart.png")

##### Individual Intern/SubI #####
medicineTeams$date <- strptime(medicineTeams$ACCESS_DATE, "%d-%B-%y") #Warning if you do this, will be hard to DDPLY later
medicineTeams$first <- "SubI"
medicineTeams[str_detect(medicineTeams$PROV_NAME, "TT MED UNIV [ABCDE]1"),]$first <- "Intern 1"
medicineTeams[str_detect(medicineTeams$PROV_NAME, "TT MED UNIV [ABCDE]2"),]$first <- "Intern 2"
qplot(data = medicineTeams, date, team_size, color = first, geom = c("point", "line")) + geom_smooth() +facet_wrap(~team)
ggsave("CensusOverTime-ByIndividual.png")

######### OTHER APPROACH
#PER PATIENT, IN TIME AND OUT TIME, LOS, AND TREATMENT TEAM

summarizedData2 <- ddply(data, .(DE_PAT_ID, HOSP_ADMSN_TIME, PROV_NAME), summarise, HOSP_DISCH_TIME = HOSP_DISCH_TIME[1],
				TRTMNT_TM_BEGIN_DT = TRTMNT_TM_BEGIN_DT[1], TRTMNT_TM_END_DT = TRTMNT_TM_END_DT[1], count = 1)

summarizedData2[summarizedData2$DE_PAT_ID == 40822,]
summarizedData2[summarizedData2$DE_PAT_ID == 3715995,]

 
summarizedData2$timeAdmittedInHours <-  as.numeric(strptime(summarizedData2$HOSP_DISCH_TIME, "%d-%B-%y %H:%M:%OS") - strptime(summarizedData2$HOSP_ADMSN_TIME, "%d-%B-%y %H:%M:%OS")) /60 /60
summarizedData2 <- summarizedData2[summarizedData2$timeAdmittedInHours >= 12,]
summarizedData2$ADMSN_DATE <- format(strptime(summarizedData2$HOSP_ADMSN_TIME, "%d-%B-%y %H:%M:%OS"), "%m/%d/%Y")
summarizedData2$DISCH_DATE <- format(strptime(summarizedData2$HOSP_DISCH_TIME, "%d-%B-%y %H:%M:%OS"), "%m/%d/%Y")
summarizedData2$los <-  (as.numeric(strptime(summarizedData2$DISCH_DATE , "%m/%d/%Y") - strptime(summarizedData2$ADMSN_DATE , "%m/%d/%Y")) /60/60/24)

qplot(summarizedData2$timeAdmittedInHours, binwidth = 1) +xlim(0,100)

write.csv(summarizedData2, "UniquePatientDateTeam.csv")

str(summarizedData2)
qplot(data = summarizedData2, los, fill = PROV_NAME, binwidth = 1)

medicineTeams2 <- summarizedData2[str_detect(summarizedData2$PROV_NAME, "TT MED UNIV "),]
write.csv(medicineTeams2 , "UniquePatientDateTeamMedicineOnly.csv")
