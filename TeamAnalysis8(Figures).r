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


theme_Publication <- function(base_size=14, base_family="helvetica") {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size, base_family=base_family)
       + theme(plot.title = element_text(face = "bold",
                                         size = rel(1.2), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.y = element_text(angle=90,vjust = 2),
               axis.title.x = element_text(vjust = -0.2),
               axis.text = element_text(), 
               axis.line = element_line(colour="black"),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour="#f0f0f0"),
               panel.grid.minor = element_blank(),
               legend.key = element_rect(colour = NA),
               legend.position = "bottom",
               legend.direction = "horizontal",
               legend.key.size= unit(0.2, "cm"),
               legend.margin = unit(0, "cm"),
               legend.title = element_text(face="italic"),
               plot.margin=unit(c(10,5,5,5),"mm"),
               strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
               strip.text = element_text(face="bold")
          ))
      
}

scale_fill_Publication <- function(...){
      library(scales)
      discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}

scale_colour_Publication <- function(...){
      library(scales)
      discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}



data <- read.csv("StanfordWardResidentsWithTeamCleaned.csv")
data2 <- read.csv("medicineTeamCensusTotal-FromPython.csv")

str(data)
str(data2)
data$merger <- paste(str_sub(data$team, 15, 15), format(strptime(data$date2, "%m/%d/%Y"), "%Y-%m-%d"))
data2$merger <- paste(data2$teamTotal, data2$ACCESS_DATE)

allData <- merge(data,data2, all.x = TRUE, by = "merger")
str(allData)

allData$firstActionTime <- strptime(allData$firstAction, "%H:%M:%OS")
allData$lastActionTime <- strptime(allData$lastAction, "%H:%M:%OS")
allData$difference2 <- as.numeric( allData$lastActionTime  - allData$firstActionTime )/60/60

ggplot(data = allData, aes(y = difference2, x = team_size, group = round_any(team_size,1)),binwidth = 1) + geom_boxplot() +geom_smooth() + 
xlim(0,20.5) + scale_y_continuous(limits = c(0,24), breaks = c(0,6,12,18,24)) + scale_fill_Publication() + 
theme_Publication() + labs(y = "Hours Per Day", x = "Team Census") 
#ggsave("Figure 1 - Daily Hours vs Census.png", width = 15, height = 10)
#ggsave("Figure 1 - Daily Hours vs Census (Narrow).png", width = 15, height = 7)

ggplot(data = allData, aes(y = difference2, x = team_size, group = round_any(team_size,1)),binwidth = 1) + geom_boxplot() +geom_smooth() + 
scale_y_continuous(limits = c(0,24), breaks = c(0,6,12,18,24)) + scale_fill_Publication() + 
theme_Publication() + labs(y = "Hours Per Day", x = "Team Census") 
#ggsave("Figure 1B - Daily Hours vs Census (No Limits).png", width = 15, height = 10)
#ggsave("Figure 1B - Daily Hours vs Census (No Limits) (Narrow).png", width = 15, height = 7)

ggplot(data = allData, aes(y = breaks, x = team_size, group = round_any(team_size,1)),binwidth = 1) + geom_boxplot() +geom_smooth() + xlim(0,20.5) + 
xlim(0,20.5) + scale_y_continuous(limits = c(0,65), breaks = c(0,10,20,30,40,50,60)) + scale_fill_Publication() + 
theme_Publication() + labs(y = "Number of Computer Sessions Per Day", x = "Team Census") 
#ggsave("Figure 3 - Number of Computer Sessions vs Census.png", width = 15, height = 10)


ggplot(data = allData, aes(y = breaks, x = team_size, group = round_any(team_size,1)),binwidth = 1) + geom_boxplot() +geom_smooth() +
scale_y_continuous(limits = c(0,65), breaks = c(0,10,20,30,40,50,60)) + scale_fill_Publication() + 
theme_Publication() + labs(y = "Number of Computer Sessions Per Day", x = "Team Census") 
#ggsave("Figure 3B - Number of Computer Sessions vs Census (No Limits).png", width = 15, height = 10)
#ggsave("Figure 3B - Number of Computer Sessions vs Census (No Limits) (Narrow).png", width = 15, height = 7)

ggplot(data = allData, aes(y = numberActions, x = team_size, group = round_any(team_size,1)),binwidth = 1) + geom_boxplot() +geom_smooth() + xlim(0,20.5) + 
xlim(0,20.5) + scale_y_continuous(limits = c(0,3200), breaks = c(0,1000,2000,3000)) + scale_fill_Publication() + 
theme_Publication() + labs(y = "Number of Electronic Actions Per Day", x = "Team Census") 
#ggsave("Figure 2 - Number of Electronic Actions vs Census.png", width = 15, height = 10)

ggplot(data = allData, aes(y = numberActions, x = team_size, group = round_any(team_size,1)),binwidth = 1) + geom_boxplot() +geom_smooth() + 
scale_y_continuous(limits = c(0,3200), breaks = c(0,1000,2000,3000)) + scale_fill_Publication() + 
theme_Publication() + labs(y = "Number of Electronic Actions Per Day", x = "Team Census") 
#ggsave("Figure 2B - Number of Electronic Actions vs Census (No Limits).png", width = 15, height = 10)
#ggsave("Figure 2B - Number of Electronic Actions vs Census (No Limits) (Narrow).png", width = 15, height = 7)



ggplot(data = allData, aes(x = team_size)) + geom_bar(binwidth = 1)  + scale_y_continuous(limits = c(0,500), breaks = c(0,100,200,300,400,500)) + scale_fill_Publication() + 
theme_Publication() + labs(y = "Number of Patients", x = "Team Census") 
#ggsave("Figure 11 - Number of Patients vs. Census.png", width = 15, height = 5)


data <- read.csv("StanfordWardResidentsWithTeamCleaned.csv", stringsAsFactors = FALSE)
data2 <- read.csv("medicineTeamCensusTotal-FromPython.csv", stringsAsFactors = FALSE)

str(data)
str(data2)
data$merger <- paste(str_sub(data$team, 15, 15), format(strptime(data$date2, "%m/%d/%Y"), "%Y-%m-%d"))
data2$merger <- paste(data2$teamTotal, data2$ACCESS_DATE)

allData <- merge(data,data2, all.x = TRUE, by = "merger")
str(allData)

allData$firstActionTime <- strptime(allData$firstAction, "%H:%M:%OS")
allData$lastActionTime <- strptime(allData$lastAction, "%H:%M:%OS")
allData$difference2 <- as.numeric( allData$lastActionTime  - allData$firstActionTime )/60/60
allData$date <- strptime(allData$ACCESS_DATE, "%Y-%m-%d")
allData$month <- format(strptime(allData$ACCESS_DATE, "%Y-%m-%d"), "%Y-%b")

data3 <- read.csv("ResidentToID.csv", stringsAsFactors = FALSE)
data3$resident <- data3$ID
allData2 <- merge(allData, data3, by = "resident", all.x = TRUE)
str(allData2)



##################################################################################
###############FAILED ATTEMPT
#
#meltInterns <- read.csv("meltInternsForEachDay-WithID.csv", stringsAsFactors = FALSE)
#meltResidents <- read.csv("meltResidentsForEachDay-WithID.csv", stringsAsFactors = FALSE)
#
#allData2Interns <- allData2[allData2$pgy == 1,]
#allData2Residents <- allData2[allData2$pgy != 1,]
#
#str(meltInterns)
#str(allData2Interns)
#
#allData2Interns$mergerbyNameDate <- paste(allData2Interns$date2, allData2Interns$name)
#meltInterns$mergerbyNameDate <- paste(meltInterns$date, meltInterns$value)
#meltInterns$resident 
#
#meltInterns <- data.frame(meltInterns[5], meltInterns[3])
#
#meltInterns2 <- merge(meltInterns, data3, by.x = "value", by.y = "name", all.x = TRUE)
#
#
#allData2Interns <- merge(allData2Interns, meltInterns, by = "mergerbyNameDate", all.x = TRUE)
#str(allData2Interns)
#
#
#names <- c(unique(allData2Interns$name), unique(meltInterns$value))
#
####################################################################################################






#Merging by Name Failed Hard, Not Sure Why The Same Name Was Not Merging Well
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


weeklyhours <- ddply(allData3[-c(19:20, 22) ], .(sorter), summarize,
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


# Rotate X Axis Text
theme_Publication2 <- function(base_size=14, base_family="helvetica") {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size, base_family=base_family)
       + theme(plot.title = element_text(face = "bold",
                                         size = rel(1.2), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.y = element_text(angle=90,vjust =2),
               axis.title.x = element_text(vjust = -0.2),
               axis.text = element_text(), 
               axis.line = element_line(colour="black"),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour="#f0f0f0"),
               panel.grid.minor = element_blank(),
               legend.key = element_rect(colour = NA),
               legend.position = "bottom",
               legend.direction = "horizontal",
               legend.key.size= unit(0.2, "cm"),
               legend.margin = unit(0, "cm"),
               legend.title = element_text(face="italic"),
               plot.margin=unit(c(10,5,5,5),"mm"),
               strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
               strip.text = element_text(face="bold"),
		   axis.text.x = element_text(angle = 90, hjust = 1)
          ))
      
}


ggplot(data = weeklyhours, aes( x = week, fill = over80)) + geom_bar(position="fill") + scale_fill_Publication(name = "Over 80 Hours", breaks = c(FALSE,TRUE), labels = c("TRUE","FALSE"))+ 
theme_Publication2() + labs(y = "Proportion of Housestaff working >80 Hours", x = "Week") 
#ggsave("Figure 4 - Proportion Over 80 Hours Over Time.png", width = 15, height = 7)

str(weeklyhours)
weeklyhours$weekDateUseful <- as.POSIXct(weeklyhours$weekDate)
summary(lm(data = weeklyhours, over80 ~ weekDateUseful ))
#p for trend 0.740. 

#summary(lm(data = weeklyhours[weeklyhours$pgy == 1,], over80 ~ weekDateUseful ))
#p for trend 0.740. 
#summary(lm(data = weeklyhours[weeklyhours$pgy != 1,], over80 ~ weekDateUseful ))
#p for trend 0.740. 

#Not normalized - because overlap, switch rotations at the periphery, higher number of residents, less people working over 80 hours per week
ggplot(data = weeklyhours, aes( x = week, fill = over80)) + geom_bar() + scale_fill_Publication()+ 
theme_Publication2() + labs(y = "Proportion of Housestaff working >80 Hours", x = "Week") 
#ggsave("proportionOver80hoursPerWeek.png")




sumMedicineTeams<- read.csv("medicineTeamCensusSpecific-FromPython.csv")
#sumMedicineTeams<- read.csv("medicineTeamCensusTotal-FromPython.csv")
str(sumMedicineTeams)
sumMedicineTeams$census <- sumMedicineTeams$team_size

sumMedicineTeams$date <- strptime(sumMedicineTeams$ACCESS_DATE, "%Y-%m-%d")




theme_Publication3 <- function(base_size=14, base_family="helvetica") {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size, base_family=base_family)
       + theme(plot.title = element_text(face = "bold",
                                         size = rel(1.2), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.y = element_text(angle=90,vjust =2),
               axis.title.x = element_text(vjust = -0.2),
               axis.text = element_text(), 
               axis.line = element_line(colour="black"),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour="#f0f0f0"),
               panel.grid.minor = element_blank(),
               legend.key = element_rect(colour = NA),
               legend.position = "bottom",
               legend.direction = "horizontal",
               legend.key.size= unit(0.2, "cm"),
               legend.margin = unit(0, "cm"),
               legend.title = element_text(face="italic"),
               plot.margin=unit(c(10,5,5,5),"mm"),
               strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
               strip.text = element_text(face="bold"),
		   axis.text.x = element_text(angle = 90, hjust = 1),
			legend.title=element_blank()
          ))
      
}



ggplot(data = sumMedicineTeams[sumMedicineTeams$team != 'N',], aes(group = team, x = team, y = census, color = team))  + geom_jitter() + geom_boxplot( outlier.shape = NA) + 
scale_colour_Publication()+ theme_Publication2() + labs(y = "Intern Census", x = "Team") 
#ggsave("Figure 5 - Intern Census by Team.png", width = 15, height = 10)

teamTotal <- ddply(sumMedicineTeams, .(team, ACCESS_DATE),summarize,  totalTeamSize = sum(team_size))
ggplot(data = teamTotal[teamTotal$team %in% c("A","B","C","D","E"),], aes(group = team, x = team, y = totalTeamSize , color = team))  + geom_jitter() + geom_boxplot( outlier.shape = NA) + 
scale_colour_Publication()+ theme_Publication2() + labs(y = "Team Census", x = "Team") 
#ggsave("Figure 6 - Team Census.png", width = 15, height = 10)







data <- read.csv("UniquePatientDateTeamMedicineOnlyWithMedicineLOSAndFirstDayAnd30DayReadmission.csv", stringsAsFactors = FALSE)
data$causedReadmission <- data$X30dayReadmission == 1
data$causedReadmission <- !data$causedReadmission
ggplot(data = data, aes( x = FirstDayTeamCensus , fill = causedReadmission )) + geom_bar(position="fill", binwidth = 1) + scale_fill_Publication(breaks = c(FALSE,TRUE), labels = c("TRUE","FALSE"))+ 
theme_Publication() + labs(x = "Team Census on Day of Admission", y = "Proportion Readmitted Within 30 Days") 
#ggsave("Figure 7 - Readmission Rate by Team Census on Admission.png", width = 15, height = 4)


ggplot(data = data, aes( x = FirstDayTeamCensus , y = los, group = round_any(FirstDayTeamCensus,1) )) + geom_boxplot() +ylim(0,20) + scale_fill_Publication() + 
theme_Publication() + labs(y = "Length of Stay", x = "Team Census on Admission") 
#ggsave("Figure 8 - LOS by Team Census on Admission.png", width = 15, height = 4)


ggplot(data = data, aes(x = FirstDayTeamCensus )) + geom_bar(binwidth = 1)  + scale_y_continuous(limits = c(0,1000), breaks = c(0,200,400,600,800,1000)) + scale_fill_Publication() + 
theme_Publication() + labs(y = "Number of Patients", x = "Team Census on Admission") 
#ggsave("Figure 12 - Number of Patients vs. Census on Admission.png", width = 15, height = 4)








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
ggplot(data = allData2, aes( x = FirstDayTeamCensus , fill = diedInHosp )) + geom_bar(position="fill", binwidth = 1) + scale_fill_Publication(breaks = c(FALSE,TRUE), labels = c("TRUE","FALSE"))+ 
theme_Publication() + labs(x = "Team Census on Day of Admission", y = "In-Hospital Mortality") 
#ggsave("Figure 9 - In Hospital Mortality by Team Census on Admission.png", width = 15, height = 4)








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

str(dedupdata)
summary(dedupdata)


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

										

2222 / (2222  + 3339 ) # NOT DEDUPED

1952 / 4767 # TRULY DEDUPED






t.test(dedupdataFinal[dedupdataFinal$over80,]$los, dedupdataFinal[!dedupdataFinal$over80,]$los)
t.test(dedupdataFinal[dedupdataFinal$over80,]$died , dedupdataFinal[!dedupdataFinal$over80,]$died )
t.test(dedupdataFinal[dedupdataFinal$over80,]$readmission , dedupdataFinal[!dedupdataFinal$over80,]$readmission )







#### OLD T - TESTS ###################
#
#
#t.test(dedupdata[dedupdata$over80,]$los, dedupdata[!dedupdata$over80,]$los)
#t.test(dedupdata[dedupdata$over80,]$died , dedupdata[!dedupdata$over80,]$died )
#t.test(dedupdata[dedupdata$over80,]$readmission , dedupdata[!dedupdata$over80,]$readmission )#
#
#str(dedupdata[dedupdata$over80,])
#str(dedupdata[!dedupdata$over80,])
#
#t.test(dedupdata[dedupdata$readmission ,]$los, dedupdata[!dedupdata$readmission ,]$los)
#t.test(dedupdata[dedupdata$died ,]$los, dedupdata[!dedupdata$died ,]$los)
#
#
#t.test(dedupdata[dedupdata$readmission ,]$averageTeamCensus, dedupdata[!dedupdata$readmission ,]$averageTeamCensus)
#t.test(dedupdata[dedupdata$died ,]$averageTeamCensus, dedupdata[!dedupdata$died ,]$averageTeamCensus)
#
########################################


scale_fill_Publication2 <- function(...){
      library(scales)
      discrete_scale("fill","Publication",manual_pal(values = c("#fdb462","#386cb0","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}









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

t.test(dedupdataFINAL[dedupdataFINAL$over80,]$los, dedupdataFINAL[!dedupdataFINAL$over80,]$los)
sd(dedupdataFINAL[dedupdataFINAL$over80,]$los)
sd(dedupdataFINAL[!dedupdataFINAL$over80,]$los)
t.test(dedupdataFINAL[dedupdataFINAL$over80,]$died , dedupdataFINAL[!dedupdataFINAL$over80,]$died )
table(dedupdataFINAL[dedupdataFINAL$over80,]$died)
table(dedupdataFINAL[!dedupdataFINAL$over80,]$died)
t.test(dedupdataFINAL[dedupdataFINAL$over80,]$readmission , dedupdataFINAL[!dedupdataFINAL$over80,]$readmission )
table(dedupdataFINAL[dedupdataFINAL$over80,]$readmission)
table(dedupdataFINAL[!dedupdataFINAL$over80,]$readmission)
t.test(dedupdataFINAL[dedupdataFINAL$over80,]$ICU_Care, dedupdataFINAL[!dedupdataFINAL$over80,]$ICU_Care)
table(dedupdataFINAL[dedupdataFINAL$over80,]$ICU_Care)
table(dedupdataFINAL[!dedupdataFINAL$over80,]$ICU_Care)

dedupdataFINAL$ICUorDEATH <- dedupdataFINAL$died | dedupdataFINAL$ICU_Care
t.test(dedupdataFINAL[dedupdataFINAL$over80,]$ICUorDEATH , dedupdataFINAL[!dedupdataFINAL$over80,]$ICUorDEATH )
table(dedupdataFINAL[dedupdataFINAL$over80,]$ICUorDEATH)
table(dedupdataFINAL[!dedupdataFINAL$over80,]$ICUorDEATH)


summary(dedupdataFINAL)

mean(dedupdataFINAL$los)
sd(dedupdataFINAL$los)
mean(dedupdataFINAL$died)
mean(dedupdataFINAL$readmission)
length(unique(dedupdata$DE_PAT_ID))
str(dedupdataFINAL)
summary(dedupdataFINAL)

1952 / 4767 

#write.csv(dedupdataFINAL, "patientsDeduplicated.csv")
#write.csv(dedupdata, "patientsNOTDeduplicated.csv")

length(unique(paste(dedupdata$ADMSN_DATE, dedupdata$DE_PAT_ID))) #4767
# THIS IS FOR VISUALIZATION ONLY #
dedupdata <- dedupdata[dedupdata$week != "2013-22" & dedupdata$week != "2013-24",]

ggplot(data = dedupdata, aes( x = week, fill = over80)) + geom_bar() + scale_fill_Publication2(name = "Taken Care of by Housestaff Working Over 80 Hours",breaks = c(TRUE,FALSE), labels = c("TRUE","FALSE"))+ 
theme_Publication2() + labs(y = "Number Of Patients", x = "Week") 
#ggsave("Figure 10 - Number of Patients Over 80 Hours Over Time.png", width = 15, height = 7)


dedupdata$dateUseful <- as.POSIXct(strptime(dedupdata$ADMSN_DATE, "%m/%d/%Y"))
summary(lm(data = dedupdata, over80 ~ dateUseful ))
summary(lm(data = dedupdata, died ~ dateUseful ))







data <- read.csv("UniquePatientDateTeamMedicineOnlyWithMedicineLOSAndFirstDayAnd30DayReadmission.csv", stringsAsFactors = FALSE)
data$causedReadmission <- data$X30dayReadmission == 1
data$causedReadmission <- !data$causedReadmission
ggplot(data = data, aes( x = FirstDayTeamCensus , fill = causedReadmission )) + geom_bar(position="fill", binwidth = 1) + scale_fill_Publication(breaks = c(FALSE,TRUE), labels = c("TRUE","FALSE"))+ 
theme_Publication() + labs(x = "Team Census on Day of Admission", y = "Proportion Readmitted Within 30 Days") 


data$uniqueHosp <- paste(data$ADMSN_DATE, data$DE_PAT_ID)
data <- merge(data, dedupdataFINAL, by = "uniqueHosp", all.x = TRUE)
str(data)

data$ICU_Care <- !data$ICU_Care


theme_Publication <- function(base_size=14, base_family="helvetica") {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size, base_family=base_family)
       + theme(plot.title = element_text(face = "bold",
                                         size = rel(1.2), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.y = element_text(angle=90,vjust = 2),
               axis.title.x = element_text(vjust = -0.2),
               axis.text = element_text(), 
               axis.line = element_line(colour="black"),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour="#f0f0f0"),
               panel.grid.minor = element_blank(),
               legend.key = element_rect(colour = NA),
               legend.position = "bottom",
               legend.direction = "horizontal",
               legend.key.size= unit(0.2, "cm"),
               legend.margin = unit(0, "cm"),
               legend.title = element_text(face="italic"),
               plot.margin=unit(c(10,5,5,5),"mm"),
               strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
               strip.text = element_text(face="bold")
          ))
      
}
summary(data)

ggplot(data = data, aes( x = FirstDayTeamCensus , fill = ICU_Care )) + geom_bar(position="fill", binwidth = 1) + scale_fill_Publication(breaks = c(FALSE,TRUE), labels = c("TRUE","FALSE"))+ 
theme_Publication() + labs(x = "Team Census on Day of Admission", y = "Proportion Transfered to ICU") 
#ggsave("Figure 13 - Transfer to ICU by Team Census on Admission.png", width = 15, height = 4)
