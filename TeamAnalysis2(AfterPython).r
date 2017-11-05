setwd("C:\\Users\\David\\Dropbox\\Stanford\\Epic Research\\AccessLog\\Census3.0")

library(ggplot2)
library(plyr)
library(reshape2)
library(strptime)
library(stringr)
library(scales)


data <- read.csv("medicineTeamCensusSpecific-FromPython.csv")
data2 <- read.csv("medicineTeamCensusTotal-FromPython.csv")
str(data)
str(data2)

qplot(data2$team_size, binwidth = 1)
ggsave("TeamCensus.png")
qplot(data$team_size, binwidth = 1)
ggsave("IndividualCensus.png")

data$date <- strptime(data$ACCESS_DATE, "%Y-%m-%d")
data2$date <- strptime(data2$ACCESS_DATE, "%Y-%m-%d")

data$first <- "SubI"
data[str_detect(data$PROV_NAME, "TT MED UNIV [ABCDE]1"),]$first <- "Intern 1"
data[str_detect(data$PROV_NAME, "TT MED UNIV [ABCDE]2"),]$first <- "Intern 2"

qplot(data = data, date, team_size, color = first, geom = c("point", "line")) + geom_smooth() +facet_wrap(~team)
ggsave("CensusOverTime-ByIndividual.png")

qplot(data = data2, date, team_size, color = teamTotal, geom = c("point", "line")) + geom_smooth() +facet_wrap(~teamTotal)
ggsave("CensusOverTime-ByTeam.png")

t.test(data2[data2$teamTotal == "A",]$team_size,data2[data2$teamTotal != "A",]$team_size)
t.test(data2[data2$teamTotal == "B",]$team_size,data2[data2$teamTotal != "B",]$team_size)
t.test(data2[data2$teamTotal == "C",]$team_size,data2[data2$teamTotal != "C",]$team_size)
t.test(data2[data2$teamTotal == "D",]$team_size,data2[data2$teamTotal != "D",]$team_size)
t.test(data2[data2$teamTotal == "E",]$team_size,data2[data2$teamTotal != "E",]$team_size)
summary(data2$team_size)

summary(aov(team_size ~ teamTotal, data = data2))
summary(lm(team_size ~ teamTotal, data = data2))


# Overall Census #

data <- read.csv("UniquePatientDateTeamMedicineOnlyWithMedicineLOS.csv")
str(data)

qplot(data = data, aes(x = medicineLOS, group = round_any(averageTeamCensus, 1))) + geom_smooth()
qplot(data = data, y = medicineLOS, averageInternCensus) + geom_smooth()

summary(data$medicineLOS)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.000   1.000   2.000   2.664   3.000  54.000 


ggplot(data = data, aes(y = medicineLOS, x = averageTeamCensus, group = round_any(averageTeamCensus,2)),binwidth = 2) + geom_boxplot() +geom_smooth()# + ylim(0,15)
ggsave("averageTeamCensusToLOS.png")

ggplot(data = data, aes(y = timeAdmittedInHours, x = averageTeamCensus, group = round_any(averageTeamCensus,1)),binwidth = 1) + geom_boxplot() +geom_smooth() + ylim(0,1000)
ggsave("averageTeamCensusToTotalHospitalTime.png")

qplot(data = data, averageInternCensus, averageTeamCensus)
ggsave("InternVsTeamCensus.png")


#data$admitTime <- format(strptime(data$HOSP_ADMSN_TIME, "%m/%d/%Y %H:%M"), "%H:%M" ) # "%m/%d/%Y")
data$admitTime <- strptime(str_sub(data$HOSP_ADMSN_TIME, -5, -1), "%H:%M")
qplot(data = data, admitTime, medicineLOS) + scale_x_datetime(breaks = date_breaks("1 hour"),
  minor_breaks = date_breaks("1 hour"), labels = date_format("%H:%M"))

ggsave("NoObviousTrendInTimeOfAdmissionToLOS.png")

ggplot(data = data, aes(admitTime, weight = count )) + geom_bar(binwidth = 60*10) + scale_x_datetime(breaks = date_breaks("1 hour"),
  minor_breaks = date_breaks("1 hour"), labels = date_format("%H:%M")) #fill = factor(round_any(medicineLOS,1))
ggsave("TimeDistributionOfAdmissions.png")



data <- read.csv("UniquePatientDateTeamMedicineOnlyWithMedicineLOS.csv")
str(data)
summary(data$medicineLOS)
summary(data[data$medicineLOS>0,]$medicineLOS)

