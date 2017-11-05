setwd("C:\\Users\\David\\Dropbox\\Stanford\\Epic Research\\AccessLog\\Census3.0")

library(ggplot2)
library(plyr)
library(stringr)
library(reshape2)

data <- read.csv("StanfordWardsWithTimeDuringEducation.csv", stringsAsFactors = FALSE)

data$firstActionTime <- strptime(data$firstAction, "%H:%M:%OS")
data$lastActionTime <- strptime(data$lastAction, "%H:%M:%OS")
data$difference2 <- as.numeric( data$lastActionTime  - data$firstActionTime )/60/60

#data <- data[data$difference2 > 10000/60/60,]
#data <- data[data$difference2 < 70000/60/60,]

str(data)

qplot(data$numActionsDuringTeaching,binwidth = 10)
qplot(data$numberActions,binwidth = 10)

qplot(data = data, numberActions, difference2)+geom_smooth()
qplot(data = data, numActionsDuringTeaching, difference2)+geom_smooth()
qplot(data = data, numberActions, numActionsDuringTeaching) +geom_smooth()
ggsave("NumberActionsVsNumberActionsDuringTeachingConference.png")

data$merger <- paste(data$resident, strptime(data$date, "%Y-%m-%d"))

data2 <- read.csv("StanfordWardResidentsWithTeamCleaned.csv")
data2$merger <- paste(data2$resident, strptime(data2$date, "%m/%d/%Y"))

alldata <- merge(data, data2, by = "merger", all.x = TRUE)
str(alldata)

t.test(alldata[alldata$type == "CallDay",]$numActionsDuringTeaching, alldata[alldata$type != "CallDay",]$numActionsDuringTeaching)

t.test(alldata[alldata$difference2 >14.498,]$numActionsDuringTeaching, alldata[alldata$difference2 <= 14.498,]$numActionsDuringTeaching)

data3 <- read.csv("medicineTeamCensusTotal-FromPython.csv")

alldata$merger <- paste(str_sub(alldata$team, 15, 15), format(strptime(alldata$date2, "%m/%d/%Y"), "%Y-%m-%d"))
data3$merger <- paste(data3$teamTotal, data3$ACCESS_DATE)

allData2 <- merge(alldata ,data3, all.x = TRUE, by = "merger")

qplot(data = allData2, team_size, numActionsDuringTeaching) +geom_smooth()
t.test(allData2[allData2$team_size < 16,]$numActionsDuringTeaching, allData2[allData2$team_size >= 16,]$numActionsDuringTeaching)

summary(allData2$numActionsDuringTeaching/allData2$numberActions.x)