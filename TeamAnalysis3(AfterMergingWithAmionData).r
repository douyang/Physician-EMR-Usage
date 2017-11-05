setwd("C:\\Users\\David\\Dropbox\\Stanford\\Epic Research\\AccessLog\\Census3.0")

library(ggplot2)
library(plyr)
library(reshape2)
library(strptime)
library(stringr)
library(scales)

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

ggplot(data = allData, aes(y = difference2, x = team_size, group = round_any(team_size,1)),binwidth = 1) + geom_boxplot() +geom_smooth() + xlim(0,20.5)
ggsave("TimeSpentPerDayVSCensus.png")

summary(lm(data = allData, difference2 ~ team_size))

ggplot(data = allData, aes(y = breaks, x = team_size, group = round_any(team_size,1)),binwidth = 1) + geom_boxplot() +geom_smooth() + xlim(0,20.5)
ggsave("NumberOfComputerSessionsVSCensus.png")


ggplot(data = allData, aes(y = numberActions, x = team_size, group = round_any(team_size,1)),binwidth = 1) + geom_boxplot() +geom_smooth() + xlim(0,20.5)
ggsave("NumberOfActionsVSCensus.png")

160/(590+160)