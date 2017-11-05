setwd("C:\\Users\\David\\Dropbox\\Stanford\\Epic Research\\AccessLog\\Census3.0")

library(stringi)
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

#ggplot(data = allData, aes(y = difference2, x = team_size, group = round_any(team_size,1)),binwidth = 1) + geom_boxplot() +geom_smooth() + xlim(0,20.5)
#ggsave("TimeSpentPerDayVSCensus.png")

#ggplot(data = allData, aes(y = breaks, x = team_size, group = round_any(team_size,1)),binwidth = 1) + geom_boxplot() +geom_smooth() + xlim(0,20.5)
#ggsave("NumberOfComputerSessionsVSCensus.png")


#ggplot(data = allData, aes(y = numberActions, x = team_size, group = round_any(team_size,1)),binwidth = 1) + geom_boxplot() +geom_smooth() + xlim(0,20.5)
#ggsave("NumberOfActionsVSCensus.png")

#ggplot(data = allData, aes(y = difference2, x = team_size, group = round_any(team_size,1)),binwidth = 1) + geom_boxplot() +geom_smooth() + xlim(0,20.5) + facet_wrap(~callDay)
#ggsave("TimeSpentPerDayVSCensus-SplitByCallDay.png")

#ggplot(data = allData[is.na(allData$callDay),], aes(y = difference2, x = team_size, group = round_any(team_size,1)),binwidth = 1) + geom_boxplot() +geom_smooth() + xlim(0,20.5)

allData$date <- strptime(allData$ACCESS_DATE, "%Y-%m-%d")
allData$month <- format(strptime(allData$ACCESS_DATE, "%Y-%m-%d"), "%Y-%b")
#ggplot(data = allData[is.na(allData$callDay),], aes(y = difference2, x = date, group = 1)) + geom_point()+ geom_smooth(method = "lm")



#summary(lm(data = allData[is.na(allData$callDay),], difference2~month))
#ggplot(data = allData[is.na(allData$callDay) & !is.na(allData$month),], aes(y = difference2, x = date, group = month)) + geom_boxplot()+ geom_smooth()
#ggsave("TimeSpentByMonthWithoutCallDays.png")
t.test(allData[is.na(allData$callDay) & allData$month == "2013-Jul",]$difference2, allData[is.na(allData$callDay) & allData$month == "2014-Jun",]$difference2)
#mean of x mean of y 
#10.718082  9.462969 
#t = 2.4574, df = 358.907, p-value = 0.01447

#ggplot(data = allData[ !is.na(allData$month),], aes(y = difference2, x = date, group = month)) + geom_boxplot()+ geom_smooth()
#ggsave("TimeSpentByMonthWithCallDays.png")
t.test(allData[ allData$month == "2013-Jul",]$difference2, allData[ allData$month == "2014-Jun",]$difference2)
#mean of x mean of y 
# 12.44303  11.15224 
#t = 3.7696, df = 628.291, p-value = 0.0001789

data3 <- read.csv("ResidentToID.csv")
data3$resident <- data3$ID
allData2 <- merge(allData, data3, by = "resident", all.x = TRUE)
str(allData2)


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

summary(lm(data = allData2[is.na(allData2$callDay) & allData2$pgy == 1,], difference2~team_size + as.numeric(ACCESS_DATE)))

#Coefficients::
#                         Estimate Std. Error t value Pr(>|t|)    
#(Intercept)              9.024892   0.392366  23.001  < 2e-16 ***
#team_size                0.194110   0.038546   5.036 5.33e-07 ***
#as.numeric(ACCESS_DATE) -0.006048   0.001052  -5.750 1.08e-08 ***

summary(lm(data = allData2[is.na(allData2$callDay) & allData2$pgy != 1,], difference2~team_size + as.numeric(ACCESS_DATE) + hasSubI))

#Coefficients::
#                        Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             8.174111   0.675553  12.100  < 2e-16 ***
#team_size               0.290276   0.064841   4.477 8.76e-06 ***
#as.numeric(ACCESS_DATE) 0.003184   0.001651   1.928   0.0542 .  


# 0.194110 * 60
#[1] 11.6466
# 0.290276 * 60
#[1] 17.41656 


summary(lm(data = allData2[is.na(allData2$callDay) & allData2$pgy == 1,], difference2~team_size + as.numeric(ACCESS_DATE) + hasSubI))


#Coefficients:
                         Estimate Std. Error t value Pr(>|t|)    
#(Intercept)              9.269723   0.399669  23.194  < 2e-16 ***
#team_size                0.197354   0.038459   5.132 3.25e-07 ***
#as.numeric(ACCESS_DATE) -0.006222   0.001051  -5.921 3.95e-09 ***
#hasSubITRUE             -0.694073   0.230320  -3.014  0.00263 ** 


summary(lm(data = allData2[is.na(allData2$callDay) & allData2$pgy != 1,], difference2~team_size + as.numeric(ACCESS_DATE) + hasSubI))

#Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             8.114863   0.686087  11.828  < 2e-16 ***
#team_size               0.288236   0.065000   4.434 1.06e-05 ***
#as.numeric(ACCESS_DATE) 0.003208   0.001653   1.941   0.0527 .  
#hasSubITRUE             0.179794   0.357602   0.503   0.6153    


-0.694 * 60
#[1] -41.64
0.18 * 60
#[1] 10.8

t.test(allData2[ allData2$hasSubI & allData2$pgy != 1,]$difference2, allData2[ !allData2$hasSubI & allData2$pgy != 1,]$difference2)
t.test(allData2[ allData2$hasSubI & allData2$pgy == 1,]$difference2, allData2[ !allData2$hasSubI & allData2$pgy == 1,]$difference2)







############## weekly duty hours ##########

allData2$week <- format(strptime(allData2$date2, "%m/%d/%Y"), format="%Y-%U")
allData2$counter <- 1
allData2$sorter <- paste(allData2$resident, "    ", allData2$week)
allData2$firstActionTime <- as.POSIXct(allData2$firstActionTime)
allData2$lastActionTime <- as.POSIXct(allData2$lastActionTime)
allData2$date <- as.POSIXct(allData2$date)

allData3 <- allData2[allData2$difference2 > 10000/60/60,]
allData3 <- allData3[allData3 $difference2 < 70000/60/60,]


weeklyhours <- ddply(allData3[-c(18:19, 21) ], .(sorter), summarize,
		resident = resident[1],
		week = week[1],
		totalhours = sum(difference2), total = sum(counter),
		pgy = pgy[1], 
		hasSubI = hasSubI[1],
		averageTeamSize = sum(team_size)/sum(counter),
		team = team[1],
		counter = counter[1]) 

#### MONTHLY AVERAGES ####
ddply(weeklyhours, .(resident), summarize, total = sum(totalhours), totalAvg = sum(totalhours)/sum(counter))

allData3$month <- format(strptime(allData2$date2, "%m/%d/%Y"), format="%Y-%m")
allData3$sorter2 <- paste(allData3$resident, "    ", allData3$month)

monthlyhours <- ddply(allData3[-c(18:19, 21) ], .(sorter2), summarize,
		resident = resident[1],
		week = week[1],
		totalhours = sum(difference2), total = sum(counter),
		pgy = pgy[1], 
		hasSubI = hasSubI[1],
		averageTeamSize = sum(team_size)/sum(counter),
		team = team[1],
		counter = counter[1]) 

monthlyhours$over320 <- monthlyhours$totalhours > 320

13/(13+265)
#4.67

###########

summary(weeklyhours[weeklyhours$pgy == 1,])
summary(weeklyhours[weeklyhours$pgy != 1,])

#ggplot(data = weeklyhours, aes(y = totalhours , x = week , group = week)) + geom_boxplot() +geom_smooth( aes(group = 1), method = "lm") + facet_wrap(~pgy) 
#ggsave("WeeklyHoursByWeekOfYear-InternsVsResidents.png")

str(weeklyhours)
weeklyhours$weekAsNumber <- as.numeric(strptime(weeklyhours$week, "%Y-%U"))
summary(lm(data = weeklyhours[weeklyhours$pgy == 1,], totalhours ~ averageTeamSize + weekAsNumber  + hasSubI ))
summary(lm(data = weeklyhours[weeklyhours$pgy != 1,], totalhours ~ averageTeamSize + weekAsNumber  + hasSubI ))

weeklyhours$over80 <- weeklyhours$totalhours<80
weeklyhours$weekDate <-  strptime(weeklyhours$week, "%Y-%U") 
ggplot(data = weeklyhours, aes(y = totalhours , x = week , group = week)) + geom_point() 

#ggplot(data = weeklyhours, aes( x = week, fill = over80)) + geom_bar(position="fill")
#ggsave("proportionOver80hoursPerWeek.png")

summary(weeklyhours$over80)

summary(lm(data = weeklyhours, over80 ~ averageTeamSize + weekAsNumber  + hasSubI))

summary(lm(data = weeklyhours[weeklyhours$pgy != 1,], over80 ~ averageTeamSize + weekAsNumber  + hasSubI))

summary(lm(data = weeklyhours[weeklyhours$pgy == 1,], over80 ~ averageTeamSize + weekAsNumber  + hasSubI))

summary(lm(data = weeklyhours, over80 ~ weekAsNumber))

summary(allData3)

summary(weeklyhours)










############# CROSS VALIDATION FOR 2013-31, 2013-43, and 2014-06 #######

concernweeks <- weeklyhours[weeklyhours$week == "2013-31" | weeklyhours$week == "2013-43" | weeklyhours$week == "2014-06" ,]

summary(concernweeks)

summary(concernweeks[concernweeks$pgy!=1,])

concernweekA <- weeklyhours[weeklyhours$week == "2013-31" ,]
concernweekB <- weeklyhours[weeklyhours$week == "2013-43" ,]
concernweekC <- weeklyhours[weeklyhours$week == "2014-06" ,]


# All residents in week 3 work > 80
summary(concernweekC[concernweekC$pgy!=1,])

# Team D, one intern more than 80, one intern less than 80, no resident
concernweekA

#B,C,D,E all the same way
concernweekB