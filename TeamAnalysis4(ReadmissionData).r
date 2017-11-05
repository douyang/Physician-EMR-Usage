setwd("C:\\Users\\David\\Dropbox\\Stanford\\Epic Research\\AccessLog\\Census3.0")

library(ggplot2)
library(plyr)
library(reshape2)
library(strptime)
library(stringr)
library(scales)


############## WITH READMISSION DATA ###################

data <- read.csv("UniquePatientDateTeamMedicineOnlyWith30DayReadmission.csv")
str(data)

qplot(data = data, averageTeamCensus, fill = factor(X30dayReadmission), binwidth = 1) + facet_wrap(~X30dayReadmission, scale = "free_y")
ggsave("DistributionOfReadmissionsByCensus.png")

t.test(data[data$X30dayReadmission == 0,]$averageTeamCensus, data[data$X30dayReadmission == 1,]$averageTeamCensus)
# SAME AVG CENSUS, 12.01 vs. 12.05, p = 0.7492

t.test(data[data$X30dayReadmission == 0,]$medicineLOS, data[data$X30dayReadmission == 1,]$medicineLOS)
# t = -1.2602, df = 1056.839, p-value = 0.2079
#mean of x mean of y 
# 2.641372  2.810209 

length(unique(data$DE_PAT_ID))
#[1] 3450

# 5574 hospitalizations

summary(data)
sd(data$timeToReadmission, na.rm = TRUE)
sd(data$medicineLOS, na.rm = TRUE)
sd(data$los, na.rm = TRUE)
sd(data$averageTeamCensus, na.rm = TRUE)


##### INTERN SPECIFIC LOS AND 30 DAY READMISSION
qplot(data = data, averageInternCensus, fill = factor(X30dayReadmission), binwidth = 1) + facet_wrap(~X30dayReadmission, scale = "free_y")

t.test(data[data$X30dayReadmission == 0,]$averageInternCensus, data[data$X30dayReadmission == 1,]$averageInternCensus)


ggplot(data = data, aes(x = averageInternCensus, y = medicineLOS, group = round_any(averageInternCensus,1))) + geom_boxplot() 
ggsave("medicineLOSbyInternCensus.png")



############### PLOT READMISSION BY LOS #########


data <- read.csv("UniquePatientDateTeamMedicineOnlyWithMedicineLOSAndFirstDayAnd30DayReadmission.csv", stringsAsFactors = FALSE)
data$causedReadmission <- data$X30dayReadmission == 1
ggplot(data = data, aes( x = FirstDayTeamCensus , fill = causedReadmission )) + geom_bar(position="fill", binwidth = 1)

ggplot(data = data, aes( x = FirstDayTeamCensus , y = los, group = round_any(FirstDayTeamCensus,1) )) + geom_boxplot() +ylim(0,20)