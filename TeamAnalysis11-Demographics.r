###############################################
#                                             #
#  David Ouyang, Stanford Internal Medicine   #
#  2015, Ouyangd@stanford.edu                 #
#                                             #
###############################################



# Analysis of Demographics and DRGs

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



dedupdataFINAL <- read.csv("patientsDeduplicated.csv")
str(dedupdataFINAL)


dedupdataFINAL$date <- strptime(dedupdataFINAL$ADMSN_DATE, format = "%m/%d/%Y")
dedupdataFINAL$uniqueHosp <- paste(dedupdataFINAL$date, dedupdataFINAL$DE_PAT_ID)
head(dedupdataFINAL$uniqueHosp)
str(dedupdataFINAL)

demographics <- read.csv("Combo_v8_v9_v10_demographicsOnly.csv")
str(demographics )

#Identify unique patients in all those files
uniqueDemo <- ddply(demographics , .(DE_PAT_ID ), summarize,
								sex = GENDER[1],
								race = PAT_RACE[1],
								income = INCOME[1],
								numDiags = str_count(ICD_PATIENT_LEVEL[1], '/') + 1,
								age = mean(AGE_AT_ADMIT))

str(uniqueDemo)

head(uniqueDemo)

summary(uniqueDemo)


#http://www.census.gov/content/dam/Census/library/publications/2014/demo/p60-249.pdf
#https://en.wikipedia.org/wiki/Household_income_in_the_United_States
#https://www.census.gov/hhes/www/cpstables/032014/hhinc/hinc01_000R.htm

uniqueDemo$incomeQuartile <- -9
uniqueDemo[!is.na(uniqueDemo$income) & 0 <= uniqueDemo$income & uniqueDemo$income < 22000,]$incomeQuartile <- 4
uniqueDemo[!is.na(uniqueDemo$income) & 22000 <= uniqueDemo$income & uniqueDemo$income < 53585,]$incomeQuartile <- 3
uniqueDemo[!is.na(uniqueDemo$income) & 53585 <= uniqueDemo$income & uniqueDemo$income < 96000,]$incomeQuartile <- 2
uniqueDemo[!is.na(uniqueDemo$income) & 96000 <= uniqueDemo$income & uniqueDemo$income < 250001,]$incomeQuartile <- 1

table(uniqueDemo$incomeQuartile)

#Merge Data
dedupdataFINALWithDemo <- merge(dedupdataFINAL, uniqueDemo, by = "DE_PAT_ID", all.x = TRUE)
str(dedupdataFINALWithDemo )
summary(dedupdataFINALWithDemo )



summary(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,])
str(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,])
summary(dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,])
str(dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,])

t.test(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$age, dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$age)
sd(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$age, na.rm = TRUE)
sd(dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$age, na.rm = TRUE)


t.test(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$numDiags , dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$numDiags )
sd(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$numDiags , na.rm = TRUE)
sd(dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$numDiags , na.rm = TRUE)


t.test(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$sex , dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$sex )
table(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$sex)
table(dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$sex)
chisq.test(matrix(c(1002,950,1458,1347), ncol = 2))

1002/(1002+950)
1458/(1458 + 1347)

t.test(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$race, dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$race)
table(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$race)
table(dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$race)


chisq.test(matrix(c(955,177,329,5,486,1389,272,(337+108),12,697), ncol = 2))#chisq.test(matrix(c(436,1871,408, 2052), ncol = 2))  # Not Removing Outliers

955/1952
177/1952
329/1952
5/1952
486/1952
955/1952 + 177/1952 + 329/1952 + 5/1952 + 486/1952
1389/2815
272/2815
(337+108)/2815
12/2815
697/2815
1389/2815 +272/2815+(337+108)/2815+12/2815+697/2815

1389 + 272 +(337+108)+ 12 + 697

chisq.test(matrix(c(682 ,910 ,302 ,1,57,994 ,1305 ,432 ,2,82 ), ncol = 2))#chisq.test(matrix(c(436,1871,408, 2052), ncol = 2))  # Not Removing Outliers
chisq.test(matrix(c(1510 ,1952 -1510 ,2187 , 2815 - 2187 ), ncol = 2))
1510 /(1952 - 1510)
2187 / (2815 - 2187)
1510 /(1952 )
2187 / (2815 )

t.test(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$Income , dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$Income )
sd(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$Income , na.rm = TRUE)
sd(dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$Income , na.rm = TRUE)


table(dedupdataFINALWithDemo[dedupdataFINALWithDemo$over80,]$incomeQuartile)
table(dedupdataFINALWithDemo[!dedupdataFINALWithDemo$over80,]$incomeQuartile)

682/1952
910/1952
302/1952
1/1952
57/1952
994/2815
1305/2815
432/2815
2/2815
82/2815

dedupdataFINALWithDemo[is.na(dedupdataFINALWithDemo$sex),]$DE_PAT_ID
length(unique(dedupdataFINALWithDemo[is.na(dedupdataFINALWithDemo$sex),]$DE_PAT_ID))








############ DRGs ##############
# First combine only patients that have 1 hospitalization




DRGs <- read.csv("Combo_v8_v9_DRGsOnly.csv")
DRGs$date <- strptime(DRGs$ENC_ADMISSION_DATE, format = "%m/%d/%Y")
DRGs$uniqueHosp <- paste(DRGs$date, DRGs$DE_PAT_ID)
str(DRGs)
head(DRGs$uniqueHosp)
length(unique(DRGs$uniqueHosp))

uniqueDRG <- ddply(DRGs[-10], .(uniqueHosp ), summarize,
								los = mean(ENC_LOS),
								weight = mean(ENC_MS_DRG_WEIGHT))

head(uniqueDRG$uniqueHosp)
head(dedupdataFINALWithDemo$uniqueHosp)

dedupdataFINALwithDRG <- merge(dedupdataFINALWithDemo, uniqueDRG, by = "uniqueHosp") 
str(dedupdataFINALwithDRG )



str(dedupdataFINALwithDRG[dedupdataFINALwithDRG$over80,])
str(dedupdataFINALwithDRG[!dedupdataFINALwithDRG$over80,])
t.test(dedupdataFINALwithDRG[dedupdataFINALwithDRG$over80,]$weight , dedupdataFINALwithDRG[!dedupdataFINALwithDRG$over80,]$weight )
sd(dedupdataFINALwithDRG[dedupdataFINALwithDRG$over80,]$weight , na.rm = TRUE)
sd(dedupdataFINALwithDRG[!dedupdataFINALwithDRG$over80,]$weight , na.rm = TRUE)


dedupdataFINALwithDRG <- merge(dedupdataFINALWithDemo, uniqueDRG, by = "uniqueHosp", all.x = TRUE) 

str(dedupdataFINALwithDRG )

write.csv(dedupdataFINALwithDRG[!is.na(dedupdataFINALwithDRG$weight),], "patientsDeduplicatedWithDRGs.csv")


dedupdataFINALwithDRG[is.na(dedupdataFINALwithDRG$weight),]$weight <- -99
write.csv(dedupdataFINALwithDRG[!is.na(dedupdataFINALwithDRG$weight),], "patientsDeduplicatedWithDRGs-All.csv")



write.csv(dedupdataFINALwithDRG[is.na(dedupdataFINALwithDRG$weight),], "missingPatients4.csv")








###### VERIFY WHICH DATASET THEY ARE FROM #######
#UPDATE: THEY ARE ALL IN BOTH V3 and V4 #

theMissingOnes <- dedupdataFINALwithDRG[is.na(dedupdataFINALwithDRG$weight),]$DE_PAT_ID

data <- read.csv("JChi_DOuyang_acclog_tteamv4.csv")
str(data)

theMissingOnes %in% data$DE_PAT_ID

data2 <- read.csv("JChi_DOuyang_acclog_tteamv3.csv")
str(data2)

theMissingOnes %in% data2$DE_PAT_ID


