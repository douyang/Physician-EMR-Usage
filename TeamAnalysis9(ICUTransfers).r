# TEAM ANALYSIS 9 - Add Transfer TO ICU



setwd("C:\\Users\\David\\Dropbox\\Stanford\\Epic Research\\AccessLog\\Census3.0")

library(ggplot2)
library(gridExtra)
library(plyr)
library(reshape2)
library(strptime)
library(stringr)
library(scales)
library(ggthemes)


data <- read.csv("JChi_DOuyang_all_tteamv6.csv", stringsAsFactors = FALSE)
str(data)
table(data[str_detect(data$PROV_NAME, "MICU"),]$PROV_NAME)

data <- data[str_detect(data$PROV_NAME, "MICU") | str_detect(data$PROV_NAME, "TT MED UNIV "),]

summarizedData <-  ddply(data, .(DE_PAT_ID, HOSP_ADMSN_DT_TM), summarize, deathTimeFromLastAdmission = DAYS_LOG_ENC_DISCH_DEATH[1], 
				ACCESS_DATE = ACCESS_DATE[1], ADMSN_TIME = HOSP_ADMSN_DT_TM[1], DISCH_TIME = HOSP_DISCH_DT_TM[1], 
				ICU_Care = sum(str_detect(PROV_NAME, "MICU"))>0, Med_Care = sum(str_detect(PROV_NAME, "TT MED UNIV "))>0,
				firstTeam = PROV_NAME[1])
str(summarizedData)
str(summarizedData[summarizedData$Med_Care == TRUE,])

summarizedData$date2 <- strptime(substr(summarizedData$ADMSN_TIME,1,9), "%d-%b-%y")
str(summarizedData)
