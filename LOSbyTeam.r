setwd("C:\\Users\\David\\Dropbox\\Stanford\\Epic Research\\AccessLog\\Census3.0")

library(ggplot2)
library(plyr)
library(stringr)
library(reshape2)

data <- read.csv("UniquePatientDateTeamSorted.csv")
data$admitDate <- strptime(data$ADMSN_DATE, "%m/%d/%Y")
data$dischDate <- strptime(data$DISCH_DATE, "%m/%d/%Y")
data$surgical <- str_detect(data$PROV_NAME, "SURGERY")

data$type <- "OTHER"
data[str_detect(data$PROV_NAME, "SURGERY"),]$type <- "SURGERY" 
data[str_detect(data$PROV_NAME, "CCU"),]$type <- "CCU" 
data[str_detect(data$PROV_NAME, "CVICU"),]$type <- "CVICU" 
data[str_detect(data$PROV_NAME, "HEMATOLOGY"),]$type <- "HEMATOLOGY" 
data[str_detect(data$PROV_NAME, "ONCOLOGY"),]$type <- "ONCOLOGY" 
data[str_detect(data$PROV_NAME, "MED UNIV"),]$type <- "MEDICINE" 
data[str_detect(data$PROV_NAME, "MICU"),]$type <- "MICU" 
data[str_detect(data$PROV_NAME, "SICU"),]$type <- "SICU" 
data[str_detect(data$PROV_NAME, "PAMF"),]$type <- "PAMF" 

heme <- data[str_detect(data$PROV_NAME, "TT HEMATOLOGY RESIDENT"),]

str(heme)

ggplot(data = heme, aes(y = X, x = admitDate))+ geom_segment(aes(xend = dischDate, yend = X))
ggplot(data = heme, aes(y = factor(DE_PAT_ID), x = admitDate))+ geom_segment(aes(xend = dischDate, yend = factor(DE_PAT_ID)))
ggplot(data = heme, aes(y = DE_PAT_ID, x = admitDate))+ geom_segment(aes(xend = dischDate, yend = DE_PAT_ID))

ggplot(data = data, aes(y = X, x = admitDate, colour = PROV_NAME ))+ geom_segment(aes(xend = dischDate, yend = X))

ggplot(data = data, aes(y = X, x = admitDate, colour = type ))+ geom_segment(aes(xend = dischDate, yend = X)) 
ggsave("LOSbyALLSERVICES.png")

ggplot(data = data[data$type == "MEDICINE",], aes(y = X, x = admitDate ))+ geom_segment(aes(xend = dischDate, yend = X)) 
ggsave("LOSbyMEDICINE.png")
ggplot(data = data[data$type == "CCU",], aes(y = X, x = admitDate ))+ geom_segment(aes(xend = dischDate, yend = X)) 
ggsave("LOSbyCCU.png")
ggplot(data = data[data$type == "HEMATOLOGY",], aes(y = X, x = admitDate ))+ geom_segment(aes(xend = dischDate, yend = X)) 
ggsave("LOSbyHEMATOLOGY.png")

ggplot(data = data[data$type == "HEMATOLOGY" | data$type == "CCU" | data$type == "MEDICINE"  ,],
 aes(y = X, x = admitDate ))+ geom_segment(aes(xend = dischDate, yend = X)) + facet_wrap(~type, scale = "free_y")
ggsave("VisualComparisonOfLOSOfDifferenceServices.png")

