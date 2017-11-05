import re
import io
import datetime
from datetime import datetime
from datetime import timedelta
import time
import os
from os import listdir
import math
import functools



#stanfordwardsFile = open("head100access_log.tab", 'r')


#stanfordwardsFile = open("stanfordWards2.tab", 'r')

allresidents = os.listdir("C:\Users\\David\\Dropbox\\Stanford\\Epic Research\\AccessLog\\Version2.0-StanfordWards\\SortedWithoutStanfordResident")
#allresidents = os.listdir("C:\\Users\\DeNovo\\Dropbox\\Stanford\\Epic Research\\AccessLog\\Version2.0-StanfordWards\\SortedWithoutStanfordResident")

#residentsOnStanfordWards = open("ResidentsOnStanfordWards.csv", 'r')
#filterStanfordWards = residentsOnStanfordWards.read()

lineNumber = 0
lineOne = ""
ResidentDateMin = {}
#ResidentDateMinAfter4AM = {}
ResidentDateMax = {}
ResidentDate5th = {}
ResidentDate95th = {}
ResidentDateActions = {}
ResidentDateTotalTime = {}
NumberPatients = {}
contextSwitch = {}
breaks = {}
ResidentDateDistribution = {}


for sortedresident in allresidents:
    print sortedresident
    residentFile = open("C:\\Users\\David\\Dropbox\\Stanford\\Epic Research\\AccessLog\\Version2.0-StanfordWards\\SortedWithoutStanfordResident\\" + sortedresident, "r")
    #residentFile = open("C:\\Users\\DeNovo\\Dropbox\\Stanford\\Epic Research\\AccessLog\\Version2.0-StanfordWards\\SortedWithoutStanfordResident\\" + sortedresident, "r")

    lineNumber = 0
    lineOne = ""
    currentPatient = "None"
    currentTime = "None"
    #currentSweepTime = datetime.strptime("00:00:01", "%H:%M:%S").time()
        
    for line in residentFile:
        lineNumber = lineNumber + 1

        if(lineNumber % 10000 == 0):
            print lineNumber
            

        if(lineNumber > 1):
            resident = line.split('\t')[2]
            accessDate = datetime.strptime(line.split('\t')[4].strip('"'), "%Y-%m-%d %H:%M:%S").date()
            accessTime = datetime.strptime(line.split('\t')[4].strip('"'), "%Y-%m-%d %H:%M:%S").time()
            accessDateAndTime = datetime.strptime(line.split('\t')[4].strip('"'), "%Y-%m-%d %H:%M:%S")
            patient = line.split('\t')[3]
                    
            if((resident+","+str(accessDate)) not in ResidentDateMin):
                ResidentDateMin[resident+","+str(accessDate)] = accessTime
                ResidentDateMax[resident+","+str(accessDate)] = accessTime
                #ResidentDateMinAfter4AM[resident+","+str(accessDate)] = datetime.strptime("2015-05-05 04:00:00", "%Y-%m-%d %H:%M:%S").time()
                ResidentDateActions[resident+","+str(accessDate)] = 1
                NumberPatients[resident+","+str(accessDate)] = []
                contextSwitch[resident+","+str(accessDate)] = 0
                breaks[resident+","+str(accessDate)] = 1
                currentTime = accessDateAndTime
                ResidentDateDistribution[resident+","+str(accessDate)] = []
                ResidentDateDistribution[resident+","+str(accessDate)].append(accessTime)
                
                ResidentDateTotalTime[resident+","+str(accessDate)] = 0
                    
                if(patient != "None"):
                    NumberPatients[resident+","+str(accessDate)].append(patient)
                    contextSwitch[resident+","+str(accessDate)] = contextSwitch[resident+","+str(accessDate)] + 1
                    currentPatient = patient
                                
                                    
            else:
                ResidentDateActions[resident+","+str(accessDate)] = ResidentDateActions[resident+","+str(accessDate)] + 1
                ResidentDateDistribution[resident+","+str(accessDate)].append(accessTime)
                
                if(ResidentDateMin[resident+","+str(accessDate)] > accessTime):
                    ResidentDateMin[resident+","+str(accessDate)] = accessTime
                    
                #if(ResidentDateMinAfter4AM[resident+","+str(accessDate)] > accessTime and accessTime.hour >=4):
                #    ResidentDateMinAfter4AM[resident+","+str(accessDate)] = accessTime

                if(ResidentDateMax[resident+","+str(accessDate)] < accessTime):
                    ResidentDateMax[resident+","+str(accessDate)] = accessTime
                            
                if(patient != '"None"' and patient not in NumberPatients[resident+","+str(accessDate)]):
                    NumberPatients[resident+","+str(accessDate)].append(patient)

                if(patient != currentPatient and patient != '"None"'):
                    #print currentPatient, patient, accessDateAndTime - currentTime, accessDateAndTime - currentTime < timedelta(minutes = 5)
                    currentPatient = patient

                    if (accessDateAndTime - currentTime < timedelta(minutes = 5)):
                        contextSwitch[resident+","+str(accessDate)] = contextSwitch[resident+","+str(accessDate)] + 1

                if (accessDateAndTime - currentTime > timedelta(minutes = 5)):
                    breaks[resident+","+str(accessDate)] = breaks[resident+","+str(accessDate)] + 1
                else:
                    #print (accessDateAndTime - currentTime).seconds                    
                    ResidentDateTotalTime[resident+","+str(accessDate)] = ResidentDateTotalTime[resident+","+str(accessDate)] + (accessDateAndTime - currentTime).seconds

                currentTime = accessDateAndTime

    residentFile.close()


for residentDate in ResidentDateDistribution.keys():
    ResidentDate5th[residentDate] = ResidentDateDistribution[residentDate][int(len(ResidentDateDistribution[residentDate]) * 0.05)]
    ResidentDate95th[residentDate] = ResidentDateDistribution[residentDate][int(len(ResidentDateDistribution[residentDate]) * 0.95)]
    
outputFile = open("StanfordWardsResidentMinMaxWithout17162or17102-StanfordOnly-FinallySorted-TimeSensitive-TotalTimeComputed-PatientIdentified-AddingPercentile.csv", 'w')

outputFile.write("resident,date,firstAction,numberPatients,lastAction,numberActions,contextSwitch,difference,breaks,totalTime,5thPercentile,95thPercentile,innerTotalTime,PatientArray\n")

for residentDate in ResidentDateMin:

        #secondsAfter4 = (60*60*(ResidentDateMax[residentDate].hour - ResidentDateMinAfter4AM[residentDate].hour) +
        #                      60*(ResidentDateMax[residentDate].minute - ResidentDateMinAfter4AM[residentDate].minute) +
        #                      ResidentDateMax[residentDate].second - ResidentDateMinAfter4AM[residentDate].second)

        
    seconds = (60*60*(ResidentDateMax[residentDate].hour - ResidentDateMin[residentDate].hour) +
               60*(ResidentDateMax[residentDate].minute - ResidentDateMin[residentDate].minute) +
               ResidentDateMax[residentDate].second - ResidentDateMin[residentDate].second)

    secondsIQR = (60*60*(ResidentDate95th[residentDate].hour - ResidentDate5th[residentDate].hour) +
               60*(ResidentDate95th[residentDate].minute - ResidentDate5th[residentDate].minute) +
               ResidentDate95th[residentDate].second - ResidentDate5th[residentDate].second)
        
    outputFile.write(residentDate + ',' + str(ResidentDateMin[residentDate]) + "," + str(len(NumberPatients[residentDate])) +
                     "," + str(ResidentDateMax[residentDate]) + ',' + str(ResidentDateActions[residentDate]) + ',' +
                     str(contextSwitch[residentDate]) + "," + str(datetime.utcfromtimestamp(seconds).time()) +
                     "," + str(breaks[residentDate]) + "," + str(ResidentDateTotalTime[residentDate])+ "," +
                     str(ResidentDate5th[residentDate]) + ',' + str(ResidentDate95th[residentDate]) + ',' + str(datetime.utcfromtimestamp(secondsIQR).time()) + ',' +
                     #(ResidentDate95th[residentDate] - ResidentDate5th[residentDate]).seconds + 
                     str(NumberPatients[residentDate]).replace(',','/') +'\n') #',' + str(datetime.utcfromtimestamp(secondsAfter4).time()) +

outputFile.close()
