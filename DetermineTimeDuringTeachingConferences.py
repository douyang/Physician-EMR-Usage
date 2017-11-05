import re
import io
import datetime
from datetime import datetime
from datetime import timedelta
import time
import os
from os import listdir


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
ResidentDateActions = {}
ResidentDateTotalTime = {}
NumberPatients = {}
contextSwitch = {}
breaks = {}

ResidentDateActionsInEducation = {}

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

                ResidentDateActionsInEducation[resident+","+str(accessDate)] = 0
                if(accessDate.weekday() == 2 and datetime.strptime("08:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("11:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] = 1
                if(accessDate.weekday() == 0 and datetime.strptime("10:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("11:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] = 1
                if(accessDate.weekday() == 1 and datetime.strptime("10:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("11:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] = 1
                if(accessDate.weekday() == 3 and datetime.strptime("10:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("11:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] = 1
                if(accessDate.weekday() == 4 and datetime.strptime("10:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("11:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] = 1
                if(accessDate.weekday() == 2 and datetime.strptime("12:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("13:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] = 1
                if(accessDate.weekday() == 0 and datetime.strptime("12:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("13:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] = 1
                if(accessDate.weekday() == 1 and datetime.strptime("12:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("13:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] = 1
                if(accessDate.weekday() == 3 and datetime.strptime("12:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("13:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] = 1
                if(accessDate.weekday() == 4 and datetime.strptime("12:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("13:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] = 1
                
                NumberPatients[resident+","+str(accessDate)] = []
                contextSwitch[resident+","+str(accessDate)] = 0
                breaks[resident+","+str(accessDate)] = 1
                currentTime = accessDateAndTime

                ResidentDateTotalTime[resident+","+str(accessDate)] = 0
                    
                if(patient != "None"):
                    NumberPatients[resident+","+str(accessDate)].append(patient)
                    contextSwitch[resident+","+str(accessDate)] = contextSwitch[resident+","+str(accessDate)] + 1
                    currentPatient = patient
                                
                                    
            else:
                ResidentDateActions[resident+","+str(accessDate)] = ResidentDateActions[resident+","+str(accessDate)] + 1

                if(accessDate.weekday() == 2 and datetime.strptime("08:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("11:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] += 1
                if(accessDate.weekday() == 0 and datetime.strptime("10:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("11:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] += 1
                if(accessDate.weekday() == 1 and datetime.strptime("10:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("11:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] += 1
                if(accessDate.weekday() == 3 and datetime.strptime("10:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("11:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] += 1
                if(accessDate.weekday() == 4 and datetime.strptime("10:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("11:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] += 1
                if(accessDate.weekday() == 2 and datetime.strptime("12:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("13:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] += 1
                if(accessDate.weekday() == 0 and datetime.strptime("12:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("13:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] += 1
                if(accessDate.weekday() == 1 and datetime.strptime("12:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("13:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] += 1
                if(accessDate.weekday() == 3 and datetime.strptime("12:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("13:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] += 1
                if(accessDate.weekday() == 4 and datetime.strptime("12:00:00", "%H:%M:%S").time() < accessTime < datetime.strptime("13:00:00", "%H:%M:%S").time()):
                    ResidentDateActionsInEducation[resident+","+str(accessDate)] += 1

                
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


outputFile = open("StanfordWardsWithTimeDuringEducation.csv", 'w')

outputFile.write("resident,date,firstAction,numberPatients,lastAction,numberActions,contextSwitch,difference,breaks,totalTime,numActionsDuringTeaching,PatientArray\n")

for residentDate in ResidentDateMin:

        #secondsAfter4 = (60*60*(ResidentDateMax[residentDate].hour - ResidentDateMinAfter4AM[residentDate].hour) +
        #                      60*(ResidentDateMax[residentDate].minute - ResidentDateMinAfter4AM[residentDate].minute) +
        #                      ResidentDateMax[residentDate].second - ResidentDateMinAfter4AM[residentDate].second)

        
    seconds = (60*60*(ResidentDateMax[residentDate].hour - ResidentDateMin[residentDate].hour) +
               60*(ResidentDateMax[residentDate].minute - ResidentDateMin[residentDate].minute) +
               ResidentDateMax[residentDate].second - ResidentDateMin[residentDate].second)
        
    outputFile.write(residentDate + ',' + str(ResidentDateMin[residentDate]) + "," + str(len(NumberPatients[residentDate])) +
                     "," + str(ResidentDateMax[residentDate]) + ',' + str(ResidentDateActions[residentDate]) + ',' +
                     str(contextSwitch[residentDate]) + "," + str(datetime.utcfromtimestamp(seconds).time()) +
                     "," + str(breaks[residentDate]) + "," + str(ResidentDateTotalTime[residentDate])+ "," +
                     str(ResidentDateActionsInEducation[residentDate])+ "," + str(NumberPatients[residentDate]).replace(',','/') +'\n') #',' + str(datetime.utcfromtimestamp(secondsAfter4).time()) +

outputFile.close()
