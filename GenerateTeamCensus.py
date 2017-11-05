import io, datetime
from datetime import timedelta

medicineTeamFile = open("UniquePatientDateTeamMedicineOnly.csv", 'r')
medicineTeamFileWithLOS = open("UniquePatientDateTeamMedicineOnlyWithMedicineLOS.csv", 'w')
linecount = 0

totalDictionary = {}
teamDict = {}
#Key format will be Team,Date [list of patients]


for line in medicineTeamFile:
   
    if linecount == 0:
        print line
        #medicineTeamFileWithLOS.write(line.strip() + ",medicineLOS,averageInternCensus,averageTeamCensus\n")
        
    else:
        linesplit = line.split(',')

        patient = linesplit[1]
        team = linesplit[3]

        # Note the index was off because prov name has a comma, removed that comma in excel
        admissionDate = datetime.datetime.strptime(linesplit[9], '%m/%d/%Y').date()
        startTime = admissionDate
        dischargeDate = datetime.datetime.strptime(linesplit[10], '%m/%d/%Y').date()
        endTime = dischargeDate
        teamStartDate = datetime.datetime.strptime(linesplit[5], '%m/%d/%Y %H:%M').date()
        if(len(linesplit[6]) > 0):
            teamEndDate = datetime.datetime.strptime(linesplit[6], '%m/%d/%Y %H:%M').date()
            if(teamEndDate < endTime):
                #print teamEndDate, endTime
                endTime = teamEndDate
                #print teamEndDate, endTime

        
        if(teamStartDate > startTime):
            startTime = teamStartDate
        
        los = linesplit[11]
        
        #print admissionDate, dischargeDate, startTime,endTime, los

        diffTime = (endTime - startTime).days

        for i in range(diffTime + 1):
            #print team[12]
            if(team[12] + "," + str(startTime + timedelta(days=i)) not in teamDict.keys()):
                teamDict[team[12] + "," + str(startTime + timedelta(days=i))] = []
                teamDict[team[12] + "," + str(startTime + timedelta(days=i))].append(patient)
            else:
                teamDict[team[12] + "," + str(startTime + timedelta(days=i))].append(patient)
                teamDict[team[12] + "," + str(startTime + timedelta(days=i))] = list(set(teamDict[team[12] + "," + str(startTime + timedelta(days=i))]))


            if(team + "," + str(startTime + timedelta(days=i)) not in totalDictionary.keys()):
                totalDictionary[team + "," + str(startTime + timedelta(days=i))] = []
                totalDictionary[team + "," + str(startTime + timedelta(days=i))].append(patient)
            else:
                #print totalDictionary[team + "," + str(startTime + timedelta(days=i))]
                totalDictionary[team + "," + str(startTime + timedelta(days=i))].append(patient)
                totalDictionary[team + "," + str(startTime + timedelta(days=i))] = list(set(totalDictionary[team + "," + str(startTime + timedelta(days=i))]))
                #print totalDictionary[team + "," + str(startTime + timedelta(days=i))]

        

    linecount += 1

medicineTeamFile.close()
print "done processing"





medicineTeamFile2 = open("UniquePatientDateTeamMedicineOnly.csv", 'r')
linecount2 = 0
    
for line in medicineTeamFile2:

 
    if linecount2 == 0:
        medicineTeamFileWithLOS.write(line.strip() + ",medicineLOS,averageInternCensus,averageTeamCensus\n")
        #print linecount2
        linecount2 += 1
        
    else:
        #print linecount2
        linesplit = line.split(',')

        internCensus = []
        teamCensus = []

        patient = linesplit[1]
        team = linesplit[3]

        admissionDate = datetime.datetime.strptime(linesplit[9], '%m/%d/%Y').date()
        startTime = admissionDate
        dischargeDate = datetime.datetime.strptime(linesplit[10], '%m/%d/%Y').date()
        endTime = dischargeDate
        teamStartDate = datetime.datetime.strptime(linesplit[5], '%m/%d/%Y %H:%M').date()
        if(len(linesplit[6]) > 0):
            teamEndDate = datetime.datetime.strptime(linesplit[6], '%m/%d/%Y %H:%M').date()
            if(teamEndDate < endTime):
                #print teamEndDate, endTime
                endTime = teamEndDate
                #print teamEndDate, endTime

        
        if(teamStartDate > startTime):
            startTime = teamStartDate
        
        los = linesplit[11]
        
        #print admissionDate, dischargeDate, startTime,endTime, los

        diffTime = (endTime - startTime).days

        #print diffTime
        
        for i in range(diffTime + 1):
            teamCensus.append(int(len(teamDict[team[12] + "," + str(startTime + timedelta(days=i))])))
            internCensus.append(int(len(totalDictionary[team + "," + str(startTime + timedelta(days=i))])))

        #print internCensus, str(sum(internCensus)/1.0/len(internCensus)), teamCensus, str(sum(teamCensus)/1.0/len(teamCensus))
        
        medicineTeamFileWithLOS.write(line.strip() + ',' + str(int(diffTime)) + ',' + str(sum(internCensus)/1.0/len(internCensus)) + ',' + str(sum(teamCensus)/1.0/len(teamCensus)) + "\n")

                
    linecount2 = linecount2 + 1    

medicineTeamFile2.close()
medicineTeamFileWithLOS.close()


outputfile = open("medicineTeamCensusSpecific-FromPython.csv", 'w')
outputfile.write("PROV_NAME,ACCESS_DATE,team_size,team\n")
for i in totalDictionary.keys():
    #print i+ "," + str(totalDictionary[i]) + "," + i[12] + "\n"
    outputfile.write(i+ "," + str(len(totalDictionary[i])) + "," + i[12] + "\n")

outputfile.close()


outputfile = open("medicineTeamCensusTotal-FromPython.csv", 'w')
outputfile.write("teamTotal,ACCESS_DATE,team_size\n")
for i in teamDict.keys():
    #print i+ "," + str(teamDict[i])  + "\n"
    outputfile.write(i+ "," + str(len(teamDict[i]))  + "\n")

outputfile.close()
