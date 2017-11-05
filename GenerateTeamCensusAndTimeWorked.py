import io, datetime
from datetime import timedelta

medicineTeamFile = open("UniquePatientDateTeamMedicineOnlyWithMedicineLOS.csv", 'r')
medicineTeamFileWithLOS = open("UniquePatientDateTeamMedicineOnlyWithMedicineLOSAndAverageTimes.csv", 'w')
linecount = 0

totalDictionary = {}
teamDict = {}
#Key format will be Team,Date [list of patients]




internTimeDict = {}
residentTimeDict = {}

internFile = open("InternDayTeamHoursWorked.csv", 'r')
linecount3 = 0

for line in internFile:
    if(linecount3>0):
        linesplit = line.strip().split(',')
        team = linesplit[3].strip('"')
        date = datetime.datetime.strptime(linesplit[2].strip('"'), '%m/%d/%Y').date()
        hours = linesplit[5].strip('"')
        internTimeDict[team+','+str(date)] = float(hours)
        #print team,date,hours
    linecount3 += 1
internFile.close()

residentFile = open("ResidentDayTeamHoursWorked.csv", 'r')

linecount4 = 0
for line in residentFile:
    if(linecount4>0):
        linesplit = line.strip().split(',')
        team = linesplit[3].strip('"')
        date = datetime.datetime.strptime(linesplit[2].strip('"'), '%m/%d/%Y').date()
        hours = linesplit[5].strip('"')
        residentTimeDict[team+','+str(date)] =  float(hours)
        #print team,date,hours
    linecount4 += 1
residentFile.close()






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




medicineTeamFile2 = open("UniquePatientDateTeamMedicineOnlyWithMedicineLOS.csv", 'r')
linecount2 = 0
    
for line in medicineTeamFile2:

 
    if linecount2 == 0:
        medicineTeamFileWithLOS.write(line.strip() + ",averageInternTime,averageResidentTime\n")
        #print linecount2
        linecount2 += 1
        
    else:
        #print linecount2
        linesplit = line.split(',')

        internCensus = []
        teamCensus = []

        internTime = []
        residentTime = []
        

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
            if(team[12] + "," + str(startTime + timedelta(days=i)) in internTimeDict.keys()):
                internTime.append(internTimeDict[team[12] + "," + str(startTime + timedelta(days=i))])
            if(team[12] + "," + str(startTime + timedelta(days=i)) in residentTimeDict.keys()):
                residentTime.append(residentTimeDict[team[12] + "," + str(startTime + timedelta(days=i))])

        #print internCensus, str(sum(internCensus)/1.0/len(internCensus)), teamCensus, str(sum(teamCensus)/1.0/len(teamCensus))
        #print str(sum(internTime)/1.0/len(internTime)),str(sum(residentTime)/1.0/len(residentTime))

        
        if(len(internTime)>0 and len(residentTime) >0):        
            medicineTeamFileWithLOS.write(line.strip() + ',' + str(sum(internTime)/1.0/len(internTime)) + ',' + str(sum(residentTime)/1.0/len(residentTime)) +"\n")
        if(len(internTime)>0 and len(residentTime) == 0):        
            medicineTeamFileWithLOS.write(line.strip() + ',' + str(sum(internTime)/1.0/len(internTime)) + ',' + "NA"  +"\n")
        if(len(internTime) == 0 and len(residentTime) >0):        
            medicineTeamFileWithLOS.write(line.strip() + ',' + "NA" + ',' + str(sum(residentTime)/1.0/len(residentTime)) +"\n")
        if(len(internTime) == 0 and len(residentTime) == 0):        
            medicineTeamFileWithLOS.write(line.strip() + ',' + "NA" + ',' + "NA" + "\n")

                
    linecount2 = linecount2 + 1    

medicineTeamFile2.close()
medicineTeamFileWithLOS.close()
