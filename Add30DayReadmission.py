import io, datetime
from datetime import timedelta

medicineTeamFile = open("UniquePatientDateTeamMedicineOnlyWithMedicineLOS-SortedByPatientAndTime.csv", 'r')
outputFile = open("UniquePatientDateTeamMedicineOnlyWith30DayReadmission.csv", 'w')

linecount = 0

currentPatient = "NA"
allAdmissions = []
allDischarges = []
allLines = []
#Key format will be Team,Date [list of patients]


for line in medicineTeamFile:
    if linecount != 0:
            
        linesplit = line.strip().split(',')
        patient = linesplit[1]
        admissionDate = datetime.datetime.strptime(linesplit[9], '%m/%d/%Y').date()
        dischargeDate = datetime.datetime.strptime(linesplit[10], '%m/%d/%Y').date()

        if(patient == currentPatient):
            allAdmissions.append(admissionDate)
            #allAdmissions = list(set(allAdmissions))

            allDischarges.append(dischargeDate)
            #allDischarges = list(set(allDischarges))

            allLines.append(line)
            

        else:
            for i in range(0,len(allLines)):
                readmission = 0
                thisDischarge = allDischarges[i]
                ttReadmission = "NA"

                for otherDates in allAdmissions:
                    if(timedelta(days = 0) < (otherDates - thisDischarge) < timedelta(days = 30)):
                        ttReadmission = (otherDates - thisDischarge).days
                        #print currentPatient, thisDischarge, otherDates, ttReadmission
                        readmission = 1

                outputFile.write(allLines[i].strip() +"," + str(readmission) + "," + str(ttReadmission) + "\n")
                
                
            
            #Restart with new patient
            currentPatient = patient
            allAdmissions = []
            allAdmissions.append(admissionDate)
            allDischarges = []
            allDischarges.append(dischargeDate)
            allLines = []
            allLines.append(line)

    else:
        outputFile.write(line.strip()+",30dayReadmission,timeToReadmission\n")
        linecount += 1

medicineTeamFile.close()
outputFile.close()
