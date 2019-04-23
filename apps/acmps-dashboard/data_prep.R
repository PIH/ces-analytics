library(dplyr)
# library(XLConnect)
library(readxl)
library(lubridate)
library(scales)
library(plotly)




# Process Chronic Patient Data Function
CountTrue <- function(x) {
  length(which(x))
}

ProcessData <- function(acmpsPath, cronicosPath) {
  # Merge CRONICOS - Hoja de visita and indicadores for based on caseID and month
  CR1 <- read_excel(acmpsPath, sheet = 7)
  CR2 <- read_excel(acmpsPath, sheet = 8)
  Casos_Cronicos <- read_excel(cronicosPath)

  chronicPtMonths <- merge(CR1, CR2, by = c("form.case.@case_id", "form.mes", "form.ano"), all = TRUE)
  chronicPtMonths <- merge(Casos_Cronicos, chronicPtMonths, by.x = "ï»¿caseid", by.y = "form.case.@case_id", all = TRUE)

  # View(chronicPtMonths)
  chronicPtMonths <- FixDatatypes(chronicPtMonths)
  chronicTable <- CreateChronicTable(chronicPtMonths)

  return(chronicTable)
}

FixDatatypes <- function(chronicPtMonths) {
  chronicPtMonths[6] <- lapply(chronicPtMonths[6], as.numeric)
  chronicPtMonths[10:13] <- lapply(chronicPtMonths[10:13], as.numeric)
  chronicPtMonths[15] <- lapply(chronicPtMonths[15], as.numeric)
  chronicPtMonths[16] <- lapply(chronicPtMonths[16], as.numeric)
  chronicPtMonths[19:26] <- lapply(chronicPtMonths[19:26], as.numeric)
  chronicPtMonths[17] <- lapply(chronicPtMonths[17], as.numeric)
  # chronicPtMonths[9] <- lapply(chronicPtMonths[9], as.Date)

  colnames(chronicPtMonths)[colnames(chronicPtMonths) == "ï»¿caseid"] <- "case_id"
  chronicPtMonths$community[which(chronicPtMonths$community == "Salvador_Urbina")] <- "Salvador"
  chronicPtMonths$community[which(chronicPtMonths$community == "Laguna")] <- "Laguna_del_Cofre"
  
  return(chronicPtMonths)
}


# Create Clean Chronic Table Function

CreateChronicTable <- function(chronicPtMonths) {
  chronicTable <- data.frame(chronicPtMonths[, c(1, 15, 16, 17, 7, 8, 10:13, 19:26, 18)])
  chronicTable$Date <- as.Date(paste(chronicPtMonths$form.ano, chronicPtMonths$form.mes, "01"), format = "%Y %m %d")
  return(chronicTable)
}


# Create Table for Filter by Month Function

FilterByMonth <- function(chronicTable, filtermonth) {
  cronicosForMonth <- chronicTable %>% filter(chronicTable$form.mes == filtermonth)
  return(cronicosForMonth)
}


# Function to Create Filter by Community Table

FilterByCommunity <- function(chronicTable, filtercommunity) {
  if(filtercommunity != "all") {
    cronicos <- chronicTable %>% filter(chronicTable$community == filtercommunity)
  }
  return(cronicos)
}

# Function to Create Filter by Date Range

FilterByDate <- function(chronicTable, startDate, endDate) {
  chronicTable %>% filter(chronicTable$Date >= startDate & chronicTable$Date <= endDate)
}




# Function switching between control of disease column and does the patient have disease column

DISEASE_CONTROL_COLS <- c(
  "form.control_diabetes" = "does_the_patient_have_diabetes",
  "form.control_htn" = "does_the_patient_have_hypertension",
  "form.control_dep" = "does_the_patient_have_depression",
  "form.control_epilepsia" = "does_the_patient_have_epilepsy"
)


################# FUNCTIONS FOR CRONICOS GRAPHS ##################################


# Function for % hoja de visita llena presente (contains info in visits realized and planned columns)


PercentHojaVisita <- function(cronicos, controlCol, by = NULL) {
  byColumn <- switch(by,
    "Mes" = "form.mes",
    "Acompañante" = "form.nombre_acompanante",
    "Comunidad" = "community"
  )

  if (controlCol != "all") {
    cronicos <- filter(cronicos, cronicos[controlCol] == 1)
  }

  hojasVisita <- filter(cronicos, cronicos["form.numero_visita_acompanante"] >= 0)
  hojasVisita <- filter(hojasVisita, hojasVisita["form.numero_visitas_debe_realizar"] >= 0)

  CountNonNA <- function(df) {
    length(which(!is.na(df)))
  }

  if (controlCol != "all") {
    totalHojas <- aggregate(hojasVisita[controlCol], hojasVisita[byColumn], CountNonNA)
    totalPatients <- aggregate(cronicos[controlCol], cronicos[byColumn],
      sum,
      na.rm = TRUE
    )
  }

  if (controlCol == "all") {
    totalHojas <- aggregate(hojasVisita["form.numero_visita_acompanante"], hojasVisita[byColumn], CountNonNA)
    totalPatients <- aggregate(hojasVisita["form.numero_visita_acompanante"], hojasVisita[byColumn],
      sum,
      na.rm = TRUE
    )
  }

  hojasTable <- merge(totalPatients, totalHojas, by = byColumn)

  hojasTable <- setNames(hojasTable, c(byColumn, "total_patients", "hojas_llenas"))
  hojasTable <- mutate(hojasTable, percent_hojas = hojas_llenas / total_patients * 100)
  hojasTable$percent_hojas <- round(hojasTable$percent_hojas, 2)
  hojasTable <- na.omit(hojasTable)

  return(hojasTable)
}



# Function for % of Control Information Filled Out
# TODO: simplify code and test


PercentControlInfo <- function(cronicos, controlCol, by = NULL) {
  byColumn <- switch(by,
    "Mes" = "form.mes",
    "Acompañante" = "form.nombre_acompanante",
    "Comunidad" = "community"
  )

  DISEASE_CONTROL_COLS <- c(
    "form.control_diabetes" = "does_the_patient_have_diabetes",
    "form.control_htn" = "does_the_patient_have_hypertension",
    "form.control_dep" = "does_the_patient_have_depression",
    "form.control_epilepsia" = "does_the_patient_have_epilepsy"
  )

  CountNonNA <- function(df) {
    length(which(!is.na(df)))
  }


  if (controlCol != "all") {
    diseaseCol <- DISEASE_CONTROL_COLS[controlCol]

    cronicos <- filter(cronicos, cronicos[diseaseCol] == 1)

    controlInfo <- aggregate(cronicos[controlCol], cronicos[byColumn],
      sum,
      na.rm = TRUE
    )

    totalPatients <- aggregate(cronicos[diseaseCol], cronicos[byColumn], CountNonNA)
  }

  if (controlCol == "all") {
    CountTrue <- function(x) {
      length(which(x))
    }

    # compute denominator
    registeredWithDisease <- cronicos[DISEASE_CONTROL_COLS]
    anyDisease <- apply(registeredWithDisease, 1, any)
    cronicos <- cbind(cronicos, anyDisease)

    # compute numerator
    hasControlInfo <- cronicos[names(DISEASE_CONTROL_COLS)]
    hasControlInfo[hasControlInfo == 0 ] <- 1
    anyControlInfo <- apply(hasControlInfo, 1, any)
    cronicos <- cbind(cronicos, anyControlInfo)

    controlInfo <- aggregate(cronicos["anyControlInfo"], cronicos[byColumn], CountTrue)

    totalPatients <- aggregate(cronicos["anyDisease"], cronicos[byColumn], CountTrue)
  }

  controlTable <- merge(controlInfo, totalPatients, by = byColumn)

  controlTable <- setNames(controlTable, c(byColumn, "control_info", "total_patients"))
  controlTable <- mutate(controlTable, percent_control_info = control_info / total_patients * 100)
  controlTable$percent_control_info <- round(controlTable$percent_control_info, 2)
  controlTable <- na.omit(controlTable)

  return(controlTable)
}




# TODO: set default to all

# Function for % Control by Disease per month
PercentControl <- function(cronicos, controlCol, by = NULL) {
  byColumn <- switch(by,
    "Mes" = "form.mes",
    "Acompañante" = "form.nombre_acompanante",
    "Comunidad" = "community"
  )


  DISEASE_CONTROL_COLS <- c(
    "form.control_diabetes" = "does_the_patient_have_diabetes",
    "form.control_htn" = "does_the_patient_have_hypertension",
    "form.control_dep" = "does_the_patient_have_depression",
    "form.control_epilepsia" = "does_the_patient_have_epilepsy"
  )


  CountNonNA <- function(df) {
    length(which(!is.na(df)))
  }

  if (controlCol != "all") {
    diseaseControl <- aggregate(cronicos[controlCol], cronicos[byColumn],
      sum,
      na.rm = TRUE
    )
    
    diseaseTotal <- aggregate(cronicos[controlCol], cronicos[byColumn], CountNonNA)
  }

  if (controlCol == "all") {
    CountTrue <- function(x) {
      length(which(x))
    }

    # compute denominator
    hasControlInfo <- cronicos[names(DISEASE_CONTROL_COLS)]
    hasControlInfo[hasControlInfo == 0 ] <- 1
    anyControlInfo <- apply(hasControlInfo, 1, any)
    cronicos <- cbind(cronicos, anyControlInfo)

    # compute numerator
    inControl <- cronicos[names(DISEASE_CONTROL_COLS)]
    anyInControl <- apply(inControl, 1, any)
    cronicos <- cbind(cronicos, anyInControl)

    diseaseControl <- aggregate(cronicos["anyInControl"], cronicos[byColumn], CountTrue)
    diseaseTotal <- aggregate(cronicos["anyControlInfo"], cronicos[byColumn], CountTrue)
  }

  diseaseForCommunity <- merge(diseaseTotal, diseaseControl, by = byColumn)

  diseaseForCommunity <- setNames(diseaseForCommunity, c(byColumn, "Total", "Control"))

  diseaseForCommunity <- mutate(diseaseForCommunity,
    control_percent = Control / Total * 100
  )
  diseaseForCommunity <- na.omit(diseaseForCommunity)

  diseaseForCommunity$control_percent <- round(diseaseForCommunity$control_percent, 2)

  return(diseaseForCommunity)
}


# Function for # Control by Disease per month

NumberControl <- function(cronicos, controlCol, by = NULL) {
  byColumn <- switch(by,
    "Mes" = "form.mes",
    "Acompañante" = "form.nombre_acompanante",
    "Comunidad" = "community"
  )

  if (controlCol == "all") {

    # compute number in control
    inControl <- cronicos[names(DISEASE_CONTROL_COLS)]
    anyInControl <- apply(inControl, 1, any)
    cronicos <- cbind(cronicos, anyInControl)

    diseaseControl <- aggregate(cronicos["anyInControl"], cronicos[byColumn], CountTrue)
  }

  if (controlCol != "all") {
    diseaseControl <- aggregate(cronicos[controlCol], cronicos[byColumn],
      sum,
      na.rm = TRUE
    )
  }

  diseaseControl <- setNames(diseaseControl, c(byColumn, "number_control"))

  diseaseControl <- na.omit(diseaseControl)

  return(diseaseControl)
}



# Function for # Not in Control by Disease per month
NumberNotControl <- function(cronicos, controlCol, by = NULL) {
  byColumn <- switch(by,
    "Mes" = "form.mes",
    "Acompañante" = "form.nombre_acompanante",
    "Comunidad" = "community"
  )

  CountNonNA <- function(df) {
    length(which(!is.na(df)))
  }

  if (controlCol != "all") {
    diseaseControl <- aggregate(cronicos[controlCol], cronicos[byColumn],
      sum,
      na.rm = TRUE
    )

    diseaseTotal <- aggregate(cronicos[controlCol], cronicos[byColumn], CountNonNA)
  }

  if (controlCol == "all") {

    # compute denominator
    hasControlInfo <- cronicos[names(DISEASE_CONTROL_COLS)]
    hasControlInfo[hasControlInfo == 0 ] <- 1
    anyControlInfo <- apply(hasControlInfo, 1, any)
    cronicos <- cbind(cronicos, anyControlInfo)

    # compute numerator
    inControl <- cronicos[names(DISEASE_CONTROL_COLS)]
    anyInControl <- apply(inControl, 1, any)
    cronicos <- cbind(cronicos, anyInControl)

    diseaseControl <- aggregate(cronicos["anyInControl"], cronicos[byColumn], CountTrue)
    diseaseTotal <- aggregate(cronicos["anyControlInfo"], cronicos[byColumn], CountTrue)
  }

  diseaseForCommunity <- merge(diseaseTotal, diseaseControl, by = byColumn)

  diseaseForCommunity <- setNames(diseaseForCommunity, c(byColumn, "Total", "Control"))

  diseaseForCommunity <- mutate(diseaseForCommunity,
    not_control = Total - Control
  )

  diseaseForCommunity <- na.omit(diseaseForCommunity)

  return(diseaseForCommunity)
}

## Function for number of visits cronicos

NumberVisits <- function(cronicos, controlCol, by = NULL) {
  byColumn <- switch(by,
    "Mes" = "form.mes",
    "Acompañante" = "form.nombre_acompanante",
    "Comunidad" = "community"
  )
  if (controlCol != "all") {
    cronicos <- filter(cronicos, cronicos[controlCol] == 1)
  }

  cronicosRealized <- aggregate(cronicos["form.numero_visita_acompanante"], cronicos[byColumn],
    sum,
    na.rm = TRUE
  )

  cronicosPlanned <- aggregate(cronicos["form.numero_visitas_debe_realizar"], cronicos[byColumn],
    sum,
    na.rm = TRUE
  )

  cronicosTable <- merge(cronicosRealized, cronicosPlanned, by = byColumn)

  cronicosTable <- setNames(cronicosTable, c(byColumn, "realized", "planned"))

  cronicosTable <- na.omit(cronicosTable)

  return(cronicosTable)
}

# Get Measure Function

GetMeasureFunction <- function(measureName) {
  switch(measureName,
    "percentControl" = PercentControl,
    "numberControl" = NumberControl,
    "numberNotControl" = NumberNotControl,
    "numberVisits" = NumberVisits,
    "visitsPlanned" = NumberVisits,
    "percentHojaVisita" = PercentHojaVisita,
    "percentControlInfo" = PercentControlInfo
  )
}


# Select Column to Display Function

SelectPlotColumn <- function(filteredData, colName) {
  switch(colName,
    "percentControl" = filteredData[["control_percent"]],
    "numberControl" = filteredData[["number_control"]],
    "numberNotControl" = filteredData[["not_control"]],
    "numberVisits" = filteredData[["realized"]],
    "visitsPlanned" = filteredData[["planned"]],
    "percentHojaVisita" = filteredData[["percent_hojas"]],
    "percentControlInfo" = filteredData[["percent_control_info"]]
  )
}



########################################## ACMPS Data #################################################


# Process ACMPS Patient Data Function

ProcessDataAcmps <- function(acmpsPath, acmpsCasesPath) {
  # Merge ACMPS - pt/facil satisfaction, asistencia, menotria based on caseID and month
  ACMP1 <- read_excel(acmpsPath, sheet = 1)
  ACMP2 <- read_excel(acmpsPath, sheet = 2)
  ACMP3 <- read_excel(acmpsPath, sheet = 3)
  ACMP4 <- read_excel(acmpsPath, sheet = 4)
  ACMP5 <- read_excel(acmpsPath, sheet = 6)
  Casos_ACMPS <- read_excel(acmpsCasesPath)

  acmpsPtMonths <- merge(ACMP1, ACMP2, by = c("form.case.@case_id", "form.mes", "form.ano"), all = TRUE)
  acmpsPtMonths <- merge(acmpsPtMonths, ACMP3, by = c("form.case.@case_id", "form.mes", "form.ano"), all = TRUE)
  acmpsPtMonths <- merge(acmpsPtMonths, ACMP4, by = c("form.case.@case_id", "form.mes", "form.ano"), all = TRUE)
  acmpsPtMonths <- merge(acmpsPtMonths, ACMP5, by = c("form.case.@case_id", "form.mes", "form.ano"), all = TRUE)
  acmpsPtMonths <- merge(Casos_ACMPS, acmpsPtMonths, by.x = "ï»¿caseid", by.y = "form.case.@case_id", all = TRUE)

  # View(acmpsPtMonths)
  acmpsPtMonths <- FixDatatypesACMPS(acmpsPtMonths)
  acmpsPtMonths <- CreateAcmpsTable(acmpsPtMonths)

  return(acmpsPtMonths)
}

FixDatatypesACMPS <- function(acmpsPtMonths) {
  acmpsPtMonths[8:12] <- lapply(acmpsPtMonths[8:12], as.numeric)
  acmpsPtMonths[17] <- lapply(acmpsPtMonths[17], as.numeric)
  acmpsPtMonths[21] <- lapply(acmpsPtMonths[21], as.numeric)
  acmpsPtMonths[23] <- lapply(acmpsPtMonths[23], as.numeric)
  acmpsPtMonths[24] <- lapply(acmpsPtMonths[24], as.numeric)

  acmpsPtMonths$Community[which(acmpsPtMonths$Community == "Salvador_Urbina")] <- "Salvador"
  acmpsPtMonths$Community[which(acmpsPtMonths$Community == "Laguna")] <- "Laguna_del_Cofre"
  colnames(acmpsPtMonths)[colnames(acmpsPtMonths) == "Community"] <- "community"
  colnames(acmpsPtMonths)[colnames(acmpsPtMonths) == "first_name"] <- "form.nombre_acompanante"

  return(acmpsPtMonths)
}


# Create Clean ACMPs Table Function

CreateAcmpsTable <- function(acmpsPtMonths) {
  acmpsTable <- data.frame(acmpsPtMonths[, c(1, 3, 4:6, 8:10, 12, 17, 21, 23, 24)])
  acmpsTable$Date <- as.Date(paste(acmpsPtMonths$form.ano, acmpsPtMonths$form.mes, "01"), format = "%Y %m %d")
  return(acmpsTable)
}



# Function to Create Filter by Community Table

FilterByCommunityAcmps <- function(acmpsTable, filtercommunity) {
  acmpsForCommunity <- acmpsTable %>% filter(acmpsTable$Community == filtercommunity)
  return(acmpsForCommunity)
}


# Function for % Patients with satisfaction > 85% per Month

PercentPatientSatisfaction <- function(acmpsDataTable, by = NULL) {
  byColumn <- switch(by,
    "Mes" = "form.mes",
    "Acompañante" = "form.nombre_acompanante",
    "Comunidad" = "community"
  )
  patientSatisfaction <- aggregate(acmpsDataTable["form.porcentaje_satisf_pt"] >= 85, acmpsDataTable[byColumn],
    sum,
    na.rm = TRUE
  )

  CountNonNA <- function(df) {
    length(which(!is.na(df)))
  }
  patientsTotal <- aggregate(acmpsDataTable["form.porcentaje_satisf_pt"], acmpsDataTable[byColumn], CountNonNA)

  satisfactionPerMonth <- merge(patientsTotal, patientSatisfaction, by = byColumn)

  satisfactionPerMonth <- setNames(satisfactionPerMonth, c(byColumn, "Total", "greater_85"))

  satisfactionPerMonth <- mutate(satisfactionPerMonth,
    greater85_percent = greater_85 / Total * 100
  )

  satisfactionPerMonth$greater85_percent <- round(satisfactionPerMonth$greater85_percent, 2)

  satisfactionPerMonth <- na.omit(satisfactionPerMonth)

  return(satisfactionPerMonth)
}




# Average Patient Satisfaction

AveragePatientSatisfaction <- function(acmpsDataTable, by = NULL) {
  byColumn <- switch(by,
    "Mes" = "form.mes",
    "Acompañante" = "form.nombre_acompanante",
    "Comunidad" = "community"
  )
  patientSatisfaction <- aggregate(acmpsDataTable["form.porcentaje_satisf_pt"], acmpsDataTable[byColumn],
    mean,
    na.rm = TRUE
  )

  patientSatisfaction <- setNames(patientSatisfaction, c(byColumn, "average_satisfaction"))

  patientSatisfaction$average_satisfaction <- round(patientSatisfaction$average_satisfaction, 2)

  patientSatisfaction <- na.omit(patientSatisfaction)

  return(patientSatisfaction)
}


# ACMPS Percent Attendance to Platicas

PercentAttendance <- function(acmpsDataTable, by = NULL) {
  byColumn <- switch(by,
    "Mes" = "form.mes",
    "Acompañante" = "form.nombre_acompanante",
    "Comunidad" = "community"
  )
  sumAttendance <- aggregate(acmpsDataTable["form.asistencia"], acmpsDataTable[byColumn],
    sum,
    na.rm = TRUE
  )

  CountNonNA <- function(df) {
    length(which(!is.na(df)))
  }
  totalAcmps <- aggregate(acmpsDataTable["form.asistencia"], acmpsDataTable[byColumn], CountNonNA)

  attendancePerMonth <- merge(sumAttendance, totalAcmps, by = byColumn)
  attendancePerMonth <- setNames(attendancePerMonth, c(byColumn, "Attended", "Total"))
  attendancePerMonth <- mutate(attendancePerMonth, percent_attendance = Attended / Total * 100)

  attendancePerMonth$percent_attendance <- round(attendancePerMonth$percent_attendance, 2)

  attendancePerMonth <- na.omit(attendancePerMonth)

  return(attendancePerMonth)
}


# Percent Mentoria de ACMPS

PercentMentoria <- function(acmpsDataTable, by = NULL) {
  byColumn <- switch(by,
    "Mes" = "form.mes",
    "Acompañante" = "form.nombre_acompanante",
    "Comunidad" = "community"
  )
  mentoriaCronicos <- aggregate(acmpsDataTable["form.calificacion_cronicos2"] >= 80, acmpsDataTable[byColumn],
    sum,
    na.rm = TRUE
  )
  mentoriaSaludMaterna <- aggregate(acmpsDataTable["form.calificacion_embarazo2"] >= 80, acmpsDataTable[byColumn],
    sum,
    na.rm = TRUE
  )

  CountNonNA <- function(df) {
    length(which(!is.na(df)))
  }
  totalCronicos <- aggregate(acmpsDataTable["form.calificacion_cronicos2"], acmpsDataTable[byColumn], CountNonNA)

  totalSaludMaterna <- aggregate(acmpsDataTable["form.calificacion_embarazo2"], acmpsDataTable[byColumn], CountNonNA)

  mentoria <- merge(mentoriaCronicos, mentoriaSaludMaterna, by = byColumn)

  mentoria <- merge(mentoria, totalCronicos, by = byColumn)
  mentoria <- merge(mentoria, totalSaludMaterna, by = byColumn)

  mentoria <- setNames(mentoria, c(byColumn, "calificacion_cronicos", "calificacion_embarazo", "total_cronicos", "total_embarazo"))

  mentoria <- mutate(mentoria, greater80_mentoria = (calificacion_cronicos + calificacion_embarazo) /
    (total_cronicos + total_embarazo) * 100)
  mentoria$greater80_mentoria <- round(mentoria$greater80_mentoria, 2)

  mentoria <- na.omit(mentoria)

  return(mentoria)
}


# Average Mentoria de ACMPS

AverageMentoria <- function(acmpsDataTable, by = NULL) {
  byColumn <- switch(by,
    "Mes" = "form.mes",
    "Acompañante" = "form.nombre_acompanante",
    "Comunidad" = "community"
  )
  mentoriaCronicos <- aggregate(acmpsDataTable["form.calificacion_cronicos2"] >= 0, acmpsDataTable[byColumn],
    sum,
    na.rm = TRUE
  )
  mentoriaSaludMaterna <- aggregate(acmpsDataTable["form.calificacion_embarazo2"] >= 0, acmpsDataTable[byColumn],
    sum,
    na.rm = TRUE
  )

  CountNonNA <- function(df) {
    length(which(!is.na(df)))
  }
  totalCronicos <- aggregate(acmpsDataTable["form.calificacion_cronicos2"], acmpsDataTable[byColumn], CountNonNA)

  totalSaludMaterna <- aggregate(acmpsDataTable["form.calificacion_embarazo2"], acmpsDataTable[byColumn], CountNonNA)

  mentoria <- merge(mentoriaCronicos, mentoriaSaludMaterna, by = byColumn)

  mentoria <- merge(mentoria, totalCronicos, by = byColumn)
  mentoria <- merge(mentoria, totalSaludMaterna, by = byColumn)

  mentoria <- setNames(mentoria, c(byColumn, "calificacion_cronicos", "calificacion_embarazo", "total_cronicos", "total_embarazo"))

  mentoria <- mutate(mentoria, average_mentoria = (calificacion_cronicos + calificacion_embarazo) /
    (total_cronicos + total_embarazo) * 100)

  mentoria$average_mentoria <- round(mentoria$average_mentoria, 2)

  mentoria <- na.omit(mentoria)

  return(mentoria)
}




# TODO: Asistencia, satis facil,








# Get Measure Function for ACMPS

GetMeasureFunctionAcmps <- function(measureName) {
  switch(measureName,
    "percentPatientSatisfaction" = PercentPatientSatisfaction,
    "averagePatientSatisfaction" = AveragePatientSatisfaction,
    "percentAttendance" = PercentAttendance,
    "percentMentoria" = PercentMentoria,
    "averageMentoria" = AverageMentoria
  )
}


# Select Column to Display Function for ACMPS

SelectPlotColumnAcmps <- function(filteredData, colName) {
  switch(colName,
    "percentPatientSatisfaction" = filteredData[["greater85_percent"]],
    "averagePatientSatisfaction" = filteredData[["average_satisfaction"]],
    "percentAttendance" = filteredData[["percent_attendance"]],
    "percentMentoria" = filteredData[["greater80_mentoria"]],
    "averageMentoria" = filteredData[["average_mentoria"]]
  )
}
