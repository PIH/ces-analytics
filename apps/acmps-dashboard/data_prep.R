library(dplyr)
library(readxl)
library(lubridate)
library(scales)
library(plotly)
library(reshape2)

 # This data prep file contains four main types of functions:
  # 1) General Functions: Perform general calculations that are used throughout document
  # 2) Data Processing Functions: Import and merge excel exports from CommCare, clean and filter data
  # 3) Graph Indicator Functions: Calculate indicators that are used to make graphs in Shiny app UI  
  # 4) Functions that Select other Functions: Tell Shiny App Server what indicator functions to use to make graphs 



####################################### GENERAL FUNCTIONS #############################################

# Function that counts the how many cells are equal to x when passed x
CountTrue <- function(x) {
  length(which(x))
}

CountNonNA <- function(df) {
  length(which(!is.na(df)))
}


###################################  PROCESS CRONICOS DATA #########################################

# Process Chronic Patient Data Function
  # Expects input of Form Export Data Excel (acmpsPath) and Cronicos Cases Data Excel (cronicosPath)
  # Processes excel inputs to create a simplified table that merges chronic case data and chronic form data by CaseID

ProcessData <- function(acmpsPath, cronicosPath) {
# Import select sheets from From Export Data Excel   
  # Import Hoja de visita (sheet 7)
  CR1 <- read_excel(acmpsPath, sheet = 7)
  # Import Indicadores Cronicos (sheet 8)
  CR2 <- read_excel(acmpsPath, sheet = 8)

# Import Cases cronicos from CommCare Export 
  Casos_Cronicos <- read_excel(cronicosPath)

  # Merge CRONICOS - Hoja de visita and indicadores for based on caseID and month
  chronicPtMonths <- merge(CR1, CR2, by = c("form.case.@case_id", "form.mes", "form.ano"), all = TRUE)
  chronicPtMonths <- merge(Casos_Cronicos, chronicPtMonths, by.x = "ï»¿caseid", by.y = "form.case.@case_id", all = TRUE)

  chronicPtMonths <- FixDatatypes(chronicPtMonths)
  chronicTable <- CreateChronicTable(chronicPtMonths)

  return(chronicTable)
}

# Fix Data Types Function 
   # Expects input of merged cronicos table from Process Data Function
   # Converts text to numbers, standardizes community names, and changes column header for CaseID

FixDatatypes <- function(chronicPtMonths) {
  chronicPtMonths[6] <- lapply(chronicPtMonths[6], as.numeric)
  chronicPtMonths[10:13] <- lapply(chronicPtMonths[10:13], as.numeric)
  chronicPtMonths[15] <- lapply(chronicPtMonths[15], as.numeric)
  chronicPtMonths[16] <- lapply(chronicPtMonths[16], as.numeric)
  chronicPtMonths[17] <- lapply(chronicPtMonths[17], as.numeric)
  chronicPtMonths[19:26] <- lapply(chronicPtMonths[19:26], as.numeric)
 
  colnames(chronicPtMonths)[colnames(chronicPtMonths) == "ï»¿caseid"] <- "case_id"
  chronicPtMonths$community[which(chronicPtMonths$community == "Salvador_Urbina")] <- "Salvador"
  chronicPtMonths$community[which(chronicPtMonths$community == "Laguna")] <- "Laguna_del_Cofre"
  return(chronicPtMonths)
}


# Create Clean Chronic Table Function
  # Expects input of merged table from Process Data Function with FixDataTypes
  # Selects columns of interest from merged chronic table to create simplified dataframe of chronic patient information 
  # Adds new date column 

CreateChronicTable <- function(chronicPtMonths) {
  chronicTable <- data.frame(chronicPtMonths[, c(1, 15, 16, 17, 7, 8, 10:13, 19:26, 18)])
  chronicTable$Date <- as.Date(paste(chronicPtMonths$form.ano, chronicPtMonths$form.mes, "01"), format = "%Y %m %d")
  return(chronicTable)
}


# Filter by Month Function
  # Expects Input of processed chronics table and month 
  # Creates table that only shows chronics information from that month 

FilterByMonth <- function(chronicTable, filtermonth) {
  cronicosForMonth <- chronicTable %>% filter(chronicTable$form.mes == filtermonth)
  return(cronicosForMonth)
}


# Filter by Community Function
  # Expects input of processed chronics table and community 
  # Creates table that only shows chronics information from that community 

FilterByCommunity <- function(chronicTable, filtercommunity) {
  if(filtercommunity != "all") {
    cronicos <- chronicTable %>% filter(chronicTable$community == filtercommunity)
  }
  return(cronicos)
}

# Filter by Date Range Function 
  # Expects input of processed chronics table, start date and end date (yyyy-mm-dd)
  # Creates table with only information within specified date range in Date column 

FilterByDate <- function(chronicTable, startDate, endDate) {
  chronicTable %>% filter(chronicTable$Date >= startDate & chronicTable$Date <= endDate)
}


# Switch Between Chronic Disease Columns Function 
  # Expects input of DISEASE_CONTROL_COLS  or names(DISEASE_CONTROL_COLS)
  # Switches between control of disease column and does the patient have disease column

DISEASE_CONTROL_COLS <- c(
  "form.control_diabetes" = "does_the_patient_have_diabetes",
  "form.control_htn" = "does_the_patient_have_hypertension",
  "form.control_dep" = "does_the_patient_have_depression",
  "form.control_epilepsia" = "does_the_patient_have_epilepsy"
)


############################ FUNCTIONS FOR INDICATORS FOR CRONICOS GRAPHS ####################################


# Function for % hoja de visita llena presente (which is any hoja de visita which contains info in visits realized and visits planned columns)
  # Expects input processed chronics data, disease ("form.control_disease" column), and x-axis designation (byColumn)
  # Calculates the percentage of hojas de visita that are returned filled out 
  # Numerator: Count of patients contain any value in "visits realized" and "visitas planned" columns 
  # Denominator: Total number of patients is the number of patients with any information marked in "form.control_disease" column 

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
  # Expects input processed chronics data, disease ("form.control_disease" column), and x-axis designation (byColumn)
  # Calculates percent of control information for chronic patients filled out (# with control info / total number registered patients)
  # Numerator = number with information in "form.control_disease" column. Denominator = number that have 1 in "does the patient have X" column 

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
  controlTable <- mutate(controlTable, percent_nocontrol_info = 100 - percent_control_info)
  controlTable$percent_control_info <- round(controlTable$percent_control_info, 2)
  controlTable <- na.omit(controlTable)

  return(controlTable)
}



# Function for % Control by Disease per month
  # Expects input processed chronics data, disease ("form.control_disease" column), and x-axis designation (byColumn)
  # Calculates the percent of chronic patients that are in control for given disease (number in control /number with control information)
  # Numerator = number with information in "form.control_disease" column. Denominator = number that have any information in "form.control_disease" column

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
    control_percent = Control / Total * 100)
  
  diseaseForCommunity <- mutate(diseaseForCommunity, notcontrol_percent = 100 - control_percent)
  
  diseaseForCommunity <- na.omit(diseaseForCommunity)

  diseaseForCommunity$control_percent <- round(diseaseForCommunity$control_percent, 2)

  return(diseaseForCommunity)
}


# Function for # Control by Disease 
  # Expects input processed chronics data, disease ("form.control_disease" column), and x-axis designation (byColumn)
  # Calculates the number of chronic patients in control for given disease (patients with 1 in "form.control_disease" column)

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


# Function for # Not in Control by Disease
  # Expects input processed chronics data, disease ("form.control_disease" column), and x-axis designation (byColumn)
  # Calculates the number of chronic patients not in control for given disease (Total patients with disease - those in control)

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

# Number of Visits Function
  # Expects input processed chronics data, disease ("form.control_disease" column), and x-axis designation (byColumn)
  # Calculates number of visits realized  and number of visits planned by acompañantes 

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


PieControl <- function(cronicos, controlCol, by = NULL) {
  byColumn <- switch(by,
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

    totalPatients <- aggregate(cronicos[diseaseCol], cronicos[byColumn], CountNonNA)

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
                                control_percent = Control / Total * 100)

  diseaseForCommunity <- mutate(diseaseForCommunity, notcontrol_percent = 100 - control_percent)


  diseaseForCommunity <- diseaseForCommunity  %>% select(notcontrol_percent, control_percent)

 
  
  diseaseForCommunity <- na.omit(diseaseForCommunity)
  diseaseForCommunity <- melt(diseaseForCommunity)
  as.numeric(diseaseForCommunity[,2])
  diseaseForCommunity$value <- round(diseaseForCommunity$value, 2)
  
  return(diseaseForCommunity)
} 


############### FUNCTIONS THAT TELL SHINY APP WHAT CHRONIC INDICATOR TO USE FOR GRAPHS #####################


# Get Measure Function
  # Expects input of chronics measure indicator name from Shiny app UI 
  # Tells the graph in Shiny app  UI which function to use to create plot data  

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
  # Expects in put of processed chronics data and chronic indicator measure from Shiny App UI 
  # Tells graph what data to use for y-axis 

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





########################################## PROCESS ACMPS DATA #################################################


# Process ACMPS Patient Data Function
  # Expects input of Form Export Data Excel (acmpsPath) and ACMPS Cases Data Excel (acmpsCasesPath)
  # Processes excel inputs to create a simplified table that merges chronic case data and chronic form data by CaseID


ProcessDataAcmps <- function(acmpsPath, acmpsCasesPath) {
# Import select sheets from From Export Data Excel  
  # Import Satisfacción Facilitadora
  ACMP1 <- read_excel(acmpsPath, sheet = 1)
  # Import Satisfacción Pacientes
  ACMP2 <- read_excel(acmpsPath, sheet = 2)
  # Import Mentoria crónicos
  ACMP3 <- read_excel(acmpsPath, sheet = 3)
  # Import Mentoria salud materna
  ACMP4 <- read_excel(acmpsPath, sheet = 4)
  # Import Educación continua (asistencia) 
  ACMP5 <- read_excel(acmpsPath, sheet = 6)
  
# Import Casos Acompañantes from CommCare Export  
  Casos_ACMPS <- read_excel(acmpsCasesPath)
  
  # Merge ACMPS - pt/facil satisfaction, asistencia, menotria based on caseID and month
  acmpsPtMonths <- merge(ACMP1, ACMP2, by = c("form.case.@case_id", "form.mes", "form.ano"), all = TRUE)
  acmpsPtMonths <- merge(acmpsPtMonths, ACMP3, by = c("form.case.@case_id", "form.mes", "form.ano"), all = TRUE)
  acmpsPtMonths <- merge(acmpsPtMonths, ACMP4, by = c("form.case.@case_id", "form.mes", "form.ano"), all = TRUE)
  acmpsPtMonths <- merge(acmpsPtMonths, ACMP5, by = c("form.case.@case_id", "form.mes", "form.ano"), all = TRUE)
  acmpsPtMonths <- merge(Casos_ACMPS, acmpsPtMonths, by.x = "ï»¿caseid", by.y = "form.case.@case_id", all = TRUE)

  acmpsPtMonths <- FixDatatypesACMPS(acmpsPtMonths)
  acmpsPtMonths <- CreateAcmpsTable(acmpsPtMonths)

  return(acmpsPtMonths)
}

# Fix Data Types for ACMPS data Function 
  # Expects input of merged acompañantes table from Process Data Acmps Function
  # Converts text to numbers, standardizes community names, and changes column headers to be uniform

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
  acmpsPtMonths$form.calificacion_cronicos2 <- 100 * (acmpsPtMonths$form.calificacion_cronicos2)

  return(acmpsPtMonths)
}


# Create Clean Chronic Table Function
  # Expects input of merged table from Process Data Acmps Function after FixDataTypesAcmps
  # Selects columns of interest from merged chronic table to create simplified acmps dataframe
  # Adds new date column


CreateAcmpsTable <- function(acmpsPtMonths) {
  acmpsTable <- data.frame(acmpsPtMonths[, c(1, 3, 4:6, 8:10, 12, 17, 21, 23, 24)])
  acmpsTable$Date <- as.Date(paste(acmpsPtMonths$form.ano, acmpsPtMonths$form.mes, "01"), format = "%Y %m %d")
  return(acmpsTable)
}


############################ FUNCTIONS FOR INDICATORS FOR ACMPS GRAPHS ####################################

# Function for % Patients with satisfaction > 85% per Month
  # Expects innput of processed acmps data and definition of x-axis (byColumn)
  # Calculates the percentage of patients that have higher than 85% satisfaction 
  # Numerator = Number patients with satisfacition >85%. Denominator = Number of patients that have any satisfaction infomration

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



# Average Patient Satisfaction Function 
  # Expects innput of processed acmps data and definition of x-axis (byColumn)
  # Calculates the average of patient satisfaction scores

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


# ACMPS Percent Attendance Function 
  # Expects innput of processed acmps data and definition of x-axis (byColumn)
  # Calculates percent attendance of acompañantes to talks 
  # Numerator = number who attended. Denominator = number who have any data for attendance

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


# Percent Mentoria >80% Function 
  # Expects innput of processed acmps data and definition of x-axis (byColumn)
  # Calculates the percent of acompañantes who received higher than 80% grade in their mentoria visit
  # Numerator = total number acmps with grades above 80% for either cronicos or salud materna vists
  # Denominator = total number of acmps that received grade for either cronicos or salud materna visit


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

  mentoria <- setNames(mentoria, c(byColumn, "cronicos_mas_de_80", "embarazo_mas_de_80", "total_cronicos", "total_embarazo"))

  mentoria <- mutate(mentoria, greater80_mentoria = (cronicos_mas_de_80 + embarazo_mas_de_80) /
    (total_cronicos + total_embarazo) * 100)
  mentoria$greater80_mentoria <- round(mentoria$greater80_mentoria, 2)

  mentoria <- na.omit(mentoria)

  return(mentoria)
}


# Average Mentoria de ACMPS Function 
  # Expects innput of processed acmps data and definition of x-axis (byColumn)
  # Calculates average mentoria (for chronic and salud materna visits) for all acompañantes who had a mentoria visit 
  # Numerator = sum of mentoria scores cronicos + mentoria scores salud materna
  # Denominator = total number of acompañantes who received a mentoria score 

AverageMentoria <- function(acmpsDataTable, by = NULL) {
  byColumn <- switch(by,
    "Mes" = "form.mes",
    "Acompañante" = "form.nombre_acompanante",
    "Comunidad" = "community"
  )
  
  mentoriaCronicos <- aggregate(acmpsDataTable["form.calificacion_cronicos2"], acmpsDataTable[byColumn],
                                sum,
                                na.rm = TRUE
  )
  mentoriaSaludMaterna <- aggregate(acmpsDataTable["form.calificacion_embarazo2"], acmpsDataTable[byColumn],
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
  
  mentoria <- setNames(mentoria, c(byColumn, "sum_calificacion_cronicos", "sum_calificacion_embarazo", "total_cronicos", "total_embarazo"))
  
  mentoria <- mutate(mentoria, average_mentoria = (sum_calificacion_cronicos + sum_calificacion_embarazo) /
                       (total_cronicos + total_embarazo))
  
  mentoria$average_mentoria <- round(mentoria$average_mentoria, 2)
  
  mentoria <- na.omit(mentoria)
  

  return(mentoria)
}



# TODO: Satisfaction for Facilitators


############### FUNCTIONS THAT TELL SHINY APP UI WHAT ACMPS INDICATOR FUNCTIONS TO USE FOR GRAPHS ##########


# Get Measure Function for ACMPS 
  # Expects input of acmps measure indicator name from Shiny app UI 
  # Tells the graph in Shiny app  UI which function to use to create plot data  

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
  # Expects in put of processed acmps data and acmps indicator measure from Shiny App UI 
  # Tells graph what data to use for y-axis 

SelectPlotColumnAcmps <- function(filteredData, colName) {
  switch(colName,
    "percentPatientSatisfaction" = filteredData[["greater85_percent"]],
    "averagePatientSatisfaction" = filteredData[["average_satisfaction"]],
    "percentAttendance" = filteredData[["percent_attendance"]],
    "percentMentoria" = filteredData[["greater80_mentoria"]],
    "averageMentoria" = filteredData[["average_mentoria"]]
  )
}








############################# ACMP / PASANTE SATISFACTION  #######################

# Acompañante Satisfaction Score 

ProcessSatisData <- function(formDataPath){
  acmpSatis <- read_excel(formDataPath, sheet = 12)
  acmpSatis[1] <- lapply(acmpSatis[1], as.numeric)
  acmpSatis[3] <- lapply(acmpSatis[3], as.numeric)
  acmpSatis[4] <- lapply(acmpSatis[4], as.numeric)
  acmpSatis$Date <- as.Date(paste(acmpSatis$form.ano, acmpSatis$form.mes, "01"), format = "%Y %m %d")
  colnames(acmpSatis)[colnames(acmpSatis) == "form.comunidad"] <- "community"
  acmpSatis$community[which(acmpSatis$community == "planalta")] <- "Plan_Alta"
  acmpSatis$community[which(acmpSatis$community == "planbaja")] <- "Plan_Baja"
  acmpSatis$community[which(acmpSatis$community == "laguna")] <- "Laguna_del_Cofre"
  acmpSatis$community[which(acmpSatis$community == "capitan")] <- "Capitan"
  acmpSatis$community[which(acmpSatis$community == "honduras")] <- "Honduras"
  acmpSatis$community[which(acmpSatis$community == "letrero")] <- "Letrero"
  acmpSatis$community[which(acmpSatis$community == "matasano")] <- "Matasano"
  acmpSatis$community[which(acmpSatis$community == "reforma")] <- "Reforma"
  acmpSatis$community[which(acmpSatis$community == "salvador")] <- "Salvador"
  acmpSatis$community[which(acmpSatis$community == "soledad")] <- "Soledad"
  return(acmpSatis)
}

SatisAcmp <- function(acmpSatisTable, by = NULL){
  byColumn <- switch(by,
                     "Mes" = "form.mes",
                     "Comunidad" = "community"
  )
  acmpSatisfaction <- aggregate(acmpSatisTable["form.porcentaje_satisfaccion_acompanante"], acmpSatisTable[byColumn],
                                 mean,
                                 na.rm = TRUE
  )
  
  acmpSatisfaction <- setNames(acmpSatisfaction, c(byColumn, "average_satisfaction"))
  
  acmpSatisfaction$average_satisfaction <- round(acmpSatisfaction$average_satisfaction, 2)
  
  acmpSatisfaction <- na.omit(acmpSatisfaction)
  
  return(acmpSatisfaction)
}





# TODO in future: Pasante satisfaction graph 
