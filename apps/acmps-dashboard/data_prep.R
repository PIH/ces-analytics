library(dplyr)
library(XLConnect)
library(readxl)
library(lubridate)

# FullExcelRead <- function(fpath, v = FALSE) {
#   sheetnames <- excel_sheets(fpath)
#   workbook <- sapply(sheetnames, function(x) {
#     readxl::read_excel(fpath, sheet = x)
#   })
#   for (sh in sheetnames) {
#     workbook[[sh]] <- as.data.frame.table(workbook[[sh]])
#   }
#   if (v) {
#     lapply(sheetnames, function(x) {
#       View(workbook[[x]], x)
#     })
#   }
#   return(workbook)
# }

# Process Chronic Patient Data Function 

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

  return(chronicPtMonths)
}


# Create Clean Chronic Table Function 

CreateChronicTable <- function(chronicPtMonths) {
  chronicTable<- data.frame(chronicPtMonths[, c(1, 15, 16, 17, 7, 8, 10:13, 19:26, 18)])
  chronicTable$Date <- as.Date(paste(chronicPtMonths$form.ano, chronicPtMonths$form.mes, "01"), format = "%Y %m %d")
  return(chronicTable)
}


# Create Table for Filter by Month Function

FilterByMonth <- function(chronicTable, filtermonth) {
  cronicosForMonth<- chronicTable %>% filter(chronicTable$form.mes == filtermonth)
  return(cronicosForMonth)
}


# Function to Create Filter by Community Table

FilterByCommunity <- function(chronicTable, filtercommunity) {
  cronicosForCommunity <- chronicTable %>% filter(chronicTable$community == filtercommunity)
  return(cronicosForCommunity)
}

# Function to Create Filter by Date Range

FilterByDate <- function(chronicTable, startDate, endDate){
  chronicTable %>% filter(chronicTable$Date >= startDate & chronicTable$Date <= endDate)
}




# Function switching between control of disease column and does the patient have disease column 

PickDisease <- function(diseaseCol){
  switch(diseaseCol,
         "form.control_diabetes" = "does_the_patient_have_diabetes",
         "form.control_htn" = "does_the_patient_have_hypertension",
         "form.control_dep" = "does_the_patient_have_depression")
}


####### Disease Control
ControlledPatients <- function(cronicos, controlCol, by = NULL){
  # Computes table with columns 1) Category given by "by" (i.e. Mes, Acompañante, Community)
  #                             2) Total number of patients
  #                             3) Controlled patients of the disease given by "controlCol"
  #                             4) Not controlled patients
  #                             5) % Controlled
  #                             6) % Not Controlled
  byColumn <- switch( by, 
                "Mes" = "form.mes",
		"Acompañante" = "form.nombre_acompanante",
		"Comunidad" = "community"
              )

  controlled <- aggregate(cronicos[controlCol], cronicos[byColumn],
                            sum, na.rm = TRUE)
  
  total <- aggregate(cronicos[controlCol], cronicos[byColumn],
                          length)
  
  counts <- merge(total, controlled, by =  byColumn)
  
  counts <- setNames(counts, c(by, "Total_Pacientes", "Controlados"))

  counts$No_Controlados <- counts$Total_Pacientes - counts$Controlados

  counts$"% Controlados" <- (counts$Controlados/counts$Total_Pacientes*100) %>% round(digits = 2) 

  counts$"% No Controlados" <-  (counts$No_Controlados/counts$Total*100) %>% round(digits = 2)
  
  return(counts)
  
}


NumberVisits <- function(cronicos, controlCol, by = NULL){

  byColumn <- switch( by,
                "Mes" = "form.mes",
		"Acompañante" = "form.nombre_acompanante",
		"Comunidad" = "community"
              )

  visits <- filter(cronicos, cronicos[controlCol] == 1)
 
  visitsRealized <- aggregate(visits["form.numero_visita_acompanante"], visits[byColumn], 
                     sum, na.rm = TRUE)
 
  visitsPlanned <- aggregate(visits["form.numero_visitas_debe_realizar"], visits[byColumn], 
                             sum, na.rm = TRUE)
 
  visits <- merge(visitsPlanned, visitsRealized, by = byColumn)
 
  visits <- setNames(visits, c("Mes", "Visitas_Planeadas", "Visitas_Hechas"))

  visits$Total_Visitas <- visits$Visitas_Planeadas + visits$Visitas_Hechas

  visits$"% Hechas" <- (visits$Visitas_Hechas/visits$Visitas_Planeadas*100) %>% round(digits = 2)

  visits$"% Planeadas" <- (visits$Visitas_Faltantes/visits$Visitas_Planeadas*100) %>% round(digits = 2)

  visits <- visits[,c("Mes", "Total_Visitas", "Visitas_Planeadas", "Visitas_Hechas", "% Planeadas", "% Hechas")]
  
  visits <- na.omit(visits)
 
  return(visits)
}


VisitSheets <- function(cronicos, controlCol, by = NULL){

  byColumn <- switch( by,
                "Mes" = "form.mes",
		"Acompañante" = "form.nombre_acompanante",
		"Comunidad" = "community"
              )

  visits <- filter(cronicos, cronicos[controlCol] == 1)

  colnames(visits)[1] <- "case_id"
  
  hojasVisita <- filter(visits, visits["form.numero_visita_acompanante"] >= 0)
  hojasVisita <- filter(hojasVisita, hojasVisita["form.numero_visitas_debe_realizar"] >= 0)
  
  totalHojas <- aggregate(hojasVisita[controlCol], hojasVisita[byColumn], length)
  
  totalPatients <- aggregate(cronicos[controlCol], cronicos[byColumn],
                             sum, na.rm = TRUE)
  
  hojasTable <- merge(totalPatients, totalHojas, by = byColumn)
  
  hojasTable <- setNames(hojasTable, c(by , "Total_Pacientes", "Hojas_Llenas"))

  hojasTable$Hojas_Faltantes <- hojasTable$Total_Pacientes - hojasTable$Hojas_Llenas

  hojasTable$"% Hojas_LLenas" <- (hojasTable$Hojas_Llenas/hojasTable$Total_Pacientes*100) %>% round(digits = 2)

  hojasTable$"% Hojas_Faltantes" <-  (hojasTable$Hojas_Faltantes/hojasTable$Total_Pacientes*100) %>% round(digits = 2)
  
  hojasTable <- na.omit(hojasTable)
  
  return(hojasTable)
}


ControlInfo <- function(cronicos, controlCol, by = NULL){

  byColumn <- switch( by,
                "Mes" = "form.mes",
		"Acompañante" = "form.nombre_acompanante",
		"Comunidad" = "community"
              )

  diseaseCol <- PickDisease(controlCol)

  patients <- filter(cronicos, cronicos[diseaseCol] == 1)

  controlInfo <- aggregate(patients[controlCol], patients[byColumn],
                           sum, na.rm = TRUE)
  
  totalPatients <- aggregate(patients[diseaseCol], patients[byColumn], length)
  
  controlTable <- merge(totalPatients, controlInfo, by = byColumn)
  
  controlTable <- setNames(controlTable, c(by, "Total_Pacientes", "Con_Control_Inf"))

  controlTable$Sin_Control_Inf <- controlTable$Total_Pacientes - controlTable$Con_Control_Inf

  controlTable$"% Con Control Inf" <- (controlTable$Con_Control_Inf/controlTable$Total_Pacientes*100) %>% round(digits = 2)

  controlTable$"% Sin Control Inf" <- (controlTable$Sin_Control_Inf/controlTable$Total_Pacientes*100) %>% round(digits = 2)
  
  controlTable <- na.omit(controlTable)
  
  return(controlTable)
}

GetMeasureFunction <- function(measureName){
  switch(measureName,
         "numberControl" = ControlledPatients,
         "numberVisits" = NumberVisits,
         "percentHojaVisita" = VisitSheets, 
         "percentControlInfo" = ControlInfo)
}


#################  X-axis Acompañante Graph ##########################################################

# Function for % hoja de visita llena presente (contains info in visits realized and planned columns)


#PercentHojaVisitaAcmp <- function(cronicosForCommunity, controlCol){
#  visits <- filter(cronicosForCommunity, cronicosForCommunity[controlCol] == 1)
#
#  colnames(visits)[1] <- "case_id"
#  
#  hojasVisita <- filter(visits, visits["form.numero_visita_acompanante"] >= 0)
#  hojasVisita <- filter(hojasVisita, hojasVisita["form.numero_visitas_debe_realizar"] >= 0)
#  
#  CountNonNA <- function(df) { length(!is.na(df)); }
#  
#  totalHojas <- aggregate(hojasVisita[controlCol], hojasVisita["form.nombre_acompanante"], CountNonNA)
#  
#  totalPatients <- aggregate(cronicosForCommunity[controlCol], cronicosForCommunity["form.nombre_acompanante"],
#                             sum, na.rm = TRUE)
#  
#  hojasVisita <- aggregate(hojasVisita["form.numero_visita_acompanante"], hojasVisita["form.nombre_acompanante"], 
#                           CountNonNA )
#  
#  hojasTable <- merge(totalPatients, totalHojas, by = "form.nombre_acompanante")
#  
#  hojasTable <- setNames(hojasTable, c("form.nombre_acompanante", "total_patients", "hojas_llenas"))
#  hojasTable <- mutate(hojasTable, percent_hojas = (hojas_llenas / total_patients * 100) %>% round(digits = 2) )
#  
#  hojasTable <- na.omit(hojasTable)
#  
#  return(hojasTable)
#}
#
#
#
## Function for % of Control Information Filled Out 
#
#PercentControlInfoAcmp <- function(cronicosForCommunity, controlCol){
#  diseaseCol <- PickDisease(controlCol)
#  patients <- filter(cronicosForCommunity, cronicosForCommunity[diseaseCol] == 1)
#
#  
#  CountNonNA <- function(df) { length(!is.na(df)); }
#  controlInfo <- aggregate(patients[controlCol], patients["form.nombre_acompanante"],
#                           sum, na.rm = TRUE)
#  
#  totalPatients <- aggregate(patients[diseaseCol], patients["form.nombre_acompanante"], CountNonNA)
#  
# controlTable <- merge(controlInfo, totalPatients, by = "form.nombre_acompanante")
#  
#  controlTable <- setNames(controlTable, c("form.nombre_acompanante", "control_info", "total_patients"))
#  controlTable <- mutate(controlTable, percent_control_info = (control_info / total_patients * 100) %>% round(digits = 2) )
#  
#  controlTable <- na.omit(controlTable)
#  
#  return(controlTable)
#}
#
#
#
## Function for % Control by Disease per ACMP 
#PercentControlAcmp <- function(cronicosForCommunity, controlCol){
#  diseaseControl <- aggregate(cronicosForCommunity[controlCol], cronicosForCommunity["form.nombre_acompanante"],
#                              sum, na.rm = TRUE)
#  
#  CountNonNA <- function(df) { length(!is.na(df)); }
#  diseaseTotal <- aggregate(cronicosForCommunity[controlCol], cronicosForCommunity["form.nombre_acompanante"], CountNonNA)
#  
#  diseaseForCommunity <- merge(diseaseTotal, diseaseControl, by = "form.nombre_acompanante")
#  
#  diseaseForCommunity <- setNames(diseaseForCommunity, c("form.nombre_acompanante", "Total", "Control"))
#  
#  diseaseForCommunity <- mutate(diseaseForCommunity, 
#                                control_percent = (Control / Total * 100) %>% round(digits = 2))
#  
#  diseaseForCommunity <- na.omit(diseaseForCommunity)
#  
#  return(diseaseForCommunity)
#}
#
#
##Function for # Control by Disease per Acmp
#
#NumberControlAcmp <- function(cronicosForCommunity, controlCol){
#  diseaseControl <- aggregate(cronicosForCommunity[controlCol], cronicosForCommunity["form.nombre_acompanante"],
#                              sum, na.rm = TRUE)
#  
#  diseaseControl <- setNames(diseaseControl, c("form.nombre_acompanante", "number_control"))
#  
#  diseaseControl <- na.omit(diseaseControl)
#  
#  return(diseaseControl)
#}
#
#
#
## Function for # Not in Control by Disease per Acmp
#NumberNotControlAcmp <- function(cronicosForCommunity, controlCol){
#  diseaseControl <- aggregate(cronicosForCommunity[controlCol], cronicosForCommunity["form.nombre_acompanante"],
#                              sum, na.rm = TRUE)
#  
#  CountNonNA <- function(df) { length(!is.na(df)); }
#  diseaseTotal <- aggregate(cronicosForCommunity[controlCol], cronicosForCommunity["form.nombre_acompanante"], CountNonNA)
#  
#  diseaseForCommunity <- merge(diseaseTotal, diseaseControl, by = "form.nombre_acompanante")
#  
#  diseaseForCommunity <- setNames(diseaseForCommunity, c("form.nombre_acompanante", "Total", "Control"))
#  
#  diseaseForCommunity <- mutate(diseaseForCommunity, 
#                                not_control = Total - Control)
#  
#  diseaseForCommunity <- na.omit(diseaseForCommunity)
#  
#  return(diseaseForCommunity)
#}
#
#
###### Number Visits Per Acmp ######################## 
#
#NumberVisitsAcmp <- function(cronicosForCommunity, controlCol){
#  visits <- filter(cronicosForCommunity, cronicosForCommunity[controlCol] == 1)
#  
#  visitsRealized <- aggregate(visits["form.numero_visita_acompanante"], visits["form.nombre_acompanante"], 
#                              sum, na.rm = TRUE)
#  
#  visitsPlanned <- aggregate(visits["form.numero_visitas_debe_realizar"], visits["form.nombre_acompanante"], 
#                             sum, na.rm = TRUE)
#  
#  visitsTable <- merge(visitsRealized, visitsPlanned, by = "form.nombre_acompanante")
#  
#  visitsTable <- setNames(visitsTable, c("form.nombre_acompanante", "realized", "planned"))
#  
#  visitsTable <- na.omit(visitsTable)
#  
#  return(visitsTable)
#}
#
#
#
## Select Column to Display Function for by Acmp
#
#GetMeasureFunctionPerAcmp <- function(measureName){
#  switch(measureName,
#         "percentControl" = PercentControlAcmp,
#         "numberControl" = NumberControlAcmp,
#         "numberNotControl" = NumberNotControlAcmp,
#         "numberVisits" = NumberVisitsAcmp,
#         "visitsPlanned" = NumberVisitsAcmp, 
#         "percentHojaVisita" = PercentHojaVisitaAcmp, 
#         "percentControlInfo" = PercentControlInfoAcmp)
#}
#
#GetMeasureColnamesPerAcmp <- function(measureName){
#  switch(measureName,
#         "percentControl" = c("Acompañante", "# Patients", "# Controlled", "% Controlled"),
#         "numberControl" = c("Acompañante", "# Patients", "# Controlled"),
#         "numberNotControl" = c("Acompañante", "# Patients", "# Controlled", "# Not Controlled"),
#         "numberVisits" = c("Acompañante", "Patient Visits", "Planned Visits"),
#         "visitsPlanned" = c("Acompañante", "Patient Visits", "Planned Visits"),
#         "percentHojaVisita" = c("Acompañante", "# Patients", "Visit Forms Filled", "% Visit Forms Filled"),
#         "percentControlInfo" = c("Acompañante", "With Control Info", "# Patients", "% With Control Info"))  
#}
#
#
## Select Column to Display Function for by Acmp
#
#
#SelectPlotColumnPerAcmp <- function(filteredData, colName){
#  switch(colName, 
#         "percentControl" = filteredData[["control_percent"]], 
#         "numberControl" = filteredData[["number_control"]], 
#         "numberNotControl"= filteredData[["not_control"]],
#         "numberVisits" = filteredData[["realized"]],
#         "visitsPlanned" = filteredData[["planned"]], 
#         "percentHojaVisita" = filteredData[["percent_hojas"]], 
#         "percentControlInfo" = filteredData[["percent_control_info"]])
#  
#}
#
#
#
################## FUNCTIONS FOR X-AXIS MONTH GRAPH ##################################
#
#
## Function for % hoja de visita llena presente (contains info in visits realized and planned columns)
#
#
#PercentHojaVisita <- function(cronicosForCommunity, controlCol){
#  visits <- filter(cronicosForCommunity, cronicosForCommunity[controlCol] == 1)
#  
#  colnames(visits)[1] <- "case_id"
#  
#  hojasVisita <- filter(visits, visits["form.numero_visita_acompanante"] >= 0)
#  hojasVisita <- filter(hojasVisita, hojasVisita["form.numero_visitas_debe_realizar"] >= 0)
#  
#  CountNonNA <- function(df) { length(!is.na(df)); }
#  
#  totalHojas <- aggregate(hojasVisita[controlCol], hojasVisita["form.mes"], CountNonNA)
#  
#  totalPatients <- aggregate(cronicosForCommunity[controlCol], cronicosForCommunity["form.mes"],
#                             sum, na.rm = TRUE)
#  
#  hojasVisita <- aggregate(hojasVisita["form.numero_visita_acompanante"], hojasVisita["form.mes"], 
#                           CountNonNA )
#  
#  hojasTable <- merge(totalPatients, totalHojas, by = "form.mes")
#  
#  hojasTable <- setNames(hojasTable, c("form.mes", "total_patients", "hojas_llenas"))
#  hojasTable <- mutate(hojasTable, percent_hojas = (hojas_llenas / total_patients * 100) %>% round(digits = 2))
#  
#  hojasTable <- na.omit(hojasTable)
#  
#  return(hojasTable)
#}
#
#
#
## Function for % of Control Information Filled Out 
#
#PercentControlInfo <- function(cronicosForCommunity, controlCol){
#  diseaseCol <- PickDisease(controlCol)
#  patients <- filter(cronicosForCommunity, cronicosForCommunity[diseaseCol] == 1)
#  
#  
#  CountNonNA <- function(df) { length(!is.na(df)); }
#  controlInfo <- aggregate(patients[controlCol], patients["form.mes"],
#                                          sum, na.rm = TRUE)
#  
#  totalPatients <- aggregate(patients[diseaseCol], patients["form.mes"], CountNonNA)
#  
#  controlTable <- merge(controlInfo, totalPatients, by = "form.mes")
#  
#  controlTable <- setNames(controlTable, c("form.mes", "control_info", "total_patients"))
#  controlTable <- mutate(controlTable, percent_control_info = (control_info / total_patients * 100) %>% round(digits = 2))
#  
#  controlTable <- na.omit(controlTable)
#  
#  return(controlTable)
#}
#
#
#
#
## Function for % Control by Disease per month
#PercentControl <- function(cronicosForCommunity, controlCol){
#  diseaseControl <- aggregate(cronicosForCommunity[controlCol], cronicosForCommunity["form.mes"],
#                              sum, na.rm = TRUE)
#  
#  CountNonNA <- function(df) { length(!is.na(df)); }
#  diseaseTotal <- aggregate(cronicosForCommunity[controlCol], cronicosForCommunity["form.mes"], CountNonNA)
#  
#  diseaseForCommunity <- merge(diseaseTotal, diseaseControl, by = "form.mes")
#  
#  diseaseForCommunity <- setNames(diseaseForCommunity, c("form.mes", "Total", "Control"))
#  
#  diseaseForCommunity <- mutate(diseaseForCommunity, 
#                                control_percent = (Control / Total * 100) %>% round(digits = 2))
#  
#  diseaseForCommunity <- na.omit(diseaseForCommunity)
#  
#  return(diseaseForCommunity)
#}
#
#
##Function for # Control by Disease per month 
#
#NumberControl <- function(cronicosForCommunity, controlCol){
#  diseaseControl <- aggregate(cronicosForCommunity[controlCol], cronicosForCommunity["form.mes"],
#                              sum, na.rm = TRUE)
#  
#  diseaseControl <- setNames(diseaseControl, c("form.mes", "number_control"))
#  
#  diseaseControl <- na.omit(diseaseControl)
# 
#  return(diseaseControl)
#}
#
#
#
## Function for # Not in Control by Disease per month
#NumberNotControl <- function(cronicosForCommunity, controlCol){
#  diseaseControl <- aggregate(cronicosForCommunity[controlCol], cronicosForCommunity["form.mes"],
#                              sum, na.rm = TRUE)
#  
#  CountNonNA <- function(df) { length(!is.na(df)); }
#  diseaseTotal <- aggregate(cronicosForCommunity[controlCol], cronicosForCommunity["form.mes"], CountNonNA)
#  
#  diseaseForCommunity <- merge(diseaseTotal, diseaseControl, by = "form.mes")
#  
#  diseaseForCommunity <- setNames(diseaseForCommunity, c("form.mes", "Total", "Control"))
#  
#  diseaseForCommunity <- mutate(diseaseForCommunity, 
#                                not_control = Total - Control)
#  
#  diseaseForCommunity <- na.omit(diseaseForCommunity)
#  
#  return(diseaseForCommunity)
#}
#
#
#NumberVisits <- function(cronicosForCommunity, controlCol){
# visits <- filter(cronicosForCommunity, cronicosForCommunity[controlCol] == 1)
# 
# visitsRealized <- aggregate(visits["form.numero_visita_acompanante"], visits["form.mes"], 
#                     sum, na.rm = TRUE)
# 
# visitsPlanned <- aggregate(visits["form.numero_visitas_debe_realizar"], visits["form.mes"], 
#                             sum, na.rm = TRUE)
# 
# visitsTable <- merge(visitsRealized, visitsPlanned, by = "form.mes")
# 
# visitsTable <- setNames(visitsTable, c("form.mes", "realized", "planned"))
#  
# visitsTable <- na.omit(visitsTable)
# 
#  return(visitsTable)
#}
#
## Get Measure Function
#
#GetMeasureFunction <- function(measureName){
#  switch(measureName,
#         "percentControl" = PercentControl,
#         "numberControl" = NumberControl,
#         "numberNotControl" = NumberNotControl,
#         "numberVisits" = NumberVisits, 
#         "visitsPlanned" = NumberVisits,
#         "percentHojaVisita" = PercentHojaVisita, 
#         "percentControlInfo" = PercentControlInfo)
#}
#
#GetMeasureColnames <- function(measureName){
#  switch(measureName,
#         "percentControl" = c("Mes", "# Patients", "# Controlled", "% Controlled"),
#         "numberControl" = c("Mes", "# Patients", "# Controlled"),
#         "numberNotControl" = c("Mes", "# Patients", "# Controlled", "# Not Controlled"),
#         "numberVisits" = c("Mes", "Patient Visits", "Planned Visits"),
#         "visitsPlanned" = c("Mes", "Patient Visits", "Planned Visits"),
#         "percentHojaVisita" = c("Mes", "# Patients", "Visit Forms Filled", "% Visit Forms Filled"),
#         "percentControlInfo" = c("Mes", "With Control Info", "# Patients", "% With Control Info"))
#}
#
#
## Select Column to Display Function 
#
#SelectPlotColumn <- function(filteredData, colName){
# switch(colName, 
#        "percentControl" = filteredData[["control_percent"]], 
#        "numberControl" = filteredData[["number_control"]], 
#        "numberNotControl"= filteredData[["not_control"]],
#        "numberVisits" = filteredData[["realized"]], 
#        "visitsPlanned" = filteredData[["planned"]],
#        "percentHojaVisita" = filteredData[["percent_hojas"]], 
#        "percentControlInfo" = filteredData[["percent_control_info"]])
#  
#}
#
#
#
################## FUNCTIONS FOR X-AXIS COMMUNITY GRAPH ##################################
#
#
## Function for % hoja de visita llena presente (contains info in visits realized and planned columns) per Community 
#
#
#PercentHojaVisitaMonth <- function(cronicosForCommunity, controlCol){
#  visits <- filter(cronicosForCommunity, cronicosForCommunity[controlCol] == 1)
#  
#  colnames(visits)[1] <- "case_id"
#  
#  hojasVisita <- filter(visits, visits["form.numero_visita_acompanante"] >= 0)
#  hojasVisita <- filter(hojasVisita, hojasVisita["form.numero_visitas_debe_realizar"] >= 0)
#  
#  CountNonNA <- function(df) { length(!is.na(df)); }
#  
#  totalHojas <- aggregate(hojasVisita[controlCol], hojasVisita["community"], CountNonNA)
#  
#  totalPatients <- aggregate(cronicosForCommunity[controlCol], cronicosForCommunity["community"],
#                             sum, na.rm = TRUE)
#  
#  hojasVisita <- aggregate(hojasVisita["form.numero_visita_acompanante"], hojasVisita["community"], 
#                           CountNonNA )
#  
#  hojasTable <- merge(totalPatients, totalHojas, by = "community")
#  
#  hojasTable <- setNames(hojasTable, c("community", "total_patients", "hojas_llenas"))
#  hojasTable <- mutate(hojasTable, percent_hojas = (hojas_llenas / total_patients * 100) %>% round(digits = 2))
#  
#  hojasTable <- na.omit(hojasTable)
#  
#  return(hojasTable)
#}
#
#
#
## Function for % of Control Information Filled Out per Community 
#
#PercentControlInfoMonth <- function(cronicosForCommunity, controlCol){
#  diseaseCol <- PickDisease(controlCol)
#  patients <- filter(cronicosForCommunity, cronicosForCommunity[diseaseCol] == 1)
#  
#  
#  CountNonNA <- function(df) { length(!is.na(df)); }
#  controlInfo <- aggregate(patients[controlCol], patients["community"],
#                           sum, na.rm = TRUE)
#  
#  totalPatients <- aggregate(patients[diseaseCol], patients["community"], CountNonNA)
#  
#  controlTable <- merge(controlInfo, totalPatients, by = "community")
#  
#  controlTable <- setNames(controlTable, c("community", "control_info", "total_patients"))
#  controlTable <- mutate(controlTable, percent_control_info = (control_info / total_patients * 100) %>% round(digits = 2))
#  
#  controlTable <- na.omit(controlTable)
#  
#  return(controlTable)
#}
#
## Function for % Control Disease by Community 
#
#PercentControlMonth <- function(cronicosForMonth, controlCol){
#  monthControl <- aggregate(cronicosForMonth[controlCol], cronicosForMonth["community"],
#                            sum, na.rm = TRUE)
#  
#  CountNonNA <- function(df) { length(!is.na(df)); }
#  monthTotal <- aggregate(cronicosForMonth[controlCol], cronicosForMonth["community"],
#                          CountNonNA)
#  
#  diseaseForMonth <- merge(monthTotal, monthControl, by ="community")
#  
#  diseaseForMonth <- setNames(diseaseForMonth, c("community", "Total", "Control"))
#  
#  diseaseForMonth <- mutate(diseaseForMonth, control_percent = (Control / Total *  100) %>% round(digits = 2))
#  
#  diseaseForMonth <- na.omit(diseaseForMonth)
#  
#  return(diseaseForMonth)
#  
#}
#
#
## Function for # Control Disease by Community
#
#NumberControlMonth <- function(cronicosForMonth, controlCol){
#  monthControl <- aggregate(cronicosForMonth[controlCol], cronicosForMonth["community"],
#                            sum, na.rm = TRUE)
#  
#  monthControl <- setNames(monthControl, c("community", "number_control"))
#  
#  monthControl <- na.omit(monthControl)
#  
#  return(monthControl)
#  
#}
#
#
## Function for # Not in Control Disease by Community 
#
#NumberNotControlMonth <- function(cronicosForMonth, controlCol){
#  monthControl <- aggregate(cronicosForMonth[controlCol], cronicosForMonth["community"],
#                            sum, na.rm = TRUE)
#  
#  CountNonNA <- function(df) { length(!is.na(df)); }
#  monthTotal <- aggregate(cronicosForMonth[controlCol], cronicosForMonth["community"],
#                          CountNonNA)
#  
#  diseaseForMonth <- merge(monthTotal, monthControl, by ="community")
#  
#  diseaseForMonth <- setNames(diseaseForMonth, c("community", "Total", "Control"))
#  
#  diseaseForMonth <- mutate(diseaseForMonth, not_control = Total - Control)
#  
#  diseaseForMonth <- na.omit(diseaseForMonth)
#  
#  return(diseaseForMonth)
#  
#}
#
#
## Visits Planned and Realized by Community 
#
#
#NumberVisitsMonth <- function(cronicosForMonth, controlCol){
#  visits <- filter(cronicosForMonth, cronicosForMonth[controlCol] == 1)
#  
#  visitsRealized <- aggregate(visits["form.numero_visita_acompanante"], visits["community"], 
#                              sum, na.rm = TRUE)
#  
#  visitsPlanned <- aggregate(visits["form.numero_visitas_debe_realizar"], visits["community"], 
#                             sum, na.rm = TRUE)
#  
#  visitsTable <- merge(visitsRealized, visitsPlanned, by = "community")
#  
#  visitsTable <- setNames(visitsTable, c("community", "realized", "planned"))
#  
#  visitsTable <- na.omit(visitsTable)
#  
#  return(visitsTable)
#}
#
#
#
## Get Measure Function by Community 
#
#GetMeasureFunctionMonth <- function(measureName){
#  switch(measureName,
#         "percentControl" = PercentControlMonth,
#         "numberControl" = NumberControlMonth,
#         "numberNotControl" = NumberNotControlMonth,
#         "numberVisits" = NumberVisitsMonth,
#         "visitsPlanned" = NumberVisitsMonth,
#         "percentHojaVisita" = PercentHojaVisitaMonth, 
#         "percentControlInfo" = PercentControlInfoMonth)
#}
#
#GetMeasureColnamesMonth <- function(measureName){
#  switch(measureName,
#         "percentControl" = c("Comunidad", "# Patients", "# Controlled", "% Controlled"),
#         "numberControl" = c("Comunidad", "# Patients", "# Controlled"),
#         "numberNotControl" = c("Comunidad", "# Patients", "# Controlled", "# Not Controlled"),
#         "numberVisits" = c("Comunidad", "Patient Visits", "Planned Visits"),
#         "visitsPlanned" = c("Comunidad", "Patient Visits", "Planned Visits"),
#         "percentHojaVisita" = c("Comunidad", "# Patients", "Visit Forms Filled", "% Visit Forms Filled"),
#         "percentControlInfo" = c("Comunidad", "With Control Info", "# Patients", "% With Control Info"))
#}
#
#
## Select Column to Display Function for by Community
#
#SelectPlotColumnMonth <- function(filteredData, colName){
#  switch(colName, 
#         "percentControl" = filteredData[["control_percent"]], 
#         "numberControl" = filteredData[["number_control"]], 
#         "numberNotControl"= filteredData[["not_control"]],
#         "numberVisits" = filteredData[["reazlied"]],
#         "numberVisits" = filteredData[["planned"]],
#         "percentHojaVisita" = filteredData[["percent_hojas"]], 
#         "percentControlInfo" = filteredData[["percent_control_info"]])
#  
#}


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
 
  acmpsPtMonths$Community[which(acmpsPtMonths$Community == "Salvador_Urbina")] = "Salvador"
  
  colnames(acmpsPtMonths)[2] <- "community"

  return(acmpsPtMonths)
  
  
}


# Create Clean ACMPs Table Function

CreateAcmpsTable <- function(acmpsPtMonths) {
  acmpsTable<- data.frame(acmpsPtMonths[, c(1, 3, 4:6, 8:10, 12, 17, 21, 23, 24)])
  return(acmpsTable)
}


# Function to Create Filter by Community Table  

FilterByCommunityAcmps <- function(acmpsTable, filtercommunity) {
  acmpsForCommunity <- acmpsTable %>% filter(acmpsTable$Community == filtercommunity)
  return(acmpsForCommunity)
}


# Function for % Patients with satisfaction > 85% per Month 

PatientSatisfaction <- function(acmpsDataTable, by = NULL){

  byColumn <- switch( by, 
                "Mes" = "form.mes",
		"Acompañante" = "form.nombre_acompanante",
		"Comunidad" = "community"
              )

  patientSatisfaction <- aggregate(acmpsDataTable["form.porcentaje_satisf_pt"] >= 85, acmpsDataTable[byColumn],
                              sum, na.rm = TRUE)
  
  patientsTotal <- aggregate(acmpsDataTable["form.porcentaje_satisf_pt"], acmpsDataTable[byColumn], length)
  
  satisfaction <- merge(patientsTotal, patientSatisfaction, by = byColumn)
  
  satisfaction <- setNames(satisfaction, c(by, "Total_Pacientes", "Satis >= 85"))
 
  satisfaction$"Satis < 85" <- satisfaction$Total_Pacientes - satisfaction$"Satis >= 85"

  satisfaction$"% Satis >= 85" <- (satisfaction$"Satis >= 85"/satisfaction$Total_Pacientes*100) %>% round(digits = 2)

  satisfaction$"% Satis < 85" <- (satisfaction$"Satis < 85"/satisfaction$Total_Pacientes*100) %>% round(digits = 2)
  
  satisfaction <- na.omit(satisfaction)
  
  return(satisfaction)
}


# Average Patient Satisfaction 

AveragePatientSatisfaction <- function(acmpsDataTable, by = NULL){

  byColumn <- switch( by, 
                "Mes" = "form.mes",
		"Acompañante" = "form.nombre_acompanante",
		"Comunidad" = "community"
              )

  patientSatisfaction <- aggregate(acmpsDataTable["form.porcentaje_satisf_pt"], acmpsDataTable[byColumn],
                                   mean, na.rm = TRUE)
  
  patientSatisfaction <- setNames(patientSatisfaction, c(by, "Promedio Satisfacción"))
  
  patientSatisfaction <- na.omit(patientSatisfaction)

  patientSatisfaction <- patientSatisfaction %>% round(digits = 2)
  
  return(patientSatisfaction)
}

DistributionPatientSatisfaction <- function(acmpsDataTable, by = NULL){

  byColumn <- switch( by,
                "Mes" = "form.mes",
                "Acompañante" = "form.nombre_acompanante",
                "Comunidad" = "community"
              )

  patientSatisfaction <- acmpsDataTable[ , c(byColumn, "form.porcentaje_satisf_pt")]

  patientSatisfaction <- setNames(patientSatisfaction, c(by, "Satisfacción de Paciente"))

  patientSatisfaction <- na.omit(patientSatisfaction)

  patientSatisfaction <- patientSatisfaction %>% round(digits = 2)

  return(patientSatisfaction)
}


# ACMPS Percent Attendance to Platicas

PercentAttendance <- function(acmpsDataTable, by = NULL){
  
  byColumn <- switch( by, 
                "Mes" = "form.mes",
                "Acompañante" = "form.nombre_acompanante",
                "Comunidad" = "community"
              )

  sumAttendance <- aggregate(acmpsDataTable["form.asistencia"], acmpsDataTable[byColumn],
                                 sum, na.rm = TRUE)
  
  totalAcmps <- aggregate(acmpsDataTable["form.asistencia"], acmpsDataTable[byColumn], length)
  
  attendance <- merge(totalAcmps, sumAttendance, by = byColumn)
  attendance <- setNames(attendance, c(by, "Total", "Asistencias"))

  attendance$Faltas <- attendance$Total - attendance$Asistencias

  attendance$"% Asistencia" <- (attendance$Asistencias/attendance$Total*100) %>% round(digits = 2)

  attendance$"% Faltas" <- (attendance$Faltas/attendance$Total*100) %>% round(digits = 2)
  
  attendance <- na.omit(attendance)
  
  return(attendance)
  
}


# Percent Mentoria de ACMPS 

PercentMentoria <- function(acmpsDataTable, by = NULL){

  byColumn <- switch( by,
                "Mes" = "form.mes",
                "Acompañante" = "form.nombre_acompanante",
                "Comunidad" = "community"
              )

  mentoriaCronicos <- aggregate(acmpsDataTable["form.calificacion_cronicos2"] >= 80, acmpsDataTable[byColumn],
                                sum, na.rm = TRUE)
  mentoriaSaludMaterna <- aggregate(acmpsDataTable["form.calificacion_embarazo2"] >= 80, acmpsDataTable[byColumn],
                                    sum, na.rm = TRUE)
  
  CountNonNA <- function(df) { length(!is.na(df)); }                                 
  totalCronicos <-  aggregate(acmpsDataTable["form.calificacion_cronicos2"], acmpsDataTable[byColumn], length)
  
  totalSaludMaterna <- aggregate(acmpsDataTable["form.calificacion_embarazo2"], acmpsDataTable[byColumn], length)
  
  mentoria <- merge(mentoriaCronicos, mentoriaSaludMaterna, by = byColumn)
  
  mentoria <- merge(mentoria, totalCronicos, by = byColumn)
  mentoria <- merge(mentoria, totalSaludMaterna, by = byColumn)
  
  mentoria <- setNames(mentoria, c(by, "calificacion_cronicos", "calificacion_embarazo", "total_cronicos", "total_embarazo"))
                            
  
  mentoria <- mutate(mentoria, greater80_mentoria  = ((Score_Chronic + Score_Pregnancy) / (Total_Chronic + Total_Pregnancies) * 100) %>%
                                                      round(digits = 2))
  
  mentoria <- na.omit(mentoria)
  
  return(mentoria)
                        
}


# Average Mentoria de ACMPS 

# TODO: Asistencia, satis facil, mentoria, 

# Get Measure Function for ACMPS

GetMeasureFunctionAcmps <- function(measureName){
  switch(measureName,
         "percentPatientSatisfaction" = PatientSatisfaction, 
         "averagePatientSatisfaction" = AveragePatientSatisfaction,
         "percentAttendance" = PercentAttendance, 
         "percentMentoria" = PercentMentoria)
}

GetMeasureColnamesAcmps <- function(measureName){
  switch(measureName,
         "percentPatientSatisfaction" = c("Month", "Patients", "# Satisfaction >= 85%","% Satisfaction >= 85%"),
         "averagePatientSatisfaction" = c("Month", "Average Satisfaction"),
         "percentAttendance" = c("Month", "Attended Talks", "Total Talks","% Attendance"),
         "percentMentoria" = c("Month", "Score_Chronic", "Score_Pregnancy", "Total_Chronic", "Total_Pregnancies", "Mentorship >= %80"))
}


# Select Column to Display Function for ACMPS 

SelectPlotColumnAcmps <- function(filteredData, colName){
  switch(colName, 
         "percentPatientSatisfaction" = filteredData[["greater85_percent"]], 
         "averagePatientSatisfaction" = filteredData[["average_satisfaction"]], 
         "percentAttendance"= filteredData[["percent_attendance"]],
         "percentMentoria" = filteredData[["greater80_mentoria"]])

}


############################## Graph Functions

ByCategoryStackedBarplot <- function(data, addPercentLabel = TRUE, graphTitle = NULL){
  # data: data.frame where 1st Col: Category (i.e. Month, Community, or Acompañante)
  #                            2nd Col: Total counts (e.g. total # of patients, etc) (Redundant but used to get meaningful axis names)
  #                            3rd Col: Counts in condition 1 (e.g. # controlled patients, etc).
  #                            4th Col: Counts in condition 2 (e.g. # not controlled patietns, etc).
  #                            nth Col: Counts in condition (n-2) ... 
  # data should have meaningful colnames which will be displayed on graph.
  ndataCols <- ncol(data)-2
  totalCounts <- rep(data[,2], each = ndataCols)
  counts <- t( data[,3:ncol(data)] ) %>% as.vector
  categories <- rep(data[,1], each = ndataCols)
  successCat <- rep(colnames(data)[3:ncol(data)], times = nrow(data))
  plotData <- data.frame(Category = categories, SubCategory = successCat, Counts = counts, 
                         Percentage = (counts/totalCounts*100) %>% round(digits = 1), stringsAsFactors = FALSE)
  plotData <- plotData  %>% filter( ! plotData$Percentage %in% c(0,NaN)) 
  percentageLabels <- tapply(1:nrow(plotData), plotData$Category, 
                             function(x){
			       graphLabelPos <- c(0, cumsum(plotData$Counts[x]))
			       graphLabelPos <- (graphLabelPos[-1] + graphLabelPos[-length(graphLabelPos)])/2
 			       return(data.frame(graphLabelPos = graphLabelPos, stringsAsFactors = FALSE))
  			     }, simplify = FALSE)			    
  percentageLabels <- do.call(rbind, percentageLabels)
  plotData <- cbind(plotData, percentageLabels)
  plotData <- mutate(plotData, Percentage = Percentage %>% paste("%", sep = "")) 
  bplot <- ggplot(data = plotData, aes(fill = SubCategory, x = Category, y = Counts)) +
           xlab(colnames(data)[1]) + ylab(colnames(data)[2]) + labs(title = graphTitle) +
           geom_bar(stat = "identity")
  if(addPercentLabel){
    bplot + geom_label(label.size = 0.1, aes(x = Category, y = graphLabelPos, label = Percentage), show.legend = FALSE)+
    theme(legend.title= element_blank())
  }
}

ByCategoryBoxplot <- function(data, graphTitle = NULL){
  # data: data.frame where 1st Col: Category (i.e. Month, Community, or Acompañante)
  #                        2nd Col: Values to boxplot (e.g. patient satisfaction, etc)
  plotData <- data
  colnames(plotData) <- c("Category", "Values")
  ggplot(data = plotData, aes(group = Category, x = Category, y = Values)) + geom_boxplot() +
            xlab(colnames(data)[1]) + ylab(colnames(data)[2])
}
