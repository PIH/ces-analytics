source("data_prep.R")

# read_excel("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx")
# read_excel("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx")
#
# CR1 <- read_excel("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", sheet = 7)
# CR2 <- read_excel("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", sheet = 8)
# Casos_Cronicos <- read_excel("~/Documents/R Project/Cases_cronicos.xls")
#
# chronicPtMonths1 <- merge(CR1, CR2, by = c("form.case.@case_id", "form.mes", "form.ano"), all = TRUE)
# chronicPtMonths1 <- merge(Casos_Cronicos, chronicPtMonths, by.x = "ï»¿caseid", by.y = "form.case.@case_id", all = TRUE)

ProcessData("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases_cronicos.xls")

chronicPtMonths[6] <- lapply(chronicPtMonths[6], as.numeric)
chronicPtMonths[10:13] <- lapply(chronicPtMonths[10:13], as.numeric)
chronicPtMonths[15] <- lapply(chronicPtMonths[15], as.numeric)
chronicPtMonths[16] <- lapply(chronicPtMonths[16], as.numeric)
chronicPtMonths[19:26] <- lapply(chronicPtMonths[19:26], as.numeric)
chronicPtMonths[17] <- lapply(chronicPtMonths[17], as.numeric)


chronicTable <- data.frame(chronicPtMonths[, c(1, 15, 16, 17, 7, 8, 10:13, 19:26)])

CreateCommunityMonthDf(chronicTable, "Matazano")


chronics <- ProcessData("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases_cronicos.xls")
filteredData <- CreateCommunityMonthDf(chronics, "Matasano")
plotData <- CreateDmControlDf(filteredData)
ggplot(plotData, aes(form.mes, DMcontrol_percent)) +
  geom_bar(stat = "identity")



# how to build a grouped bar table for visits realized and planned

library(reshape2)
visitsRealized <- cronicosForCommunity %>%
  group_by(form.mes) %>%
  summarize(realized = sum(form.numero_visita_acompanante, na.rm = TRUE))

visitsPlanned <- cronicosForCommunity %>%
  group_by(form.mes) %>%
  summarize(planned = sum(form.numero_visitas_debe_realizar, na.rm = TRUE))

p.visitsForCommunity <- merge(visitsPlanned, visitsRealized, by = "form.mes")

ggplot(melt.visitsForCommunity, aes(form.mes, value)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = variable)) +
  ylim(0, 500) + geom_text(aes(label = melt.visitsForCommunity$value), vjust = 1)



# create filter by month and plot control DM for June

chronics <- ProcessData("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases_cronicos.xls")
filteredData <- CreateCommunityMonthDf(chronics, "Capitan")
plotData <- CreateControlDf(filteredData, filteredData$form.control_dep)
ggplot(plotData, aes(community, DMcontrol_percent)) +
  geom_bar(stat = "identity")




filteredData <- CreateCommunityMonthDf(chronics, "Capitan")
diseaseTotal <- cronicosForCommunity %>%
  group_by(form.mes) %>%
  summarize(n_disease = sum(
    length(which(form.control_dep == "1")),
    length(which(form.control_dep == "0"))
  ))

chronics <- ProcessData("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases_cronicos.xls")
filteredData <- CreateCommunityMonthDf(chronics, "Reforma")
col <- "form.control_diabetes"
diseaseControl <- aggregate(filteredData[col], filteredData["form.mes"], sum, na.rm = TRUE)

CountNonNA <- function(df) {
  length(!is.na(df))
}
diseaseTotal <- aggregate(filteredData[col], filteredData["form.mes"], CountNonNA)


diseaseForCommunity <- merge(diseaseTotal, diseaseControl, by = "form.mes")

diseaseForCommunity <- setNames(diseaseForCommunity, c("form.mes", "Total", "Control"))

diseaseForCommunity <- mutate(diseaseForCommunity,
  control_percent = Control / Total * 100
)

diseaseForCommunity <- na.omit(diseaseForCommunity)



chronics2 <- ProcessData("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases_cronicos.xls")
filteredData2 <- CreateCommunityMonthDf(chronics, "Letrero")
plotData2 <- CreateControlDf(filteredData, "form.control_diabetes")


CreateMonthCommunityDf <- function(chronicTable, filtermonth) {
  cronicosForMonth <- chronicTable %>% filter(chronicTable$form.mes == filtermonth)
  return(cronicosForMonth)
}


chronics1 <- ProcessData("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases_cronicos.xls")
filteredData <- CreateMonthCommunityDf(chronics1, 6)
plotData <- CreateControlMonthDf(filteredData, "form.control_diabetes")


chronics1 <- ProcessData("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases_cronicos.xls")
filteredData <- CreateMonthCommunityDf(chronics1, 6)
controlCol <- "form.control_diabetes"
monthControl <- aggregate(filteredData[controlCol], filteredData["community"],
  sum,
  na.rm = TRUE
)

CountNonNA <- function(df) { length(!is.na(df)) }
monthTotal <- aggregate(
  filteredData[controlCol], filteredData["community"],
  CountNonNA
)

diseaseForMonth <- merge(monthTotal, monthControl, by = "community")

diseaseForMonth <- setNames(diseaseForMonth, c("community", "Total", "Control"))

diseaseForMonth <- mutate(diseaseForMonth, control_percent = Control / Total * 100)

diseaseForMonth <- na.omit(diseaseForMonth)

return(diseaseForMonth)


GetMeasureFunction("percentControl")




chronics4 <- ProcessData("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases_cronicos.xls")
filteredData4 <- FilterByCommunity(chronics4, "Capitan")
MeasureFunction <- GetMeasureFunction("numberNotControl")
plotData5 <- MeasureFunction(filteredData4, "form.control_diabetes")
plotColumn <- SelectPlotColumn(plotData5, "numberNotControl")

ggplot(plotData5, aes(form.mes, plotColumn)) +
  geom_bar(stat = "identity", col = "deepskyblue", fill = "deepskyblue")

NumberControl(plotData5, "form.control_diabetes")



DataACMPS <- ProcessDataAcmps("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases ACMPS.xls")
ACMPS <- read_excel("~/Documents/R Project/Cases ACMPS.xls")

ACMP1 <- read_excel("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", sheet = 1)
ACMP2 <- read_excel("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", sheet = 2)
acmpsPtMonths <- merge(ACMP1, ACMP2, by = c("form.case.@case_id", "form.mes", "form.ano"), all = TRUE)


aggregate(DataACMPS["form.porcentaje_satisf_pt"] >= 85, DataACMPS["form.mes"],
          sum, na.rm = TRUE)




patientSatisfaction <- aggregate(DataACMPS["form.porcentaje_satisf_pt"] >= 85, DataACMPS["form.mes"],
                                 sum, na.rm = TRUE)

CountNonNA <- function(df) { length(!is.na(df)); }
patientsTotal <- aggregate(DataACMPS["form.porcentaje_satisf_pt"], DataACMPS["form.mes"], CountNonNA)

satisfactionPerMonth <- merge(patientsTotal, patientSatisfaction, by = "form.mes")

satisfactionPerMonth <- setNames(satisfactionPerMonth, c("form.mes", "Total", "greater_85"))

satisfactionPerMonth <- mutate(satisfactionPerMonth, 
                               greater85_percent = Satisfaction >= greater_85 / Total * 100)

satisfactionPerMonth <- na.omit(satisfactionPerMonth)

PercentPatientSatisfaction(DataACMPS)



acmps <- ProcessDataAcmps("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases ACMPS.xls")
acmps <- setNames(acmps["Community"], "community")
filteredData <- FilterByCommunity(acmps, "Capitan")



plotData <- PercentPatientSatisfaction(filteredData)
ggplot(plotData, aes(form.mes, greater85_percent)) +
  geom_bar(stat = "identity", col = "deepskyblue", fill = "deepskyblue") +
  ggtitle(paste("Percent Satisfaction >= 85% for", input$selectCommunity)) +
  labs(x = "Month", y = "Percent Satisfaction >= 85%") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label= greater85_percent), vjust=0)



read_excel("~/Documents/R Project/Cases ACMPS.xls")

  acmps <- ProcessDataAcmps("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases ACMPS.xls")
    filteredData <- FilterByCommunityAcmps(acmps, "Capitan")
    plotData <- PercentPatientSatisfaction(filteredData)
    ggplot(plotData, aes(form.mes, greater85_percent)) +
      geom_bar(stat = "identity", col = "deepskyblue", fill = "deepskyblue") 

acmps <- ProcessDataAcmps("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases ACMPS.xls")   
     
     # View(acmpsPtMonths)
      acmpsPtMonths <- FixDatatypesACMPS(acmps)
      acmpsPtMonths <- CreateAcmpsTable(acmps)
      
    

        acmps <- ProcessDataAcmps("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases ACMPS.xls")   
        acmps <- FixDatatypesACMPS(acmps)
        acmps  <- CreateAcmpsTable(acmps)
        filteredData <- FilterByCommunityAcmps(acmps, input$selectCommunity)
        plotData <- PercentPatientSatisfaction(filteredData)
        ggplot(plotData, aes(form.mes, greater85_percent))    

        acmps <- ProcessDataAcmps("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases ACMPS.xls")   
        PercentAttendance(acmps)
        

        
        acmpsPtMonths <- ProcessDataAcmps("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases ACMPS.xls") 
        
          acmpsPtMonths[8:12] <- lapply(acmpsPtMonths[8:12], as.numeric)
          acmpsPtMonths[17] <- lapply(acmpsPtMonths[17], as.numeric)
          acmpsPtMonths[21] <- lapply(acmpsPtMonths[21], as.numeric)
          acmpsPtMonths[23] <- lapply(acmpsPtMonths[23], as.numeric)
          acmpsPtMonths[24] <- lapply(acmpsPtMonths[24], as.numeric)
          
          acmpsPtMonths$Community[which(acmpsPtMonths$Community == "Salvador_Urbina")] = "Salvador"
          
           colnames(acmpsPtMonths)[2] <- "community"
          
          return(acmpsPtMonths)
           
           
           acmpsPtMonths <- ProcessDataAcmps("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases ACMPS.xls")   
          filteredData <- FilterByCommunityAcmps(acmps, "Capitan")
          MeasureFunction <- GetMeasureFunctionAcmps("percentPatientSatisfaction")
          plotData <- MeasureFunction(filteredData)
          plotColumn <- SelectPlotColumnAcmps(plotData, "percentPatientSatisfaction")
          ggplot(plotData, aes(form.mes, plotColumn)) +
            geom_bar(stat = "identity", col = "deepskyblue", fill = "deepskyblue") 
          
          
          
          
          
          cronicosForCommunity <- ProcessData("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases_cronicos.xls")      
   visitsRealized <- cronicosForCommunity %>%
              group_by(form.mes) %>%
              summarize(realized = sum(form.numero_visita_acompanante, na.rm = TRUE))
            
            visitsPlanned <- cronicosForCommunity %>%
              group_by(form.mes) %>%
              summarize(planned = sum(form.numero_visitas_debe_realizar, na.rm = TRUE))
            
            visitsForCommunity <- merge(visitsPlanned, visitsRealized, by = "form.mes")
            
            visitsForCommunity <- na.omit(visitsForCommunity)
            
            melt.visitsForCommunity <- melt(visitsForCommunity, id.vars='form.mes')
           
            
            cronicosForCommunity <- ProcessData("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases_cronicos.xls")       
            visits <- filter(cronicosForCommunity, cronicosForCommunity[controlCol] == 1)
            
            visitsRealized <- aggregate(visits["form.numero_visita_acompanante"], visits["form.nombre_acompanante"], 
                                        sum, na.rm = TRUE)
            
            visitsPlanned <- aggregate(visits["form.numero_visitas_debe_realizar"], visits["form.nombre_acompanante"], 
                                       sum, na.rm = TRUE)
            
            visitsTable <- merge(visitsRealized, visitsPlanned, by = "form.nombre_acompanante")
            
            visitsTable <- setNames(visitsTable, c("form.nombre_acompanante", "realized", "planned"))
            
            visitsTable <- na.omit(visitsTable)
            
            return(visitsTable)
            
            
            chronics <- ProcessData("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases_cronicos.xls")       
              filteredData <- FilterByCommunity(chronics, "Capitan")
              MeasureFunction <- GetMeasureFunctionPerAcmp("numberVisits")
              plotData <- MeasureFunction(filteredData,"form.control_diabetes")
              plotColumn <- SelectPlotColumnPerAcmp(plotData, "percent_control")
              ggplot(plotData, aes(form.nombre_acompanante, plotColumn)) +
                geom_bar(stat = "identity", col = "tomato", fill = "tomato") + 
                labs(x = "Acompanante", y = "Percent Control") +
                theme(plot.title = element_text(hjust = 0.5)) + 
                geom_text(aes(label= plotColumn), vjust=0)
          

              cronicosForCommunity <- ProcessData("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases_cronicos.xls")   
              diseaseCol <- PickDisease(controlCol)
                patients <- filter(cronicosForCommunity, cronicosForCommunity[diseaseCol]== 1)
                totalPatients <- aggregate(patients["does_the_patient_have_diabetes"], patients["form.nombre_acompanante"], CountNonNA)
                controlInfo <- aggregate(patients["form.control_diabetes"], patients["form.nombre_acompanante"],
                                            sum, na.rm = TRUE)
                
                controlTable <- merge(controlInfo, totalPatients, by = "form.nombre_acompanante")
                
                controlTable <- setNames(controlTable, c("form.nombre_acompanante", "control_info", "total_patients"))
                controlTable <- mutate(controlTable, percent_control_info = control_info / total_patients * 100 )
                
                controlTable <- na.omit(controlTable)
                

         
      dateRangeInput("daterange", "Date Range:", start = min("df$date"), end = max(df$date), format = "yyyy-m")        
          
      
      textOutput("startdate") 
      textOutput("enddate")
      
 output$startdate <- renderText({
   as.character(input$daterange[1])
 })     
          
 output$enddate <- renderText({
   as.character(input$daterange[2])
 })

output$subdata <- renderTable({
  s = subset(mydf, mydf$Date>= input$daterange[1] & mydf$Date <= input$daterange[2])
  table(s$Status)
  
})
                
  
daterange <- seq(Sys.Date)      
      mydate = seq(Sys.Date(), by = "day", length.out = 30)

      
      cronicosForCommunity <- ProcessData("~/Documents/R Project/ACMPS App/data/ACMPS_2018.xlsx", "~/Documents/R Project/Cases_cronicos.xls")   
      cronicosForCommunity <- FilterByDate(cronicosForCommunity, as.Date("2018-04-01"), as.Date("2018-12-01"))
      
      
      
      df <- data.frame( Month = sample(1:12 , 10 , repl = TRUE ) , Day = sample(1:30 , 10 , repl = TRUE ) )
      df$Date <- as.Date( paste( df$Month , df$Day , sep = "." )  , format = "%m.%d" )
      
                