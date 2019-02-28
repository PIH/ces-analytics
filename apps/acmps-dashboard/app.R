#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(XLConnect)
library(readxl)
library(reshape2)

source("data_prep.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Control de Enfermedades"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", "Date Range:", 
                     start = "2017-01-01", format = "yyyy-mm-dd"),  
      fileInput("formDataPath", "Form Data"),
      fileInput("cronicosPath", "Chronic Cases"),
      fileInput("acmpsCasesPath", "Acmps Cases"),
      verbatimTextOutput("summary"),
      selectInput("selectCommunity", "Community", 
        choices = c("Capitan", "Honduras", "Laguna" = "Laguna_del_Cofre", 
                    "Letrero", "Matasano", "Monterrey", "Plan Alta" = "Plan_Alta", 
                    "Plan Baja" = "Plan_Baja", "Reforma", "Salvador", "Soledad" )
      ),
      selectInput("selectMonth", "Month", 
                  choices = c("Jan" = 1, "Feb" = 2, "March" =3, "April"= 4, "May" = 5, 
                              "June" = 6, "July" = 7, "Aug" = 8, "Sept" = 9, "Oct" = 10,
                              "Nov" = 11, "Dec" = 12) 
      ), 
      selectInput("selectDisease", "Disease", 
                  choices = c("Diabetes" = "form.control_diabetes", 
                              "Hypertension" = "form.control_htn", 
                              "Depression" = "form.control_dep")
                  
      ),

      selectInput("selectMeasureAcmps", "Acmps Measure",
                  choices = c("Percent Patients with >= 85% Satisfaction" = "percentPatientSatisfaction",
                              "Average Patient Satisfaction" = "averagePatientSatisfaction",
                              "Percent Asistencia Acmps" = "percentAttendance",
                              "Percent Acmps with >= 80% Mentoria" = "percentMentoria",
                              "Average Mentoria" = "averageMentoria")
    ),
      selectInput("selectMeasure", "Measure", 
                  choices = c("Percent Control" = "percentControl", 
                              "Number Control" = "numberControl",
                              "Number Not In Control" = "numberNotControl",
                              "Number Patient Visits" = "numberVisits", 
                              "Number Patient Visits Planned" = "visitsPlanned", 
                              "Percent Hojas Visitas Llenadas" = "percentHojaVisita", 
                              "Percent Control Information Present" = "percentControlInfo"))
    ),


    
    
   
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Gráficos",
                           plotOutput("plotPerAcmp"),
                           plotOutput("plotViewMonths"),
                           plotOutput("plotViewCommunities") 
                  ),
                  tabPanel("Tablas",
                           tableOutput("tableMonths"),
                           tableOutput("tableCommunities"),
                           tableOutput("tablePerAcmp")
                  ),
                  tabPanel("Acompañantes", 
                           plotOutput("plotAcmpsGraphs"),
                           tableOutput("tableAcmpsMeasures"))
                           # tableOutput("tableView"),
                           # tableOutput("inFile"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  

  # Graph and table for cronicos measures per ACMP
  
  output$plotPerAcmp <- renderPlot({
    chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
    chronics <- FilterByDate(chronics, input$dateRange[1], input$dateRange[2])
    filteredData <- FilterByCommunity(chronics, input$selectCommunity)
    MeasureFunction <- GetMeasureFunctionPerAcmp(input$selectMeasure)
    plotData <- MeasureFunction(filteredData,input$selectDisease)
    plotColumn <- SelectPlotColumnPerAcmp(plotData, input$selectMeasure)
    ggplot(plotData, aes(form.nombre_acompanante, plotColumn)) +
      geom_bar(stat = "identity", col = "tomato", fill = "tomato") + 
      ggtitle(paste(input$selectMeasure, "in", input$selectCommunity)) +
      labs(x = "Acompanante", y = paste(input$selectMeasure)) +
      theme(plot.title = element_text(hjust = 0.5)) + 
      geom_text(aes(label= plotColumn), vjust=0)
  })
  
  output$tablePerAcmp <- renderTable({
    chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
    filteredData <- FilterByCommunity(chronics, input$selectCommunity)
    MeasureFunction <- GetMeasureFunctionPerAcmp(input$selectMeasure)
    tableData <- MeasureFunction(filteredData, input$selectDisease)
  })
 
 # Graph and table for Cronicos Measures per Month Graph
  
 output$plotViewMonths <- renderPlot({
   chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
   filteredData <- FilterByCommunity(chronics, input$selectCommunity)
   MeasureFunction <- GetMeasureFunction(input$selectMeasure)
   plotData <- MeasureFunction(filteredData, input$selectDisease)
   plotColumn <- SelectPlotColumn(plotData, input$selectMeasure)
   ggplot(plotData, aes(form.mes, plotColumn)) +
     geom_bar(stat = "identity", col = "midnightblue", fill = "midnightblue") +
     ggtitle(paste(input$selectMeasure, "for", input$selectCommunity)) +
     labs(x = "Month", y = paste(input$selectMeasure)) +
     theme(plot.title = element_text(hjust = 0.5)) +
     geom_text(aes(label= plotColumn), vjust=0)
 })
 
 output$tableMonths <- renderTable({
   chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
   filteredData <- FilterByCommunity(chronics, input$selectCommunity)
   MeasureFunction <- GetMeasureFunction(input$selectMeasure)
   plotData <- MeasureFunction(filteredData, input$selectDisease)
 })
 
 # Graph and table for Cronicos Measures Per Community (for 1 month)
 output$plotViewCommunities <- renderPlot({
   chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
   filteredData <- FilterByMonth(chronics, input$selectMonth)
   MeasureFunction <- GetMeasureFunctionMonth(input$selectMeasure)
   plotData <- MeasureFunction(filteredData, input$selectDisease)
   plotColumn <- SelectPlotColumn(plotData, input$selectMeasure)
   ggplot(plotData, aes(community, plotColumn)) +
     geom_bar(stat = "identity", col = "darkslategray2", fill = "darkslategray2") + 
     ggtitle(paste(input$selectMeasure, input$selectDisease, "in the Month of", input$selectMonth)) +
     labs(x = "Community", y = paste(input$measureSelect)) +
     theme(plot.title = element_text(hjust = 0.5)) + 
     geom_text(aes(label= plotColumn), vjust=0)
 })
 
 output$tableCommunities <- renderTable({
   chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
   filteredData <- FilterByMonth(chronics, input$selectMonth)
   MeasureFunction <- GetMeasureFunctionMonth(input$selectMeasure)
   tableData <- MeasureFunction(filteredData, input$selectDisease)
 })

 #########################################################################################
## Graph for ACMPS measures (Satis, Mentoria, Asistencia) 
 
 output$plotAcmpsGraphs <- renderPlot({
   acmps <- ProcessDataAcmps(input$formDataPath$datapath, input$acmpsCasesPath$datapath)
   filteredData <- FilterByCommunityAcmps(acmps, input$selectCommunity)
   MeasureFunction <- GetMeasureFunctionAcmps(input$selectMeasureAcmps)
   plotData <- MeasureFunction(filteredData)
   plotColumn <- SelectPlotColumnAcmps(plotData, input$selectMeasureAcmps)
   ggplot(plotData, aes(form.mes, plotColumn)) +
     geom_bar(stat = "identity", col = "dodgerblue", fill = "dodgerblue") + 
     ggtitle(paste(input$selectMeasureAcmps,  "for", input$selectCommunity)) +
     labs(x = "Month", y = input$measureSelectAcmps) +
     theme(plot.title = element_text(hjust = 0.5)) + 
     geom_text(aes(label= plotColumn), vjust=0)
     
 })
 
 output$tableAcmpsMeasures <- renderTable({
   acmps <- ProcessDataAcmps(input$formDataPath$datapath, input$acmpsCasesPath$datapath)
   filteredData <- FilterByCommunityAcmps(acmps, input$selectCommunity)
   MeasureFunction <- GetMeasureFunctionAcmps(input$selectMeasureAcmps)
   tableData <- MeasureFunction(filteredData)
 })
 

 
 ###########   NOT BEING USED RIGHT NOW ###########################
 
#  output$inFile <- renderTable({
#    input$formDataPath
#  })
#  
 output$tableView <- renderTable({
   req(input$formDataPath)
   req(input$cronicosPath)
   return(ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath))
 })
#  
#  
# # Filter by community and plot % control DM
#   
#   output$plotViewDm <- renderPlot({
#     chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
#     filteredData <- FilterByCommunity(chronics, input$selectCommunity)
#     plotData <- CreateDmControlDf(filteredData)
#     ggplot(plotData, aes(form.mes, DMcontrol_percent)) +
#     geom_bar(stat = "identity", col = "darkturquoise", fill = "darkturquoise") +
#       ggtitle(paste("Percentage Control Diabetes for", input$selectCommunity)) +
#       labs(x = "Month", y = "Percentage in Control") +
#       theme(plot.title = element_text(hjust = 0.5)) + 
#       geom_text(aes(label = round(plotData$DMcontrol_percent, 2)), vjust=0)
#    
#   })
# 
#   # HTN Control Plot
# output$plotViewHtn <- renderPlot({
#     chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
#     filteredData <- FilterByCommunity(chronics, input$selectCommunity)
#     plotData <- CreateHtnControlDf(filteredData)
#     ggplot(plotData, aes(form.mes, HTNcontrol_percent)) +
#       geom_bar(stat = "identity", col = "forestgreen", fill = "forestgreen") +
#       ggtitle(paste("Percentage Control Hypertension for", input$selectCommunity)) +
#       labs(x = "Month", y = "Percentage in Control") +
#       theme(plot.title = element_text(hjust = 0.5)) + 
#       geom_text(aes(label= round(plotData$HTNcontrol_percent, 2)), vjust=0)
#  
#   })
# 
# 
# 
# # Graph for number of visits planned vs realized per month 
# 
# output$plotViewVisits <- renderPlot({
#   chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
#   filteredData <- FilterByCommunity(chronics, input$selectCommunity)
#   plotData <- CreateVisitsDf(filteredData)
#   ggplot(plotData, aes(form.mes, value)) +
#   geom_bar(stat = "identity", position = "dodge", aes(fill = variable)) +
#   ylim(0,100) + geom_text(aes(label= plotData$value))
# })
# 
# # Graph for Percent Control Diabetes Per Community (1 month)
# output$plotViewMonthDm <- renderPlot({
#   chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
#   filteredData <- FilterByMonth(chronics, input$selectMonth)
#   plotData <- CreateDmControlMonthDf(filteredData)
#   ggplot(plotData, aes(community, DMcontrol_percent)) +
#   geom_bar(stat = "identity", col = "tomato", fill = "tomato") + 
#     ggtitle(paste("Percent Control Diabetes in the Month of", input$selectMonth)) +
#     labs(x = "Community", y = "Percentage in Control") +
#     theme(plot.title = element_text(hjust = 0.5))
# })
# 
# # Graph for Percent Control Htn Per Community (1 month)
# output$plotViewMonthHtn <- renderPlot({
#   chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
#   filteredData <- FilterByMonth(chronics, input$selectMonth)
#   plotData <- CreateHtnControlMonthDf(filteredData)
#   ggplot(plotData, aes(community, HTNcontrol_percent)) +
#     geom_bar(stat = "identity", col = "royalblue2", fill = "royalblue2") + 
#     ggtitle(paste("Percent Control Hypertension in the Month of", input$selectMonth)) +
#     labs(x = "Community", y = "Percentage in Control") +
#     theme(plot.title = element_text(hjust = 0.5))  
# })
# 


}


# Run the application
shinyApp(ui = ui, server = server)
