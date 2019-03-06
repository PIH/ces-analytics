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
library(DT)

source("data_prep.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Disease Control and Accompaniment"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", "Date Range:", 
                     start = "2017-01-01", format = "yyyy-mm-dd"),  
      fileInput("formDataPath", "Disease Control and Accompaniment"),
      fileInput("cronicosPath", "Chronic Cases"),
      fileInput("acmpsCasesPath", "Acompañante Cases"),
      verbatimTextOutput("summary"),
      selectInput("selectCommunity", "Community", 
        choices = c("Capitan", "Honduras", "Laguna" = "Laguna_del_Cofre", 
                    "Letrero", "Matasano", "Monterrey", "Plan Alta" = "Plan_Alta", 
                    "Plan Baja" = "Plan_Baja", "Reforma", "Salvador", "Soledad" )
      ),
      selectInput("selectMonth", "Month", 
                  choices = c("January" = 1, "February" = 2, "March" =3, "April"= 4, "May" = 5, 
                              "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10,
                              "November" = 11, "December" = 12) 
      ) 
    ),
    

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Disease Control",
		    fluidRow(
		      column(width=3,offset=2,
		        selectInput("selectDisease", "Disease", 
                                       choices = c("Diabetes" = "form.control_diabetes", 
                                       "Hypertension" = "form.control_htn", 
                                       "Depression" = "form.control_dep"))
		      ),
		      column(width=3,offset=1,
                        selectInput("selectMeasure", "Statistic", 
                                       choices = c(#"% Controlled Patients" = "percentControl", 
                                       "Controlled Patients" = "numberControl",
                                       #"# Patients Not In Control" = "numberNotControl",
                                       "Patient Visits" = "numberVisits", 
                                       #"# of Planned Patient Visits" = "visitsPlanned", 
                                       "Visit Forms Filled" = "percentHojaVisita", 
                                       "Patients with Control Information" = "percentControlInfo"))
		      )
		    ),
                    fluidRow(plotOutput("plotPerAcmp")),
                    fluidRow(plotOutput("plotViewMonths")),
                    fluidRow(plotOutput("plotViewCommunities")), 
	            tabsetPanel(type = "tabs",
		      tabPanel("Data by Month",
                           DTOutput("tableMonths")
		      ),
		      tabPanel("Data by Community",
                           DTOutput("tableCommunities")
		      ),
		      tabPanel("Data by Acompañante",
                           DTOutput("tablePerAcmp")
		      )
                    )
                  ),
                  tabPanel("Accompaniment",
		    fluidRow(
		      column(width=5,offset=3,
		           selectInput("selectMeasureAcmps", "Statistic",
                                       choices = c("% Patients with >= 85% Satisfaction" = "percentPatientSatisfaction",
                                       "Average Patient Satisfaction" = "averagePatientSatisfaction",
                                       "% Acompañante Attendance" = "percentAttendance",
                                       "% Acompañantes with >= 80% Mentorship" = "percentMentoria",
                                       "Average Mentorship" = "averageMentoria"))
		      )
		    ),
		    fluidRow(plotOutput("plotAcmpsGraphs")),
                    fluidRow(DTOutput("tableAcmpsMeasures"))
                           # DTOutput("tableView"),
                           # DTOutput("inFile"))
	          )		   
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Stop app in R session when closed
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  } else {
    session$onSessionEnded(function() {
      stopApp()
    })
  } 

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
  
  output$tablePerAcmp <- renderDT({
    chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
    filteredData <- FilterByCommunity(chronics, input$selectCommunity)
    MeasureFunction <- GetMeasureFunctionPerAcmp(input$selectMeasure)
    tableData <- MeasureFunction(filteredData, input$selectDisease)
    tableColNames <- GetMeasureColnamesPerAcmp(input$selectMeasure)
    tableData <- datatable(tableData, colnames = tableColNames, rownames=FALSE,
                         options = list("scrollY" = TRUE, "scrollX" = 100, "paging" = FALSE, "searching" = FALSE,
                                        "columnDefs" = list(list(className = 'dt-center', targets = '_all'))))

   return( tableData
           %>%
           formatCurrency(columns = grep("%",tableColNames), currency = "%", digits = 2, before = FALSE))
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
 
 output$tableMonths <- renderDT({
   chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
   filteredData <- FilterByCommunity(chronics, input$selectCommunity)
   MeasureFunction <- GetMeasureFunction(input$selectMeasure)
   plotData <- MeasureFunction(filteredData, input$selectDisease)
   tableColNames <- GetMeasureColnames(input$selectMeasure)
   plotData <- datatable(plotData, colnames = tableColNames, rownames=FALSE,
                         options = list("scrollY" = TRUE, "scrollX" = 100, "paging" = FALSE, "searching" = FALSE,
			                "columnDefs" = list(list(className = 'dt-center', targets = '_all'))))
   return( plotData 
           %>%
           formatCurrency(columns = grep("%",tableColNames), currency = "%", digits = 2, before = FALSE))
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
 
 output$tableCommunities <- renderDT({
   chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
   filteredData <- FilterByMonth(chronics, input$selectMonth)
   MeasureFunction <- GetMeasureFunctionMonth(input$selectMeasure)
   tableData <- MeasureFunction(filteredData, input$selectDisease)
   tableColNames <- GetMeasureColnamesPerMonth(input$selectMeasure)
   tableData <- datatable(tableData, colnames = tableColNames, rownames=FALSE,
                         options = list("scrollY" = TRUE, "scrollX" = 100, "paging" = FALSE, "searching" = FALSE,
                                        "columnDefs" = list(list(className = 'dt-center', targets = '_all'))))
   return( tableData
           %>%
           formatCurrency(columns = grep("%",tableColNames), currency = "%", digits = 2, before = FALSE))
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
 
 output$tableAcmpsMeasures <- renderDT({
   acmps <- ProcessDataAcmps(input$formDataPath$datapath, input$acmpsCasesPath$datapath)
   filteredData <- FilterByCommunityAcmps(acmps, input$selectCommunity)
   MeasureFunction <- GetMeasureFunctionAcmps(input$selectMeasureAcmps)
   tableData <- MeasureFunction(filteredData)
   tableColNames <- GetMeasureColnamesAcmps(input$selectMeasure)
   tableData <- datatable(tableData, colnames = tableColNames, rownames=FALSE,
                         options = list("scrollY" = TRUE, "scrollX" = 100, "paging" = FALSE, "searching" = FALSE,
                                        "columnDefs" = list(list(className = 'dt-center', targets = '_all'))))

   return( tableData
           %>%
           formatCurrency(columns = grep("%",tableColNames), currency = "%", digits = 2, before = FALSE))
  })
 

 
 ###########   NOT BEING USED RIGHT NOW ###########################
 
#  output$inFile <- renderDT({
#    input$formDataPath
#  })
#  
 output$tableView <- renderDT({
   req(input$formDataPath)
   req(input$cronicosPath)
   return(ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath))
 }, options = list(
    "scrollY" = TRUE, "scrollX" = 100, "paging" = FALSE, "searching" = FALSE,
    "columnDefs" = list(list("targets" = "_all", "className" = "dt-center"))
  ), rownames = FALSE)
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
