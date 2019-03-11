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
  titlePanel("Programa de Acompañantes"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", "Periodo:", 
                     start = "2017-01-01", format = "yyyy-mm-dd"),  
      fileInput("formDataPath", "Acompañamiento y Control de Crónicos"),
      fileInput("cronicosPath", "Casos Crónicos"),
      fileInput("acmpsCasesPath", "Casos por Acompañante"),
      verbatimTextOutput("summary"),
      selectInput("selectCommunity", "Comunidad", 
        choices = c("Capitan", "Honduras", "Laguna" = "Laguna_del_Cofre", 
                    "Letrero", "Matasano", "Monterrey", "Plan Alta" = "Plan_Alta", 
                    "Plan Baja" = "Plan_Baja", "Reforma", "Salvador", "Soledad" )
      ),
      selectInput("selectMonth", "Mes", 
                  choices = c("January" = 1, "February" = 2, "March" =3, "April"= 4, "May" = 5, 
                              "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10,
                              "November" = 11, "December" = 12) 
      ) 
    ),
    

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Control de Crónicos",
		    fluidRow(
		      column(width=3,offset=2,
		        selectInput("selectDisease", "Selecciona una Enferemdad", 
                                       choices = c("Diabetes" = "form.control_diabetes", 
                                       "Hypertension" = "form.control_htn", 
                                       "Depression" = "form.control_dep"))
		      ),
		      column(width=3,offset=1,
                        selectInput("selectMeasure", "Selecciona un Indicador", 
                                       choices = c( "Pacientes Controlados" = "numberControl",
                                       "Visitas de Pacientes" = "numberVisits", 
                                       "Hojas de Visita Llenadas" = "percentHojaVisita", 
                                       "Pacientes con Info de Control" = "percentControlInfo"))
		      )
		    ),
                    fluidRow(plotOutput("plotPerAcmp")),
                    fluidRow(plotOutput("plotViewMonths")),
                    fluidRow(plotOutput("plotViewCommunities")), 
	            tabsetPanel(type = "tabs",
		      tabPanel("Datos por Mes",
                           DTOutput("tableMonths")
		      ),
		      tabPanel("Datos por Comunidad",
                           DTOutput("tableCommunities")
		      ),
		      tabPanel("Datos por Acompañante",
                           DTOutput("tablePerAcmp")
		      )
                    )
                  ),
                  tabPanel("Acompañamiento",
		    fluidRow(
		      column(width=5,offset=3,
		           selectInput("selectMeasureAcmps", "Selecciona un Indicador",
                                       choices = c("% Pacientes con satisfacción >= 85%" = "percentPatientSatisfaction",
                                       "Satisfacción del Paciente" = "averagePatientSatisfaction",
                                       "% Asistencia de Acompañantes" = "percentAttendance",
                                       "% Con Mentoria >= 80%" = "percentMentoria",
                                       "Promedio de Mentoria" = "averageMentoria"))
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
    MeasureFunction <- GetMeasureFunction(input$selectMeasure)
    plotData <- MeasureFunction(filteredData, input$selectDisease, by = "Acompañante")
    ByCategoryStackedBarplot(plotData[,1:4]) 
  })
  
  output$tablePerAcmp <- renderDT({
    chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
    filteredData <- FilterByCommunity(chronics, input$selectCommunity)
    MeasureFunction <- GetMeasureFunction(input$selectMeasure)
    tableData <- MeasureFunction(filteredData, input$selectDisease, by = "Acompañante")
    cnames <- colnames(tableData)
    tableData <- datatable(tableData, rownames = FALSE, colnames = cnames,
                           options = list("scrollY" = TRUE, "scrollX" = 100, "paging" = FALSE, "searching" = FALSE,
                                        "columnDefs" = list(list(className = 'dt-center', targets = '_all'))))
    tableData <- tableData %>% formatCurrency(columns = grep("%", cnames, value = TRUE), 
                                              currency = "%", digits = 2, before = FALSE)
   return(tableData)
  })
 
 # Graph and table for Cronicos Measures per Month Graph
  
 output$plotViewMonths <- renderPlot({
    chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
    chronics <- FilterByDate(chronics, input$dateRange[1], input$dateRange[2])
    filteredData <- FilterByCommunity(chronics, input$selectCommunity)
    MeasureFunction <- GetMeasureFunction(input$selectMeasure)
    plotData <- MeasureFunction(filteredData, input$selectDisease, by = "Mes")
    ByCategoryStackedBarplot(plotData[,1:4])  
 })
 
 output$tableMonths <- renderDT({
    chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
    filteredData <- FilterByCommunity(chronics, input$selectCommunity)
    MeasureFunction <- GetMeasureFunction(input$selectMeasure)
    tableData <- MeasureFunction(filteredData, input$selectDisease, by = "Mes")
    cnames <- colnames(tableData)
    tableData <- datatable(tableData, rownames = FALSE, colnames = cnames,
                           options = list("scrollY" = TRUE, "scrollX" = 100, "paging" = FALSE, "searching" = FALSE,
                                        "columnDefs" = list(list(className = 'dt-center', targets = '_all'))))
    tableData <- tableData %>% formatCurrency(columns = grep("%", cnames, value = TRUE),
                                              currency = "%", digits = 2, before = FALSE)
   return(tableData)
 }) 
 
 # Graph and table for Cronicos Measures Per Community (for 1 month)
 output$plotViewCommunities <- renderPlot({
    chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
    chronics <- FilterByDate(chronics, input$dateRange[1], input$dateRange[2])
    filteredData <- FilterByCommunity(chronics, input$selectCommunity)
    MeasureFunction <- GetMeasureFunction(input$selectMeasure)
    plotData <- MeasureFunction(filteredData, input$selectDisease, by = "Comunidad")
    ByCategoryStackedBarplot(plotData[,1:4])
 })
 
 output$tableCommunities <- renderDT({
    chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
    filteredData <- FilterByCommunity(chronics, input$selectCommunity)
    MeasureFunction <- GetMeasureFunction(input$selectMeasure)
    tableData <- MeasureFunction(filteredData, input$selectDisease, by = "Comunidad")
    cnames <- colnames(tableData)
    tableData <- datatable(tableData, rownames = FALSE, colnames = cnames,
                           options = list("scrollY" = TRUE, "scrollX" = 100, "paging" = FALSE, "searching" = FALSE,
                                        "columnDefs" = list(list(className = 'dt-center', targets = '_all'))))
    tableData <- tableData %>% formatCurrency(columns = grep("%", cnames, value = TRUE),
                                              currency = "%", digits = 2, before = FALSE)
   return(tableData)
 })

 #########################################################################################
## Graph for ACMPS measures (Satis, Mentoria, Asistencia) 
 
 output$plotAcmpsGraphs <- renderPlot({
   acmps <- ProcessDataAcmps(input$formDataPath$datapath, input$acmpsCasesPath$datapath)
   filteredData <- FilterByCommunityAcmps(acmps, input$selectCommunity)
   plotData <- switch(input$selectMeasureAcmps,
                             "percentPatientSatisfaction" = PatientSatisfaction(filteredData, by = "Mes"),
                             "averagePatientSatisfaction" = DistributionPatientSatisfaction(filteredData, by = "Mes"),
                             "percentAttendance" = PercentAttendance(filteredData, by = "Mes"),
                             "percentMentoria" = PercentMentoria(filteredData, by = "Mes")
                             )
   data <<- plotData
   switch(input$selectMeasureAcmps,
          "percentPatientSatisfaction" = ByCategoryStackedBarplot(plotData[,1:4]),
	  "averagePatientSatisfaction" = ByCategoryBoxplot(plotData),
	  "percentAttendance" = ByCategoryStackedBarplot(plotData[,1:4]),
	  "percentMentoria" = ByCategoryStackedBarplot(plotData[,1:4]))
 })
 
 output$tableAcmpsMeasures <- renderDT({
   acmps <- ProcessDataAcmps(input$formDataPath$datapath, input$acmpsCasesPath$datapath)
   filteredData <- FilterByCommunityAcmps(acmps, input$selectCommunity)
   tableData <- switch(input$selectMeasureAcmps,
                       "percentPatientSatisfaction" = PatientSatisfaction(filteredData, by = "Mes"),
                       "averagePatientSatisfaction" = AveragePatientSatisfaction(filteredData, by = "Mes"),
                       "percentAttendance" = PercentAttendance(filteredData, by = "Mes"),
                       "percentMentoria" = PercentMentoria(filteredData, by = "Mes")
                      )
   cnames <- colnames(tableData)
   tableData <- datatable(tableData, rownames=FALSE,
                         options = list("scrollY" = TRUE, "scrollX" = 100, "paging" = FALSE, "searching" = FALSE,
                                        "columnDefs" = list(list(className = 'dt-center', targets = '_all'))))

   return( tableData
           %>%
           formatCurrency(columns = grep("%", cnames, value = TRUE), currency = "%", digits = 2, before = FALSE))
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
