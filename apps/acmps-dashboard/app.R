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
# library(XLConnect)
library(readxl)
library(reshape2)
library(DT)
library(plotly)
source("data_prep.R")

measure_options <- c("Porcentaje Control" = "percentControl", 
                     "Número Control" = "numberControl",
                     "Número Sin Control" = "numberNotControl",
                     "Número Visitas Cróncios" = "numberVisits", 
                     "Número Visitas Planeadas" = "visitsPlanned", 
                     "Porcentaje Hojas Visitas Llenadas" = "percentHojaVisita", 
                     "Porcentaje Datos Control Presente" = "percentControlInfo")

disease_options <- c("Todos" = "all", 
                     "Diabetes" = "form.control_diabetes", 
                     "Hipertensión" = "form.control_htn", 
                     "Depresión" = "form.control_dep", 
                     "Epilepsia" = "form.control_epilepsia")

month_options <- c("Enero" = 1, "Febrero" = 2, "Marzo" =3, "Abril"= 4, "Mayo" = 5, 
  "Junio" = 6, "Julio" = 7, "Augosto" = 8, "Septiembre" = 9, "Octubre" = 10,
  "Noviembre" = 11, "Diciembre" = 12) 

acmps_options <- c("Porcentaje Pacientes con >= 85% Satisfacción" = "percentPatientSatisfaction",
                   "Promedio Satisfacción de Paciente" = "averagePatientSatisfaction",
                   "Porcentaje Asistencia Acmps" = "percentAttendance",
                   "Porcentaje Acmps con >= 80% Mentoría" = "percentMentoria",
                   "Promedio Mentoría" = "averageMentoria")

community_options <- c("Capitan", "Honduras", "Laguna" = "Laguna_del_Cofre", 
  "Letrero", "Matasano", "Monterrey", "Plan Alta" = "Plan_Alta", 
  "Plan Baja" = "Plan_Baja", "Reforma", "Salvador", "Soledad", "Todos" = "all" )

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Panel de datos de Acompañantes"),
  h5("Bienvenidos al Panel de Datos. Utiliza los botónes abajo para cambiar los gráficos."),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h5("Sube los datos de CommCare abajo en el formato de excel"),
      fileInput("formDataPath", "Form Data Export"),
      fileInput("cronicosPath", "Casos Crónicos"),
      fileInput("acmpsCasesPath", "Casos Acompañantes"),
      verbatimTextOutput("summary"),
      h5("Selecciona el rango de fechas y la comunidad"),
      dateRangeInput("dateRange", "Periodo:", 
                     start = "2018-01-01", end = "2018-01-31", format = "yyyy-mm-dd"),  
      selectInput("selectCommunity", "Comunidad", 
        choices = community_options
      )
      
      ), 
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Control de Crónicos",
                           fluidRow(
                            h5("Selecciona un indicador y enfermedad"),
                             column(width=3, offset=2,
                                    selectInput("selectDisease", "Enfermedad", 
                                                choices = disease_options)
                                                
                                    ),
                             br(),
                             column(width=3,offset=1,
                                    selectInput("selectMeasure", "Selecciona un Indicador", 
                                                choices = measure_options)
                             )
                             ), 
                           br(),
                       
                           fluidRow(plotOutput("plotPerAcmp")),
                           br(),
                           # downloadButton(outputId = "downloadPlot",
                           #                label = "Download Plot!"),
                           fluidRow(plotOutput("plotViewMonths")),
                           br(),
                           fluidRow(plotOutput("plotViewCommunities")),
                           br(),
                           fluidRow(plotOutput("pieViewCommunities")),
                           tabsetPanel(type="tabs",
                                       tabPanel("Datos por Acompañante",
                                                DTOutput("tablePerAcmp")
                                       ),
                                       tabPanel("Datos por Mes",
                           DTOutput("tableMonths")
                           ),
                           tabPanel("Datos por Comunidad",
                           DTOutput("tableCommunities")
                           )
                          
                           )
                  ),
                  br(),
                 
                  tabPanel("Acompañantes", 
                           fluidRow(
                             column(width=5,offset=3,
                                    selectInput("selectMeasureAcmps", "Selecciona un Indicador",
                                                choices = acmps_options)
                                    )
                                    ),
                           br(),
                  
                           fluidRow(plotOutput("plotAcmpsGraphs")),
                           br(),
                           fluidRow(plotOutput("plotAcmpsViewMonths")),
                           br(),
                           fluidRow(plotOutput("plotAcmpsViewCommunities")),
                           br(),
                           tabsetPanel(type="tabs",
                                       tabPanel("Datos por Acompañante",
                                                DTOutput("tableAcmpsMeasures")
                                       ),
                                       tabPanel("Datos por Mes",
                                                DTOutput("tableAcmpsViewMonths")
                                       ),
                                       tabPanel("Datos por Comunidad",
                                                DTOutput("tableAcmpsViewCommunities")
                                       )
                           ),
                           br()
                           # DTOutput("tableView"),
                           # DTOutput("inFile"))
      )
    )
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  chronicsData <- reactive({
    chronics <- ProcessData(input$formDataPath$datapath, input$cronicosPath$datapath)
    chronics <- FilterByDate(chronics, input$dateRange[1], input$dateRange[2])
    return(chronics)
  })
  

  # Graph and table for cronicos measures per ACMP
  
  output$plotPerAcmp <- renderPlot({
    chronics <- chronicsData()
    filteredData <- FilterByCommunity(chronics, input$selectCommunity)
    # filteredData <- FilterByMonth(filteredData, input$selectMonth)
    MeasureFunction <- GetMeasureFunction(input$selectMeasure)
    plotData <- MeasureFunction(filteredData,input$selectDisease, by = "Acompañante")
    plotColumn <- SelectPlotColumn(plotData, input$selectMeasure)
    ggplot(plotData, aes(form.nombre_acompanante, y = plotColumn)) +
      geom_bar(stat = "identity", col = "tomato", fill = "tomato") +
      ggtitle(paste(names(measure_options[which(measure_options == input$selectMeasure)]), "Para", names(disease_options[which(disease_options == input$selectDisease)]), "En", input$selectCommunity, "Por Acompañante")) +
      labs(x = "Acompañante", y = paste(names(measure_options[which(measure_options == input$selectMeasure)]))) +
    geom_text(aes(label = plotColumn), color = "white", size = 5, position = position_stack(vjust = 0.5)) + 
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
    })
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$filename, '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = output$plotPerAcmp, device = "png") }
  )


  output$tablePerAcmp <- renderDT({
    chronics <- chronicsData()
    filteredData <- FilterByCommunity(chronics, input$selectCommunity)
    # filteredData <- FilterByMonth(filteredData, input$selectMonth)
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
   tick_names <- c("Jan", "Feb", "Mar", "Abr", "Mayo", "Junio", "Julio", "Ago", "Sep", "Oct", "Nov", "Dic") 
   chronics <- chronicsData()
   filteredData <- FilterByCommunity(chronics, input$selectCommunity)
   MeasureFunction <- GetMeasureFunction(input$selectMeasure)
   plotData <- MeasureFunction(filteredData, input$selectDisease, by = "Mes")
   plotColumn <- SelectPlotColumn(plotData, input$selectMeasure)
    ggplot(plotData, aes(form.mes, plotColumn)) +
      geom_bar(stat = "identity", col = "midnightblue", fill = "midnightblue") +
      scale_x_discrete(breaks = 1:12, labels = tick_names, limits = c(1: 12)) +
      ggtitle(paste(names(measure_options[which(measure_options == input$selectMeasure)]), 
                   "Para", names(disease_options[which(disease_options == input$selectDisease)]), "En", input$selectCommunity, "Por Mes")) +
     labs(x = "Mes", y = paste(input$selectMeasure)) +
     geom_text(aes(label = plotColumn), color = "white", size = 5, position = position_stack(vjust = 0.5)) + 
     theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
 })
 

 
 output$tableMonths <- renderDT({
   chronics <- chronicsData()
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
 
 # Graph and table for Cronicos Measures Per Community 
 

 output$plotViewCommunities <- renderPlot({
   chronics <- chronicsData()
   # filteredData <- FilterByMonth(chronics, input$selectMonth)
   MeasureFunction <- GetMeasureFunction(input$selectMeasure)
   plotData <- MeasureFunction(chronics, input$selectDisease, by = "Comunidad")
   plotColumn <- SelectPlotColumn(plotData, input$selectMeasure)
   ggplot(plotData, aes(community, plotColumn)) +
     geom_bar(stat = "identity", col = "darkslategray2", fill = "darkslategray2") + 
     ggtitle(paste(names(measure_options[which(measure_options == input$selectMeasure)]), "Para", names(disease_options[which(disease_options == input$selectDisease)]), "Por Comunidad")) +
     labs(x = "Comunidad", y = paste(names(measure_options[which(measure_options == input$selectMeasure)]))) +
     geom_text(aes(label = plotColumn), color = "white", size = 5, position = position_stack(vjust = 0.5)) + 
     theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) 
 })
 
 output$tableCommunities <- renderDT({
   chronics <- chronicsData()
   # filteredData <- FilterByMonth(chronics, input$selectMonth)
   MeasureFunction <- GetMeasureFunction(input$selectMeasure)
   tableData <- MeasureFunction(chronics, input$selectDisease, by = "Comunidad")
   cnames <- colnames(tableData)
   tableData <- datatable(tableData, rownames = FALSE, colnames = cnames,
                          options = list("scrollY" = TRUE, "scrollX" = 100, "paging" = FALSE, "searching" = FALSE,
                                         "columnDefs" = list(list(className = 'dt-center', targets = '_all'))))
   tableData <- tableData %>% formatCurrency(columns = grep("%", cnames, value = TRUE), 
                                             currency = "%", digits = 2, before = FALSE)
   return(tableData)
 })
 
 
 output$pieViewCommunities <- renderPlot({
   chronics <- chronicsData()
   filteredData <- FilterByDate(chronics, input$dateRange[1], input$dateRange[2])
   # filteredData <- FilterByMonth(filteredData, input$selectMonth)
   MeasureFunction <- GetMeasureFunction(input$selectMeasure)
   plotData <- MeasureFunction(filteredData, input$selectDisease, by = "Comunidad")
   plotColumn <- SelectPlotColumn(plotData, input$selectMeasure)
   ggplot(plotData, aes(x = "", y = plotColumn, fill = community)) +
   geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + 
     ggtitle(paste(names(measure_options[which(measure_options == input$selectMeasure)]), "Para", names(disease_options[which(disease_options == input$selectDisease)]), "Por Comunidad")) +
     geom_text(aes(label= plotColumn), position = position_stack(vjust = 0.5)) + 
     theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) 
 })
 


 #########################################################################################
## Graph for ACMPS measures (Satis, Mentoria, Asistencia) 
 
 # Graph and table for acmps measures per ACMP
 
 output$plotAcmpsGraphs <- renderPlot({
   acmps <- ProcessDataAcmps(input$formDataPath$datapath, input$acmpsCasesPath$datapath)
   acmps <- FilterByDate(acmps, input$dateRange[1], input$dateRange[2])
   filteredData <- FilterByCommunity(acmps, input$selectCommunity)
   # filteredData <- FilterByMonth(filteredData, input$selectMonth)
   MeasureFunction <- GetMeasureFunctionAcmps(input$selectMeasureAcmps)
   plotData <- MeasureFunction(filteredData, by = "Acompañante")
   plotColumn <- SelectPlotColumnAcmps(plotData, input$selectMeasureAcmps)
   ggplot(plotData, aes(form.nombre_acompanante, plotColumn)) +
     geom_bar(stat = "identity", col = "tomato", fill = "tomato") + 
     ggtitle(paste(names(acmps_options[which(acmps_options == input$selectMeasureAcmps)]), "Para", names(disease_options[which(disease_options == input$selectDisease)]),"En", names(community_options[which(community_options == input$selectCommunity)]))) +
     labs(x = "Acompañante", y = paste(names(acmps_options[which(acmps_options == input$selectMeasureAcmps)]))) + 
     theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
 })
 
 
 output$tableAcmpsMeasures <- renderDT({
   acmps <- ProcessDataAcmps(input$formDataPath$datapath, input$acmpsCasesPath$datapath)
   acmps <- FilterByDate(acmps, input$dateRange[1], input$dateRange[2])
   filteredData <- FilterByCommunity(acmps, input$selectCommunity)
   # filteredData <- FilterByMonth(filteredData, input$selectMonth)
   MeasureFunction <- GetMeasureFunctionAcmps(input$selectMeasureAcmps)
   tableData <- MeasureFunction(filteredData, by = "Acompañante")
   cnames <- colnames(tableData)
   tableData <- datatable(tableData, rownames = FALSE, colnames = cnames,
                          options = list("scrollY" = TRUE, "scrollX" = 100, "paging" = FALSE, "searching" = FALSE,
                                         "columnDefs" = list(list(className = 'dt-center', targets = '_all'))))
   tableData <- tableData %>% formatCurrency(columns = grep("%", cnames, value = TRUE), 
                                             currency = "%", digits = 2, before = FALSE)
   return(tableData)
 })
 
 
 # Graph and table for Acmps Measures per Month Graph
 
 output$plotAcmpsViewMonths <- renderPlot({
   acmps <- ProcessDataAcmps(input$formDataPath$datapath, input$acmpsCasesPath$datapath)
   acmps <- FilterByDate(acmps, input$dateRange[1], input$dateRange[2])
   filteredData <- FilterByCommunity(acmps, input$selectCommunity)
   MeasureFunction <- GetMeasureFunctionAcmps(input$selectMeasureAcmps)
   plotData <- MeasureFunction(filteredData, by = "Mes")
   plotColumn <- SelectPlotColumnAcmps(plotData, input$selectMeasureAcmps)
   ggplot(plotData, aes(form.mes, plotColumn)) +
     geom_bar(stat = "identity", col = "midnightblue", fill = "midnightblue") +
     ggtitle(paste(names(acmps_options[which(acmps_options == input$selectMeasureAcmps)]), "Para", names(disease_options[which(disease_options == input$selectDisease)]), "En", names(community_options[which(community_options == input$selectCommunity)]))) +
     labs(x = "Mes", y = paste(names(acmps_options[which(acmps_options == input$selectMeasureAcmps)]))) +
     theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
     geom_text(aes(label = plotColumn), color = "white", size = 5, position = position_stack(vjust = 0.5)) + 
     theme_minimal()
 })
 
 
 

 output$tableAcmpsViewMonths <- renderDT({
   acmps <- ProcessDataAcmps(input$formDataPath$datapath, input$acmpsCasesPath$datapath)
   acmps <- FilterByDate(acmps, input$dateRange[1], input$dateRange[2])
   filteredData <- FilterByCommunity(acmps, input$selectCommunity)
   MeasureFunction <- GetMeasureFunctionAcmps(input$selectMeasureAcmps)
   tableData <- MeasureFunction(filteredData, by = "Mes")
   cnames <- colnames(tableData)
   tableData <- datatable(tableData, rownames = FALSE, colnames = cnames,
                          options = list("scrollY" = TRUE, "scrollX" = 100, "paging" = FALSE, "searching" = FALSE,
                                         "columnDefs" = list(list(className = 'dt-center', targets = '_all'))))
   tableData <- tableData %>% formatCurrency(columns = grep("%", cnames, value = TRUE), 
                                             currency = "%", digits = 2, before = FALSE)
   return(tableData)
 })
 
 # Graph and table for Acmps Measures Per Community (for 1 month)
 output$plotAcmpsViewCommunities <- renderPlot({
   acmps <- ProcessDataAcmps(input$formDataPath$datapath, input$acmpsCasesPath$datapath)
   filteredData <- FilterByDate(acmps, input$dateRange[1], input$dateRange[2])
   # filteredData <- FilterByMonth(filteredData, input$selectMonth)
   MeasureFunction <- GetMeasureFunctionAcmps(input$selectMeasureAcmps)
   plotData <- MeasureFunction(filteredData, by = "Comunidad")
   plotColumn <- SelectPlotColumnAcmps(plotData, input$selectMeasureAcmps)
   ggplot(plotData, aes(community, plotColumn)) +
     geom_bar(stat = "identity", col = "darkslategray2", fill = "darkslategray2") + 
     ggtitle(paste(names(acmps_options[which(acmps_options == input$selectMeasureAcmps)]), 
                   "Para", names(disease_options[which(disease_options == input$selectDisease)]), "Por Communidad")) +
     labs(x = "Comunidad", y = paste(names(acmps_options[which(acmps_options == input$selectMeasureAcmps)]))) +
     theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) + 
     geom_text(aes(label = plotColumn), color = "white", size = 5, position = position_stack(vjust = 0.5)) + 
     theme_minimal()
 })
 

 output$tableAcmpsViewCommunities <- renderDT({
   acmps <- ProcessDataAcmps(input$formDataPath$datapath, input$acmpsCasesPath$datapath)
   filteredData <- FilterByDate(acmps, input$dateRange[1], input$dateRange[2])
   # filteredData <- FilterByMonth(filteredData, input$selectMonth)
   MeasureFunction <- GetMeasureFunctionAcmps(input$selectMeasureAcmps)
   tableData <- MeasureFunction(filteredData, by = "Comunidad")
   cnames <- colnames(tableData)
   tableData <- datatable(tableData, rownames = FALSE, colnames = cnames,
                          options = list("scrollY" = TRUE, "scrollX" = 100, "paging" = FALSE, "searching" = FALSE,
                                         "columnDefs" = list(list(className = 'dt-center', targets = '_all'))))
   tableData <- tableData %>% formatCurrency(columns = grep("%", cnames, value = TRUE), 
                                             currency = "%", digits = 2, before = FALSE)
   return(tableData)
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


}


# Run the application
shinyApp(ui = ui, server = server)
