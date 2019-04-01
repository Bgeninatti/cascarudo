library(shiny)
library(ggplot2)
library(DT)
source("helpers.R")

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Cascarudo"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file", "Flujo de fondos",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      br(),
      
      # Input: Slider for the number of observations to generate ----
      sliderInput("rate",
                  "Tasa requerida:",
                  value = 50,
                  min = 1,
                  max = 100),
      br(),
      downloadButton("bajarEjemplo", "Bajar tabla de ejemplo"),
      br(),
      uiOutput("botonBajarProyeccion")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Flujo de fondos", DT::dataTableOutput("flujo")),
                  tabPanel("Proyección", DT::dataTableOutput("proyeccion"))
      )
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {

  getRate <- reactive({
    input$rate / 100
  })
  output$proyeccion <- DT::renderDataTable({
    req(input$file)
    df <- loadCsv(input$file$datapath)
    projectPV(df$Fecha[1], df$Saldo[1], df$Fecha, df$Capital, df$Rendimiento, getRate())
  })
  output$flujo <- DT::renderDataTable({
    req(input$file)
    loadCsv(input$file$datapath)
  })
  output$bajarEjemplo <- downloadHandler(
    filename = function() {
      return("flujo_ejemplo.csv")
    },
    content = function(file) {
      sample <- loadCsv("sample.csv")
      write.csv(sample, file, row.names = FALSE)
    }
  )
  output$botonBajarProyeccion <- renderUI({
    req(input$file)
    downloadButton("bajarProyeccion", "Bajar proyección")
  })
  output$bajarProyeccion <- downloadHandler(
    filename = function() {
      paste("proyeccion_", input$rate, ".csv", sep = "")
    },
    content = function(file) {
      req(input$file)
      df <- loadCsv(input$file$datapath)
      proyeccion <- projectPV(df$Fecha[1], df$Saldo[1], df$Fecha, df$Capital, df$Rendimiento, getRate())
      write.csv(proyeccion, file, row.names = FALSE)
    }
  )
}
# Run the app ----
shinyApp(ui, server)