library(shiny)
library(shinyBS)

fluidPage(

    titlePanel("Tracing"),

    sidebarLayout(
        sidebarPanel(
            fluidRow(
              style = "padding: 0px;",
              column(12, fileInput("in_file", "Select data file",
                accept = c("txt/csv", "text/comma-seperated-values, text/plain", ".csv"),
                width = "100%"
              ), style = "padding: 0px;"),
              column(12, fileInput("in_seq", "Select sequence file",
                accept = c("txt/csv", "text/comma-seperated-values, text/plain", ".csv"),
                width = "100%"
              ), style = "padding: 0px;")
            ),
            fluidRow(
              column(7, selectInput("metabolite", "Metabolite", 
                                     choices = NULL, width = "100%")
              ),
              column(5, selectInput("sample", "Sample", 
                                     choices = NULL, width = "100%"))
            ),
            bsButton("button", "button"),
            fluidRow(
              column(7, selectInput("metabolite1", "Metabolite", 
                                     choices = NULL, width = "100%")
              ),
              column(5, bsButton("button1", "button"))
            ),
            
            fluidRow(
              column(7, selectInput("metabolite2", "Metabolite", 
                                     choices = NULL, width = "100%")
              ),
              column(5, selectInput("time", "time", 
                                    choices = NULL, width = "100%"))
            ),
            bsButton("button2", "met+time (multiple groups)")
        ),

        mainPanel(
            plotOutput("ggPlot")
        )
    )
)
