library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(micromapST)
source("global.R")
source("ht2.R")

ui <- navbarPage(title = "Exploratory Data Analysis",
                 collapsible = TRUE,
                 fluid = FALSE,
                 theme = shinytheme("simplex"),
                 
                 tabPanel("Visualizing COVID-19",
                          fixedPage(
                            fixedRow(width = 10,br(),br()),
                            fixedRow(
                              column(width = 4,
                                     h2("PROJECT", align="center"),
                                     h4("Within this app contains visualizations mapping COVID-19 data.
                                        Some visualizations have an added sociopolitical consideration
                                        connected to differences in confirmed COVID-19 case rates between U.S. states.
                                        That consideration is measured by data on the 2020 Presidential Election and whether a
                                        state's majority vote was for the Democratic Party, Republican Party, or flipped to the Democratic Party."),
                                     h2("DATA", align = "center"),
                                     h4("All COVID-19 data was obtained from USA Facts.
                                        The U.S. state and county 2019 population data was also obtained from USA Facts.
                                        Data on the 2020 Presidential Election was obtained from the New York Times.
                                        The observations used in this project range from January 22nd, 2020 to April 8th, 2021.
                                        The case rates in each state are per 100,000 people.")),
                              column(width=6, align = "center",
                                     imageOutput("Image"))),
                            fixedRow(
                              column(width = 11,
                                     align = "center")))),
                 
                 tabPanel("Time Series Micromap",
                          fluidPage(
                            fluidRow(
                              column(12, offset=1, fluid = TRUE,
                              plotOutput(outputId = "plot1",
                                         height ="700px",
                                         width = "700px"))))),
                 
                 tabPanel("HT Map",
                          fluidPage(
                            fluidRow(br(),br()),
                            fluidRow(
                              column(12,
                            mainPanel(plotOutput(outputId = "plot2", height ="450px",
                                                 width = '900px')))))),
                 
                 tabPanel("Assignment 5B",
                          fluidPage(
                            fluidRow(br()),
                            fluidRow(
                              column(12, 
                                mainPanel(plotOutput(outputId = "plot3",
                                                 height = "600px",
                                                 width = "600px")))))),
                 tabPanel("Data",
                          fluidPage(
                            fluidRow(
                              column(4,
                                     selectInput("Date",
                                                 "Date:",
                                                 c("All",
                                                   unique(as.character(c.rates$Date))))),
                              column(4,
                                     selectInput("State",
                                                 "State:",
                                                 c("All",
                                                   unique(as.character(c.rates$State))))),
                              column(4,
                                     selectInput("Election_Majority",
                                                 "Election Majority:",
                                                 c("All",
                                                   unique(as.character(c.rates$Election_Majority)))))),
                            DT::dataTableOutput("table")
                          )))



server <- function(input, output) {
  output$Image <- renderImage({
    list(src = "www/covidpic.png",
         width = 500,
         height = 450)
    }, deleteFile = F)
  
  output$plot1 <-renderPlot(
   micromapST(cases.ts, paneldesc,
              sortVar="P12",ascend=FALSE,
              title = "United States Monthly COVID-19 Case Rate (May 2020 - April 2021)",
              ))
  
  output$plot2 <- renderPlot(
    ht2(case.rank, 
        vars = c(2,3),
        fillCol = c(rgb(0,.2,.8,.5), gray(.9),rgb(1,0,.2,.5)),
        title = title
        ))
  
  output$plot3<- renderPlot(
    ggarrange(Zero, NULL, One,Two,Three,Four,
              ncol = 2, nrow = 3,
              heights = c(1,6,6),
              legend = "bottom",
              common.legend = TRUE))
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- c.rates
    if (input$Date != "All") {
      data <- data[data$Date == input$Date,]
    }
    if (input$State != "All") {
      data <- data[data$State == input$State,]
    }
    if (input$Election_Majority != "All") {
      data <- data[data$Election_Majority == input$Election_Majority,]
    }
    data
  }))

}

# Run the application 
shinyApp(ui = ui, server = server)
  
