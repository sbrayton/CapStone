library(shiny)
library(shinyjs)

source('data.R', local = TRUE)


shinyUI(fluidPage(
  
  titlePanel("Blown Away DMR Assignment"),
  # sidebarLayout(
  #   sidebarPanel(
  #  
  #     # # Select type of trend to plot
  #     # selectInput(inputId = "type", label = strong("Choose a Side"),
  #     #             choices = c("Offensive Stats", "Defensive Stats"),
  #     #             selected = "Offensive Stats"),
  #     # 
  #     # # Select date range to be plotted
  #     # dateRangeInput("date", strong("Date range"), start = "2007-01-01", end = "2017-07-31",
  #     #                min = "2007-01-01", max = "2017-07-31")     
  # 
  #     ),
  #   
    
    mainPanel(  

               tabsetPanel(
                 tabPanel("Frequency True Shots", 
                          sliderInput("n",
                                      "Number of observations:",
                                      value = 100,
                                      min = 1,
                                      max = 200),
                          
                          
                          plotOutput("HistTrueFreq")),
                 
                 tabPanel("Cluster Analysis", 
                              # # Select type of trend to plot
                              # selectInput(inputId = "type2", label = strong("Choose a Side"),
                              #             choices = c("Offensive Stats", "Defensive Stats"),
                              #             selected = "Offensive Stats"),
                              # 
                              plotOutput("plot1"),
                              tableOutput("data_table")
                              ),
                 
                             tabPanel("Random Forrest", 

                              plotOutput("plot2"))
  
                          )#tabsetPanel
                        
               
               
               )#mainPanel

            # )#sidebarLayout
  
    )#fluidPage
  )#shinyUI

