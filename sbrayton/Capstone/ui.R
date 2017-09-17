library(shiny)
library(shinyjs)

source('data.R', local = TRUE)


shinyUI(fluidPage(
  
  titlePanel("Blown Away DMR Assignment"),
  sidebarLayout(
    sidebarPanel(
   
      # Select type of trend to plot
      selectInput(inputId = "type", label = strong("Choose a Side"),
                  choices = c("Offensive Stats", "Defensive Stats"),
                  selected = "Offensive Stats"),
 
      # Select date range to be plotted
      dateRangeInput("date", strong("Date range"), start = "2007-01-01", end = "2017-07-31",
                     min = "2007-01-01", max = "2017-07-31")     

      ),
    
    
    mainPanel( tableOutput("data_table") ,

               # tableOutput("data_table2"),
               # 
               # tableOutput("data_table3"),
               # 
               # tableOutput("data_table4"),
               # 
               # plotOutput('plot1')
               
               # plotOutput('plot2')
               tabsetPanel(
                 tabPanel("Cluster Analysis", 
                              # # Select type of trend to plot
                              # selectInput(inputId = "type2", label = strong("Choose a Side"),
                              #             choices = c("Offensive Stats", "Defensive Stats"),
                              #             selected = "Offensive Stats"),
                              # 
                              # plotOutput("plot1")
                              # 
                              # ), 
                             tabPanel("Random Forrest", plotOutput("plot2")), 
                             tabPanel("Table1", tableOutput("data_table1")), 
                             tabPanel("Table2", tableOutput("data_table2"))
                          )#tabsetPanel
                        )
               
               
               )#mainPanel

            )#sidebarLayout
  
    )#fluidPage
  )#shinyUI

