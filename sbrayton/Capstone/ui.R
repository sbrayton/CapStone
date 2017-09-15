source('data.R', local = TRUE)

shinyUI(fixedPage(

  checkboxInput("showScatter", "Scatter Plot"),
  conditionalPanel(
    condition = "input.showScatter == true",
    selectInput("smoothMethod", "Method",
                list("lm", "glm", "gam", "loess", "rlm"))
  ) ,
  
  mainPanel(
    tableOutput("data_table")
  )
))

