
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("FanDueleR"),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(
    numericInput("game_id_input"
                ,"Game Number"
                ,value = "")
    ,numericInput("table_id_input"
                  ,"Table Id"
                  ,value = "")
    ,numericInput("salary_cap_input"
                  ,"Salary Cap"
                  ,value = 60000)
    ,sliderInput("salary_range"
                 ,label = "Salary Range"
                 ,min = 3500
                 ,max = 12000
                 ,step = 100
                 ,value = c(3500,12000))
    ,numericInput("num_iter_input"
                  ,"Number of Simulations"
                  ,min = 1
                  ,max = 100000
                  ,value = 1000)
    ,actionButton("run_sim","Generate Lineup")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("plot")
    ,dataTableOutput("lineup")
  )
))
