library(shiny)

# draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Hello Shiny!"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      sliderInput("srls",
                  "Number of ...:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # plot 
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
