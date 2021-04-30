install.packages("shiny")

library(shiny)
runExample("01_hello")

#UI OBJECT FOR THE HELLO SHINY EXAMPLE
#define ui for app that draws a histogram
ui = fluidPage(
  
  #app title
  titlePanel("Hello World!"),
  
  #sidebar layout with input and output definitions
  sidebarLayout(
    
    #sidebar panel for inputs
    sidebarPanel(
      
      #input: slider for the number of bins
      sliderInput(inputId = "bins",
                  label = "number of bins:",
                  min = 5,
                  max = 50,
                  value = 30)

    ),
    
    #main panel for displaying outputs
    mainPanel(
      
      #output: histogram
      plotOutput(outputId = "distPlot")
      
      
    )
  )
)




#SERVER
#here is the server function for the hello shiny example
#define server logic required to draw a histogram
server = function(input, output) {
  
  #histogram of the old faithful geyser data
  #with requested number of bins
  #this expression that generates a histogram is wrapped in a call
  #to renderPlot to indicate that:
  #1. it is "reactive" and therefore should be automatically 
  #   re-executed when inputs (input$bins) change
  #2. its output type is a plot
  output$distPlot = renderPlot({
    
    
    x   <- faithful$waiting
    bins<- seq(min(x), max(x), length.out = input$bins + 1)
    
    
    hist(x, breaks = bins, col = "#75AADB", border = "orange",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")

    
      })

  
}

