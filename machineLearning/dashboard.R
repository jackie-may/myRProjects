#initialize
library(shiny)
library(ggplot2)
library(DT)


#housing data
housing = read.csv("housing.csv")
head(housing)
summary(housing)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Housing Price Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    # Sidebar with 2 select inputs and a numeric input
    sidebarPanel(
      selectInput("xCol", "X", names(housing)),
      selectInput("yCol", "Y", names(housing))),
  
    # Shows the plot
    mainPanel(plotOutput("plot"))
  ))

# Show a plot of the generated distribution
mainPanel(
  fluidRow(
    column(1,
      plotOutput("distPlot"))),
  
  fluidRow(
    column(2,
      selectInput("ocean_proximity", "Proximity to Ocean:",
                  c("NEAR BAY" = "NEAR BAY",
                    "<1H OCEAN" = "<1H OCEAN",
                    "INLAND" = "INLAND",
                    "NEAR OCEAN" = "NEAR OCEAN",
                    "ISLAND" = "ISLAND")),
      tableOutput("location")))
  
)


#server setup
server <- function(input, output, session) {
  df <- reactive({housing[, c(input$xCol, input$yCol)]})
  
  #creates the plot
  output$plot <- renderPlot({
    plot(df(), 
         pch = 20, 
         cex = 3, 
         col = "blue",
         main = "Housing Price dataset")})
  
  output$location <- renderTable({
    data <- df[df$housing == input$ocean_proximity, ]
    housing[, c("ocean_proximity", input$variable), drop = FALSE]
  }, rownames = TRUE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)