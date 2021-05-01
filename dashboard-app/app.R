library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
source("data.R")

ui <- dashboardPage(
  
  dashboardHeader(title = "Iris Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Histograms", tabName = "histograms", icon = icon("chart-bar")),
      menuItem("Dispersion", tabName = "dispersion", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "histograms",
              fluidRow(
                box(width = 2,
                    selectInput(inputId = "select_species", label = "Select a species", 
                                choices = unique(df$Species), 
                                selected = 1)),
              ),
              fluidRow(
                box(
                  plotlyOutput(outputId = "hist_sepal_len")),
                box(
                  plotlyOutput(outputId = "hist_sepal_wid")),
                box(
                  plotlyOutput(outputId = "hist_petal_len")),
                box(
                  plotlyOutput(outputId = "hist_petal_wid")),
                
              )
      ),
      tabItem(
        tabName = "dispersion",
        fluidRow(
          box(
            plotlyOutput(outputId = "scatter_1")),
          box(
            plotlyOutput(outputId = "scatter_2"))
        )
      )
    )
  )
)



#SERVER
#define server logic required to draw a histogram
server <- function(input, output) {
  
  output$hist_sepal_len <- renderPlotly({
    data <- df[df$Species == input$select_species, ]
    plot <- plot.histogram(data, "Sepal.Length", "Sepal Length", "Frequency")
    ggplotly(plot)
  })
  
  output$hist_sepal_wid <- renderPlotly({
    data <- df[df$Species == input$select_species, ]
    plot <- plot.histogram(data, "Sepal.Width", "Sepal Width", "Frequency")
    ggplotly(plot)
  })
  
  output$hist_petal_len <- renderPlotly({
    data <- df[df$Species == input$select_species, ]
    plot <- plot.histogram(data, "Petal.Length", "Petal Length", "Frequency")
    ggplotly(plot)
  })
  
  output$hist_petal_wid <- renderPlotly({
    data <- df[df$Species == input$select_species, ]
    plot <- plot.histogram(data, "Petal.Width", "Petal Width", "Frequency")
    ggplotly(plot)
  })
  
  output$value_observations <- renderValueBox({
    valueBox(
      nrow(df[df$Species == input$select_species, ]), "Units Observed", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$value_sepal_len_mean <- renderValueBox({
    valueBox(
      round(mean(df[df$Species == input$select_species, "Sepal.Length"]),1), "Compr. Medium Sepal", icon = icon("calculator"),
      color = "teal"
    )
  })
  
  output$value_sepal_wid_mean <- renderValueBox({
    valueBox(
      round(mean(df[df$Species == input$select_species, "Sepal.Width"]),1), "Sepal Medium Width", icon = icon("calculator"),
      color = "navy"
    )
  })
  
  output$value_petal_len_mean <- renderValueBox({
    valueBox(
      round(mean(df[df$Species == input$select_species, "Petal.Length"]),1), "Compr. Medium Petal", icon = icon("calculator"),
      color = "light-blue"
    )
  })
  
  output$value_petal_wid_mean <- renderValueBox({
    valueBox(
      round(mean(df[df$Species == input$select_species, "Petal.Width"]),1), "Mean Petal Width", icon = icon("calculator"),
      color = "purple"
    )
  })
  
  output$scatter_1 <- renderPlotly({
    plot <- plot.scatter(df, "Petal.Length", "Sepal.Length", "Species", "Petal Length", "Sepal Length", "Species")
    ggplotly(plot)
  })
  
  output$scatter_2 <- renderPlotly({
    plot <- plot.scatter(df, "Petal.Width", "Sepal.Width", "Species", "Petal Width", "Sepal Width", "Species")
    ggplotly(plot)
  })
}
  
  
# Run the app
shinyApp(ui = ui, server = server)
