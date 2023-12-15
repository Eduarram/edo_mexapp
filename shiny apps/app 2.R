library(tidyverse)
library(plotly)
library(shiny)


sidebar_histogram <- sidebarPanel(
  selectInput("varX","Histogram variable", choices = g, selected = "GINI")
)

body_histogram <- fluidRow(
  plotlyOutput("histogram")
)

server_histogram <- function(input, output){
  output$histogram <- renderPlotly({
    g1 <- ggplot(datos_Edomex, aes_string(x=input$varX)) + 
      geom_histogram(color="#000000", fill="green") + 
      labs(title = "histogram forgini index", x="variable", y="count") +
      geom_density(color = "#000000", fill = "blue", alpha = 0.6) +
      theme(panel.background = element_rect(fill = "gray20"),
            panel.grid.major = element_line(colour = "black"))
    
    ggplotly(g1)
  })
}

shinyApp(ui = fluidPage(sidebar_histogram, body_histogram), server_histogram)
