
#datos_Edomex <- datos_Edomex %>% dplyr::filter(!Monthly.Wage == -Inf)

library(tidyverse)
library(plotly)
library(shiny)

sidebar <- sidebarPanel(
selectInput("VariableX", "X-axis Variable", choices = g , selected = 'education'),
selectInput("VariableY", "Y-axis Variable", choices = g, selected = 'GINI')
)

# Scatterplot
body <- fluidRow(
  plotlyOutput("scatterplot")
)


# Define the server function
server <- function(input, output) {
  output$scatterplot <- renderPlotly({
    
    gg <- ggplot(data = datos_Edomex, aes_string(x = input$VariableX, 
                                                 y = input$VariableY)) +
      geom_point(color = "blue", size=1.6, shape=10) + 
      geom_density_2d(color="green") + 
      theme(panel.background = element_rect(fill = "gray20"),
            panel.grid.major = element_line(colour = "black"))
    
    
    
    ggplotly(gg)
  })
}

# Combine components
shinyApp(ui = fluidPage(sidebar, body), server)


