library(shiny)
library(leaflet)
library(RColorBrewer)
library(sp)

# Crear un dataframe de ejemplo
d <- data.frame(
  lat = c(40.7128, 34.0522, 41.8781),
  lon = c(-74.0060, -118.2437, -87.6298),
  vble = c(10, 20, 30),
  vble2 = c(5, 15, 25)
)

# Paleta de colores
pal <- colorRampPalette(brewer.pal(9, "YlOrRd"))

# Crear la aplicación Shiny
ui <- fluidPage(
  selectInput("variable", "Seleccionar variable:",
              choices = c("vble", "vble2")),
  leafletOutput("map")
)

server <- function(input, output, session) {
  # Renderizar el mapa
  output$map <- renderLeaflet({
    # Crear el mapa inicial
    leaflet(d) %>%
      addTiles() %>%
      setView(lng = -95.7128, lat = 37.0902, zoom = 4)
  })
  
  # Observar cambios en la variable seleccionada
  observe({
    # Obtener la variable seleccionada
    selected_variable <- input$variable
    
    # Actualizar el mapa con la nueva variable
    leafletProxy("map") %>%
      clearShapes() %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~get(selected_variable),
        color = ~pal(8)[cut(get(selected_variable), breaks = 8)],
        fillOpacity = 0.7,
        popup = ~paste("Valor", get(selected_variable))
      )
  })
}

# Correr la aplicación Shiny
shinyApp(ui, server)






###############################################################################################

maps_creator = function(data, columna="GINI", map_title="indice Gini"){
  tm_shape(Edo_Mex) +
    tm_fill(col = columna, style = "jenks", pallete="Blues") +
    #tm_style(stylis) +
    tm_polygons(col="black") +
    tm_layout(title =map_title,
              legend.text.fontface ="plain",
              legend.title.color = "black",
              legend.position = c("right", "bottom"))
}


ma1 <- maps_creator(data=Edo_Mex, columna = "Total.Investment", map_title = "inversión total")#stylis = "classic")

ma2 <- maps_creator(data=Edo_Mex, columna = "population", map_title = "poblacion" 
)#stylis = "cobalt")

ma3 <- maps_creator(data = Edo_mex, columna = "Total.Income", map_title = "ingresos totales")#stylis = "bw")

ma4 <- maps_creator(data = Edo_Mex, columna = "Total.Expenses", map_title = "gasto_total"
)#stylis = "col_blind")



tmap_arrange(ma1, ma2, ma3, ma
################################################################################################################################



Edo_mex <- load("C:/Users/eduar/OneDrive/Desktop/proyecto salud/healt proyect/data/edo_mex.RData")

f <- names(Edo_Mex)[!(names(Edo_Mex) %in% c("NAME_1", "NAME_2", "geometry"))]

selectInput("VariableX", "correlacion X",choices= f, selected = 'education')
selectInput("VariableY", "correlacion y", choices = f, selected = 'GINI')





renderPlotly({
  gg <- ggplot(data=Edo_Mex, maping=aes(x=Edo_Mex[, input$VariableX], y=education)) +
    geom_point() +  
    theme_linedraw()
  plotly(gg)  
})

#################################################################################

sidebar <- selectInput("VariableX", "X-axis Variable", choices = colnames(datos_Edomex), selected = 'education')

body <- fluidRow(
  plotlyOutput("histogram")
)

server <- function(input, output){
  output$histogram <- renderPlotly({ 
    
    g1 <- ggplot(data =data_Edomex, mapping = aes(x=input$VariableX)) +
      geom_histogram(bins=20, fill="#41B7C4") +
      geom_density(color="#0033FF")
    
    ggplotly(g1)
    
  })
}

shinyApp(ui=fluidPage(sidebar, body), server)
