library(xts)
library(shiny)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(data.table)

all <- readRDS("Data/GunViolence.rds")

ui <- navbarPage("US Gun Violence", id="nav",
                 tabPanel("Interactive Map",
                          div(class="outer",
                              tags$head(
                                includeCSS("Assets/styles.css")
                              ),
                              leafletOutput("map", width="100%", height="100%"), 
                              absolutePanel(id = "controls", 
                                            class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            h3("US Gun Violence"),
                                            h4("2014-2018"),
                                            radioButtons("incidentweight", "Incident Factor:",
                                                         c("Killed"="Killed", "Injured"="Injured"), 
                                                         selected = "Killed", inline=TRUE),
                                            sliderInput(inputId = "date", label = "Date Range",
                                                        min = min(all$Date), 
                                                        max = max(all$Date),
                                                        value = max(all$Date),
                                                        ticks = TRUE,
                                                        step=365/12, 
                                                        animate = animationOptions(interval = 1000,
                                                                                   playButton = icon('play', "fa-2x"),
                                                                                   pauseButton = icon('pause', "fa-2x"))),
                                            textOutput("counts")
                              ),
                              tags$div(id="cite",
                                       'Data source:  The Gun Violence Archive, 2014-2018.'
                              )
                          )
                 )
                          )

server <- function(input, output, session) {
  history <- reactive({
    all %>%
      filter(Date <= input$date)
  })
  
  color <- reactive({
    if (input$incidentweight == "Killed") {
      col = "OrRd"
    } else {
      col = "YlGn"
    }
  })
  
  sc <- reactiveVal(7000)
  
  observeEvent(input$incidentweight, {
    if (input$incidentweight == "Killed") {
      newValue <- 7000
      sc(newValue)
    } else {
      newValue <- 4000
      sc(newValue)
    }
  })
  
  name <- reactive({
    if (input$incidentweight == "Killed") {
      nam = "Killed"
    } else {
      nam = "Injured"
    }
  })
  
  output$counts <- renderText({
    c <- sum(history()[[input$incidentweight]])
    paste("Total ", name(), ": ", c)
  })
  
  colorpal <- reactive({
    colorNumeric(color(), all[[input$incidentweight]])
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
     addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
              attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>,
              <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; 
              <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%  # Add default OpenStreetMap map tiles%>%
      addLegend(position = "bottomleft",
                pal = colorpal(), values = all[[input$incidentweight]],
                title = name()) %>%
      setView(lng = -83.7129, lat = 37.0902, zoom = 4)
  })
  
  observe({
    pal <- colorpal()
    proxy <- leafletProxy("map", data = history()) 
    proxy %>%
      clearShapes() %>%
      addCircles(lng = ~lon,
                 lat = ~lat,
                 radius = ~history()[[input$incidentweight]] * sc(),
                 weight = 1,
                 popup= ~Content,
                 color = "white",
                 fillColor = ~pal(history()[[input$incidentweight]]),
                 stroke = F, 
                 fillOpacity = 0.8,
                 data = history()
      ) 
  })
}

shinyApp(ui, server)
