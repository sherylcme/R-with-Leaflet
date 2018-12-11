#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Importing libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(dplyr)
library(ggmap)
library(htmltools)
library(DT)

# Importing dataset
fbi_data <- read.csv('../data/database.csv')

# Stats on fbi_data on Solved Rate
us <- fbi_data %>%
  mutate(Solved = ifelse(Crime.Solved == "Yes", 1, 0)) %>%
  filter(Crime.Type == "Murder or Manslaughter")

# us data
states <- readOGR('../data/cb_2016_us_state_500k/cb_2016_us_state_500k.shp')

# To make the naming of the state is consistent
levels(us$State)[40] <- "Rhode Island"
states <- subset(states, is.element(states$NAME, us$State))

# Ordering data
us <- us[order(match(us$State, states$NAME)),]

# Set Bins
bins <- c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
pal <- colorBin("RdYlBu", domain = c(0,1), bins = bins)


# Define UI for application
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Homicide Dashboard"),
  dashboardSidebar(
    sliderInput("date_range", label = "Date Range",
                min = min(us$Year),
                max = max(us$Year),
                value = c(min(us$Year), max(us$Year)),
                sep = "",
                step = 1)
  ),
  dashboardBody(
    fluidRow(box(width = 12, leafletOutput(outputId = "mymap"))),
    fluidRow(box(width = 12, dataTableOutput(outputId = "summary_table")))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # User input
  data_input <- reactive({
    us %>%
      filter(Year >= input$date_range[1]) %>%
      filter(Year <= input$date_range[2]) %>%
      group_by(State) %>%
      summarise(Num_Murders = n(),
                Num_Solved = sum(Solved)) %>%
      mutate(Num_Unsolved = Num_Murders - Num_Solved,
             Solved_Rate = Num_Solved / Num_Murders)
  })
  
  
  data_input_ordered <- reactive({
    data_input()[order(match(data_input()$State, states$NAME)),]
  })
  
  labels <- reactive({
    paste("<p>", data_input_ordered()$State, "</p>",
          "<p>", "Solved Rate: ", round(data_input_ordered()$Solved_Rate, digits=3), "</p>",
          sep = "")
  })
  
  output$mymap <- renderLeaflet(
    leaflet() %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolygons(data = states,
                  weight = 1,
                  smoothFactor = 0.5,
                  color = "white",
                  fillOpacity = 0.8,
                  fillColor = pal(data_input_ordered()$Solved_Rate),
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE
                  ),
                  label = lapply(labels(), HTML)) %>%
      addLegend(pal = pal, 
                values = data_input_ordered()$Solved_Rate,
                opacity = 0.7,
                position = "topright")
  )
  
  output$summary_table <- renderDataTable(data_input())
}

# Run the application 
shinyApp(ui = ui, server = server)

