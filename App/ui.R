library(shiny)
library(shinythemes)
library(ggplot2)
library(leaflet)

tab1 <- sidebarLayout(
          sidebarPanel(
            class="control",
            #Control Widget
            checkboxGroupInput(
              inputId = "input_1",
              label = "Choose cities",
              choices = unique(listings$city),
              selected = "lisbon"
            ),
            selectInput(
              inputId = "features",
              label = "Features",
              choices = c("Availability", "Revenue", "Price")
            ),
            selectInput(
              inputId = "Dim",
              label = "Dim",
              choices = c("None", "House type", "Number of beds", "Neighborhood")
            ),
            selectInput(
              inputId = "grap_type",
              label = "Plot type",
              choices = c("Average", "Distribution", "Density", "Histogram")
            )
          ),
          mainPanel(
            h3("Comparing cities"),
            wellPanel(
              textOutput("cities"),
              textOutput("plot_type"),
              textOutput("plot_title"),
              plotOutput("plot1", click = "plot_click")
            )
            
          )
        )


tab2 <- sidebarLayout(
          sidebarPanel(
            class="control",
            #Control Widget
            selectInput(
              inputId = "cities_t2",
              label = "Choose a city",
              choices = unique(listings$city),
              selected = "lisbon"
            )
          ),
          mainPanel(
            h3("Deep dive into one city"),
            fluidRow(
              class="insights w-100",
              column(4, class="row-sm-4 h-100 card", p('Number of listings'), textOutput("total_listing")%>%tagAppendAttributes(class = 'res')),
              column(4, class="card", p('Average price'), textOutput("avg_price")%>%tagAppendAttributes(class = 'res'))
            ),
            wellPanel(
              id="body-panel",
              tags$div(class="section-title",h3("Map")),
              tags$div(class="marg",leafletOutput("city_map"))
            )
            
          )
        )
  
  
  
  
  
# Define UI for application that draws a histogram
shinyUI(fluidPage(#theme = "bootstrap.css",
  includeCSS("www/bootstrap.css"),
  # Application title
  
  # Grid Layout
  fluidRow(
    wellPanel(
      class="title-bar",
      img(src='logo-white.png', id="logo"),
      titlePanel("AirBnB listings analysis")
      )
    ),
  tabsetPanel(type = "tabs",
              tabPanel("Analysis 1", tab1),
              tabPanel("Analysis 2", tab2)
              )
  
  
  
))
