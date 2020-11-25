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
            #uiOutput("date_slider"),
            dateRangeInput("date_range", "Select the:",
                           start = (listings%>%summarise(min = min(as.Date(date)), max = max(as.Date(date))))$min,
                           end = (listings%>%summarise(min = min(as.Date(date)), max = max(as.Date(date))))$max, 
                           min = (listings%>%summarise(min = min(as.Date(date)), max = max(as.Date(date))))$min,
                           max = (listings%>%summarise(min = min(as.Date(date)), max = max(as.Date(date))))$max,
                           format = "yyyy-mm-dd"
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


map <- sidebarLayout(
          sidebarPanel(
            class="control",
            #Control Widget
            selectInput(
              inputId = "cities_t2",
              label = "Select city:",
              choices = unique(listings$city),
              selected = "lisbon"
            ),
            selectInput(
              inputId = "housing",
              label = "Select housing type:",
              choices = c("Loading"),
              selected = ""
            ),
            sliderInput(
              inputId = "price_slider",
              label = "Price range:",
              min = 0,
              max = 1000,
              value = c(0,1000),
              round = TRUE
            ),
            textOutput("try"),
            sliderInput(
              inputId = "availability_slider",
              label = "Availability range:",
              min = 0,
              max = max(listings$availability_30),
              value = c(0,max(listings$availability_30)),
              round = TRUE
            ),
            selectInput(
              inputId = "date_city",
              label = "Select dates",
              choices = c()
            )
            #uiOutput("date_city")
          ),
          mainPanel(
            wellPanel(
              id="body-panel",
              tags$div(class="section-title",h3("Map")),
              tags$div(class="marg",leafletOutput("city_map"))
            )
          )
        )

insight<- sidebarLayout(
            sidebarPanel(
              selectInput(
                inputId = "yoooo",
                label = "Pick a feature",
                choices = unique(listings$city),
                selected = "lisbon"
              )
            ),
            mainPanel(
              wellPanel(
                id="body-panel",
                tags$div(class="section-title",h3("Proportion of house type")),
                tags$div(class="marg",plotOutput("plot2"))
              )
            )
        )
  
tab2<- fluidRow(
          h3("Deep dive into one city - ",textOutput("dive_city")),
          fluidRow(
            class="insights w-100",
            column(4, class="row-sm-4 h-100 card", p('Number of listings'), textOutput("total_listing")%>%tagAppendAttributes(class = 'res')),
            column(4, class="card", p('Average price'), textOutput("avg_price")%>%tagAppendAttributes(class = 'res'))
          ),
          map%>%tagAppendAttributes(class = "no-padding"),
          insight%>%tagAppendAttributes(class = "no-padding")
  )
  
  
  
# Define UI for application that draws a histogram
shinyUI(fluidPage(#theme = "bootstrap.css",
  includeCSS("www/bootstrap.css"),
  # Application title
  navbarPage("AirBnB listings analysis",
             tabPanel("Analysis 1", tab1),
             tabPanel("Analysis 2", tab2)
  )
  # Grid Layout
  # fluidRow(
  #   wellPanel(
  #     class="title-bar",
  #     img(src='logo-white.png', id="logo"),
  #     titlePanel("AirBnB listings analysis")
  #     )
  #   ),
  # tabsetPanel(type = "tabs",
  #             tabPanel("Analysis 1", tab1),
  #             tabPanel("Analysis 2", tab2)
  #             )
  
  
  
))
