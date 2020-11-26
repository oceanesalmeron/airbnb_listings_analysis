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
              selected = c("lisbon","amsterdam")
            ),
            selectInput(
              inputId = "features",
              label = "Features",
              choices = c("Availability", "Revenue", "Price")
            ),
            selectInput(
              inputId = "Dim",
              label = "Dim",
              choices = c("None", "House type", "House size", "Neighborhood")
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
            wellPanel(
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
              inputId = "housing",
              label = "Select housing type:",
              choices = c("Loading"),
              selected = ""
            ),
            selectInput(
              inputId = "nb_bed",
              label = "Select number of beds:",
              choices = c(),
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

insight<- fluidRow(
            class="insights-dive",
            wellPanel(
              class="w-100 controller",
              selectInput(
                inputId = "tab2_ft",
                label = "Pick a feature",
                choices = c("House type", "House size", "Neighborhood"),
                selected = "House type"
              ),
              selectInput(
                inputId = "tab2_dim",
                label = "Add a dimension",
                choices = c("Availability", "Revenue", "Price")
              )
            ),
            mainPanel(
              class="plot-group",
              wellPanel(
                class="panel-plot",
                id="body-panel",
                tags$div(class="section-title",h3("Proportion")),
                tags$div(class="marg",plotOutput("plot_pro"))
              ),
              wellPanel(
                class="panel-plot",
                id="body-panel",
                tags$div(class="section-title",h3("Distribution")),
                tags$div(class="marg",plotOutput("plot_dis"))
              ),
              wellPanel(
                class="panel-plot",
                id="body-panel",
                tags$div(class="section-title",h3("Density")),
                tags$div(class="marg",plotOutput("plot_den"))
              ),
              wellPanel(
                class="panel-plot",
                id="body-panel",
                tags$div(class="section-title",h3("Average")),
                tags$div(class="marg",plotOutput("plot_avg"))
              )
            )
        )
  
tab2<- fluidRow(
          h3("Deep dive into one city - ",textOutput("dive_city")),
          wellPanel(
            class="controller",
            selectInput(
              inputId = "cities_t2",
              label = "Select city:",
              choices = unique(listings$city),
              selected = "lisbon"
            ),
            selectInput(
              inputId = "date_city",
              label = "Select dates",
              choices = c()
            )
          ),
          fluidRow(
            class="insights w-100",
            column(3, class="row-sm-4 h-100 card", p('Number of listings'), textOutput("total_listing")%>%tagAppendAttributes(class = 'res')),
            column(3, class="card", p('Average price'), textOutput("avg_price")%>%tagAppendAttributes(class = 'res')),
            column(3, class="card", p('Median price'), textOutput("median")%>%tagAppendAttributes(class = 'res'))
          ),
          map%>%tagAppendAttributes(class = "no-padding"),
          insight%>%tagAppendAttributes(class = "no-padding")
  )
  
tab3<-wellPanel(
  tags$h1("Airbnb listings analysis project"),
  tags$p("By Océane Salmeron, Nathan Lancman, Charles Mabbyalas, Baptise Lafay (ING5, BDA 2)"),
  tags$h3("Project"),
  tags$p("This is the listings analysis project for the Data Analytics course at ECE Paris."),
  tags$p("This project allows you to visualize Airbnb Data for those five cities :"),
  tags$li("Amsterdam"),
  tags$li("Berlin"),
  tags$li("Munich"),
  tags$li("Lisbon"),
  tags$li("Porto"),
  tags$p("You can access the project here : link"),
  tags$a(href=""),
  tags$h3("Data"),
  tags$p("Airbnb data was provided by insideAirbnb.com"),
  tags$h2("Installation"),
  tags$h3("Requirement"),
  tags$p("Dplyr, ggplot2, shiny are required libraries."),
  tags$h2("Install libraries"),
  tags$p("Run in your console: "),
  tags$li("`install.packages('dplyr')`"),
  tags$li("`install.packages('ggplot2')`"),
  tags$li("`install.packages('shiny')`"),
  tags$h3("Clone repository"),
  tags$p("To clone the repo run `https://github.com/oceanesalmeron/airbnb_listings_analysis.git` in your terminal"),
)
  
  
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  includeCSS("www/bootstrap.css"),

  navbarPage(title=div(img(src='logo-white.png', id="logo"), "AirBnB listings analysis"),
             tabPanel("Analysis 1", h3("Comparing cities"),tab1),
             tabPanel("Analysis 2", tab2),
             tabPanel("Documentation", tab3),
             selected = "Analysis 1"
  )
))
