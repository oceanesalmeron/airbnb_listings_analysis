library(shiny)
library(ggplot2)
library(leaflet)

source('../Scripts/plot_data.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$cities <- renderText({
    paste("Cities selected : ", paste(input$input_1, collapse = ", "))
  })
  
  output$features <- renderText({
    paste("You choose feature :", input$features)
  })
  
  output$features_2 <- renderText({
    paste("You choose add : ", input$Dim)
  })
  
  output$plot_type <- renderText({
    paste("You choose plot type :", input$grap_type)
  })
  
  output$plot_title <- renderText({
    x1 <- if(input$features=="price") "" else "over the next 30 days"
    x2 <- if(input$Dim=="None") "" else paste("and",input$Dim)
    text <- paste(input$grap_type, "of", input$features, x1, "of listings per city",x2)
    
  })
  
  choosen_cities <- reactive({
    listings %>% 
      filter(city %in% str_split(input$input_1," "),
             as.Date(date) <= input$date_range[2],
             as.Date(date) >= input$date_range[1]) %>%
      group_by(city)
  })
  
  output$date_slider <- renderUI({
    
    dates <- listings%>%summarise(min = min(as.Date(date)), max = max(as.Date(date)))
    min_date <- dates$min
    max_date <- dates$max
    
    dateRangeInput("date", "Select the date range:",
                   start = min_date,
                   end = max_date, 
                   min = min_date,
                   max =max_date,
                   format = "yyyy-mm-dd")
    
    
  })
  
  average <- reactive({
    choosen_cities() %>%
      summarise(avg = mean(switch(input$features,
                                  "Availability" = availability_30,
                                  "Revenue" = revenue_30,
                                  "Price" = price)))
  })
  
  
  select_features <- reactive(({
    
    feature <- switch(input$features,
                      "Availability" = choosen_cities()$availability_30,
                      "Revenue" = choosen_cities()$revenue_30,
                      "Price" = choosen_cities()$price)
    
    additional_feature <- switch(input$Dim,
                                "None"=choosen_cities()$city,
                                "House type" = choosen_cities()$room_type,
                                "Number of beds" = choosen_cities()$bedrooms,
                                "Neighborhood" = choosen_cities()$neighbourhood_cleansed)
    
    res <- list(feature,additional_feature)
    
    # p <-ggplot(choosen_cities(), aes(x=additional_feature, y=feature, colour=city))+
    #   scale_y_continuous(limits = quantile(feature, c(0.1, 0.9), na.rm = T))+
    #   scale_colour_brewer(palette="Set2")

  }))

  
  output$plot1 <- renderPlot({
    
    features <- select_features()
    
    # if(input$grap_type == "Average"){
    #   data <- average()
    # }
    # else{
    #   data <- choosen_cities()
    # }
    # 
    # plot_data(data, features, input$grap_type)

    
    if(input$grap_type == "Average"){
      p <-ggplot(average(), aes(x=city, y=avg, fill=city))
      p + geom_bar(stat = "identity")+
        ylab(paste(input$grap_type,input$features))+
        xlab("City")+
        guides(fill = FALSE) +
        geom_text(aes(label=avg), vjust=1.6, color="white", size=3.5) +
        scale_fill_brewer(palette = "Set2")
    }

    else if(input$grap_type == "Distribution"){
      p <-ggplot(choosen_cities(), aes(x=features[[2]], y=features[[1]], colour=city))+
        scale_y_continuous(limits = quantile(features[[1]], c(0.1, 0.9), na.rm = T))+
        scale_colour_brewer(palette="Set2")

      p + geom_boxplot(outlier.shape = NA) +
        facet_wrap(~ city,scale="free")  +
        ylab("Estimated revenue for the next 30 days")+
        guides(colour = FALSE)
    }

    else if(input$grap_type == "Histogram"){
      #graph <- choosen_cities() %>%
       # gather(city, features[[1]])

      #ggplot(graph, aes(x = features[[1]], fill = city)) +
       # geom_histogram(binwidth = 0.5, color = "black")
    }

    else if(input$grap_type == "Density"){

      if(input$Dim == "None"){
        choosen_cities() %>%
          ggplot( aes(x=features[[1]])) +
          geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
          facet_wrap(~ city,scale="free")  +
          scale_x_continuous(limits = quantile(features[[1]], c(0.1, 0.9), na.rm = T))
      }
      else{
        choosen_cities() %>%
          ggplot( aes(x=features[[1]], fill=features[[2]])) +
          geom_density(color="#e9ecef",alpha=0.8) +
          scale_fill_brewer(palette="Set2")+
          facet_wrap(~ city,scale="free")  +
          scale_x_continuous(limits = quantile(features[[1]], c(0.1, 0.9), na.rm = T))
      }

    }
    
  })
  
  #Tab2
  choosen_city <- reactive({
    listings %>% 
      filter(city == input$cities_t2)
  })
  
  output$total_listing <- renderText({
    total <- count(choosen_city())$n
  })
  
  output$avg_price <- renderText({
    df <- choosen_city()%>%
                  summarise(avg = mean(price))
    
    avg_price<-df$avg
  })
  
  output$city_map <- renderLeaflet({
    map<-choosen_city()%>%
          leaflet() %>% 
          addTiles()%>%
          addMarkers(clusterOptions = markerClusterOptions(),
                     popup = paste(choosen_city()$price)
                     )
  })
  
  
  
  
})
