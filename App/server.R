library(shiny)
library(ggplot2)
library(leaflet)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
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
    data <- choosen_cities() %>%
              group_by(city)
      
    if(input$Dim == "House type"){
      data <- choosen_cities() %>%
        group_by(city, room_type)
    }
    else if(input$Dim == "House size"){
      data <- choosen_cities() %>%
        group_by(city, bedrooms)
    }
    else if(input$Dim == "Neighborhood"){
      data <- choosen_cities() %>%
        sample_n(size=20)%>%
        group_by(city, neighbourhood_cleansed)
    }
    
    data %>%
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
                                "House size" = choosen_cities()$bedrooms,
                                "Neighborhood" = choosen_cities()$neighbourhood_cleansed)
    
    res <- list(feature,additional_feature)

  }))

  
  output$plot1 <- renderPlot({
    
    features <- select_features()

    if(input$grap_type == "Average"){
      if(input$Dim == "None"){
        p <-ggplot(average(), aes(x=city, y=avg, fill=city))
        p + geom_bar(stat = "identity")+
          ylab(paste(input$grap_type,input$features))+
          xlab("City")+
          guides(fill = FALSE) +
          geom_text(aes(label=avg), vjust=1.6, color="white", size=3.5) +
          scale_fill_brewer(palette = "Set2")
      }
      else{
        avg_data <- average()
        avg_ft <- switch(input$Dim,
                                     "House type" = average()$room_type,
                                     "House size" = average()$bedrooms,
                                     "Neighborhood" = average()$neighbourhood_cleansed)
        
        p <-ggplot(average(), aes(x=avg_ft, y=avg, fill=avg_ft))
        p + geom_bar(stat = "identity")+
          ylab(paste(input$grap_type,input$features))+
          xlab("City")+
          facet_wrap(~city, ncol=2)+
          guides(fill = FALSE) +
          scale_fill_brewer(palette = "Set2")
      }
    }

    else if(input$grap_type == "Distribution"){
      p <-ggplot(choosen_cities(), aes(x=features[[2]], y=features[[1]], colour=city))+
        scale_y_continuous(limits = quantile(features[[1]], c(0.1, 0.9), na.rm = T))+
        scale_colour_brewer(palette="Set2")

      p + geom_boxplot(outlier.shape = NA) +
        facet_wrap(~ city, ncol=2)  +
        ylab(paste("Estimated", input$features, "for the next 30 days"))+
        xlab("Feature")+
        guides(colour = FALSE)
    }

    else if(input$grap_type == "Histogram"){

      if(input$Dim == "None"){
        choosen_cities() %>%
          ggplot( aes(x=features[[1]])) +
          geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
          facet_wrap(~ city, ncol=2) + 
          xlab(input$features)+
          scale_x_continuous(limits = quantile(features[[1]], c(0.1, 0.9), na.rm = T))
      }
      else{
        choosen_cities() %>%
          ggplot( aes(x=features[[1]], fill=features[[2]])) +
          geom_histogram(color="#e9ecef",alpha=0.8) +
          scale_fill_brewer(palette="Set2")+
          facet_wrap(~ city, ncol=2) + 
          xlab(input$features)+
          labs(fill=input$Dim)+
          scale_x_continuous(limits = quantile(features[[1]], c(0.1, 0.9), na.rm = T))
      }
    }

    else if(input$grap_type == "Density"){

      if(input$Dim == "None"){
        choosen_cities() %>%
          ggplot( aes(x=features[[1]])) +
          geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
          facet_wrap(~ city, ncol=2)  +
          xlab(input$features)+
          scale_x_continuous(limits = quantile(features[[1]], c(0.1, 0.9), na.rm = T))
      }
      else{
        choosen_cities() %>%
          ggplot( aes(x=features[[1]], fill=features[[2]])) +
          geom_density(color="#e9ecef",alpha=0.8) +
          scale_fill_brewer(palette="Set2")+
          facet_wrap(~ city,scale="free")  +
          xlab(input$features)+
          labs(fill=input$Dim)+
          scale_x_continuous(limits = quantile(features[[1]], c(0.1, 0.9), na.rm = T))
      }

    }
    
  })
  
  #Tab2
  choosen_city <- reactive({
    listings %>% 
      filter(city == input$cities_t2)%>%
      distinct(id, .keep_all = TRUE)
      
  })
  
  observeEvent(input$cities_t2,{
    updateSelectInput(session, "date_city", choices = unique(choosen_city()$date), selected = choosen_city()$date[1])
    updateSelectInput(session, "housing", choices=unique(choosen_city()$room_type), selected = choosen_city()$room_type[1])
    updateSelectInput(session, "nb_bed", choices=sort(unique(choosen_city()$beds[choosen_city()$beds!=0])), selected = choosen_city()$beds[1])
    updateSliderInput(session, "availability_slider", max=max(choosen_city()$availability_30), value= c(0,max(choosen_city()$availability_30)))
  })
  
  output$try<- renderText({
    paste(input$price_slider[1])
  })
  
  output$dive_city <- renderText(
    paste0(input$cities_t2)
  )
  
  choosen_options <- reactive({
    data <- choosen_city()%>%
              filter(date == input$date_city,
                     room_type == input$housing,
                     beds == input$nb_bed,
                     availability_30 <= input$availability_slider,
                     price <= input$price_slider[2],
                     price >= input$price_slider[1])
  })
  
  output$total_listing <- renderText({
    total <- count(choosen_options())$n
  })
  
  output$avg_price <- renderText({
    df <- choosen_options()%>%
      summarise(avg = mean(price))
    
    avg_price<-df$avg
  })
  
  output$median <- renderText({
    df <- choosen_options()%>%
      summarise(median = median(price))
    
    avg_price<-df$median
  })
  
  output$city_map <- renderLeaflet({
    
    map<-choosen_options()%>%
          leaflet() %>% 
          addTiles()%>%
          addMarkers(clusterOptions = markerClusterOptions(),
                     popup = paste0("Details <br> Type: ",choosen_options()$room_type,
                                   "  -  Beds:",
                                   choosen_options()$beds,
                                   '<br>',choosen_options()$price,'$',
                                   '<br><a href="https://www.airbnb.com/rooms/',
                                   choosen_options()$id,
                                   '">Go to ad!</a>')
                     )
  })
  
  select_features_tab2 <- reactive({
    if(input$tab2_ft == "Neighborhood"){
      data <- choosen_city()%>%sample_n(size=20)
    }else{
      data <- choosen_city()
    }
    
    feature <- switch(input$tab2_ft,
                      "House type" = data$room_type,
                      "House size" = data$bedrooms,
                      "Neighborhood" = as.character(data$neighbourhood_cleansed))
    
    additional_feature <- switch(input$tab2_dim,
                                 "None"=data$city,
                                 "Availability" = data$availability_30,
                                 "Revenue" = data$revenue_30,
                                 "Price" = data$price)
    
    features <- list(feature, additional_feature)
  })
  
  output$plot_pro <- renderPlot({
  
    if(input$tab2_ft == "Neighborhood"){
      data <- choosen_city()%>%sample_n(size=20)
    }else{
      data <- choosen_city()
    }
    features <- select_features_tab2()
    p <-ggplot(data, aes(x=city, fill=features[[1]]))
    p + geom_bar(position ="fill", width=1, color="white") +
      coord_polar("y", start=0)+
      scale_fill_brewer(palette="Set2") +
      labs(fill=input$tab2_ft)+
      theme_void()
  })
  
  
  
  output$plot_den <- renderPlot({
    if(input$tab2_ft == "Neighborhood"){
      data <- choosen_city()%>%sample_n(size=20)
    }else{
      data <- choosen_city()
    }
    features <- select_features_tab2()
    data %>%
      ggplot( aes(x=features[[2]], fill=features[[1]])) +
      geom_density(color="#e9ecef",alpha=0.8) +
      scale_fill_brewer(palette="Set2")+
      facet_wrap(~ city,scale="free")  +
      xlab(input$tab2_dim)+
      labs(fill=input$tab2_ft)+
      scale_x_continuous(limits = quantile(features[[2]], c(0.1, 0.9), na.rm = T))
  })
  
  output$plot_avg <- renderPlot({
    if(input$tab2_ft == "Neighborhood"){
      data <- choosen_city()%>%sample_n(size=20)
    }else{
      data <- choosen_city()
    }
  
    
    if(input$tab2_ft == "House type"){
      data <- choosen_cities() %>%
        group_by(city, room_type)
    }
    else if(input$tab2_ft == "House size"){
      data <- choosen_cities() %>%
        group_by(city, bedrooms)
    }
    else if(input$tab2_ft == "Neighborhood"){
      data <- choosen_cities() %>%
        sample_n(size=20)%>%
        group_by(city, neighbourhood_cleansed)
    }
    
    data <- data %>%
      summarise(avg = mean(switch(input$tab2_dim,
                                  "Availability" = availability_30,
                                  "Revenue" = revenue_30,
                                  "Price" = price)))
    
    ft_avg <- switch(input$tab2_ft,
                        "House type" = data$room_type,
                        "House size" = data$bedrooms,
                        "Neighborhood" = as.character(data$neighbourhood_cleansed))
    
    p <- ggplot(data, aes(fill=ft_avg, y=avg, x=ft_avg)) 
    p + geom_bar(position="dodge", stat="identity")+
      scale_fill_brewer(palette="Set2") +
      xlab(input$tab2_ft)+
      labs(fill=input$tab2_ft)
  })
  
  output$plot_dis <- renderPlot({
    if(input$tab2_ft == "Neighborhood"){
      data <- choosen_city()%>%sample_n(size=20)
    }else{
      data <- choosen_city()
    }
    features <- select_features_tab2()
    p <-ggplot(data, aes(x=features[[1]], y=features[[2]], colour=features[[1]]))+
      scale_y_continuous(limits = quantile(features[[2]], c(0.1, 0.9), na.rm = T))+
      scale_colour_brewer(palette="Set2")
    
    p + geom_boxplot(outlier.shape = NA) +
      xlab(input$tab2_ft)+
      ylab(input$tab2_dim)+
      guides(colour = FALSE)
  })
  
  
  
  
})
