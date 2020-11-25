library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(tidyr)

#setwd("/Users/oceanesalmeron/Documents/Data Analytics/airbnb_listings_analysis")


plot_data<- function(data, features, graph_type){
  if(graph_type == "Average"){
    p <-ggplot(data, aes(x=city, y=avg, fill=city))
    p + geom_bar(stat = "identity")+
      #ylab(paste(graph_type,input$features))+
      xlab("City")+
      guides(fill = FALSE) +
      geom_text(aes(label=avg), vjust=1.6, color="white", size=3.5) +
      scale_fill_brewer(palette = "Set2")
  }
  
  else if(graph_type == "Distribution"){
    p <-ggplot(data, aes(x=features[[2]], y=features[[1]], colour=city))+
      scale_y_continuous(limits = quantile(features[[1]], c(0.1, 0.9), na.rm = T))+
      scale_colour_brewer(palette="Set2")
    
    p + geom_boxplot(outlier.shape = NA) +
      facet_wrap(~ city,scale="free")  +
      ylab("Estimated revenue for the next 30 days")+
      guides(colour = FALSE) 
  }
  
  else if(graph_type == "Histogram"){
    #graph <- data %>% 
    # gather(city, features[[1]])
    
    #ggplot(graph, aes(x = features[[1]], fill = city)) +
    # geom_histogram(binwidth = 0.5, color = "black")
  }
  
  else if(graph_type == "Density"){
    
    if(length(features) <= 1){
      p<-data %>%
        ggplot( aes(x=features[[1]])) +
        geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
        facet_wrap(~ city,scale="free")  +
        scale_x_continuous(limits = quantile(features[[1]], c(0.1, 0.9), na.rm = T)) 
    }
    else{
      p<-data %>%
        ggplot( aes(x=features[[1]], fill=features[[2]])) +
        geom_density(color="#e9ecef",alpha=0.8) +
        scale_fill_brewer(palette="Set2")+
        facet_wrap(~ city,scale="free")  +
        scale_x_continuous(limits = quantile(features[[1]], c(0.1, 0.9), na.rm = T)) 
    }
    
  }
  
  return (p)
}