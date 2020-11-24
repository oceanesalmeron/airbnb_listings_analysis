library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(tidyr)

setwd("/Users/oceanesalmeron/Documents/Data Analytics/airbnb_listings_analysis")


prepare_country_data <- function(url, country, city, data_date){
  
  print('starting')
  #Load csv data from url in dataframe
  listings <- read.csv(textConnection(readLines(gzcon(url(url)))))
  print('finishing reading 1 ')
  calendar <- read.csv(textConnection(readLines(gzcon(url(str_replace(url, "listings", "calendar"))))))
  print('finishing reading 2')
  
  # Clean and Organize listings
  
  ##Add Keys: columns city and day date
  listings$country <- country
  listings$city <- city
  listings$date <- data_date
  
  print('finishing adding keys')
  
  #
  #Select interesting columns
  columns_listings <- c("country","city", "date", "id", "neighbourhood_cleansed", 
                        "latitude", "longitude", 
                        "property_type", "room_type", "accommodates", "bedrooms", 
                        "beds", "price", "minimum_nights",  "maximum_nights")
  
  listings <- listings %>% 
    select(columns_listings) %>% 
    arrange(id)
  print('finishing selecting')
  
  ## clean price column and transform to numeric
  listings <- listings %>%
    mutate(price = str_replace(price, "\\$", ""))
  listings <- listings %>%
    mutate(price = str_replace(price, ",", ""))
  listings <- listings %>%
    mutate(price = as.numeric(price))
  
  listings$price[is.na(listings$price)]<-0
  listings$bedrooms[is.na(listings$bedrooms)]<-0
  listings$beds[is.na(listings$beds)]<-0
  
  # Cleaning calendar dataframe
  
  ## arrange by id and date
  calendar <- calendar %>% 
    arrange(listing_id, date)
  
  ## add day number (starting first day)
  calendar <- calendar %>%
    group_by(listing_id) %>%
    mutate(day_nb = row_number()) %>%
    ungroup()
  
  ## change available column to binary
  calendar <- calendar %>%
    mutate(available = ifelse(available=="t", 1, 0))
  
  ## clean price column and transform to numeric
  calendar <- calendar %>%
    mutate(price = str_replace(price, "\\$", ""),
           adjusted_price = str_replace(adjusted_price, "\\$", ""))
  calendar <- calendar %>%
    mutate(price = str_replace(price, ",", ""),
           adjusted_price = str_replace(adjusted_price, ",", ""))
  calendar <- calendar %>%
    mutate(price = as.numeric(price),
           adjusted_price = as.numeric(adjusted_price))
  
  ## calculate estimated revenue for upcoming day
  calendar <- calendar %>%
    mutate(revenue = price*(1-available))
  
  ## calculate availability, price, revenue for next 30, 60 days ... for each listing_id
  calendar <- calendar %>%
    group_by(listing_id) %>%
    summarise(availability_30 = sum(available[day_nb<=30], na.rm = TRUE),
              #availability_60 = sum(available[day_nb<=60], na.rm = TRUE),
              #availability_90 = sum(available[day_nb<=90], na.rm = TRUE),
              #availability_365 = sum(available[day_nb<=365], na.rm = TRUE),
              price_30 = mean(price[day_nb<=30 & available==0], na.rm = TRUE),
              #price_60 = mean(price[day_nb<=60 & available==0], na.rm = TRUE),
              #price_90 = mean(price[day_nb<=90 & available==0], na.rm = TRUE),
              #price_365 = mean(price[day_nb<=365 & available==0], na.rm = TRUE),
              revenue_30 = sum(revenue[day_nb<=30], na.rm = TRUE),
              #revenue_60 = sum(revenue[day_nb<=60], na.rm = TRUE),
              #revenue_90 = sum(revenue[day_nb<=90], na.rm = TRUE),
              #revenue_365 = sum(revenue[day_nb<=365], na.rm = TRUE)           
    )
  
  calendar$price_30[is.na(calendar$price_30)]<-0
  
  listings_cleansed <- listings %>% left_join(calendar, by = c("id" = "listing_id"))
  
  dir.create(file.path("Data/data_cleansed", country, city, data_date), recursive = TRUE)
  write.csv(listings_cleansed, file.path("Data/data_cleansed", country, city, data_date, "listings.csv"))
  print(paste0("saving data into ", file.path("Data/data_cleansed", country, city, data_date, "listings.csv")))

}

file_dir <- file.path(".", "Data/all_data.csv")
df <- read.csv(file_dir)
df <- df %>%
  filter(country %in% c("germany", "the-netherlands", "portugal")) %>%
  group_by(city) %>%
  slice_head(n=3)


for (row in 1:nrow(df)) {
  prepare_country_data(as.character(df[row,]$listings_url), as.character(df[row,]$country), as.character(df[row,]$city), as.Date(df[row,]$data_date))
  print(paste0("done for ",df[row,]$data_date ))
  print(row)
}

# Clean Environment
rm(list=ls())



































