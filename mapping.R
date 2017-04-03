library(dplyr)
library(plotly)
library(ggplot2)
library(readr)
library(prophet)
library(extrafont)
library(leaflet)
library(car)
library(ggmap)

poop <- read_csv('https://docs.google.com/spreadsheets/d/1Ow1OTP3QfyQyi1K2jeyqjv5eg_AzqF36Tn3sn5NSQQw/pub?gid=444517334&single=true&output=csv')

poop$locate <- Recode(poop$`Where are You?`, "'Church' = '1600 Salem Road, Mount Vernon IL'; 
                                      'EIU' = '600 Lincoln Avenue, Charleston IL';
                                      'Fairview Heights' = 'Fairview Heights, IL';
                                      'FBC' = '1600 Salem Road, Mount Vernon IL';
                                      'Galleria' = 'Galleria St. Louis, MO';
                                      'Home' = '1803 Isabella Avenue, Mt Vernon IL';
                                      'Okawville' = 'Okawville, IL'")

mapdata <- poop %>% group_by(locate) %>% summarise(count = n())        

coords <- geocode(mapdata$locate)

mapdata <- cbind(mapdata, coords)

pop<-paste0("<b>Address</b>: ", mapdata$locate, "<br>",
            "<b>Count</b>: ", mapdata$count)

leaflet() %>% 
  addProviderTiles(providers$Stamen.TonerHybrid) %>% 
  addCircleMarkers(lng = mapdata$lon, lat = mapdata$lat, radius = mapdata$count, popup = pop)









