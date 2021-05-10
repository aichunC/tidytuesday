library(tidytuesdayR)
library(tidyverse)
library(forcats)

tuesdata <- tidytuesdayR::tt_load(2021, week = 19)
water <- tuesdata$water

save(water, file="water.rdata")

load(water)
# Q1, draw a map of the water point data in each African country 

maps_world <- map_data("world")  # maps of each country included in this dataset 

#1a  try to plot one country "China" from  the world map, ggplot has to put dataset maps_world
ggplot(maps_world)+
  geom_map(map=maps_world, aes(map_id="China"))+
  expand_limits(x=maps_world$long,y=maps_world$lat)

#1b, try to plot the water points in the map
# does not give much info 
water %>%group_by(country_name) %>% summarise(n=n()) %>%
ggplot(aes(fill=n))+
   geom_map(map=maps_world, aes(map_id=country_name))+
   expand_limits(x=maps_world$long,y=maps_world$lat)
  

#1c try to plot the water souces in country Kenya
# I found out some water point data are outside of the country's scale 

kenya <- water %>% filter(country_name=="Kenya")
  ggplot()+
  geom_point(data=kenya,aes(x=lon_deg,y=lat_deg, color="red"))+
  geom_map(map=maps_world, aes(map_id="Kenya"))+
  expand_limits(x=0:50,y=-5:12)

  
  # functional water sources 

 water_fun<- water[grep("Functional",water$status),] %>% 
    group_by(country_name) %>% 
    summarise(n=n()) %>% 
    arrange(desc(n)) 

 #assign a factor to country name 
 water_fun$country_name<-factor(water_fun$country_name, level=water_fun$country_name)

    water_fun%>%
    mutate(country_name= reorder(country_name,n)%>% fct_rev())%>%
    ggplot() +
    geom_col(aes(x=country_name, y=n,fill=country_name)) +
    labs(
      title="Country vs # of functional water points",
      subtitle = "Nealy 90% of functional water points located in top 4 countries",
      caption = "data from Water Point Data Exchange, graph @sky32456985"
    )
      
 #   geom_map(map=maps_world, aes(map_id=country_name)) +
  #  expand_limits(x=maps_world$long, y=maps_world$lat)

  

