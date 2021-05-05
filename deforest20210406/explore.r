#1 all the datasets all in tibble format 

library(tidyverse)

#forest dataset : Q1 which country has the most net forest converstion from 1990-2010

# from this analysis, I found out China the most positive conversation with 8220540 
# Brazil has the worse negative conversion rate of -10723950

view(forest)
forest %>% 
  group_by(entity) %>% 
  summarise(total=sum(net_forest_conversion)) %>%
  arrange(desc(total))

#forst dataset : Q2, how many country has positive conversion? negative conversion?
# 57 countries has positive #, 60 countries has negative 

forest %>% 
  group_by(entity) %>% 
  summarise(total=sum(net_forest_conversion)) %>%
  filter(total>0) %>%
  summarise(n_distinct(entity))

#####################################################

#Q1, in the brazil_loss dataset, plot all reasons of loss, year vs value 
# from the graph, we can "guess" the top reason of loss are 1, pasture, 2, commercial crop
# 3, fire, 4, small scale cleaning . need to check the data to confirm 




brazildata <- brazil_loss %>% 
  gather(key="typeoloss",value="value", 4:14) %>%
  ggplot() %>%
  geom_line(mapping=aes(x=year,y=value, color=typeoloss))



# Q1 create a tibble of entity=Brazil, so we can use semi_joint to extract info

Brazil <- tibble(entity="Brazil")
Brazil.Forest <- semi_join(forest, Brazil,by="entity")
Brazil.ForestArea <- semi_join(forest_area, Brazil,by="entity")
Brazil.Soyloss <- semi_join(soybean_use,Brazil,by="entity")
Brazil.vegloss <- semi_join(vegetable_oil,Brazil,by="entity")

Brail.all  <- full_join(Brazil.Forest,Brazil.ForestArea) %>% full_join(Brazil.Soyloss) %>% full_join(Brazil.vegloss)
#Q2 create a dot plot, it shows how the forest loss related to commericial crop, 
# when the data was beyond 200000, spread was large, and the cluster near 100000 shows forest loss was stable at this point 

brazil_loss %>% 
         ggplot()+
       geom_dotplot(mapping=aes(x=commercial_crops,color=year))

# the year vs commercial loss shows that the commerical loss was increasing at 2001-2004, 
# peaked at 2004, after it decreasing 2004-2008, and stablized after 2008. 
# it shows the commerical activity was vital between 2001-2004 and increasing at a faster pace, but decreasing gradually in 2004-2008,
# I supposed the 2008 financial crisis hit Brazil hard, and very slow commercial expansion after 2008

brazil_loss %>% 
      ggplot()+
    geom_point(mapping=aes(x=year, y=commercial_crops,color=year))

# the loss of forest related to different reasons in a graph. 
braziloss2 <- filter(brazil_loss, year=="2005") %>%gather(key="loss",value="value",4:14)
brazilloss3 <-brazil_loss %>% 
  gather(key="loss",value="value",4:14)

brazil_loss %>% 
  gather(key="loss",value="value",4:14) %>%
  ggplot(mapping=aes(x=year,y=value))+
  geom_line(mapping=aes(color=loss))+
  geom_point(aes(alpha=0.25),position="jitter") +
  
  labs( title="Brazil forest loss year vs value",
        subtitle="Pasture causes the most forest loss consistently",
        caption ="data from tidyverse"
     )+
  
   annotate("text",x=2005,y=2e+06,label="forest loss is declining")+
     theme(legend.position="bottom")+
   theme_dark()
# arrage the data to get the top_5 reasons for the forest loss in brazil 


top5 <- brazil_loss  %>% 
  gather(key="loss",value="value",4:14) %>%
  group_by(year) %>%
  mutate(rank=min_rank(value)) %>%
  ungroup %>%
  group_by(loss) %>%
  summarise(score=median(rank)) %>%
  arrange(desc(score)) %>%
  top_n(5)

top5_data <- semi_join(brazilloss3,top5,by="loss")

top5_data %>%
ggplot(mapping=aes(x=year,y=value))+
  geom_line(mapping=aes(color=loss))+
  geom_point(aes(alpha=0.1),position="jitter") +
  
  labs( title="Brazil forest loss year vs value",
        subtitle="Illustrated below are the top 5 reasons of Brazil forest loss from 2001-2013",
        caption ="data @ourworldindata graph@sky013088"
  )+
  
  annotate("text",x=2006,y=2e+06,label="Pasture causes the most loss")+
  theme(legend.position="bottom")+
  theme_dark()

############################## build a model 
world2<-tibble(entity="World")
forest2<-left_join(forest_area,forest,by=c("entity","year")) %>%
  select(entity,year, forest_area,net_forest_conversion) %>%
    drop_na() 


 forest2 <- anti_join(forest2,world2,by="entity")  # delete the entity world 

#add models to tibble 
models2<-forest2 %>%
  group_by(entity) %>%
  nest() %>% # a tilbbel of 132x2
  mutate(models=map(data,~lm(forest_area~net_forest_conversion,data=.x)))

#add coeff to tibble  
coeff2 <-models2 %>%
  mutate(coeffs=map_dbl(models,  ~coefficients(.x) %>% pluck("(Intercept)")))

#add r square to tibble 
rsquare2 <-coeff2 %>%
  mutate(rsquared=map_dbl(models,  ~summary(.x) %>% pluck("r.squared")))
  
  


