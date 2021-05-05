library(tidytuesdayR)
library(tidyverse)
library(forcats)

# get the data file, read description about the variables. 
tuesdata <- tidytuesdayR::tt_load(2021, week = 18)
departures <- tuesdata$departures
departures2 <- departures %>% select(coname,fyear,exec_fullname,departure_code,
                                     ceo_dismissal,tenure_no_ceodb,max_tenure_ceodb)
                                     
                                     
#Q1,  which company has the most departures?
# sears holding corp 11 departures, Great Elm Captial Group Inc 10 departures
departures2 %>% group_by(coname) %>%count() %>% arrange(desc(n))

#Q2, which year has the most departure? 2008-2009?
# hoop, we foud out it is 2017-2018

departures2 %>% group_by(fyear) %>% summarise(n=n()) %>%arrange(desc(n)) %>%
  ggplot()+ 
  geom_point(mapping=aes(x=fyear,y=n))


#Q3, what are the most common reason of departure ?
CR<-departures2 %>% group_by(departure_code) %>% count() %>% na.omit() %>%
  arrange(desc(n)) 
 CR$departure_code <-factor(CR$departure_code,level=unique(CR$departure_code)) 
 
 
 CR %>% mutate(departure_code=fct_reorder(departure_code, n) %>% fct_rev()) %>%
 ggplot()+ 
  geom_point(mapping=aes(x=n,y=departure_code, color=departure_code)) +
   labs( title="The most common departure reason: voluntar- CEO retired",
         subtitle="Plot of the reasons in increasing order",
         caption ="data from Gentry et al. by way of DatalsPlural"
   )+
   
   annotate("text",x=3250,y=9,label="Voluntary-CEO retired")+

   theme_linedraw() +
   scale_color_discrete(label=c("Missing","CEO death","Execucomp error","CEO illness","CEO new opportunity","CEO dismissed for legal violations or concerns",
                                "CEO dismissed for job performance","Other","CEO retired")) +
   theme(legend.position = "bottom")
 


#Q4 ceo_dismissal : a dummy code for involuntary, non-health realted turnover
# oh there are ~1489 ceo dismissal in the USA 
departures2 %>% count(ceo_dismissal) 

#Q5 for CEO who return,(tenure_no_ceodb >=2), what is their max_tenure_ceo,ie, 
#how many times did s/he serve as CEO

Mx<-departures2  %>% group_by(tenure_no_ceodb) %>% nest()%>%mutate(mx=map(data,~pluck(.x)%>%
                                                                            select(max_tenure_ceodb)%>%
                                                                            group_by(max_tenure_ceodb)%>%
                                                                            count()))
Mx_models <- Mx %>% mutate( models=map(mx, ~lm(max_tenure_ceodb~n,data=.x) ) )
Mx_coeff <- Mx_models %>% mutate(coeffs = map_dbl(models, ~coefficients(.x)%>%pluck("(Intercept)")))             
  












