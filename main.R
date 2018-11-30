library(tidyverse)
library(here)
library(GGally)
library(car)
library(haven)

###### Load and treat files #####
#Load Farm Profit
profit_hh <- read_dta(here("raw_data/glss4/aggregates","agg2.dta"))
#exclude depreciation from profit and exclude profit = 0
profit_hh <- profit_hh %>% mutate(profit= agri1-hhagdepn) %>% 
    filter(profit > 0 | profit < 0 ) %>% 
    mutate(ID = paste(clust,nh,sep="_")) %>% 
    select(ID,profit)
  
#descriptive analize of profit
summary(profit_hh)

#Import HH info
inf_house <- read_dta(here("raw_data/glss4","sec0a.dta"))  #DATA ABOUT HOUSE HOLD

#Name Tables
region_name <- c("Western","Central","Greater Accra","Eastern","Volta","Ashanti",
                 "Brong Ahafo","Northern","Upper East","Upper West")
eco_zone_name <- c("Costal","Forest", "Savannah")
loc2_name <- c("Urban","Rural")
loc5_name <- c("Accra","Other Urban","Rural Coastal","Rural Forest","Rural Savannah")
loc3_name <- c("Accra","Other Urban","Rural")


#Treating information about house hold
inf_house <- inf_house %>% mutate(ID = paste(clust,nh,sep="_"),
                                  region = region_name[region],
                                  ez = eco_zone_name[ez],
                                  loc2 = loc2_name[loc2],
                                  loc5 = loc5_name[loc5],
                                  loc3 = loc3_name[loc3]) %>% select(ID,
                                                                     clust,
                                                                     nh,
                                                                     region, 
                                                                     district,
                                                                     ez,
                                                                     loc2,
                                                                     loc5,
                                                                     loc3) 


### LAND SIZE
land_size <- read_dta(here("raw_data/glss4","sec8b.dta")) #Second source of land size
land_size <- land_size %>% rename(farm_size = s8bq4a,unit = s8bq4b) %>%
  mutate(ID = paste(clust,nh,sep="_"))%>%select(ID,farm_size,unit) 

#transfor mesuare to Acres, 1 Pole = 1 Acre and 1 Rope = 1/9Acre and sum the farm sizes
land_size <- land_size %>% 
  mutate(farm_size = ifelse( unit == 3,farm_size/9,
                             ifelse( unit == 4, 0,farm_size))
  ) %>% 
  select(ID,farm_size) %>% 
  group_by(ID) %>% 
  summarise(farm_size = sum(farm_size))


inf_farmers <- read_dta(here("raw_data/glss4","sec6.dta")) #Identification of farm and No-n farm Enterprises

inf_farmers <- inf_farmers %>% filter(s6q1==1) #exclude non farmers
inf_farmers <- inf_farmers %>% rename(farmer = s6q1, fishing = s6q4, own_business = s6q8) %>% 
  mutate(ID = paste(clust,nh,sep="_")) %>% select(ID,farmer,fishing,own_business)



###### Join in one variable

base <- inf_house %>% 
  left_join(inf_farmers, by = "ID") %>% 
  left_join(land_size, by="ID") %>% 
  left_join(profit_hh,by="ID")


base <- base %>% filter(!is.na(farm_size)&!is.na(profit))
base <- base %>% mutate(profit_per_acre = profit/farm_size,
                        profit_usd = profit/3550,
                        profit_per_acre_usd = profit_usd/farm_size)

base %>% filter(fishing==2 & own_business==2) %>%  group_by(loc5) %>% 
  ggplot(aes(x=loc5,y=profit_per_acre_usd,group=loc5)) +geom_boxplot()

base %>% group_by(loc5) %>% 
  ggplot(aes(x=loc5,y=profit_per_acre_usd,group=loc5)) +geom_boxplot()
