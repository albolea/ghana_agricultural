library(tidyverse)
library(here)
library(GGally)
library(car)
library(haven)
#to read data use haven or foreign

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
  select(clust,nh,farm_size,unit) %>% mutate(ID = paste(clust,nh,sep="_"))

#transfor mesuare to Acres, 1 Pole = 1 Acre and 1 Rope = 1/9Acre and sum the farm sizes
land_size <- land_size %>% 
  mutate(farm_size = ifelse( unit == 3,farm_size/9,
                                        ifelse( unit == 4, 0,farm_size))
                                      ) %>% 
  select(clust,nh,farm_size) %>% 
  mutate(ID = paste(clust,nh,sep="_"))  %>% 
  group_by(clust,nh) %>% 
  summarise(farm_size = sum(farm_size))
  

inf_farmers <- read_dta(here("raw_data/glss4","sec6.dta")) #Identification of farm and No-n farm Enterprises

inf_farmers <- inf_farmers %>% filter(s6q1==1) #exclude non farmers
inf_farmers <- inf_farmers %>% rename(farmer = s6q1, fishing = s6q4, own_business = s6q8) %>% 
  select(clust,nh,farmer,fishing,own_business) %>% mutate(ID = paste(clust,nh,sep="_"))

