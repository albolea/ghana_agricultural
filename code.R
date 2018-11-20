library(tidyverse)
library(here)
library(GGally)
library(car)
library(haven)
#to read data use haven or foreign
#start with G4report.pdf

#Import files
sec0a <- read_dta(here("raw_data/glss4","sec0a.dta"))  #DATA ABOUT HOUSE HOLD
sec1 <- read_dta(here("raw_data/glss4","sec1.dta"))    #DATA ABOUT EDUCATION PER PERSON
sec4a <- read_dta(here("raw_data/glss4","sec4a.dta"))  #EMPLOYMENT AND TIME USE
sec8a1 <- read_dta(here("raw_data/glss4","sec8a1.dta"))
sec8f <- read_dta(here("raw_data/glss4","sec8f.dta")) #Agricultural cost and expenses
sec8g <- read_dta(here("raw_data/glss4","sec8g.dta")) #Agricultural Processing of agricultural produce

#aux <- sec0a %>% filter(eanum==2)
#auxb <- sec0a %>% filter(clust == 4002 & nh==1) # The Unique Household Identifier must be clulst + nh
#auxc <- sec1 %>% filter(clust==4002 & nh==1 & pid==1)# The Unique person Identifier must be clulst + nh + pid

#Next step: Understand which variables we need from the files.

#Transform data Frame to Tibble
sec0a <- as_data_frame(sec0a)

#Name Tables
region_name <- c("Western","Central","Greater Accra","Eastern","Volta","Ashanti",
                 "Brong Ahafo","Northern","Upper East","Upper West")
eco_zone_name <- c("Costal","Forest", "Savannah")
loc2_name <- c("Urban","Rural")
loc5_name <- c("Accra","Other Urban","Rural Coastal","Rural Forest","Rural Savannah")
loc3_name <- c("Accra","Other Urban","Rural")


#Treating information about house hold
inf_house <- sec0a %>% mutate(ID = paste(clust,nh,sep="_"),
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

expense_per_hh <- sec8f %>% mutate(ID = paste(clust,nh,sep="_"),s8fq2 = ifelse(s8fq2>1e+100,NA,s8fq2)) %>% 
  group_by(ID) %>% summarise(total=sum(s8fq2))
summary(expense_per_hh)

processing_per_hh <- sec8g %>% mutate(ID = paste(clust,nh,sep="_"),
                                      s8gq7 = ifelse(s8gq7>1e+100,NA,s8gq7), #Labor Cost
                                      s8gq8 = ifelse(s8gq8>1e+100,NA,s8gq8), #?????
                                      s8gq9 = ifelse(s8gq9>1e+100,NA,s8gq9),  #Other Cost
                                      total_cost = s8gq7+s8gq8+s8gq9,
                                      s8gq10 = ifelse(s8gq10>1e+100,NA,s8gq10), #Sell any processed food
                                      s8gq12 = ifelse(s8gq12>1e+100,NA,s8gq12)) %>%  # Values of sales
  group_by(ID) %>% summarise(total_cost=sum(total_cost),total_sell_proc_food = sum(s8gq10),
                             total_sell_value = sum(s8gq12))
summary(processing_per_hh)


inc1 <- read_dta(here("raw_data/glss4/aggregates","inc1.dta")) #value of scholarship
