library(tidyverse)
library(here)
library(GGally)
library(car)
library(haven)

###### Load and treat files #####
#Load Farm Profit
profit_hh <- read_dta(here("raw_data/glss4/aggregates","agg2.dta"))
#exclude depreciation from profit and exclude profit = 0
profit_hh <- profit_hh %>% mutate(profit= agri1c-hhagdepn) %>% 
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
  filter(unit<4) %>% #excluding other measures
  mutate(farm_size = ifelse( unit == 3,farm_size/9,farm_size)) %>% 
  filter(farm_size > 0 & farm_size < 1e+100) %>% 
  select(ID,farm_size) %>% 
  group_by(ID) %>% 
  summarise(farm_size = sum(farm_size))



inf_farmers <- read_dta(here("raw_data/glss4","sec6.dta")) #Identification of farm and No-n farm Enterprises

inf_farmers <- inf_farmers %>% filter(s6q1==1) #exclude non farmers
inf_farmers <- inf_farmers %>% rename(farmer = s6q1, fishing = s6q4, own_business = s6q8) %>% 
  mutate(ID = paste(clust,nh,sep="_"),
         fishing = fishing==1,
         own_business = own_business==1) %>% select(ID,farmer,fishing,own_business)



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


base %>%ggplot(aes(x=profit_per_acre_usd)) +geom_density()

base %>% select(farm_size) %>% summary()

base %>% filter(farm_size<50) %>%  ggplot(aes(farm_size,profit)) +geom_point()
base %>% select(profit_per_acre,farm_size) %>% summary()


family_info <- read_dta(here("raw_data/glss4","sec1.dta")) 
family_info <- family_info %>% rename(spouse_live_hh = s1q7) %>% 
  mutate(ID = paste(clust,nh,sep="_"),
         spouse_live_hh = spouse_live_hh==1,
         sex_male = sex==1) %>% 
  filter(rel==1) %>% 
  select(ID,pid,spouse_live_hh,sex_male,agey)

base <- base %>% left_join(family_info)

educ_info <- read_dta(here("raw_data/glss4","sec2a.dta")) 
educ_info <- educ_info %>% select(clust,nh,pid,s2aq1,s2aq3) %>% 
  mutate(ID = paste(clust,nh,sep="_"),
         educ_none = s2aq1==2 | s2aq3==1,
         educ_bece = s2aq1==1 & s2aq3==2,
         educ_voc = s2aq1==1 & s2aq3==3,
         educ_O_level = s2aq1==1 & s2aq3==4,
         educ_sss = s2aq1==1 & s2aq3==5,
         educ_a_level = s2aq1==1 & s2aq3==6,
         educ_t_cert = s2aq1==1 & (s2aq3==7 |s2aq3==8),
         educ_nurse = s2aq1==1 & s2aq3==9,
         educ_tech = s2aq1==1 & (s2aq3==10 |s2aq3==11),
         educ_bach = s2aq1==1 & s2aq3==12,
         educ_master = s2aq1==1 & s2aq3==13,
         educ_phd = s2aq1==1 & s2aq3==14,
         educ_other = s2aq1==1 & s2aq3==96
  ) %>% 
  select(-s2aq1,-s2aq3,-clust,-nh)

base <- base %>% left_join(educ_info)

mod_2 <- lm(profit_per_acre~farm_size+agey+spouse_live_hh+sex_male+fishing+own_business+
              educ_bece+educ_voc +educ_O_level +educ_sss +educ_a_level +educ_t_cert +
              educ_nurse +educ_tech +educ_bach +educ_master +educ_phd +educ_other,data=base)
summary(mod_2)

base_no_fishing <- base %>% filter(fishing==FALSE)
mod_2 <- lm(profit_per_acre~farm_size+agey+spouse_live_hh+sex_male+fishing+own_business+
              educ_none,data=base_no_fishing)
summary(mod_2)

mod_3 <- lm(profit_per_acre~farm_size+spouse_live_hh+sex_male+fishing+
              educ_none,data=base)
summary(mod_3)


educ_info_2 <- read_dta(here("raw_data/glss4","sec2c.dta")) 
educ_info_2 <- educ_info_2 %>% 
  mutate(ID = paste(clust,nh,sep="_"),
         do_math = s2cq5==1) %>% 
  select(ID,pid,do_math)

base <- base %>% left_join(educ_info_2)
mod_4 <- lm(profit_per_acre~farm_size+spouse_live_hh+sex_male+fishing+
              educ_none+do_math,data=base)
summary(mod_4)

####### HOUSING INFO

housing_info <- read_dta(here("raw_data/glss4","sec7.dta")) 
housing_info <- housing_info %>% 
  mutate(ID = paste(clust,nh,sep="_"),
         light_eletricity = s7dq8 == 1,
         light_generator = s7dq8 == 2,
         light_others = s7dq8>2,
         cooking_full_gas = s7dq10 ==3,
         cooking_full_eletricity = s7dq10 ==4,
         cooking_other = (s7dq10<3|s7dq10>4),
         toilet_flush = s7dq13==1,
         toilet_latrine = s7dq13==2,
         toilet_others = s7dq13>2,
         wall_mud = s7eq1==1,
         wall_wood = s7eq1==2,
         wall_iron = s7eq3==3,
         wall_stone = s7eq3==4,
         wall_cement = s7eq3==5,
         wall_other = s7eq3==6
  ) %>% 
  select(-clust,-nh,-starts_with("s7"))
base <- base %>% left_join(housing_info)
mod_5 <- lm(profit_per_acre~light_eletricity+light_generator+cooking_full_gas+cooking_full_eletricity+
              toilet_flush+toilet_latrine+wall_mud+wall_wood+wall_iron+
              wall_stone+wall_cement,data=base)
summary(mod_5)

mod_6 <- lm(profit_per_acre~light_eletricity+cooking_full_gas+
              toilet_flush+toilet_latrine+wall_mud+wall_wood+wall_iron,data=base)
summary(mod_6)