shhh <- suppressPackageStartupMessages

shhh(library(tidyverse))
shhh(library(here))
shhh(library(GGally))
shhh(library(car))
shhh(library(haven))
shhh(library(fastDummies))

###### Load and treat files #####
#Load Farm Profit
profit_hh <- read_dta(here("raw_data/glss4/aggregates","agg2.dta"))
#exclude depreciation from profit and exclude profit = 0
profit_hh <- profit_hh %>% mutate(profit= agri1c) %>% #-hhagdepn
  filter(profit > 0 | profit < 0 ) %>% 
  mutate(ID = paste(clust,nh,sep="_")) %>% 
  select(ID,profit)

#descriptive analize of profit
summary(profit_hh)

#Import HH info
inf_house <- read_dta(here("raw_data/glss4","sec0a.dta"))  #DATA ABOUT HOUSE HOLD

#Name Tables
region_name <- c("Western","Central","Greater_Accra","Eastern","Volta","Ashanti",
                 "Brong_Ahafo","Northern","Upper_East","Upper_West")
eco_zone_name <- c("Costal","Forest", "Savannah")
loc2_name <- c("Urban","Rural")
loc5_name <- c("Accra","Other_Urban","Rural_Coastal","Rural_Forest","Rural_Savannah")
loc3_name <- c("Accra","Other_Urban","Rural")


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
inf_house <- inf_house %>% dummy_cols(select_columns =c("region","ez","loc2"))
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

#excluding farm that is too big / to small compared with others
land_size <- land_size %>% filter(farm_size>=0.2 & farm_size <150)

inf_farmers <- read_dta(here("raw_data/glss4","sec6.dta")) #Identification of farm and No-n farm Enterprises

inf_farmers <- inf_farmers %>% filter(s6q1==1) #exclude non farmers
inf_farmers <- inf_farmers %>% rename(farmer = s6q1, fishing = s6q4, own_business = s6q8) %>% 
  mutate(ID = paste(clust,nh,sep="_"),
         fishing = fishing==1,## Dummy
         own_business = own_business ==1) %>%  ##Dummy
  select(ID,farmer,fishing,own_business)



###### Join in one variable
base <- inf_house %>% 
  left_join(inf_farmers, by = "ID") %>% 
  left_join(land_size, by="ID") %>% 
  left_join(profit_hh,by="ID")

#aux <- base[which(! complete.cases(base)),]


base <- base %>% filter(!is.na(farm_size) & !is.na(profit) & loc2 == "Rural" & !is.na(farmer))

#base <- base %>% filter(!is.na(farm_size) & !is.na(profit) )
base <- base %>% mutate(profit_per_acre = profit/farm_size,
                        profit_usd = profit/3550,
                        profit_per_acre_usd = profit_usd/farm_size)



#exclude outliers 99% confidence
se = sd(base$profit_per_acre)
avg = mean(base$profit_per_acre)
base <- base %>% filter(profit_per_acre>(avg-1.96*se)&profit_per_acre<(avg+1.96*se))

#scale profit to use log
base <- base %>%
  mutate(
    profit_log = log(profit-(min(profit)-10000)),
    profit_per_acre_log = log((profit-(min(profit)-10000))/farm_size)
  )

# #scale profit to use log
# base <- base %>%
#   mutate(
#     profit_log = log(profit-(min(profit)-10000)),
#     profit_per_acre_log = log((profit-(min(profit)-10000))/farm_size)
#   )

base %>% filter(fishing==2 & own_business==2) %>%  group_by(loc5) %>% 
  ggplot(aes(x=loc5,y=profit_per_acre_usd,group=loc5)) +geom_boxplot()

base %>% group_by(loc5) %>% 
  ggplot(aes(x=loc5,y=profit_per_acre_usd,group=loc5)) +geom_boxplot()


base %>%ggplot(aes(x=profit_per_acre_usd)) +geom_density()

base %>%ggplot(aes(x=profit_per_acre_usd)) +geom_histogram()

base %>% select(farm_size) %>% summary()

base %>% filter(farm_size<50,profit_per_acre_usd<2500) %>%  
  ggplot(aes(farm_size,profit_per_acre_usd)) +geom_point()
base %>% select(profit_per_acre_log,farm_size) %>% summary()


family_info <- read_dta(here("raw_data/glss4","sec1.dta")) 
family_info <- family_info %>% rename(spouse_live_hh = s1q7) %>% 
  mutate(ID = paste(clust,nh,sep="_"),
         spouse_live_hh = spouse_live_hh==1,
         sex_male = sex==1) %>% 
  filter(rel==1) %>% 
  select(ID,pid,spouse_live_hh,sex_male,agey)

base <- base %>% left_join(family_info, by="ID")


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
         educ_other = s2aq1==1 & s2aq3==96,
         educ_advanced = s2aq1==1 & s2aq3>2
  ) %>% 
  select(-s2aq1,-s2aq3,-clust,-nh)

base <- base %>% left_join(educ_info,by=c("ID","pid"))
base <- base %>% filter(!is.na(educ_none) )


# mod_1 <- lm(profit_per_acre~farm_size+agey+spouse_live_hh+sex_male+fishing+own_business+
#               educ_bece+educ_voc +educ_O_level +educ_sss +educ_a_level +educ_t_cert +
#               educ_nurse +educ_tech +educ_bach +educ_master +educ_phd +educ_other+
#               region_Western+region_Central+region_Greater_Accra+
#               region_Eastern+region_Volta+region_Ashanti+region_Brong_Ahafo+
#               region_Northern+region_Upper_East,data=base)
# summary(mod_1)

mod_1 <- lm(profit_per_acre~farm_size+agey+spouse_live_hh+sex_male+fishing+own_business+
              educ_bece+educ_advanced+
              region_Western+region_Central+region_Greater_Accra+
              region_Eastern+region_Volta+region_Ashanti+region_Brong_Ahafo+
              region_Northern+region_Upper_East,data=base)
summary(mod_1)
mod_1_log <- lm(profit_per_acre_log~farm_size+agey+spouse_live_hh+sex_male+fishing+own_business+
                  educ_bece+educ_advanced+
                  region_Western+region_Central+region_Greater_Accra+
                  region_Eastern+region_Volta+region_Ashanti+region_Brong_Ahafo+
                  region_Northern+region_Upper_East,data=base)
summary(mod_1_log)

mod_1b <- lm(profit_per_acre~farm_size+spouse_live_hh+fishing+ez_Costal+ez_Forest,data=base)
summary(mod_1b)



mod_2 <- lm(profit_per_acre~farm_size+agey+spouse_live_hh+sex_male+fishing+own_business+
              educ_bece+educ_voc +educ_O_level +educ_sss +educ_a_level +educ_t_cert +
              educ_nurse +educ_tech +educ_bach +educ_master +educ_phd +educ_other,data=base)
summary(mod_2)

mod_2_log <- lm(profit_per_acre_log~farm_size+agey+spouse_live_hh+sex_male+fishing+own_business+
                  educ_bece+educ_voc +educ_O_level +educ_sss +educ_a_level +educ_t_cert +
                  educ_nurse +educ_tech +educ_bach +educ_master +educ_phd +educ_other,data=base)
summary(mod_2_log)

mod_2b <- lm(profit_per_acre~farm_size+agey+spouse_live_hh+sex_male+fishing+own_business+
               educ_bece+educ_voc +educ_O_level +educ_sss +educ_a_level +educ_t_cert +
               educ_nurse +educ_tech +educ_bach +educ_master +educ_phd +educ_other,data=base)
summary(mod_2b)

mod_2b_log <- lm(profit_per_acre_log~farm_size+agey+spouse_live_hh+sex_male+fishing+own_business+
                   educ_advanced,data=base)
summary(mod_2b_log)

mod_3 <- lm(profit_per_acre~farm_size+spouse_live_hh+sex_male+fishing+
              educ_none,data=base)
summary(mod_3)


educ_info_2 <- read_dta(here("raw_data/glss4","sec2c.dta")) 
educ_info_2 <- educ_info_2 %>% 
  mutate(ID = paste(clust,nh,sep="_"),
         do_math = s2cq5==1) %>% 
  select(ID,pid,do_math)

base <- base %>% left_join(educ_info_2, by=c("ID","pid"))

base <- base %>% filter(!is.na(do_math) )

mod_4 <- lm(profit_per_acre~farm_size+spouse_live_hh+sex_male+fishing+
              educ_none+do_math,data=base)
summary(mod_4)

mod_4_log <- lm(profit_per_acre_log~farm_size+spouse_live_hh+sex_male+fishing+
                  educ_advanced+do_math,data=base)
summary(mod_4_log)

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
         wall_iron = s7eq1==3,
         wall_stone = s7eq1==4,
         wall_cement = s7eq1==5,
         wall_other = s7eq1==6
  ) %>% 
  select(-clust,-nh,-starts_with("s7"))
base <- base %>% left_join(housing_info,by="ID")


mod_5 <- lm(profit_per_acre~light_eletricity+light_generator+cooking_full_gas+cooking_full_eletricity+
              toilet_flush+toilet_latrine+wall_mud+wall_wood+wall_iron+
              wall_stone+wall_cement,data=base)
summary(mod_5)

mod_5_log <- lm(profit_per_acre_log~light_eletricity+light_generator+cooking_full_gas+cooking_full_eletricity+
                  toilet_flush+toilet_latrine+wall_mud+wall_wood+wall_iron+
                  wall_stone+wall_cement,data=base)
summary(mod_5_log)

mod_6 <- lm(profit_per_acre~light_eletricity+cooking_full_gas+
              toilet_flush+toilet_latrine+wall_mud+wall_wood+wall_iron,data=base)
summary(mod_6)

mod_6_log <- lm(profit_per_acre_log~light_eletricity+cooking_full_gas+
                  toilet_flush+toilet_latrine+wall_mud+wall_wood+wall_iron,data=base)
summary(mod_6_log)

work_force <- read_dta(here("raw_data/glss4","sec8c1.dta")) 

work_force <- work_force %>% 
  mutate(ID = paste(clust,nh,sep="_"),
         harvest_sold_pre = s8cq6 == 1,
         harvest_sold_gate = s8cq6 == 2,
         harvest_sold_market = s8cq6 == 3,
         harvest_sold_consumer = s8cq6 == 4,
         harvest_sold_state_org = s8cq6 == 5,
         harvest_sold_coop = s8cq6 == 6,
         paid_at_sale = s8cq10a == 1,
         paid_at_week = s8cq10a == 2,
         paid_at_month = s8cq10a == 3,
         paid_at_over_m = s8cq10a == 4,
         males_on_farme = ifelse(s8cq17a>1e+100,0,s8cq17a),
         females_on_farme = ifelse(s8cq17b>1e+100,0,s8cq17b),
         workers_on_farm = males_on_farme + females_on_farme
  ) %>% 
  select(-clust,-nh,-starts_with("S8")) %>% 
  group_by(ID) %>% 
  summarise(harvest_sold_pre=any(harvest_sold_pre),
            harvest_sold_gate=any(harvest_sold_gate),
            harvest_sold_market=any(harvest_sold_market),
            harvest_sold_consumer=any(harvest_sold_consumer),
            harvest_sold_state_org=any(harvest_sold_state_org),
            harvest_sold_coop=any(harvest_sold_coop),
            paid_at_sale=any(paid_at_sale),
            paid_at_week=any(paid_at_week),
            paid_at_month=any(paid_at_month),
            paid_at_over_m=any(paid_at_over_m),
            males_on_farme=sum(males_on_farme),
            females_on_farme=sum(females_on_farme),
            workers_on_farm=sum(workers_on_farm))


base <- base %>% left_join(work_force,by="ID")
base <- base %>% filter(!is.na(paid_at_month) )


mod_7 <- lm(profit_per_acre~harvest_sold_gate+harvest_sold_market+
              harvest_sold_consumer+harvest_sold_state_org+harvest_sold_coop+
              paid_at_sale+paid_at_week+paid_at_month+males_on_farme+
              females_on_farme+workers_on_farm,data=base)
summary(mod_7)

mod_7_log <- lm(profit_per_acre_log~harvest_sold_gate+harvest_sold_market+
                  harvest_sold_consumer+harvest_sold_state_org+harvest_sold_coop+
                  paid_at_sale+paid_at_week+paid_at_month+males_on_farme+
                  females_on_farme+workers_on_farm,data=base)
summary(mod_7_log)

mod_7b_log <- lm(profit_per_acre_log~harvest_sold_gate+harvest_sold_market+
                   harvest_sold_consumer+harvest_sold_state_org+harvest_sold_coop+
                   paid_at_sale+paid_at_week+paid_at_month+workers_on_farm,data=base)
summary(mod_7b_log)



###### Including Community Survey
com_surv_2 <- read_dta(here("raw_data/glss4/community","cs2.dta")) 
com_surv_2 <- com_surv_2 %>% 
  mutate(clust = eanum + 4000,
         most_impor_farming = s2q1a==1,
         most_impor_fishing = s2q1a==2,
         moto_road = s2q4 ==1,
         #new(Q5: distance to the near road)
         moto_road_distance = ifelse(s2q5<1e+100,s2q5,0) ,
         moto_road_impassable = s2q6 ==1,
         #new(Q9: most households have electricty. Q11: most households have pipe water)
         most_have_electricty = s2q9 ==1,
         most_have_pipe_water = s2q11 ==1,
         #revise(Q14: have the bar. The bar is in the question 14, but the previouse code mis-locate this quesion to the question 13)
         have_bar = s2q14 ==1,
         have_post_of_pub_telephone = s2q15 == 1,
         #new(Q16: the distance to the post office or public telephone)
         distance_to_post_of_pub_telephone = ifelse(s2q16<1e+100,s2q16,0),
         have_bank = s2q17 == 1,
         #new(Q18: distance to the nearest bank)
         distance_to_nearest_bank = ifelse(s2q18<1e+100,s2q18,0),
         have_daily_mkt = s2q19 ==1,
         have_week_mkt = s2q21b ==1,
         #new(Q22: distance to the nearest market)
         distance_to_nearest_market = ifelse(s2q22<1e+100,s2q22,0),
         public_transp = s2q23 ==1,
         #new(Q25: distance to the public transport)
         distance_to_public_transportation = ifelse(s2q25<1e+100,s2q25,0),
         people_come_for_job_farming = s2q32 == 1 & (s2q34a==1 |s2q34b==1)
         
  ) %>% select(-starts_with("s2"),-eanum,-region,-district)

com_surv_2 <- com_surv_2 %>% group_by(clust) %>% mutate_if(is.logical,any)
com_surv_2 <- com_surv_2 %>% unique()

#add the question of section3: education  
com_surv_3 <- read_dta(here("raw_data/glss4/community","cs3.dta")) 
com_surv_3 <- com_surv_3 %>% 
  mutate(clust = eanum + 4000,
         have_primary_schol = s3q1 == 1,
         distance_to_the_nearest_primary_school = ifelse(s3q2<1e+100,s3q2,0),
         have_junior_2ndschool = s3q11 == 1,
         distance_to_the_nearest_junior_2ndschool = ifelse(s3q12<1e+100,s3q12,0),
         age_of_junior_2ndschool = s3q18,
         have_senior_2ndschool = s3q20 == 1,
         distance_to_the_nearest_senior_2ndschool = ifelse(s3q21<1e+100,s3q21,0),
         age_of_senior_2ndschool = s3q27,
         have_adult_literacy_probram = s3q29 == 1,
         age_of_adult_literacy_probram = s3q30
  ) %>% select(-starts_with("s3"),-eanum,-region,-district)         
com_surv_3 <- com_surv_3 %>% group_by(clust) %>% mutate_if(is.logical,any)
com_surv_3 <- com_surv_3 %>% unique()        


com_surv_4b <- read_dta(here("raw_data/glss4/community","cs4b.dta")) 
com_surv_4b <- com_surv_4b %>% 
  mutate(clust = eanum + 4000,
         have_hospital = s4bq0 ==10 & s4bq5==1) %>% 
  select(-starts_with("s4"),-eanum,-region,-district)
com_surv_4b <- com_surv_4b %>% group_by(clust) %>% mutate_if(is.logical,any)
com_surv_4b <- com_surv_4b %>% unique()

com_surv_5b <- read_dta(here("raw_data/glss4/community","cs5b.dta")) 
com_surv_5b <- com_surv_5b %>% 
  mutate(clust = eanum + 4000,
         have_agric_ext_center = s5bq5==1,
         #new(Q6: the distance to the agricultural extension centre.)
         distance_to_agri_extention_centre = ifelse(s5bq6<1e+100,s5bq6,0),
         #new(Q7: Agriculture extension centre officer visit this community)
         agri_officer_visit_community = s5bq7 == 1,
         have_cooperative = s5bq10==1,
         #new(Q13: number of tractors in this community)
         number_tractors = s5bq13,
         #new(Q14: if rick-husking machine is available in this community)
         have_rice_husking_machine = s5bq14 == 1,
         any_farm_use_fert = s5bq15 ==1,
         any_farm_use_inset_herb = s5bq16 ==1,
         any_farm_use_irrigate = s5bq17 ==1,
         mutual_aid_farm = s5bq23==1
  ) %>% 
  select(-starts_with("s5"),-eanum,-region,-district)
com_surv_5b <- com_surv_5b %>% group_by(clust) %>% mutate_if(is.logical,any)
com_surv_5b <- com_surv_5b %>% unique()

###### Join communit Survey
suppressWarnings(
  base <- base %>% 
    left_join(com_surv_2, by="clust") %>% 
    left_join(com_surv_4b, by="clust") %>% 
    left_join(com_surv_5b, by="clust")
)

base <- base %>% filter(!is.na(most_impor_farming)) 
aux <- base[which(! complete.cases(base)),]

mod_8_log <- lm(profit_per_acre_log~most_impor_farming + most_impor_fishing +  moto_road +
                  moto_road_impassable +  have_bar + have_post_of_pub_telephone + have_bank +
                  have_daily_mkt + have_week_mkt + public_transp + people_come_for_job_farming +
                  have_hospital + have_agric_ext_center + have_cooperative + any_farm_use_fert +
                  any_farm_use_inset_herb + any_farm_use_irrigate + mutual_aid_farm,data=base)
summary(mod_8_log)

#excluding NAs
#base <- base %>% filter(!is.na(educ_none) & !is.na(do_math))


base <- base %>% filter(!is.na(farm_size) & !is.na(profit) & loc2 == "Rural" & !is.na(farmer))

base<- base %>% filter(most_impor_fishing==FALSE)
mod_9_log <- lm(profit_per_acre_log~most_impor_farming + most_impor_fishing +  moto_road +
                  moto_road_impassable +  have_bar + have_post_of_pub_telephone + have_bank +
                  have_daily_mkt + have_week_mkt + public_transp + people_come_for_job_farming +
                  have_hospital + have_agric_ext_center + have_cooperative + any_farm_use_fert +
                  any_farm_use_inset_herb + any_farm_use_irrigate + mutual_aid_farm +
                  farm_size+I(farm_size^2)+agey+spouse_live_hh+sex_male+fishing+own_business+
                  educ_bece+educ_advanced+
                  region_Western+region_Central+region_Greater_Accra+
                  region_Eastern+region_Volta+region_Ashanti+region_Brong_Ahafo+
                  region_Northern+region_Upper_East+light_eletricity+light_generator+
                  cooking_full_gas+
                  toilet_flush+toilet_latrine+wall_mud+wall_wood+wall_iron+
                  wall_stone+wall_cement+harvest_sold_gate+harvest_sold_market+
                  harvest_sold_consumer+harvest_sold_state_org+harvest_sold_coop+
                  paid_at_sale+paid_at_week+paid_at_month+males_on_farme+
                  females_on_farme, data=base)
summary(mod_9_log)

base$fitted <- mod_9_log$fitted.values
base$residuals <- mod_9_log$residuals
base %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point()

mod_10_log <- lm(profit_per_acre_log~have_bar + have_post_of_pub_telephone + 
                   any_farm_use_fert +
                   farm_size+agey+spouse_live_hh+sex_male+educ_bece+educ_advanced+
                   region_Western+region_Central+region_Greater_Accra+
                   region_Eastern+region_Volta+region_Ashanti+region_Brong_Ahafo+
                   region_Northern+region_Upper_East+light_eletricity+
                   cooking_full_gas+harvest_sold_gate+harvest_sold_market+
                   harvest_sold_state_org+harvest_sold_coop, data=base)
summary(mod_10_log)

#Excluding education
mod_11_log <- lm(profit_per_acre_log~have_bar + have_post_of_pub_telephone + 
                   any_farm_use_fert +
                   farm_size+agey+spouse_live_hh+sex_male+
                   region_Western+region_Central+region_Greater_Accra+
                   region_Eastern+region_Volta+region_Ashanti+region_Brong_Ahafo+
                   region_Northern+region_Upper_East+light_eletricity+
                   cooking_full_gas+harvest_sold_gate+harvest_sold_market+
                   harvest_sold_state_org+harvest_sold_coop, data=base)
summary(mod_11_log)

#Eco Zone instead of region
mod_12_log <- lm(profit_per_acre_log~have_bar + have_post_of_pub_telephone + 
                   any_farm_use_fert +
                   farm_size+agey+spouse_live_hh+sex_male+
                   region_Western+region_Central+region_Greater_Accra+
                   region_Eastern+region_Volta+region_Ashanti+region_Brong_Ahafo+
                   region_Northern+region_Upper_East+light_eletricity+
                   cooking_full_gas+harvest_sold_gate+harvest_sold_market+
                   harvest_sold_state_org+harvest_sold_coop+ez_Costal+ez_Forest, data=base)
summary(mod_12_log)



#droping region_Western,region_Central,region_Ashanti,region_Northern

# mod_13_log <- lm(profit_per_acre_log~ any_farm_use_fert + farm_size + agey + spouse_live_hh + sex_male +
#                    region_Greater_Accra + region_Eastern+region_Volta + 
#                    region_Upper_East + light_eletricity+ cooking_full_gas + harvest_sold_market + 
#                    harvest_sold_state_org  , data=base)

mod_13_log <- lm(profit_per_acre_log~ any_farm_use_fert + farm_size + I(farm_size^2)+ agey + I(agey^2) + spouse_live_hh + sex_male +
                   region_Western+region_Central+region_Greater_Accra+
                   region_Eastern+region_Volta+region_Ashanti+region_Brong_Ahafo+
                   region_Northern+region_Upper_East + light_eletricity+ cooking_full_gas + 
                   harvest_sold_market + harvest_sold_state_org + ez_Costal+ ez_Forest  , data=base)
summary(mod_13_log)


#Standardised residuals plot
base <- base %>% 
  mutate(
    stand_res = rstandard(mod_13_log)
  )
base %>% ggplot(aes(x = stand_res)) +
  geom_histogram(binwidth = 0.25) + xlab("Standardized residuals")
# standardized residual more normal
suppressMessages(
  ggsave(here("figures", "model_stand_res.png"))
)
# residual versus the fitted value
base$fitted <- mod_13_log$fitted.values
base$residuals <- mod_13_log$residuals
base %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point()
#homoskedasticity 
suppressMessages(
  ggsave(here("figures", "model_res_vs_fitted.png")) 
)

