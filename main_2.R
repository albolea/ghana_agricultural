shhh <- suppressPackageStartupMessages

shhh(library(tidyverse))
shhh(library(here))
shhh(library(haven))
shhh(library(fastDummies))

###### Load and treat files #####

#step.1 - load the data of farm profit
profit_hh <- read_dta(here("raw_data/glss4/aggregates","agg2.dta"))

#step.2 - clean the data by excluding depreciation from profit and exclude profit = 0
profit_hh <- profit_hh %>% mutate(profit= agri1c) %>% #-hhagdepn
  filter(profit > 0 | profit < 0 ) %>% 
  mutate(ID = paste(clust,nh,sep="_")) %>% 
  select(ID,profit)

#step.3 - have a peek of data by proforming the descriptive analize of profit
summary(profit_hh)

#step.4 - load the data of household infomation 
inf_house <- read_dta(here("raw_data/glss4","sec0a.dta"))  #DATA ABOUT HOUSE HOLD

#step.5 - organize the categorical data by replacing the number with the category name 
#step.5.1 - define vectors containing category names
region_name <- c("Western","Central","Greater_Accra","Eastern","Volta","Ashanti",
                 "Brong_Ahafo","Northern","Upper_East","Upper_West")
eco_zone_name <- c("Costal","Forest", "Savannah")
loc2_name <- c("Urban","Rural")
loc5_name <- c("Accra","Other_Urban","Rural_Coastal","Rural_Forest","Rural_Savannah")
loc3_name <- c("Accra","Other_Urban","Rural")


#step.5.2 - replace the numbers with category names,  creating a new ID - a combination of cluster ID and household ID
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

#step.6 - load the data of land size 
land_size <- read_dta(here("raw_data/glss4","sec8b.dta")) #Second source of land size
#step6.1 - rename vairables, creating a new ID - a combination of cluster ID and household ID
land_size <- land_size %>% rename(farm_size = s8bq4a,unit = s8bq4b) %>%
  mutate(ID = paste(clust,nh,sep="_"))%>%select(ID,farm_size,unit) 

#step.6.2 - conver the unit, transfor mesuare to Acres, 1 Pole = 1 Acre and 1 Rope = 1/9Acre and sum the farm sizes
land_size <- land_size %>% 
  filter(unit<4) %>% #excluding other measures
  mutate(farm_size = ifelse( unit == 3,farm_size/9,farm_size)) %>% 
  filter(farm_size > 0 & farm_size < 1e+100) %>% 
  select(ID,farm_size) %>% 
  group_by(ID) %>% 
  summarise(farm_size = sum(farm_size))

#step6.3 - excluding farm that is too big / to small compared with others
land_size <- land_size %>% filter(farm_size>=0.2 & farm_size <150)

#step.7 - load the data of farmers 
inf_farmers <- read_dta(here("raw_data/glss4","sec6.dta")) #Identification of farm and No-n farm Enterprises

#step.7.1 - exclude the non-famers from the dataframe
inf_farmers <- inf_farmers %>% filter(s6q1==1) #exclude non farmers

#step.7.2 - rename vaiables, create two dummies
inf_farmers <- inf_farmers %>% rename(farmer = s6q1, fishing = s6q4, own_business = s6q8) %>% 
  mutate(ID = paste(clust,nh,sep="_"),
         fishing = fishing==1,## Dummy
         own_business = own_business ==1) %>%  ##Dummy
  select(ID,farmer,fishing,own_business)



#step.8 - join the dataframe of household information, farmers information, land size, and profit information
base <- inf_house %>% 
  left_join(inf_farmers, by = "ID") %>% 
  left_join(land_size, by="ID") %>% 
  left_join(profit_hh,by="ID")

#step.9 - exclude observations that do not have farm size, profit, and not in the rural areas, and not have information if it is farmer of not
base <- base %>% filter(!is.na(farm_size) & !is.na(profit) & loc2 == "Rural" & !is.na(farmer))

#step.10 - create the profit per acre and in us dollar
base <- base %>% mutate(profit_per_acre = profit/farm_size,
                        profit_usd = profit/3550,
                        profit_per_acre_usd = profit_usd/farm_size)



#step.11 - exclude profit per acre outliers 97.5% confidence
se = sd(base$profit_per_acre)
avg = mean(base$profit_per_acre)
base <- base %>% filter(profit_per_acre>(avg-1.96*se)&profit_per_acre<(avg+1.96*se))

#step.12 - scale profit to use log
base <- base %>%
  mutate(
    profit_log = log(profit-(min(profit)-10000)),
    profit_per_acre_log = log((profit-(min(profit)-10000))/farm_size)
  )

#step.13 - load the data of family information
family_info <- read_dta(here("raw_data/glss4","sec1.dta")) 
#step.13.1 - tidy the data of family info
family_info <- family_info %>% rename(spouse_live_hh = s1q7) %>% 
  mutate(ID = paste(clust,nh,sep="_"),
         spouse_live_hh = spouse_live_hh==1,
         sex_male = sex==1) %>% 
  filter(rel==1) %>% # filter the info for just the household head 
  select(ID,pid,spouse_live_hh,sex_male,agey)

#step.14 - merge the data of family info to the main dataframe
base <- base %>% left_join(family_info, by="ID")

#step.15 - load the data of education info
educ_info <- read_dta(here("raw_data/glss4","sec2a.dta")) 

#step.16 - tidy the data with selected potential useful variable and convert the variable into dummies
educ_info <- educ_info %>% select(clust,nh,pid,s2aq1,s2aq3) %>% 
  mutate(ID = paste(clust,nh,sep="_"),
         educ_none = s2aq1==2 | s2aq3==1,
         educ_bece = s2aq1==1 & s2aq3==2,
         educ_advanced = s2aq1==1 & s2aq3>2
  ) %>% 
  select(-s2aq1,-s2aq3,-clust,-nh)

#step.17 - merge the data into the main dataframe
base <- base %>% left_join(educ_info,by=c("ID","pid"))

#step.18 - exclude the observations that do not have info about education
base <- base %>% filter(!is.na(educ_none) )

#step.19 - load data of education
educ_info_2 <- read_dta(here("raw_data/glss4","sec2c.dta")) 

#step.19.1 - add the variable of "do math", convert the variable into dummies, and merge the variable into main dataframe
educ_info_2 <- educ_info_2 %>% 
  mutate(ID = paste(clust,nh,sep="_"),
         do_math = s2cq5==1) %>% 
  select(ID,pid,do_math)
base <- base %>% left_join(educ_info_2, by=c("ID","pid"))

#step19.2 - exclude the observations of do not have info about if do math or not
base <- base %>% filter(!is.na(do_math) )


#step.20 - load the info of housing

housing_info <- read_dta(here("raw_data/glss4","sec7.dta")) 

#step.21 - following the previous rule to set the ID, and add potential variables
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

#step.22 - merge the info to the main dataframe
base <- base %>% left_join(housing_info,by="ID")

#step.23 - load the data of work force
work_force <- read_dta(here("raw_data/glss4","sec8c1.dta")) 

#step.24 - following the previous rule to set the ID, and add potential variables
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
  group_by(ID) %>% # tidy the data
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

#step.25 - merge the data to the main dataframe
base <- base %>% left_join(work_force,by="ID")
base <- base %>% filter(!is.na(paid_at_month) )



#step.26 - load the data of community 
com_surv_2 <- read_dta(here("raw_data/glss4/community","cs2.dta")) 

#step.26.1 - load the data, create the cluser info along with rules,  and add potential variables
com_surv_2 <- com_surv_2 %>% 
  mutate(clust = eanum + 4000,
         most_impor_farming = s2q1a==1,
         most_impor_fishing = s2q1a==2,
         moto_road = s2q4 ==1,
         moto_road_impassable = s2q6 ==1,
         most_have_electricty = s2q9 ==1,
         most_have_pipe_water = s2q11 ==1,
         have_bar = s2q14 ==1,
         have_post_of_pub_telephone = s2q15 == 1,
         have_bank = s2q17 == 1,
         have_daily_mkt = s2q19 ==1,
         have_week_mkt = s2q21b ==1,
         public_transp = s2q23 ==1,
         people_come_for_job_farming = s2q32 == 1 & (s2q34a==1 |s2q34b==1)
  ) %>% select(-starts_with("s2"),-eanum,-region,-district)

#step.26.2 - tidy the data
com_surv_2 <- com_surv_2 %>% group_by(clust) %>% mutate_if(is.logical,any)
com_surv_2 <- com_surv_2 %>% unique() # exclude the dupicates

#step.27 - load the data, create the cluser info along with rules,  and add potential variables
com_surv_3 <- read_dta(here("raw_data/glss4/community","cs3.dta")) 
com_surv_3 <- com_surv_3 %>% 
  mutate(clust = eanum + 4000,
         have_primary_schol = s3q1 == 1,
         have_junior_2ndschool = s3q11 == 1,
         age_of_junior_2ndschool = s3q18,
         have_senior_2ndschool = s3q20 == 1,
         age_of_senior_2ndschool = s3q27,
         have_adult_literacy_probram = s3q29 == 1,
         age_of_adult_literacy_probram = s3q30
  ) %>% select(-starts_with("s3"),-eanum,-region,-district)      

#step.27.1 - tidy the data
com_surv_3 <- com_surv_3 %>% group_by(clust) %>% mutate_if(is.logical,any)
com_surv_3 <- com_surv_3 %>% unique()        

#step.28 - load the data, create the cluser info along with rules,  and add potential variables
com_surv_4b <- read_dta(here("raw_data/glss4/community","cs4b.dta")) 
com_surv_4b <- com_surv_4b %>% 
  mutate(clust = eanum + 4000,
         have_hospital = s4bq0 ==10 & s4bq5==1) %>% 
  select(-starts_with("s4"),-eanum,-region,-district)

#step.28.1 - tidy the data
com_surv_4b <- com_surv_4b %>% group_by(clust) %>% mutate_if(is.logical,any)
com_surv_4b <- com_surv_4b %>% unique()

#step.29 - load the data, create the cluser info along with rules,  and add potential variables
com_surv_5b <- read_dta(here("raw_data/glss4/community","cs5b.dta")) 
com_surv_5b <- com_surv_5b %>% 
  mutate(clust = eanum + 4000,
         have_agric_ext_center = s5bq5 == 1,
         agri_officer_visit_community = s5bq7 == 1,
         have_cooperative = s5bq10 == 1,
         have_rice_husking_machine = s5bq14 == 1,
         any_farm_use_fert = s5bq15 ==1,
         any_farm_use_inset_herb = s5bq16 ==1,
         any_farm_use_irrigate = s5bq17 ==1,
         mutual_aid_farm = s5bq23==1
  ) %>% 
  select(-starts_with("s5"),-eanum,-region,-district)

#step.29.1 - tidy the data
com_surv_5b <- com_surv_5b %>% group_by(clust) %>% mutate_if(is.logical,any) %>% mutate_if(is.numeric,max)
com_surv_5b <- com_surv_5b %>% unique()

#step.30 - join the community data into the main dataframe
suppressWarnings(
  base <- base %>% 
    left_join(com_surv_2, by="clust") %>% 
    left_join(com_surv_4b, by="clust") %>% 
    left_join(com_surv_5b, by="clust")
)

#step.31 - exclude the communities that do not have complete info 
base <- base %>% filter(!is.na(most_impor_farming)) 

#step.32 - exclude the communities that fishing does the most of revenue
base<- base %>% filter(most_impor_fishing==FALSE)

#step.33 - general model
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

#step.34 - polt the residual
base$fitted <- mod_9_log$fitted.values
base$residuals <- mod_9_log$residuals
base %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point()

base <- base %>% 
  mutate(
    stand_res = rstandard(mod_9_log)
  )
base %>% ggplot(aes(x = stand_res)) +
  geom_histogram(binwidth = 0.25) + xlab("Standardized residuals")

#step.35 - polt the fitted values to residuals, to check for heteroskedasticity
base$fitted <- mod_9_log$fitted.values
base$residuals <- mod_9_log$residuals
base %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point()

#step.36 - model for "small" farms
base_small_farms <- base %>% filter(farm_size<2)

mod_9_log <- lm(profit_per_acre_log~most_impor_farming + most_impor_fishing +  moto_road +
                  moto_road_impassable +  have_bar + have_post_of_pub_telephone + have_bank +
                  have_daily_mkt + have_week_mkt + public_transp + people_come_for_job_farming +
                  have_hospital + have_agric_ext_center + have_cooperative + any_farm_use_fert +
                  any_farm_use_inset_herb + any_farm_use_irrigate + mutual_aid_farm +
                  farm_size+I(farm_size^2)+agey+I(agey^2)+spouse_live_hh+sex_male+fishing+own_business+
                  educ_bece+educ_advanced+
                  region_Western+region_Central+region_Greater_Accra+
                  region_Eastern+region_Volta+region_Ashanti+region_Brong_Ahafo+
                  region_Northern+region_Upper_East+light_eletricity+light_generator+
                  cooking_full_gas+
                  toilet_flush+toilet_latrine+wall_mud+wall_wood+wall_iron+
                  wall_stone+wall_cement+harvest_sold_gate+harvest_sold_market+
                  harvest_sold_consumer+harvest_sold_state_org+harvest_sold_coop+
                  paid_at_sale+paid_at_week+paid_at_month+males_on_farme+
                  females_on_farme,data=base_small_farms)
summary(mod_9_log)
base_small_farms <- base_small_farms %>% 
  mutate(
    stand_res = rstandard(mod_9_log)
  )
base_small_farms %>% ggplot(aes(x = stand_res)) +
  geom_histogram(binwidth = 0.25) + xlab("Standardized residuals")
base_small_farms$fitted <- mod_9_log$fitted.values
base_small_farms$residuals <- mod_9_log$residuals
base_small_farms %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point() 


#step.37 - model for "normal" farms
base_normal_farms <- base %>% filter(farm_size>=2 & farm_size<=10)

mod_9_log <- lm(profit_per_acre_log~most_impor_farming + most_impor_fishing +  moto_road +
                  moto_road_impassable +  have_bar + have_post_of_pub_telephone + have_bank +
                  have_daily_mkt + have_week_mkt + public_transp + people_come_for_job_farming +
                  have_hospital + have_agric_ext_center + have_cooperative + any_farm_use_fert +
                  any_farm_use_inset_herb + any_farm_use_irrigate + mutual_aid_farm +
                  farm_size+I(farm_size^2)+agey+I(agey^2)+spouse_live_hh+sex_male+fishing+own_business+
                  educ_bece+educ_advanced+
                  region_Western+region_Central+region_Greater_Accra+
                  region_Eastern+region_Volta+region_Ashanti+region_Brong_Ahafo+
                  region_Northern+region_Upper_East+light_eletricity+light_generator+
                  cooking_full_gas+
                  toilet_flush+toilet_latrine+wall_mud+wall_wood+wall_iron+
                  wall_stone+wall_cement+harvest_sold_gate+harvest_sold_market+
                  harvest_sold_consumer+harvest_sold_state_org+harvest_sold_coop+
                  paid_at_sale+paid_at_week+paid_at_month+males_on_farme+
                  females_on_farme,
                data=base_normal_farms)
summary(mod_9_log)

base_normal_farms <- base_normal_farms %>% 
  mutate(
    stand_res = rstandard(mod_9_log)
  )
base_normal_farms %>% ggplot(aes(x = stand_res)) +
  geom_histogram(binwidth = 0.25) + xlab("Standardized residuals")

base_normal_farms$fitted <- mod_9_log$fitted.values
base_normal_farms$residuals <- mod_9_log$residuals
base_normal_farms %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point() 

#step.38 - model for "large" farms
base_large_farms <- base %>% filter(farm_size>10)

mod_9_log <- lm(profit_per_acre_log~most_impor_farming + most_impor_fishing +  moto_road +
                  moto_road_impassable +  have_bar + have_post_of_pub_telephone + have_bank +
                  have_daily_mkt + have_week_mkt + public_transp + people_come_for_job_farming +
                  have_hospital + have_agric_ext_center + have_cooperative + any_farm_use_fert +
                  any_farm_use_inset_herb + any_farm_use_irrigate + mutual_aid_farm +
                  farm_size+I(farm_size^2)+agey+I(agey^2)+spouse_live_hh+sex_male+fishing+own_business+
                  educ_bece+educ_advanced+
                  region_Western+region_Central+region_Greater_Accra+
                  region_Eastern+region_Volta+region_Ashanti+region_Brong_Ahafo+
                  region_Northern+region_Upper_East+light_eletricity+light_generator+
                  cooking_full_gas+
                  toilet_flush+toilet_latrine+wall_mud+wall_wood+wall_iron+
                  wall_stone+wall_cement+harvest_sold_gate+harvest_sold_market+
                  harvest_sold_consumer+harvest_sold_state_org+harvest_sold_coop+
                  paid_at_sale+paid_at_week+paid_at_month+males_on_farme+
                  females_on_farme,
                data=base_large_farms)
summary(mod_9_log)

base_large_farms <- base_large_farms %>% 
  mutate(
    stand_res = rstandard(mod_9_log)
  )
base_large_farms %>% ggplot(aes(x = stand_res)) +
  geom_histogram(binwidth = 0.25) + xlab("Standardized residuals")

base_large_farms$fitted <- mod_9_log$fitted.values
base_large_farms$residuals <- mod_9_log$residuals
base_large_farms %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point() 