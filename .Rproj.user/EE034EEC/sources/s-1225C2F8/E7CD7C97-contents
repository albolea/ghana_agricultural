
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
                  females_on_farme, data=base_small_farms)
summary(mod_9_log)

base_small_farms$fitted <- mod_9_log$fitted.values
base_small_farms$residuals <- mod_9_log$residuals
base_small_farms %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point()     

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
                  females_on_farme, data=base_normal_farms)
summary(mod_9_log)

base_normal_farms$fitted <- mod_9_log$fitted.values
base_normal_farms$residuals <- mod_9_log$residuals
base_normal_farms %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point()

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
                  females_on_farme, data=base_large_farms)
summary(mod_9_log)

base_large_farms$fitted <- mod_9_log$fitted.values
base_large_farms$residuals <- mod_9_log$residuals
base_large_farms %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point()

write.csv(base,'base.csv')
