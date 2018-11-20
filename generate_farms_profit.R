#### READ AND TREAT FARM'S REVENUE AND EXPENSES PER HOUSEHOLD

library(tidyverse)
library(haven)

## Revenues
subagg13 <- read_dta(here("raw_data/glss4/aggregates","subagg13.dta")) #Sale of cash crops (main/others)
subagg14 <- read_dta(here("raw_data/glss4/aggregates","subagg14.dta")) #Sale of roots/fruit/vegetbles
subagg15 <- read_dta(here("raw_data/glss4/aggregates","subagg15.dta")) #other agricultural sources
subagg16 <- read_dta(here("raw_data/glss4/aggregates","subagg16.dta")) #Sale of transformed crop prod

#Create ID to join variables later
subagg13 <- subagg13 %>% mutate(ID = paste(clust,nh,sep="_"))
subagg14 <- subagg14 %>% mutate(ID = paste(clust,nh,sep="_"))
subagg15 <- subagg15 %>% mutate(ID = paste(clust,nh,sep="_"))
subagg16 <- subagg16 %>% mutate(ID = paste(clust,nh,sep="_"))

#join variables
farms_profit <- subagg13 %>% full_join(subagg14,by = "ID")
farms_profit <- farms_profit %>% full_join(subagg15,by = "ID")
farms_profit <- farms_profit %>% full_join(subagg16,by = "ID")
farms_profit <-farms_profit %>%  select(ID,crpinc1,crpinc2,incothag,trcrpinc)

#COSTS
subagg22 <- read_dta(here("raw_data/glss4/aggregates","subagg22.dta")) #renting farm land
subagg23 <- read_dta(here("raw_data/glss4/aggregates","subagg23.dta")) #crop input
subagg24 <- read_dta(here("raw_data/glss4/aggregates","subagg24.dta")) #livestock input
subagg25 <- read_dta(here("raw_data/glss4/aggregates","subagg25.dta")) # foodprocessing (labor /others)
subagg31 <- read_dta(here("raw_data/glss4/aggregates","subagg31.dta")) # depreciation farming equip

#Create ID to join variables later
subagg22 <- subagg22 %>% mutate(ID = paste(clust,nh,sep="_")) %>% select(-clust,-nh)
subagg23 <- subagg23 %>% mutate(ID = paste(clust,nh,sep="_")) %>% select(-clust,-nh)
subagg24 <- subagg24 %>% mutate(ID = paste(clust,nh,sep="_")) %>% select(-clust,-nh)
subagg25 <- subagg25 %>% mutate(ID = paste(clust,nh,sep="_")) %>% select(-clust,-nh)
subagg31 <- subagg31 %>% mutate(ID = paste(clust,nh,sep="_")) %>% select(-clust,-nh)

#join variables
farms_profit <- farms_profit %>% left_join(subagg22)
farms_profit <- farms_profit %>% left_join(subagg23)
farms_profit <- farms_profit %>% left_join(subagg24)
farms_profit <- farms_profit %>% left_join(subagg25)
farms_profit <- farms_profit %>% left_join(subagg31)

#replace NA by 0
farms_profit <- farms_profit %>% mutate_all(funs(replace(., is.na(.), 0)))

#include total Revenue, Total Cost and Total Profit
farms_profit <- farms_profit %>% mutate(tot_rev = crpinc1 + crpinc2 + incothag + trcrpinc,
                                        tot_cost = expland + expcrop + expliv + expfdpr1 + expfdpr2 + depneq,
                                        tot_profit = tot_rev - tot_cost)

#remove variables
rm(subagg13,subagg14,subagg15,subagg16,subagg22,subagg23,subagg24,subagg25,subagg31)
#saving fina variable
saveRDS(farms_profit, file = here("data","farms_profit.RData"))

