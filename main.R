library(tidyverse)
library(here)
library(GGally)
library(car)
library(haven)

readRDS(file=here("data", "farms_profit.RData"))
if (!exists("farms_profit")) source("generate_farms_profit.R")

summary(farms_profit)
farms_profit %>% filter(tot_profit > 0 | tot_profit < 0 ) %>% ggplot(aes(tot_profit/100000)) + geom_histogram(binwidth = 10)

farms_profit %>% filter(tot_profit > 0 | tot_profit < 0 ) %>% summary(farms_profit)
