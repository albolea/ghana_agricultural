library(tidyverse)
library(here)
library(GGally)
library(car)
library(foreign)
#to read data use haven or foreign
#start with G4report.pdf

sec0a <- read.dta(here("raw_data/glss4","sec0a.dta"))
sec1 <- read.dta(here("raw_data/glss4","sec1.dta"))

View(sec0a)
View(sec1)

aux <- sec0a %>% filter(eanum==2)
auxb <- sec0a %>% filter(clust == 4002 & nh==1) # The Unique Household Identifier must be clulst + nh
auxc <- sec1 %>% filter(clust==4002 & nh==1 & pid==1)# The Unique person Identifier must be clulst + nh + pid

#Next step: Understand which variables we need from the files.
