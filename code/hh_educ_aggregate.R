#import package
library(haven)
library("here")
library("tidyverse")
library("GGally")
library(plyr)
getwd()
#load raw data

sec2a<-read_dta(here::here("raw_data/glss4","sec2a.dta")) # education data part A
educ_level<- read_csv(here::here("raw_data","heduclv.csv")) # customized education leve 

# tidying data
hh_educ_a<-sec2a %>% select(clust, nh, pid,s2aq2)#gather household education by housthold and person id
hh_educ_a$s2aq2<-as.integer(hh_educ_a$s2aq2) #convert datatype and remove NA (opt)
    #hh_educ_a[is.na(hh_educ_a) ] <- 0

# aggregate number member of household in each education lv
hh_educ_a<- hh_educ_a %>% inner_join(educ_level) %>%
                      group_by(clust, nh, categID) %>%
  count(c("clust", "nh", "categID","educ_categ") )
    
#spread education category to cloumn with value is freq in hh
hh_educ_lv<-hh_educ_a %>%
              select(clust,nh,educ_categ,freq)%>%
              spread(key=educ_categ,value = freq)


