#import package
library(haven)
library("here")
library("tidyverse")
library("GGally")
library(plyr)
##################################################################

# find the highest level in household
sec2a<-read_dta(here::here("raw_data/glss4","sec2a.dta")) # education data part A
educ_level<- read_csv(here::here("raw_data","heduclv.csv")) # customized education leve 

# tidying data
hh_educ_a<-sec2a %>% select(clust, nh, pid,s2aq2)#gather household education by housthold and person id
hh_educ_a$s2aq2<-as.integer(hh_educ_a$s2aq2) #convert datatype and remove NA (opt)
#hh_educ_a[is.na(hh_educ_a) ] <- 0

# aggregate number member of household in each education lv
hh_educ_dfa<- hh_educ_a %>% group_by(clust, nh, s2aq2) %>% filter(s2aq2!=96)
hh_educ_dfb<- hh_educ_a %>% group_by(clust, nh, s2aq2) 
hh_educ_dfc<-hh_educ_a %>% group_by(clust, nh, s2aq2) %>% filter(s2aq2==96)

dfa<-aggregate(s2aq2~ clust + nh, data=hh_educ_dfa,max)
dfb<-aggregate(s2aq2~ clust + nh, data=hh_educ_dfb,max)
dfc<-aggregate(s2aq2~ clust + nh, data=hh_educ_dfc,max)
dfd<-rbind(dfa,dfc)

highest_education<- function(df){
  for (i in 1:nrow(df)){
    vclust= df$clust[i]
    vnh=df$nh[i]
    vs2aq2=df$s2aq2[i]
    if (!is.null(vs2aq2) && 
        length(vs2aq2) == 1 && 
        !is.na(vs2aq2) && 
        vs2aq2){
      df<-df[!(df$clust==vclust & df$nh==vnh & df$s2aq2==96),]
    }
  }
  return(df)
}
dff<-highest_education(dfd)
nrow(dfd)