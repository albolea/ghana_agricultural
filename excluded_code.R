#Don't have all farmers
land_size <- sec8a1 %>% mutate(ID = paste(clust,nh,sep="_"),
                               hh_own_land = s8aq1,
                               hh_own_land_12m = s8aq2,
                               unit_area = s8aq3,
                               hh_owned_land = s8aq4,
                               hh_rent_land = s8aq13,
                               hh_rented_land = s8aq14,
                               hh_scrop = s8aq16,
                               hh_size_scrop = s8aq17,
                               prop_by_hh = s8aq19) %>% select(-starts_with("s")) %>% 
  filter(hh_own_land == 1 | hh_own_land_12m == 1)


aux <- sec0a %>% filter(eanum==2)
#auxb <- sec0a %>% filter(clust == 4002 & nh==1) # The Unique Household Identifier must be clulst + nh
#auxc <- sec1 %>% filter(clust==4002 & nh==1 & pid==1)# The Unique person Identifier must be clulst + nh + pid

#Next step: Understand which variables we need from the files.
sec8a1 <- read_dta(here("raw_data/glss4","sec8a1.dta"))
sec8a1 <- sec8a1 %>% mutate_all(funs(ifelse(.>1e+100,NA,as.numeric(.))))



s <- summary(aux)
capture.output(s, file = "myfile.txt")