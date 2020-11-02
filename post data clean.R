library(haven)
library(tidyverse)
setwd("C:/Users/a2627/Downloads")
raw_data2 <- read_dta("usa_00002/usa_00002.dta")
raw_data2 <- labelled::to_factor(raw_data2)

raw_data2$age <- as.integer(raw_data2$age)  

reduced_data2 <- 
  raw_data2 %>% 
  na.omit() %>%
  filter(age >= 16) %>%
  dplyr::select(#region,
    #statefip,
    sex, 
    age,
    race)%>%
    #hispan,
    #marst, 
    #bpl,
    #citizen,
    #educd,
    #inctot
    #labforce
  rename(gender = sex) %>% mutate(gender = ifelse(gender=="male","Male","Female"))

reduced_data2<-reduced_data2 %>% mutate(race_asian_pacific = ifelse(race == "chinese"|race=="japanese"|race== "other asian or pacific islander" ,1,0))
reduced_data2<-reduced_data2 %>% mutate(race_white = ifelse(race == "white" ,1,0))
reduced_data2<-reduced_data2 %>% mutate(race_black = ifelse(race == "black/african american/negro" ,1,0))
reduced_data2<-reduced_data2 %>% mutate(race_other = ifelse(race != "chinese" & race!= "japanese"&
                                                            race != "other asian or pacific islander"&
                                                            race != "white"& race != "black/african american/negro",1,0))
reduced_data2<- reduced_data2 %>% dplyr::select(-race)
reduced_data2 <- 
  reduced_data2 %>%
  count(gender,age,race_asian_pacific,race_white,race_black,race_other) %>%
  group_by(gender,age,race_asian_pacific,race_white,race_black,race_other) 

write_csv(reduced_data2, "census_data.csv")
