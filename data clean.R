library(haven)
library(tidyverse)
setwd("C:/Users/a2627/Downloads")
raw_data <- read_dta("ns20200625/ns20200625.dta")
raw_data <- labelled::to_factor(raw_data)

reduced_data <- 
  raw_data %>% 
  dplyr::select(
         vote_2020,
         gender,
         race_ethnicity,
         age)%>%
  filter(vote_2020 != "I would not vote") %>% 
  filter(vote_2020 != "I am not sure/don't know") %>% 
  filter(vote_2020 != "Someone else") %>%
  mutate(vote_trump = ifelse(vote_2020 =="Donald Trump",1,0))%>% 
  dplyr::select(-vote_2020) %>%
  na.omit() %>%
  rename(race = race_ethnicity)

reduced_data <- reduced_data %>% mutate(race_asian_pacific = ifelse(race == "Asian (Asian Indian)"|race == "Asian (Chinese)"|
                                            race == "Asian (Filipino)"|race == "Asian (Japanese)"|
                                            race == "Asian (Korean)" |race == "Asian (Korean)"|
                                            race == "Asian (Vietnamese)" |race == "Asian (Other)"|
                                            race == "Pacific Islander (Native Hawaiian)"|race == "Pacific Islander (Guamanian)"|
                                            race == "Pacific Islander (Samoan)" | race == "Pacific Islander (Other)" ,1,0))

reduced_data <- reduced_data %>% mutate(race_white = ifelse(race == "White" ,1,0))
reduced_data <- reduced_data %>% mutate(race_black = ifelse(race == "Black, or African American" ,1,0))
reduced_data <- reduced_data %>% mutate(race_other = ifelse(race == "	American Indian or Alaska Native"| race=="Some other race" ,1,0))

reduced_data <- reduced_data %>% dplyr::select(-race)


write_csv(reduced_data, "survey_data.csv")