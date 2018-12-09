library(tidyverse)

survey1 <- read.csv("./Data/GEP_2013_EMP009.csv")
survey2 <- read.csv("./Data/GEP_2014_00A2.csv")
survey3 <- read.csv("./Data/GEP_2015_00A2.csv")
survey4 <- read.csv("./Data/GEP_2016_00A2.csv")

survey <- survey1 %>% 
  bind_rows(survey1, survey2, survey3, survey4)
  
survey.group <- as_tibble(survey) %>%
  rename(state = GEO.display.label, job = GOVEMPFUNCT.display.label) %>% 
  mutate(YEAR.id = as.factor(YEAR.id)) %>% 
  filter(state != "United States") %>% 
  group_by(state,job) %>% 
  summarise(avg = mean(PAYANN)) %>% 
  ungroup()
levels(survey.group$job)








