## direction of changes

library(pacman)
p_load(tidyverse)

pos <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDate&metric=uniqueCasePositivityBySpecimenDateRollingSum&metric=uniquePeopleTestedBySpecimenDateRollingSum&metric=newCasesBySpecimenDateRollingSum&format=csv")

est_cases <- pos %>%
  mutate(cases = uniqueCasePositivityBySpecimenDateRollingSum * uniquePeopleTestedBySpecimenDateRollingSum/100)
est_cases %>% 
  mutate(diff = cases - newCasesBySpecimenDateRollingSum,
         cumdiff = cumsum(diff)) %>%
  
est_cases %>% 
  select(-c(uniqueCasePositivityBySpecimenDateRollingSum, uniquePeopleTestedBySpecimenDateRollingSum)) %>%
  pivot_longer(names_to = "metric", values_to = "data", cols =5:ncol(.)) %>%
  ggplot(aes(date, data, colour = metric)) +
  geom_line()

direction <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newAdmissionsDirection&metric=newCasesBySpecimenDateDirection&metric=newVirusTestsDirection&metric=newDeaths28DaysByPublishDateDirection&format=csv")

head(direction)

direction %>%
  pivot_longer(names_to = "metric", values_to = "value", 5:ncol(.)) %>%
  mutate(change = ifelse(value == "UP", 1,
                         ifelse(value == "DOWN", -1, 0))) %>%
  ggplot(aes(date, change, color = value)) +
  geom_col() +
  geom_vline(xintercept = as.Date("2020-11-05")) +
  facet_wrap(~metric)
