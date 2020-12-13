## direction of changes

library(pacman)
p_load(tidyverse)

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
