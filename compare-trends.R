## comparison test - hosp - deaths

library(ukcovid19)
library(tidyverse)


filters <- "areaName=England"

structure <- list(
  
  date = "date",
  cases = "newCasesBySpecimenDate",
  deaths = "newDeaths28DaysByDeathDate",
  admissions = "newAdmissions",
  mv = "covidOccupiedMVBeds"
  
)

data <- get_data(
  
  filters = filters,
  structure = structure
  
)

data_long <- data %>%
  pivot_longer(names_to = "metric", 
               values_to = "value", 
               cols = 2:5) %>%
  mutate(date = as.Date(date))

data_long %>%
  ggplot(aes(date, value, colour = metric, group = metric)) +
  geom_line() +
  geom_smooth(method = "gam")
