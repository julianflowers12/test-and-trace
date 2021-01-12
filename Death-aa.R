library(tidyverse)
library(GGally)

## download cumulative death rates for each local authority

data <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&metric=cumDeaths60DaysByDeathDateRate&metric=cumOnsDeathsByRegistrationDateRate&metric=cumDeaths28DaysByDeathDateRate&format=csv")

summary(data)

## convert dates to week numbers

latest_data <- data %>%
  mutate(week = ifelse(!is.na(cumOnsDeathsByRegistrationDateRate), 1, 0)) %>%
  mutate(weekno = lubridate::week(date))

## transform to long format, filter for England LAs, calculate mean weekly rate

ldw <- latest_data %>%
  pivot_longer(names_to = "metric", values_to = "data", cols = 5:7) %>%
  filter(str_detect(areaCode, "^E")) %>%
  group_by(areaName, metric, weekno) %>%
  summarise(weeksum = mean(data, na.rm = TRUE ))

## plot latest week as scatter plot

ldw_latest <- ldw %>%
  filter(weekno == 50) %>%
  pivot_wider(names_from = "metric", values_from = "weeksum") 

ldw_latest %>%
  ggscatmat(columns = 3:5) +
  geom_smooth(method = "lm", se = FALSE)

## model
library(ggfortify)
ldw_latest_mod <- lm(cumOnsDeathsByRegistrationDateRate ~ cumDeaths60DaysByDeathDateRate, data = ldw_latest)

broom::augment(ldw_latest_mod ,ldw_latest) %>%
  ggplot(aes(cumOnsDeathsByRegistrationDateRate, .fitted)) +
  geom_point() +
  geom_smooth(method = "lm")

broom::tidy(ldw_latest_mod)



## ONS death rates (COVID on death certificate) are
## highly correlated with death rates in people with 
## +ve covid tests within 28 and 60 days


