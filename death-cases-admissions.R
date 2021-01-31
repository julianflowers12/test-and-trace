library(tidyverse)



url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newAdmissions&metric=newCasesBySpecimenDate&metric=newDeaths28DaysByDeathDate&format=csv"


data <- readr::read_csv(url) %>%
  filter(date >= "2020-04-01")

scaled <- data %>%
  #mutate_if(is.numeric, scale) %>%
  .[5:7] %>%
  lm(lag(newDeaths28DaysByDeathDate, n = 21) ~ ., .)

mod <- function(df, n){
  
  model <- data %>%
    #mutate_if(is.numeric, scale) %>%
    .[5:7] %>%
    lm(lag(newDeaths28DaysByDeathDate, n = n) ~ newAdmissions, .)
  
  r2 <- broom::glance(model)[1]
  out <- list(model = model, r2 = r2)
  
}




test <- map(1:40, ~(mod(data, .x))) %>%
  map(., "r2")

test

test %>% 
  mutate(n = row_number()) %>%
  ggplot(aes(n, r.squared)) +
  geom_point() +
  geom_smooth()

## opt = 9/10

opt_mod <- mod(data, 10)

broom::tidy(opt_mod$model)


broom::glance(scaled)


%>%
  ggplot(aes(date, newAdmissions[, 1])) +
  geom_smooth(method = "gam")  +           
  geom_smooth(aes(date, newCasesBySpecimenDate[, 1]), method = "gam", colour = "red") +
  geom_smooth(aes(date, newDeaths28DaysByDeathDate[, 1]), method = "gam", colour = "green") 
  

data %>%
  mutate_if(is.numeric, scale) %>%
  ggplot(aes(newCasesBySpecimenDate[,1], newAdmissions[, 1])) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_point(aes(newCasesBySpecimenDate[,-1], newDeaths28DaysByDeathDate[,-1]))

+           
  geom_smooth(aes(date, newCasesBySpecimenDate[, 1]), method = "gam", colour = "red") +
  geom_smooth(aes(date, newDeaths28DaysByDeathDate[, 1]), method = "gam", colour = "green") 


data %>%
  ggplot(aes(newDeaths28DaysByDeathDate, newAdmissions)) +
  geom_point() +
  geom_point(aes(newDeaths28DaysByDeathDate, newAdmissions))

