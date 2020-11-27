library(tidyverse)

uri <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=changeInNewCasesBySpecimenDate&metric=covidOccupiedMVBeds&metric=newAdmissions&metric=newDeaths28DaysByDeathDate&metric=newCasesBySpecimenDate&format=csv"

data <- read_csv(uri)

data <- data  %>% 
       mutate(date = lubridate::ymd(date))


1259/59


data_filtered <- data  %>% 
      filter(date >= "2020-09-01")  %>% 
      na.omit()

data_filtered  %>% 
       pivot_longer(names_to = "metric", values_to = "value", cols = 5:ncol(.))  %>% 
       group_by(metric)  %>% 
       filter(!str_detect(metric, "change")) %>%
       mutate(delta = (value  + 0.005) / value[83])  %>% 
       arrange(metric, desc(date))
       ggplot(aes(date, delta, color = metric)) +
       geom_smooth(method = "loess", se = FALSE) +
       geom_col(fill = 'blue', colour = 'blue', alpha = 0.5) +
       facet_wrap(~metric)

