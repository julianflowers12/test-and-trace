library(tidyverse)
library(data.table)

csv <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=newDeaths28DaysByDeathDate&metric=newDeaths28DaysByPublishDateChange&metric=newDeaths28DaysByPublishDateChangePercentage&metric=newDeaths28DaysByPublishDate&release=2020-11-21&format=csv"

data <- fread(csv)

head(data)

data <- data  %>% 
      mutate(date = lubridate::ymd(date)) 

data_long <- data %>% 
     pivot_longer(names_to = "metric", values_to = "value", cols = 5:ncol(.))     
      
data_long %>%
      filter(str_detect(metric, "Change") & !str_detect(metric, "Percentage")) %>%
      ggplot(aes(date, value, colour = metric)) +
      geom_line(show.legend = FALSE) +
      facet_wrap(~areaName)

data_long[date >= "2020-09-01", ]
    filter9
