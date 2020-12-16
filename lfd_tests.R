library(tidyverse)
library(jsonlite)

url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&metric=newLFDTests&metric=cumLFDTests&format=csv"

lfd <- read_csv(url)

lfd %>%
  count(areaName)

lfd %>%
  head()

lfd  %>% 
  mutate(date = lubridate::ymd(date)) %>%
  ggplot(aes(date, cumLFDTests, fill = areaName)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~areaName) +
  scale_y_log10()
