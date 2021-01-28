## delays in deployment

library(jsonlite)
library(tidyverse)
library(lubridate)
library(qicharts2)

times <- jsonlite::fromJSON("https://coronavirus.data.gov.uk/public/assets/dispatch/dates.json")

times1 <- map(times, lubridate::ymd_hms) %>%
  enframe() %>%
  unnest() %>%
  mutate(time = hour(value), 
         date = ymd(name), 
         date = as.POSIXct(date) + 60*60*16, 
         diff_mins = difftime(value, date, units = "mins")) 


times1 %>%
  ggplot(aes(as.Date(name), diff_mins)) +
  geom_point()

c <- qicharts2::qic(x = times1$date, y = times1$diff_mins, chart = "i", target = 0, part = c(56, 100))
c
glimpse(c)
c$data %>%
  View()

c$data[which(c$data$y.sum > c$data$ucl), ]

