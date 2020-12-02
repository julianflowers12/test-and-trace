library(tidyverse)
library(lubridate)
library(data.table)

csv <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&metric=newDeaths28DaysByDeathDate&metric=newDeaths28DaysByPublishDateChange&metric=newDeaths28DaysByPublishDateChangePercentage&metric=newDeaths28DaysByPublishDate&release=2020-11-21&format=csv"
csv1 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&metric=newDeaths28DaysByDeathDate&metric=newDeaths28DaysByPublishDateChange&metric=newDeaths28DaysByPublishDateChangePercentage&metric=newDeaths28DaysByPublishDate&release=2020-11-15&format=csv"

post <- fread(csv)
pre <- fread(csv1)


post <- post[, date := ymd(date)]
pre <- pre[, date := ymd(date)]


setkeyv(pre, c("date", "areaName"))
setkeyv(post, c("date", "areaName"))

comb <- pre  %>% 
       full_join(post, by = c("areaCode", "date"))

comb[date %between% c("2020-09-01", "2020-11-21"),] %>% write_csv("death-check-2.csv")

comb <- comb[, `:=` (delta = newDeaths28DaysByDeathDate - i.newDeaths28DaysByDeathDate, delta1 = newDeaths28DaysByPublishDate - i.newDeaths28DaysByPublishDate)][]


comb_wide <- comb[date %between% c("2020-09-01", "2020-11-20"), .(date, areaName, delta)][order(areaName, desc(date))]  %>% 
       pivot_wider(names_from = "date", values_from = "delta", values_fill = 0) 



comb %>% write_csv("deaths_check_1.csv")


nov <- post[date == "2020-11-15", ]
nov1 <- pre[date == "2020-11-15", ]

nov1[nov][str_detect(areaCode, "^E"),]


cum <- read_csv("https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=England&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesByPublishDate%22:%22newCasesByPublishDate%22,%22cumCasesByPublishDate%22:%22cumCasesByPublishDate%22%7D&format=csv")

cum %>% summarise(across(where(is.numeric), sum, na.rm = TRUE))
head(cum)

























































































































































































































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
