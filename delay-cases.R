library(pacman)
p_load(tidyverse, data.table, jsonlite)

url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&metric=changeInNewDeaths28DaysByDeathDate&metric=newCasesByPublishDate&metric=newDeaths28DaysByPublishDate&format=json"
dates <- seq.Date(to = as.Date("2020-11-24"), from = as.Date("2020-11-16"), by = "days")
dates

urls <- paste0("https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&metric=changeInNewCasesBySpecimenDate&metric=newDeaths28DaysByPublishDate&metric=newDeaths28DaysByDeathDate&release=", dates, "&format=json")
urls1 <- c(url, urls)

jsons <- map_dfr(urls1, fromJSON)


str(jsons)
setNames(jsons, "dates" )
data <- map_dfr(jsons, "body")
data1 <- map(jsons, "body")

data1  %>% 
    map(., "date")  %>% 
    map(., max)

head(data)

setDT(data)
data[areaName == "Leeds" & date == "2020-11-16",  ]

[order(desc(date))]  %>% 
       group_by(date)  %>% 
       mutate(id = row_number())  %>% 
       select(-changeInNewCasesBySpecimenDate)  %>% 
       mutate(start = ifelse)
