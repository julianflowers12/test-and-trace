library(jsonlite)
library(tidyverse)
library(data.table)
library(ukcovid19)

filters <- "areaType=ltla"

structure <- list(

       date = "date",
       area = "areaName",
       code = "areaCode",
       deaths = "newDeaths28DaysByDeathDate",
       cumdeaths = "cumDeaths28DaysByDeathDate"

)

test_data <- get_data(
       filters = filters,
       structure = structure
)
dim(test_data_1)
dim(test_data)
setDT(test_data)[date %between% c("2020-11-10", "2020-11-19"),]


test <- fread("~/Downloads/ltla_2020-11-15.csv")
test[, .N, by = .(areaName)]
test_wide <- test[date >= "2020-10-01", .(date, newDeaths28DaysByDeathDate, areaCode, areaName)]  %>% 
    pivot_wider(names_from = "date", values_from = "newDeaths28DaysByDeathDate")


test15 <- fread("~/Downloads/ltla_2020-11-15.csv")
test15[, .N, by = .(areaName)]
test_wide_15 <- test15[date >= "2020-10-01", .(date, newDeaths28DaysByDeathDate, areaCode, areaName)]  %>% 
    pivot_wider(names_from = "date", values_from = "newDeaths28DaysByDeathDate")


test_wide_15 %>% gt::gt()

test_wide %>% gt::gt()

test17 <- fread("~/Downloads/ltla_2020-11-17.csv")
test17[, .N, by = .(areaName)]
test_wide_17 <- test17[date >= "2020-10-01", .(date, newDeaths28DaysByDeathDate, areaCode, areaName)]  %>% 
    pivot_wider(names_from = "date", values_from = "newDeaths28DaysByDeathDate")

test_wide_17 %>% gt::gt()

deltas <- test17  %>% 
       select(areaCode, areaName, date, newDeaths28DaysByDeathDate, newDeaths28DaysByPublishDate, newDeaths28DaysByPublishDateChange)  %>% 
       left_join(test, by = c("areaName", "areaCode", "date"))  %>% 
       mutate(deaths_change = newDeaths28DaysByDeathDate.x - newDeaths28DaysByDeathDate.y, 
              deaths_change_1 = newDeaths28DaysByPublishDate.x - newDeaths28DaysByPublishDate.y,
              deaths_change_2 = newDeaths28DaysByPublishDateChange.x - newDeaths28DaysByPublishDateChange.y) %>% 
       group_by(areaName) %>%
       summarise(delta = sum(deaths_change, na.rm = TRUE), 
                 delta1 = sum(deaths_change_1, na.rm = TRUE), 
                 delta2 = sum(deaths_change_2, na.rm = TRUE)) 

mean(deltas$delta)      
       
deltas %>%
       ggplot(aes(reorder(areaName, delta), delta)) +
       geom_col() +
       coord_flip() +
       theme(axis.text = element_text(size = 4))

deltas  %>% 
       filter(delta < -9|delta >= 10)  %>% 
       arrange(delta)  %>% 
       gt::gt()


## england

la_change <- fread("~/Downloads/utla_2020-11-21.csv")

test_json <- fromJSON("~/Downloads/utla_E92000001_2020-11-21.json")

test_json

test <- la_change  %>%   
        pivot_longer(names_to = "metric", values_to = "values", cols = 5:9)

la_change  %>% 
       #filter(!str_detect(metric, "^cum")) %>%
       ggplot(aes(date, newDeaths28DaysByPublishDateChange, group = areaName, colour = areaName)) +
       geom_line(show.legend = FALSE) +
       facet_wrap(~areaName)




url <- "https://api.coronavirus.data.gov.uk/v1/data?filareaType=ltla&metric=cumDeaths28DaysByDeathDate&metric=newDeaths28DaysByDeathDate&format=csv"
url1 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=cumDeaths28DaysByDeathDate&metric=newDeaths28DaysByDeathDate&format=csv&release=2020-11-15"

data <- fread(url)
data1 <- fread(url1)


d <- data[date %between% c("2020-11-10", "2020-11-15"), ][order(areaCode, desc(date))]
d1 <- data1[date %between% c("2020-11-10", "2020-11-15"), ][order(areaCode, desc(date))]

d[, .N, by = .(areaName)]
data[, .N, by = .(areaName)]


setkeyv(data, c("areaCode", "areaName", "date"))
setkeyv(data1, c("areaCode", "areaName", "date"))

glimpse((data1))
data2 <- data  %>% 
    full_join(data1, by = c("areaCode", "date"))

head(data2)    

#data2 <- data.table::rbindlist(list(data, data1))

data2 <- data2[date >= "2020-11-10" & str_detect(areaCode, "^E"), ]

data2 %>% reactable::reactable(sortable = TRUE, filterable = TRUE)

url2 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&metric=cumDeaths28DaysByPublishDate&metric=newDeaths28DaysByPublishDate&format=csv&release=2020-11-16"
url3 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=cumDeaths28DaysByPublishDate&metric=newDeaths28DaysByPublishDate&format=csv&release=2020-11-16"

data3 <- fread(url2)
data4 <- fread(url3)

data5 <- data.table::rbindlist(list(data3, data4))

data5 <- data5[date >= "2020-11-10" & str_detect(areaCode, "^E"), ]

data5  %>% 
    left_join(data2, by = c("date", "areaCode"))
