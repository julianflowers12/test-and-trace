library(tidyverse)
library(data.table)
data <- fread("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=cumCasesByPublishDate&metric=newCasesByPublishDate&metric=newCasesBySpecimenDate&metric=cumCasesBySpecimenDate&format=csv")

head(data)

data[,][order(areaName, date)][, cumsum := cumsum(newCasesByPublishDate)][,diff := cumsum - cumCasesByPublishDate][date == max(date), ][]

data_w <- data  %>% 
    pivot_wider(names_from = "areaName", values_from = c("newCasesByPublishDate", "newCasesBySpecimenDate", "cumCasesByPublishDate". "cumCasesBySpecimenDate"))  %>% 
    select(-areaCode)  %>% 
    setDT()

data_w

data1 <- data %>% 
    group_by(areaName) %>%
    arrange(date) %>%
    mutate(cumsum = sum(newCasesByPublishDate))  %>% 
    arrange(areaName, desc(date) ) %>% 
    mutate(diff = cumsum - cumCasesByPublishDate) %>% 
    select(date, areaName, cumCasesByPublishDate, cumsum, diff, cumCasesBySpecimenDate)
data %>% select(date, areaName, cumsum, diff) %>% 
     filter(date >= "2020-07-01")  %>% 
     head()
    ggplot(aes(date, diff )) +
    geom_col()
summary(data$diff)
data  %>% 
    ggplot(aes(date, cumCasesByPublishDate)) +
    geom_col() +
    geom_line(aes(date, cumsum))
