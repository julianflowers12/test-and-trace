## daily positivity
library(tidyverse)
library(lubridate)

data <- read_csv("https://daisy.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=England&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesBySpecimenDate%22:%22newCasesBySpecimenDate%22,%22cumCasesBySpecimenDate%22:%22cumCasesBySpecimenDate%22,%22newPeopleTestedBySpecimenDate%22:%22newPeopleTestedBySpecimenDate%22,%22cumPeopleTestedBySpecimenDate%22:%22cumPeopleTestedBySpecimenDate%22%7D&format=csv")

head(data)

pos <- data %>%
  mutate(date = lubridate::ymd(date)) %>%
  arrange(date) %>%
  mutate(pos =  newCasesBySpecimenDate / newPeopleTestedBySpecimenDate, 
         cumpos = cumCasesBySpecimenDate / cumPeopleTestedBySpecimenDate, 
         sevenDayPos = zoo::rollmean(pos, k = 7, align = "center", na.pad = TRUE)) %>%
  filter(date < today() - days(5))

head(pos) 
%>%
  select(date)
pos %>% 
  ggplot() +
  #geom_col(aes(date, pos)) +
  geom_line(aes(date, cumpos)) +
  geom_line(aes(date, sevenDayPos, colour = "red"))
