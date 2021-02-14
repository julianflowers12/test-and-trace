## lockdowns

library(pacman)
p_load(tidyverse, data.table)

### url

data <- fread("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newAdmissions&metric=newCasesBySpecimenDate&metric=newDeaths28DaysByDeathDate&metric=covidOccupiedMVBeds&format=csv")
ld <- fread("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=alertLevel&format=csv")

ld[areaCode == "E06000054", ]


data <- data[!is.na(newAdmissions) & date >= "2020-10-01", ] 

data_tx <- data %>%
  pivot_longer(names_to = "metric", 
               values_to = "value", 
               cols = 5:8) %>%
  group_by(metric) %>%
  mutate(mra = zoo::rollmean(value, 7, align = "center", na.pad = TRUE), 
         max = max(mra, na.rm = TRUE), 
         max_match = ifelse(max == mra, 1, 0)) 

data_tx %>%
  View()
dates <- data_tx %>%
  #ount(max)
  filter(max_match == 1) %>%
  pluck("date")


data_tx %>%
  ggplot(aes(date, value, colour = metric)) +
  geom_line(aes(date, mra)) +
  #geom_smooth(method = "gam", se = FALSE) +
  geom_vline(xintercept = as.Date(dates[1]),  lwd = .2, lty = 2) +
  geom_vline(xintercept = as.Date(dates[2]), lwd = .2, lty = 2) +
  geom_vline(xintercept = as.Date(dates[3]), lwd = .2, lty = 2) +
  geom_vline(xintercept = as.Date(dates[4]), lwd = .2, lty = 2) +
  geom_vline(xintercept = as.Date("2021-01-04"), colour = "red") +
  geom_vline(xintercept = as.Date("2020-11-04"), colour = "red") +
  scale_y_log10() +
  ggthemes::theme_economist() +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 8)) +
  labs(caption = "Seven day rolling averages\nSource: https://coronavirus.data.gov.uk\nAccessed 31st Jan 2021") 
 
