library(jsonlite)
library(tidyverse)
library(patchwork)

url <- "https://daisy.coronavirus.data.gov.uk/api/v1/data?filters=areaType=utla&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22uniqueCasePositivityBySpecimenDateRollingSum%22:%22uniqueCasePositivityBySpecimenDateRollingSum%22,%22uniquePeopleTestedBySpecimenDateRollingSum%22:%22uniquePeopleTestedBySpecimenDateRollingSum%22%7D&format=json"
urls <- paste0(url, "&page=", 1:19)


data <- map(urls, fromJSON)

head(data)

data <- map_dfr(data, "data")
data

data <- data %>%
  mutate(date = lubridate::ymd(date)) 

head(data)

g <- data %>%
  #group_by(areaName) %>%
  ggplot(aes(date, uniqueCasePositivityBySpecimenDateRollingSum, group = date, colour = areaName)) +
  geom_boxplot(fill = 'grey') +
  labs(title = "Boxplot of daily positivity rates") +
  ggthemes::theme_economist()
library(plotly)

ggplotly(g)

ol <- data %>%
  ungroup() %>%
  group_by(date) %>%
  summarise(q = 1.5 * quantile(uniqueCasePositivityBySpecimenDateRollingSum, 0.75, na.rm = TRUE)) %>%
  left_join(data) %>%
  mutate(outlier = ifelse(uniqueCasePositivityBySpecimenDateRollingSum > q, 1, 0)) %>%
  filter(outlier > 0) %>%
  ggplot(aes(date, fct_rev(areaName))) +
  geom_tile() +
  scale_y_discrete(position = "right") +
  labs(y = "", 
       title = "Daily outliers", 
       subtitle = "Oultier  = 1.5 * 75th centile") +
  theme(axis.text.y = element_text(size = 6))
  
  
g + ol + ggsave("boxplot.png")
