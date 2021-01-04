## hospital cases and tiers

library(tidyverse)
library(readxl)
library(tidytext)

hosp <- read_csv("https://coronavirus.data.gov.uk/api/v2/data?areaType=nhsTrust&metric=hospitalCases&format=csv")
mv  <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsTrust&metric=covidOccupiedMVBeds&format=csv")


alert <- read_csv("https://coronavirus.data.gov.uk/api/v2/data?areaType=ltla&metric=alertLevel&format=csv")

lu <- read_csv("lu.csv")

cpop <- read_csv("cpop.csv")

cpop <- cpop %>%
  filter(CatchmentYear == 2018) %>%
  group_by(TrustCode) %>%
  summarise(sum = sum(Catchment))

## from here <https://app.box.com/s/qh8gzpzeo1firv1ezfxx2e6c4tgtrudl/file/646073527000>

lu %>%
  count(TrustType)

## latest alert level
alert_eng <- alert %>%
     filter(str_detect(areaCode, "^E"), 
            date == max(date))  

## mapping trust to ltlas

trust_alerts <- alert_eng %>%
  left_join(lu, by = c("areaCode" = "LTLAApr19CD")) %>%
  filter(TrustType != "Acute - Specialist")

hosp_alerts <- trust_alerts %>%
  left_join(mv, by = c("TrustCode" = "areaCode")) %>%
  select(date.y, alertLevel, alertLevelName, TrustCode, TrustName, covidOccupiedMVBeds)


hosp_alerts %>%
  mutate(TrustName = str_remove_all(TrustName, "NHS|Trust|Foundation"),
         TrustName = str_trim(TrustName)) %>%
  left_join(cpop) %>%
  group_by(TrustCode) %>%
  mutate(seven_day = zoo::rollsum(covidOccupiedMVBeds, k = 7, na.pad = TRUE)) %>%
  mutate(seven_day_rate = 1000000 * seven_day/sum) %>%
  filter(date.y >= "2020-11-01") %>%
  mutate(trust = fct_reorder(TrustName, alertLevel)) %>%
  group_by(alertLevelName) %>%
  ggplot(aes(date.y, seven_day_rate)) +
  geom_line(aes(group = alertLevelName, colour = factor(alertLevel))) +
  facet_wrap(~TrustName) +
  scale_y_reordered() +
  labs(title = "Population rate of mechanical ventialation bed occupancy per million population", 
         subtitle = "By NHS Trust: 7-day rate") +
  viridis::scale_color_viridis(discrete = TRUE, direction = -1, option = "plasma", end = .5)

hosp_alerts %>%
    mutate(TrustName = str_remove_all(TrustName, "NHS|Trust|Foundation"), 
           date.y = lubridate::ymd(date.y)) %>%
    left_join(cpop) %>%
    group_by(TrustCode) %>%
    mutate(seven_day = zoo::rollsum(covidOccupiedMVBeds, k = 7, na.pad = TRUE)) %>%
    mutate(seven_day_rate = 1000000 * seven_day/sum) %>%
    filter(date.y == max(date.y) - 3) %>%
    ggplot(aes(reorder(TrustName, seven_day_rate), seven_day_rate, fill = factor(alertLevelName))) +
    geom_col() +
    coord_flip()

           