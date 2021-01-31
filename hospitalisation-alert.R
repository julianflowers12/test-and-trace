## hospital cases and tiers

library(tidyverse)
library(readxl)
library(tidytext)

hosp <- read_csv("https://coronavirus.data.gov.uk/api/v2/data?areaType=nhsTrust&metric=hospitalCases&metric=newAdmissions&metric=covidOccupiedMVBeds&format=csv")

alert <- read_csv("https://coronavirus.data.gov.uk/api/v2/data?areaType=ltla&metric=alertLevel&format=csv")

lu <- read_excel("~/Dropbox/My Mac (Julians-MBP-2)/Downloads/2020 Trust Catchment Populations_Supplementary Trust Area lookup.xlsx")

write_csv(lu, "lu.csv")

cpop <- read_excel("~/Dropbox/My Mac (Julians-MBP-2)/Downloads/2020 Trust Catchment Populations Worksheet.xlsx", sheet = 2)

write_csv(cpop, "cpop.csv")


cpop <- cpop %>%
  filter(CatchmentYear == 2018) %>%
  group_by(TrustCode) %>%
  summarise(sumpop = sum(Catchment))

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
  left_join(hosp, by = c("TrustCode" = "areaCode")) %>%
  select(date.y, alertLevel, alertLevelName, TrustCode, TrustName, hospitalCases, newAdmissions, covidOccupiedMVBeds) %>%
  pivot_longer(names_to = "metrics", values_to = "vals", cols = hospitalCases:covidOccupiedMVBeds)



hosp_alerts %>%
  mutate(TrustName = str_remove_all(TrustName, "NHS|Trust|Foundation")) %>%
  left_join(cpop) %>%
  group_by(TrustCode, metrics) %>%
  mutate(seven_day = zoo::rollsum(vals, k = 7, na.pad = TRUE)) %>%
  mutate(seven_day_rate = 1000000 * seven_day/sumpop) %>%
  filter(date.y >= "2020-11-01") %>%
  mutate(trust = fct_reorder(TrustName, alertLevel)) %>%
  group_by(alertLevelName) %>%
  ggplot(aes(date.y, seven_day_rate, colour = metrics)) +
  geom_line(aes(group = metrics)) +
  facet_wrap(~TrustName) +
  scale_y_log10()
  scale_y_discrete()
  viridis::scale_color_viridis(discrete = TRUE, direction = -1, option = "inferno")

  hosp_alerts %>%
    mutate(TrustName = str_remove_all(TrustName, "NHS|Trust|Foundation"), 
           date.y = lubridate::ymd(date.y)) %>%
    left_join(cpop) %>%
    group_by(TrustCode) %>%
    mutate(seven_day = zoo::rollsum(hospitalCases, k = 7, na.pad = TRUE)) %>%
    mutate(seven_day_rate = 1000000 * seven_day/sum) %>%
    filter(date.y == max(date.y) - 3) %>%
    ggplot(aes(reorder(TrustName, seven_day_rate), seven_day_rate, fill = factor(alertLevelName))) +
    geom_col() +
    coord_flip()

#### corr
  
hosp1 <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=hospitalCases&metric=newAdmissions&metric=covidOccupiedMVBeds&format=csv")

hosp1 %>%
  na.omit() %>%
  filter(date >= "2020-09-01") %>%
  .[, 5:7] %>%
  lm(hospitalCases ~ lag(newAdmissions, 1) + lead(covidOccupiedMVBeds, 1), data = .) %>%
  broom::augment() %>%
  ggplot(aes(hospitalCases, .fitted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  xlim(c(0, 32500)) +
  ylim(c(0, 32500))






           