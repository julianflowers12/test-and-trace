## hospital cases and tiers

library(tidyverse)
library(readxl)

hosp <- read_csv("https://coronavirus.data.gov.uk/api/v2/data?areaType=nhsTrust&metric=hospitalCases&format=csv")

alert <- read_csv("https://coronavirus.data.gov.uk/api/v2/data?areaType=ltla&metric=alertLevel&format=csv")

lu <- read_excel("~/Dropbox/My Mac (Julians-MBP-2)/Downloads/2020 Trust Catchment Populations_Supplementary Trust Area lookup.xlsx")

cpop <- read_excel("~/Dropbox/My Mac (Julians-MBP-2)/Downloads/2020 Trust Catchment Populations Worksheet.xlsx", sheet = 2)

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
  left_join(hosp, by = c("TrustCode" = "areaCode")) %>%
  select(date.y, alertLevel, alertLevelName, TrustCode, TrustName, hospitalCases)


hosp_alerts %>%
  mutate(TrustName = str_remove_all(TrustName, "NHS Trust|Foundation")) %>%
  left_join(cpop) %>%
  group_by(TrustCode) %>%
  mutate(seven_day = zoo::rollsum(hospitalCases, k = 7, na.pad = TRUE)) %>%
  mutate(seven_day_rate = 1000000 * seven_day/sum) %>%
  filter(date >= "2020-09-01") %>%
  ggplot(aes(date.y, seven_day_rate)) +
  geom_line(aes(group = alertLevelName, colour = factor(alertLevel))) +
  facet_wrap(~TrustName) +
  viridis::scale_color_viridis(discrete = TRUE, direction = -1, option = "inferno")


