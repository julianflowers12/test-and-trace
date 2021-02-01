## 

library(pacman)

p_load(tidyverse, scales, gtsummary)

url <- "https://api.daisy.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDateRollingSum&metric=newCasesBySpecimenDate&&metric=newLFDTests&metric=uniqueCasePositivityBySpecimenDateRollingSum&metric=uniquePeopleTestedBySpecimenDateRollingSum&format=csv"

csv <- read_csv(url)

head(csv)


data <- csv %>%
  mutate(seven_day = uniqueCasePositivityBySpecimenDateRollingSum * uniquePeopleTestedBySpecimenDateRollingSum) 

data %>%
  keep(is.numeric) %>%
  lm(newCasesBySpecimenDateRollingSum ~ .,, data = .) %>%
  #broom::glance()
  broom::augment() %>%
  ggplot(aes(newCasesBySpecimenDateRollingSum, .fitted)) +
  geom_point() +
  geom_smooth(method = "gam")
  gtsummary::tbl_regression()

data %>%
  ggplot(aes(newCasesBySpecimenDateRollingSum, seven_day, colour = date)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) 




