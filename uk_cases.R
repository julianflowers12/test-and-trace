library(tidyverse)
options(scipen = 999)

uk_data <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newCasesBySpecimenDateRollingRate&metric=newCasesBySpecimenDateRollingSum&format=csv")

uk_data %>%
  mutate(uk_pop = 66787086,
         est_cases = newCasesBySpecimenDateRollingRate * uk_pop / 100000, 
         est_pop = newCasesBySpecimenDateRollingSum / newCasesBySpecimenDateRollingRate * 100000, 
         newCasesBySpecimenDateRollingSum - est_cases) %>%
  View()

 
