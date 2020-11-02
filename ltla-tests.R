## ltla data

library(tidyverse)
library(lubridate)

ltla <- read_ods("data/P2_tests_processed_LTLA_w21.ods", sheet = 2)

ltla <- ltla %>%
  mutate(`29/06/20` = as.numeric(`29/06/20`)) %>%
  pivot_longer(names_to = "date", values_to = "value", cols = 7:ncol(.))

ltla$date %>%
  unique()

totals <- ltla %>%
  filter(date == "Total since Test and Trace launched")

dated <- ltla %>%
  filter(date != "Total since Test and Trace launched")

date1 <- dated %>%
  mutate(date = paste0(date, "20"),
         date = lubridate::dmy(date))
View(date1)

date1 %>%
  filter(`LTLA Name` == "ENGLAND") %>%
  ggplot(aes(date, value)) +
  geom_line() +
  geom_smooth()


areas <- pull(dated, "LTLA Name") %>%
  unique()

plot_tests <- function(area) {
 
  g <-  date1 %>%
    filter(`LTLA Name`== area) %>%
    ggplot(aes(date, value)) +
    geom_line() +
    geom_smooth(method = "gam") +
    ggtitle(paste0("Daily tests in ", area))
  
  print(g)
}

plot_tests(area = areas[158])
