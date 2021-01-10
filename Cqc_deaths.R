## cqc care notified home deaths

library(tidyverse)
library(readxl)
library(janitor)



cqc <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fnumberofdeathsincarehomesnotifiedtothecarequalitycommissionengland%2f2020/20210104officialsensitivecoviddeathnotification.xlsx"

destfile <- tempfile()

curl::curl_download(cqc, destfile = destfile)

cqc_data <- excel_sheets(destfile)

cqc_data

info <- read_excel(destfile, sheet = 2, skip = 2) %>%
  clean_names() %>%
  remove_empty()

info %>%
  knitr::kable()


## Totals
t1 <- read_excel(destfile, sheet = 3, skip = 2) %>%
  clean_names() %>%
  mutate(date_of_notification = excel_numeric_to_date(as.numeric(date_of_notification)),
         info = cqc_data[3])


## By Location
t2 <- read_excel(destfile, sheet = 4, skip = 2) %>%
  clean_names() %>%
  pivot_longer(names_to = "date", values_to = "count", cols = 2:ncol(.)) %>%
  mutate(date = str_remove(date, "x"), 
         date = excel_numeric_to_date(as.numeric(date)),
         info = cqc_data[4], 
         cause = "covid") %>%
  rename(location = x1) 

t2 %>%
  count(cause)


## plot England
t2 %>%
  filter(location == "England") %>%
  ggplot(aes(date, count)) +
  geom_line() +
  geom_smooth(se = FALSE)



t3 <- read_excel(destfile, sheet = 5, skip = 2) %>%
  clean_names() %>%
  pivot_longer(names_to = "date", values_to = "count", cols = 2:ncol(.)) %>%
  mutate(date = str_remove(date, "x"), 
         date = excel_numeric_to_date(as.numeric(date)),
         info = cqc_data[5], 
         cause = "all cause") %>%
  rename(location = x1)

t3 %>%
  count(cause)

t3 %>%
  bind_rows(t2) %>%
  select(-info) %>%
  #dim()
  #filter(location == "England") %>%
  pivot_wider(names_from = "cause", values_from = "count", values_fn = list) %>% 
  unnest() %>%
  slice(1:ncol(.)-263) %>%
  mutate(covid_percent = covid/(covid + `all cause`)) %>%
  #unnest()
  reactable::reactable(filterable = TRUE, sortable = TRUE)
  ggplot(aes(date, covid_percent)) +
  geom_col() +
  geom_smooth(se = FALSE)





