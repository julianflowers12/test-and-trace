## cqc care notified home deaths

library(tidyverse)
library(readxl)
library(janitor)

d <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fnumberofdeathsincarehomesnotifiedtothecarequalitycommissionengland%2f2020/20210110officialsensitivecoviddeathnotificationv2.xlsx"

#cqc <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fnumberofdeathsincarehomesnotifiedtothecarequalitycommissionengland%2f2020/20210104officialsensitivecoviddeathnotification.xlsx"

destfile <- tempfile()

curl::curl_download(d, destfile = destfile)

cqc_data <- excel_sheets(destfile)

cqc_data %>%
  excel_sheets()

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

t4 <- t3 %>%
  bind_rows(t2) %>%
  select(-info) 

t4 %>%
  ggplot(aes(date, count, fill = cause)) +
  geom_col(position = "fill")

t4 %>%
  #dim()
  #filter(location == "England") %>%
  pivot_wider(names_from = "cause", values_from = "count", values_fn = list) %>% 
  unnest() %>%
  #slice(1:ncol(.)-263) %>%
  mutate(non_covid = `all cause` - covid) %>%
  select(-`all cause`) %>%
  mutate(rmc = zoo::rollmean(covid,  k = 7, na.pad = TRUE, align = "center"), 
         rmnc = zoo::rollmean(non_covid,  k = 7, na.pad = TRUE, align = "center")) %>%
  select(-c(3:4)) %>%
  pivot_longer(names_to = "metric", values_to = "val", cols = 3:4) %>%
  ggplot(aes(date, val, fill = metric)) +
  geom_col(position = "fill") +
  labs(y = "proportion registered deaths")
  




