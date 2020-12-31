### NHS sitreps

library(pacman)
p_load(downloader, tidyverse, readxl, janitor)

### Download file

url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/12/UEC-Daily-SitRep-Acute-Web-File-Timeseries-3.xlsx"

temp <- tempfile()

downloader::download(url, temp)

sheets <- excel_sheets(temp) 
sheets

## ICU occupancy

range <- "A15:CY158"
header <- "A14:CY14"


test <- read_excel(temp, sheet = sheets[6], range = range)
head_test <- read_excel(temp, sheet = sheets[6], range = header)
head_test

## reshape data

df1 <- test %>%
  remove_empty() %>%
  slice(-3) %>%
  mutate_at(.vars = 4:ncol(.), as.numeric) %>%
  pivot_longer(names_to = "type", values_to = "data", cols = 4:ncol(.)) %>%
  mutate(type = str_remove(type, "\\...\\d{1,}")) %>%
  pivot_wider(names_from = "type", values_from = "data") %>%
  clean_names()
  # mutate(date = excel_numeric_to_date(as.numeric(date))) %>%
  # fill(date, .direction = "down")

## facet plot

df1 <- df1 %>%
    unnest() %>%
    mutate(date = rep(seq.Date(from = as.Date("2020-11-02"), to = as.Date("2020-12-20"), by = "day"), 141)) %>%
    clean_names() %>%
    group_by(name) %>%
    mutate(bed_occ = cc_adult_occ / cc_adult_open ,
          mean_beds = ceiling(mean(cc_adult_open))) 

df1 %>%
    ggplot(aes(date, bed_occ, colour = mean_beds, group = name)) +
    geom_line(show.legend = FALSE) +
    geom_smooth(se = FALSE, show.legend = FALSE) +
    facet_wrap(~name)
  

### download covid patients in hospital

hosp <- read_csv("https://coronavirus.data.gov.uk/api/v2/data?areaType=nhsTrust&metric=hospitalCases&metric=covidOccupiedMVBeds&format=csv")

hosp %>%
  left_join(df1, by = c("date", "areaCode" = "code")) %>%
  filter(!is.na(mean_beds)) %>%
  mutate(vent_occ = covidOccupiedMVBeds / cc_adult_occ) %>%
 # View()
  ggplot(aes(date, vent_occ )) +
  geom_line() + 
  facet_wrap(~areaName, scales = "free")
