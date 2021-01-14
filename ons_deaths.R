## ons deaths

library(readxl)
library(data.table)
library(tidyverse)

## ons la file of registrations and deaths
url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek53.xlsx"

destfile <- tempfile()

curl::curl_download(url, destfile)

##

excel_sheets(destfile)

registrations <- readxl::read_xlsx(destfile, 4, skip = 2) %>%
  mutate(type = "registered")
registrations

occurrence <- read_xlsx(destfile, 6, skip = 2) %>%
  mutate(type = "occurrence")

occurrence

all_deaths <- bind_rows(registrations, occurrence) %>%
  janitor::clean_names()

all_deaths <- setDT(all_deaths %>%
  pivot_wider(names_from = "type", values_from = "number_of_deaths"))

ad <- all_deaths[,  `:=` (totalr = sum(registered, na.rm = TRUE), totalo = sum(occurrence, na.rm = TRUE)),  by = .(area_name, cause_of_death, week_number) ][, `:=` (per_deaths_r = registered / totalr, per_deaths_o = occurrence / totalo)][str_detect(area_name, "^E"),]

ad %>%
  group_by(week_number) %>%
  summarise(rt = sum(totalr), 
            ro = sum(totalo)) %>%
  tail()
