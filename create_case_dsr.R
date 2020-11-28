## calculate dsrs for case rates

case_dsrs_las <- function(area = "utla"){

require(tidyverse)
require(data.table)
require(readxl)
require(PHEindicatormethods)
if(!require(phutils)) devtools::install_github("daudi/phutils")
library(phutils)

## import age-specific case data
la_cases <- fread("https://coronavirus.data.gov.uk/downloads/demographic/cases/specimenDate_ageDemographic-stacked.csv")

## reformat agebands

la_cases <- la_cases %>%
  mutate(ageband = str_replace(age, "_", "-") , 
         ageband = case_when(ageband == "90+" ~ "90-94",
                             TRUE ~ ageband
         ))

## download 2019 LA populations from ONS
destfile <- tempfile()
curl::curl_download("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2019localauthoritydistrictcodes/ukmidyearestimates20192019ladcodes.xls", destfile = destfile)

sheets <- excel_sheets(destfile)
la_pops_m <- read_excel(destfile, sheet = 7, skip = 4)
la_pops_f <- read_excel(destfile, sheet = 8, skip = 4)

## convert to stacked format (long)

la_pops_m_l <- la_pops_m  %>% 
  pivot_longer(names_to = "age", values_to = "pop", 4:ncol(.))  %>% 
  mutate(gender = "m")

la_pops_f_l <- la_pops_f  %>% 
  pivot_longer(names_to = "age", values_to = "pop", 4:ncol(.))  %>% 
  mutate(gender = "f")   

## stack male and female tables

la_pops_p <- bind_rows(la_pops_m_l, la_pops_f_l)

# set agebands
age.breaks <- seq(from = 0, to = 100, by = 5)
# phutils::age.groups(x, age.breaks, final.open = TRUE,ordered_result = TRUE )

## calculate populations by ageband and LA

la_pops_p <- la_pops_p  %>% 
  filter(age != "All ages", 
         !is.na(Name))  %>% 
  mutate(age = ifelse(age == "90+", "90", age),
          age = as.numeric(age),
         ageband = phutils::age.groups(age, age.breaks)) %>%
  group_by(Code, Name, gender, ageband,  Geography1) %>%
  summarise(pop_est = sum(pop))

## unstack by gender and calculate populations for all persons

la_pops_p_w <- la_pops_p %>%
  pivot_wider(names_from = "gender", values_from = "pop_est") %>%
  mutate(pop_p = f + m)

## rejoin with cases data

la_final <- la_cases %>%
  rename(Code = areaCode) %>%
  left_join(la_pops_p_w, by = c("Code", "ageband")) %>%
  na.omit()
  
## calculate dsrs with CIs for UTLAs 

la_dsr_la <- la_final %>%
  filter(areaType == "utla") %>%
  #filter(str_detect(areaName, "Bark"))
  #count(Geography1) %>%
  # filter(n != 19) %>%
  group_by(areaName, date) %>%
  #select(-Geography1) %>%
  #distinct() %>%
  #filter(areaName == "Barking and Dagenham", date == "2020-03-01") %>% 
  phe_dsr(newCasesBySpecimenDateRollingSum, pop_p)

 output <- list(data = la_final, la_cases = la_cases, dsr = la_dsr_la, la_pops = la_pops_p_w)

}


