## calculate dsrs for case rates

library(tidyverse)
library(data.table)
library(readxl)
library(PHEindicatormethods)
devtools::install_github("daudi/phutils")
library(phutils)

## import age-specific case data
la_cases <- fread("https://coronavirus.data.gov.uk/downloads/demographic/cases/specimenDate_ageDemographic-stacked.csv")

glimpse(la_cases)


la_cases  %>% 
  ggplot(aes(date, areaName, fill = newCasesBySpecimenDateRollingRate)) +
  geom_tile() +
  facet_wrap(~age)

## reformat agebands

la_cases <- la_cases %>%
  mutate(ageband = str_replace(age, "_", "-") , 
         ageband = case_when(ageband == "90+" ~ "90-94",
                             TRUE ~ ageband
         ))

la_cases %>%
  count(areaType)

## download 2019 LA populations from ONS

curl::curl_download("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2019localauthoritydistrictcodes/ukmidyearestimates20192019ladcodes.xls", destfile = "lapops.xls")
la_pops_m <- read_excel("~/New-VSCode-projects/test-and-trace/lapops.xls", sheet = 6, skip = 4)
la_pops_f <- read_excel("~/New-VSCode-projects/test-and-trace/lapops.xls", sheet = 7, skip = 4)

head(la_pops_f, 20)

## convert to stacked format (long)

la_pops_m_l <- la_pops_m  %>% 
  pivot_longer(names_to = "age", values_to = "pop", 4:ncol(.))  %>% 
  mutate(gender = "m")

la_pops_f_l <- la_pops_m  %>% 
  pivot_longer(names_to = "age", values_to = "pop", 4:ncol(.))  %>% 
  mutate(gender = "f")   

## stack male and female tables

la_pops_p <- bind_rows(la_pops_m_l, la_pops_f_l)

# x <- 0:89
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

la_pops_p  %>% 
    ungroup()  %>% 
    count(Geography1)


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

la_dsr_utla <- la_final %>%
  filter(areaType == "utla") %>%
  #filter(str_detect(areaName, "Bark"))
  #count(Geography1) %>%
  # filter(n != 19) %>%
  group_by(areaName, date) %>%
  #select(-Geography1) %>%
  #distinct() %>%
  #filter(areaName == "Barking and Dagenham", date == "2020-03-01") %>% 
  phe_dsr(newCasesBySpecimenDateRollingSum, pop_p)

 la_dsr_utla 


## plot

plot <- la_dsr_utla %>%
  #filter(areaName == "Nottingham") %>% 
  ggplot(aes(date, value)) +
  geom_line(lwd = 0.1) +
  geom_ribbon(aes(ymin = lowercl, ymax = uppercl), fill = "grey60") +
  labs(y = "Age-adjusted rate per 100,000", 
       title = "Age-adjusted rates", 
       subtitle = "NW LAs and University cities are mostly beyond the peak; Yorkshire LAs are still on the rise",
       caption = "Directly standardised to the 2003 ESR") +
  geom_hline(yintercept = 100, colour = "red") +
  geom_vline(xintercept = as.Date("2020-11-06"), lty = "dotted") + 
  #annotate("text", label = "National\nlockdown", x = as.Date("2020-09-15"), y = 300) + 
  ylim(c(0, 400)) +
  facet_wrap(~areaName) 

plot  










