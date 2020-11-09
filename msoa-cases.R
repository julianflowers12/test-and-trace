library(pacman)
p_load(tidyverse, data.table)
lu <- "https://coronavirus.data.gov.uk/downloads/supplements/lookup_table.csv"
msoa <- "https://coronavirus.data.gov.uk/downloads/msoa_data/MSOAs_latest.csv"
la <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
lu <- read_csv(lu)
msoa <- read_csv(msoa)
la <- read_csv(la)

summary(msoa)
head(msoa)
head(lu)

msoa <- msoa %>% 
    left_join(lu, by = c("areaCode" = "MSOA"))

glimpse(msoa)   

daily_ltla <- msoa %>%
      group_by(UTLA, UTLA_areaName, date)  %>%
      summarise(case_sum = sum(newCasesBySpecimenDateRollingSum, na.rm = TRUE)) 

daily_ltla_sheet <-  la  %>% 
    janitor::clean_names() %>% 
    mutate(date = lubridate::epiweek(specimen_date))  %>% 
    group_by(area_code, area_name, date)  %>% 
    mutate(date, case_sum_1 = sum(daily_lab_confirmed_cases, na.rm = TRUE))  %>% 
    select(area_name, area_name, specimen_date, date, case_sum_1)  %>% 
    left_join(daily_ltla, by = c("area_code" = "UTLA", "specimen_date" = "date"))  %>% 
    select(area_name, area_code, specimen_date, date, case_sum_1, case_sum)      

daily_ltla_sheet  %>% 
     filter(!is.na(case_sum))  %>% 
     mutate(prop = case_sum/case_sum_1)  %>% 
     ggplot(aes(date, fct_rev(area_name), fill = prop)) +
     geom_tile() +
     viridis::scale_fill_viridis() +
     theme(axis.text.y = element_text(size = rel(.5)))

msoa  %>% 
    filter(str_detect(areaName, "Balsham"))  %>% 
    formattable::formattable()