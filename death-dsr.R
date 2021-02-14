## calculate dsrs for case rates

library(tidyverse)
library(data.table)
library(readxl)
library(PHEindicatormethods)
devtools::install_github("daudi/phutils")
library(phutils)


url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&metric=newDeaths28DaysByDeathDateAgeDemographics&format=csv"
## import age-specific death data

la_deaths_1 <- fread(url)


la_deaths <- jsonlite::fromJSON("https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=region&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newDeaths28DaysByDeathDateAgeDemographics%22:%22newDeaths28DaysByDeathDateAgeDemographics%22%7D&format=json", simplifyDataFrame = TRUE)

la_deaths <- la_deaths$data %>%
  unnest("newDeaths28DaysByDeathDateAgeDemographics")

la_deaths %>%
  count(areaName)

summary(la_deaths)

la_deaths %>%
  filter(areaName == "Yorkshire and The Humber") ## seems to be missing
## download data directly

la_deaths_yh <- jsonlite::fromJSON("https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=region;areaName=Yorkshire%2520and%2520The%2520Humber&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newDeaths28DaysByDeathDateAgeDemographics%22:%22newDeaths28DaysByDeathDateAgeDemographics%22%7D&format=json", simplifyDataFrame = TRUE)

la_deaths_yh <- la_deaths_yh$data %>%
  unnest("newDeaths28DaysByDeathDateAgeDemographics")

###########
# Combine #
###########
  
la_deaths_c <- la_deaths %>%
    bind_rows(la_deaths_yh) %>%
    distinct()

summary(la_deaths_c)

count(la_deaths_c, areaName, age) %>%
  pivot_wider(names_from = "age", values_from = "n")
  
## remove 0-59, 60+
la_deaths_c <- la_deaths_c %>%
  filter(age != "00_59", 
         age != "60+")

##########


## plot

la_deaths_c  %>% 
  ggplot(aes(date, areaName, fill = rollingRate)) +
  geom_tile() +
  facet_wrap(~age)


## download 2019 LA MYE populations from ONS

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

# calculate age breaks (using phutils package)
age.breaks <- seq(from = 0, to = 100, by = 5)

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
    count(ageband)


## unstack by gender and calculate populations for all persons and relabel agebands

la_pops_p_w <- la_pops_p %>%
  pivot_wider(names_from = "gender", values_from = "pop_est") %>%
  mutate(pop_p = f + m) %>%
  mutate(ageband = str_replace(ageband, "0-4", "00-04"),
         ageband = str_replace(ageband, "5-9", "05-09"), 
         ageband = str_replace(ageband, "400-044", "40-44"),
         ageband = str_replace(ageband, "500-054", "50-54"), 
         ageband = str_replace(ageband, "90-94", "90+"))

la_pops_p_w %>%
  arrange(desc(ageband))


## rejoin with deaths data

la_final <- la_deaths_c %>%
  rename(Code = areaCode) %>%
  na.omit() %>%
  select(date, areaName, ageband = age, rollingSum, areaType, Code) 

la_final %>%
  count(areaType)

la_final <- la_final %>%
  arrange(date, ageband) %>%
  mutate(ageband = str_replace(ageband, "_", "-")) %>%
  filter(date >= "2020-03-15") %>%
  distinct() %>%
  left_join(la_pops_p_w, by = c("Code", "ageband")) %>%
  filter(ageband != "00-59", 
         ageband != "60+") %>%
  distinct()



la_final[la_final$areaName == "Yorkshire and The Humber", ] %>%
  arrange(desc(date))
  
la_final %>%
  ungroup() %>%
  count(areaName) %>%
  arrange(-n)



## calculate dsrs with CIs for UTLAs 

la_dsr_utla <- la_final %>%
  filter(areaType == "region") %>%
  #filter(str_detect(areaName, "Bark"))
  #count(Geography1) %>%
  # filter(n != 19) %>%
  group_by(areaName, date) %>%
  #select(-Geography1) %>%
  #distinct() %>%
  #filter(areaName == "Barking and Dagenham", date == "2020-03-01") %>% 
  phe_dsr(rollingSum, pop_p)



 la_dsr_utla %>%
   filter(str_detect(areaName, "York"))


## plot

plot1 <- la_dsr_utla %>%
  mutate(date = lubridate::ymd(date)) %>%
  #filter(areaName == "Nottingham") %>% 
  ggplot(aes(date, value)) +
  geom_col(lwd = 0.1) +
  geom_ribbon(aes(ymin = lowercl, ymax = uppercl), fill = "grey10") +
  labs(y = "Age-adjusted rate per 100,000", 
       title = "Age-adjusted rates", 
       subtitle = "Death rates in London, South East and East of England continue to rise",
       caption = "Directly standardised to the 2003 ESR. Note DSRs aren't calculated where death counts < 10") +
  geom_hline(yintercept = 10, colour = "red") +
  geom_vline(xintercept = as.Date("2020-11-06"), lty = "dotted") + 
  #annotate("text", label = "National\nlockdown", x = as.Date("2020-09-15"), y = 300) + 
  ylim(c(0, 20)) +
  facet_wrap(~areaName) +
  theme_light()

plot1 +
  theme(panel.background = element_blank(), 
        strip.text = element_text(size = 6))


la_dsr_la <- la_final %>%
  filter(areaType == "utla") %>%
  #filter(str_detect(areaName, "Bark"))
  #count(Geography1) %>%
  # filter(n != 19) %>%
  group_by(areaName, date) %>%
  #select(-Geography1) %>%
  #distinct() %>%
  #filter(areaName == "Barking and Dagenham", date == "2020-03-01") %>% 
  phe_dsr(rollingSum, pop_p)

#######

modelled_peak <- function(df, area){
  
  require(mgcv)
  require(dplyr)
  require(ggplot2)
  require(lubridate)
  require(broom)
  
  df <- df
  area <- area
  
  df <- df %>%
    filter(date >= "2020-03-01") %>%
    mutate(date = lubridate::ymd(date), date1 = as.numeric(date)) %>%
    filter(areaName == area) 
  
  obs_max <- df %>%
    filter(value == max(value, na.rm = TRUE))
  
  ## estimating peak
  
  mod <- gam(data = df, value ~ s(date1))
  
  tidy <- summary(mod)
  
  data <- data.frame(obs_max, .fitted = mod$fitted.values)
  
  est_max <- data %>%
    filter(value == max(value))
  
  ## plot
  plot_fitted <- data %>%
    ggplot(aes(date, .fitted)) +
    geom_line() +
    geom_point(aes(date, value)) +
    geom_vline(xintercept = as.Date(est_max$date), colour = "red") +
    geom_vline(xintercept = as.Date("2020-11-06", lty = "dotted")) +
  ggtitle(paste("Estimated cases for", area))
  
  ## return
  out <- list(area = area, obs_max = obs_max, est_max = est_max, model = tidy, data = data, plot = plot_fitted)
  
}


area_list <- pull(la_dsr_utla, "areaName") %>%
  unique()

area_list


peaks <- map(area_list[9], function(x) modelled_peak(df = la_dsr_utla, x))
peaks[[1]]$plot +
  scale_x_date(breaks = "month")

