## etl
if(!require("myScraoers"))devtools::install_github("julianflowers/myScrapers")
library(myScrapers)
library(tidyverse)
library(downloader)
library(readODS)

url <- "https://www.gov.uk/government/collections/nhs-test-and-trace-statistics-england-weekly-reports"

## extract links

latest_data <- get_page_links(url) %>%
  .[22] %>%
  enframe() %>%
  mutate(latest = paste0("https://www.gov.uk", value), 
         links = map(latest, get_page_links)) %>%
  unnest("links") %>%
  filter(str_detect(links, "ods$"))


## downloads
filenames <- basename(latest_data$links)


for(i in 1:8){
download(latest_data$links[i], paste0("data/", filenames[i]))
}

## extract files

files <- list.files("data", pattern = "ods")

get_sheets <- map(paste0("data/",files), ~(readODS::ods_sheets(.x)))

get_sheets

ltlap2 <- read_ods(paste0("data/",files[3]), sheet = 2) %>%
  mutate_at(.vars = 7:ncol(.), as.numeric) %>%
  pivot_longer(names_to = "metric", values_to = "value", 7:ncol(.)-1) %>%
  mutate(date = lubridate::dmy(metric)) %>%
  janitor::clean_names()

ltlap2 %>%
  tail()

ltlap2 %>%
  filter(!is.na(date), utla_name != "ENGLAND") %>%
  ggplot(aes(date, value, colour = utla_name)) +
  geom_line(show.legend = FALSE)

sheets1 <- list_ods_sheets(paste0("data/",files[2]))   

sheets2 <- list_ods_sheets(paste0("data/",files[4]))   


## LA tests
la_cases <- read_ods((paste0("data/",files[2])), sheet = get_sheets[9], skip = 2)
la_tests <- read_ods((paste0("data/",files[2])), sheet = sheets1[8], skip = 2)

la_cases_l <- la_cases %>%
  mutate_at(.vars = 7:ncol(.), as.numeric) %>%
  pivot_longer(names_to = "metric", values_to = "value", 7:ncol(.))

la_tests_l <- la_tests %>%
  mutate_at(.vars = 7:ncol(.), as.numeric) %>%
  pivot_longer(names_to = "metric", values_to = "value", 7:ncol(.))

pos <- data.frame(la_cases_l, tests = la_tests_l$value) %>%
  separate(metric, c("start", "end"), sep = "-") %>%
  mutate(start = ifelse(str_detect(start, "2020"), start, paste0(str_trim(start), "20")), 
         end = ifelse(str_detect(end, "2020"), end, paste0(str_trim(end), "20")), 
         start = lubridate::dmy(start), 
         end = lubridate::dmy(end)) %>%
  filter(!is.na(start)) %>%
  group_by(Region.Name, end) %>%
  summarise(suma = sum(value, na.rm = TRUE), 
            testsa = sum(tests)) %>%
  mutate(pos = round(100 * suma / testsa, 2))
  
pos

p <- pos %>%
  ggplot(aes(end, pos, colour = Region.Name)) +
  geom_line()

p +
  labs(y = "Positivity rate", 
       title = "Weekly positivity rate by region (based on week-end dates)", 
       caption = "Source: Weekly test and trace report") +
  theme(plot.title.position = "plot", 
        panel.background = element_blank()) +
  viridis::scale_color_viridis(discrete = TRUE, option = "plasma") +
  ggsave("pos.png")
  

## tests processed

processed <- read_ods((paste0("data/",files[8])), sheet = sheets1[3])

read_ods((paste0("data/",files[4])), sheet = sheets2[8], skip = 1) %>%
  janitor::remove_empty("rows") %>%
  select(-contains("NA")) %>%
  .[2:10]



