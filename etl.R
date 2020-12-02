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

