### evan odell parli scripts

library(data.table)
library(readr)
library(tidyverse)
library(lubridate)
library(quanteda)
library(spacyr)

## download

destfile <- tempfile()

curl::curl_download("https://zenodo.org/record/4066772/files/hansard-speeches-v301.rds?download=1", destfile, quiet = FALSE)

speeches <- read_rds(destfile) 

speeches <- setDT(speeches)

dim(speeches)

skimr::skim(speeches)

glimpse(speeches)

recent_speeches <- speeches[year %between% c(2000, 2020), ]

head(speeches)


rs_corpus <- corpus(recent_speeches, text_field = "speech")

rs_dfm <- dfm(rs_corpus, remove = stopwords("en"))

covid <- kwic(rs_corpus, "covid*", window = 10)
