
library(spacyr)

library(tidyverse)

spacy_initialize()


extract_locations <- function(x){

  require(spacyr)
  require(dplyr)
  anno <- spacy_extract_entity(x)
  geo <- anno %>%
    filter(str_detect(ent_type, "GPE"))
  geo <- select(geo, doc_id, text)
}


extract_org <- function(x){

  require(spacyr)
  require(dplyr)
  anno <- spacy_extract_entity(x)
  org <- anno %>%
    filter(str_detect(ent_type, "ORG"))
  org <- select(org, doc_id, text)
}

extract_date <- function(x){

  require(spacyr)
  require(dplyr)
  anno <- spacy_extract_entity(x)
  date <- anno %>%
    filter(str_detect(ent_type, "DAT"))
  date <- select(date, doc_id, text)
}


extract_cardinal <- function(x){

  require(spacyr)
  require(dplyr)
  anno <- spacy_extract_entity(x)
  cardinal <- anno %>%
    filter(str_detect(ent_type, "CARD"))
  numbers <- select(cardinal, doc_id, text)
}

extract_geo <- function(x){

  require(spacyr)
  require(dplyr)
  anno <- spacy_extract_entity(x)
  locations <- anno %>%
    filter(str_detect(ent_type, "LOC"))
  locs <- select(locations, doc_id, text)
}

extract_norp <- function(x){

  require(spacyr)
  require(dplyr)
  anno <- spacy_extract_entity(x)
  norp <- anno %>%
    filter(str_detect(ent_type, "NORP"))
  norp <- select(norp, doc_id, text)
}

extract_percent <- function(x){

  require(spacyr)
  require(dplyr)
  anno <- spacy_extract_entity(x)
  percent <- anno %>%
    filter(str_detect(ent_type, "PERC")) %>%
    select(doc_id, text)

}

extract_np <- function(x){

  require(spacyr)
  require(dplyr)
  np <- spacy_extract_nounphrases(x)
  np <- select(np, doc_id, text)

}

