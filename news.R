library(pacman)

p_load(tidyverse, newsanchor)

s <- newsanchor::terms_sources
t <- newsanchor::terms_category

t

results <- newsanchor::get_everything(sources = c(s[12,]), t[4,]), "covid|coronavirus")  
results <- results$results_df  %>% 
    select(published_at, content, title, description, url) %>% as_tibble()

install.packages("bigrquery")
install.packages("odbc")
install.packages("DBI")
install.packages("dbplyr")

library(pacman)
p_load(tidyverse, bigrquery, odbc, DBI)

library(DBI)
library(bigrquery)

con <- dbConnect(
  bigrquery::bigquery(),
  project = "publicdata",
  dataset = "samples",
  billing = billing
  )