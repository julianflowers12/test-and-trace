library(myScrapers)
library(europepmc)

n <- 79
start <- 2020
end <- 2020
ncbi_key <- Sys.getenv("ncbi_key")

search <- "sensitivity lateral flow test covid19, systematic[sb]"

results <- pubmedAbstractR(search = search, start = start, end = end, ncbi_key = ncbi_key, n = n)

results$abstracts %>%
  reactable::reactable(filterable = TRUE, searchable = TRUE, pagination = TRUE, defaultPageSize = 25)

