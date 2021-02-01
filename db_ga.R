### dashboard google analytics

library(higaR)
library(patchwork)
library(changepoint)
library(ggfortify)

uses <- higaR::get_ga_list()

id <- "222273358"

stats <- get_ki_analytics(ids = id, first = "2020-04-01")

write_rds(stats, "db_ga.rds")
