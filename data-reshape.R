## reshape data

source("etl.R")
library(tidyverse)

## read ods files

ods <- list.files("data", pattern = ".ods")

## check sheets


sheets <- map(paste0("data/", ods), readODS::list_ods_sheets)

## read in sheets

map(paste0("data/", ods)[1], function(x) read_ods(x, sheet = sheets[[1]]))

uk <- readODS::read_ods("data/UK_testing_w21.ods")
demog <- read_ods("data/Demographic_LA_tables_w21.ods", sheet = "Table_2", skip = 1)
ltla <- read_ods("data/P2_tests_processed_LTLA_w21.ods", sheet = 2)
t_t_tables <- read_ods("data/NHS_T_T_data_tables_w21.ods")


demog <- list_ods_sheets("data/P2_tests_processed_LTLA_w21.ods")
