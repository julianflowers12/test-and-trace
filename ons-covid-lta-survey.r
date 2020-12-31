library(readxl)
library(tidyverse)
test <- read_excel("~/Downloads/datadownload.xlsx", skip = 2)  %>% 
    janitor::clean_names()
glimpse(test)
test  %>%
    slice(-1)  %>%
    mutate(la = str_split(local_authority_areas,  pattern  = "; " ))  %>% 
    select(-local_authority_areas)  %>% 
    unnest("la")  %>% 
    mutate(areaCode = str_replace(geography_code, "J", "E"))  %>% 
    select(-geography_code)  %>% 
    mutate(test = 100 * percentage_testing_positive)  %>% 
    ggplot(aes(reorder(la, test), test)) +
    geom_col() +
    coord_flip()

