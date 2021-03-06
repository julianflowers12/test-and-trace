## calculate dsrs for case rates

library(tidyverse)
library(data.table)
library(readxl)
library(PHEindicatormethods)
devtools::install_github("daudi/phutils")
library(phutils)

## import age-specific case data
la_cases <- fread("https://coronavirus.data.gov.uk/downloads/demographic/cases/specimenDate_ageDemographic-stacked.csv")

tail(la_cases)


la_cases  %>% 
  ggplot(aes(date, areaName, fill = newCasesBySpecimenDateRollingRate)) +
  geom_tile() +
  facet_wrap(~age)

## reformat agebands

la_cases <- la_cases %>%
  mutate(ageband = str_replace(age, "_", "-") , 
         ageband = case_when(ageband == "90+" ~ "90-94",
                             TRUE ~ ageband
         ))

la_cases %>%
  count(areaType)

## download 2019 LA populations from ONS


## unstack by gender and calculate populations for all persons

la_pops_p_w <- la_pops_p %>%
  pivot_wider(names_from = "gender", values_from = "pop_est") %>%
  mutate(pop_p = f + m)


## rejoin with cases data

la_final <- la_cases %>%
  rename(Code = areaCode) %>%
  left_join(la_pops_p_w, by = c("Code", "ageband")) %>%
  na.omit()
  
## calculate dsrs with CIs for UTLAs 

la_dsr_ltla <- la_final %>%
  filter(areaType == "region") %>%
  #filter(str_detect(areaName, "Bark"))
  #count(Geography1) %>%
  # filter(n != 19) %>%
  group_by(Code, areaName, date) %>%
  #select(-Geography1) %>%
  #distinct() %>%
  #filter(areaName == "Barking and Dagenham", date == "2020-03-01") %>% 
  phe_dsr(newCasesBySpecimenDateRollingSum, pop_p)

 la_dsr_ltla 


## plot

plot <- la_dsr_ltla %>%
  #filter(areaName %in% c("Isle of Wight", "Cambridgeshire", "Hartlepool", "Nottingham", "Liverpool", "Medway", "Havering", "Birmingham")) %>% 
  ggplot(aes(date, value)) +
  geom_col(lwd = 0.1) +
  #geom_ribbon(aes(ymin = lowercl, ymax = uppercl), fill = "goldenrod", colour = "blue") +
  labs(y = "Age-adjusted rate per 100,000", 
       title = "Age-adjusted case rates per 100,000", 
       subtitle = "Highest in East of England, London and the South East",
       caption = "Directly standardised to the 2003 ESP") +
  geom_hline(yintercept = 100, colour = "red", lty = "dotted") +
  geom_vline(xintercept = as.Date("2020-11-06"), lty = "dotted") + 
  annotate("text", label = "National\nlockdown", x = as.Date("2020-09-15"), y = 600) + 
  ylim(c(0, 400)) +
  facet_wrap(~areaName) +
  theme_minimal() +
  theme(panel.background = element_blank(), 
        strip.text = element_text(size = 6))

plot +
  ggsave("dsr.png")
  

### heatmap ordered by mean value since 1st September

las_matrix <- la_dsr_ltla %>%
  select(-c(total_count, total_pop, lowercl, uppercl, confidence, statistic, method)) 
  
las_matrix %>%
  group_by(areaName) %>%
  filter(date >= "2020-09-01") %>%
  mutate(median = mean(value, na.rm = TRUE)) %>%
  ggplot(aes(date, fct_reorder(areaName, median), fill = value)) +
  geom_tile() +
  viridis::scale_fill_viridis(direction = -1, option = "viridis") +
  theme(axis.text.y = element_text(size = 7)) +
  scale_y_discrete(position = "right") +
  labs(y = "") +
  coord_equal() +
  geom_vline(xintercept = as.Date("2020-11-06"))

###############
# correlation #
###############

library(tidygraph)
library(ggraph)


la_cases[between(date, "2020-12-01", "2020-12-29"), ][areaName == "England" & !age %in% c("unassigned", "0_59", "60+"), .(age, date, newCasesBySpecimenDateRollingRate)] %>%
  pivot_wider(names_from = "date", values_from = "newCasesBySpecimenDateRollingRate") %>%
  slice(-20) %>%
  select(-1) %>%
  cor() %>%
  pairs()
  as_tbl_graph() %>%
  ggraph(., layout = "fr", weights = weight) +
     geom_edge_link() +
     geom_node_point()
  










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


peaks <- map(area_list[61], function(x) modelled_peak(df = la_dsr_utla, x))
peaks[[1]]$plot +
  scale_x_date(breaks = "month")


