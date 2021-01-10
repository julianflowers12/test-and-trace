Deaths in care homes
================
2021-01-03

### Deaths in care homes as a proportion of all COVID19-related deaths

``` r
library(pacman)
p_load(tidyverse)

care_homes <- read_csv("https://daisy.coronavirus.data.gov.uk/api/v1/data?filters=areaType=nation;areaName=England&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newWeeklyNsoCareHomeDeathsByRegDate%22:%22newWeeklyNsoCareHomeDeathsByRegDate%22,%22cumWeeklyNsoCareHomeDeathsByRegDate%22:%22cumWeeklyNsoCareHomeDeathsByRegDate%22%7D&format=csv") %>%
  mutate(pod = "care_home")
 
ons_deaths <- read_csv("https://daisy.coronavirus.data.gov.uk/api/v1/data?filters=areaType=nation;areaName=England&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newWeeklyNsoDeathsByRegDate%22:%22newWeeklyNsoDeathsByRegDate%22,%22cumWeeklyNsoDeathsByRegDate%22:%22cumWeeklyNsoDeathsByRegDate%22%7D&format=csv") %>%
  mutate(pod = "all")

deaths <- left_join(ons_deaths, care_homes, by = "date")

summary(deaths)
```

    ##   areaType.x         areaName.x         areaCode.x             date           
    ##  Length:51          Length:51          Length:51          Min.   :2020-01-03  
    ##  Class :character   Class :character   Class :character   1st Qu.:2020-03-30  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :2020-06-26  
    ##                                                           Mean   :2020-06-26  
    ##                                                           3rd Qu.:2020-09-21  
    ##                                                           Max.   :2020-12-18  
    ##  newWeeklyNsoDeathsByRegDate cumWeeklyNsoDeathsByRegDate    pod.x          
    ##  Min.   :   0                Min.   :    0               Length:51         
    ##  1st Qu.:  97                1st Qu.: 2285               Class :character  
    ##  Median : 401                Median :47111               Mode  :character  
    ##  Mean   :1379                Mean   :34533                                 
    ##  3rd Qu.:2364                3rd Qu.:49510                                 
    ##  Max.   :8335                Max.   :70320                                 
    ##   areaType.y         areaName.y         areaCode.y       
    ##  Length:51          Length:51          Length:51         
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##  newWeeklyNsoCareHomeDeathsByRegDate cumWeeklyNsoCareHomeDeathsByRegDate
    ##  Min.   :   0.0                      Min.   :    0.0                    
    ##  1st Qu.:  18.5                      1st Qu.:  115.5                    
    ##  Median :  89.0                      Median :14120.0                    
    ##  Mean   : 361.6                      Mean   : 9842.0                    
    ##  3rd Qu.: 448.5                      3rd Qu.:14796.5                    
    ##  Max.   :2669.0                      Max.   :18443.0                    
    ##     pod.y          
    ##  Length:51         
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ## 

``` r
prop_ch <- deaths %>%
  mutate(new = round(100 * newWeeklyNsoCareHomeDeathsByRegDate / newWeeklyNsoDeathsByRegDate, 2),
         cum = round(100 * cumWeeklyNsoCareHomeDeathsByRegDate / cumWeeklyNsoDeathsByRegDate, 2))

prop_ch %>%
  select(date, new, cum) %>%
  filter(date >= "2020-03-01") %>%
  ggplot(aes(date, new)) +
  geom_col() +
  geom_line(aes(date, cum)) +
  labs(title = "% total covid19 related deaths in care homes") +
  theme(plot.title.position = "plot")
```

![](care-home-deaths_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Lifetime positivity compared with weekly positivity

``` r
library(devtools)
library(lubridate)

pos1 <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=uniquePeopleTestedBySpecimenDateRollingSum&metric=uniqueCasePositivityBySpecimenDateRollingSum&format=csv")

str(data)
```

    ## function (..., list = character(), package = NULL, lib.loc = NULL, verbose = getOption("verbose"), 
    ##     envir = .GlobalEnv, overwrite = TRUE)

``` r
data1 <- read_csv("https://daisy.coronavirus.data.gov.uk/api/v1/data?filters=areaType=nation;areaName=England&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesBySpecimenDate%22:%22newCasesBySpecimenDate%22,%22cumCasesBySpecimenDate%22:%22cumCasesBySpecimenDate%22,%22newPeopleTestedBySpecimenDate%22:%22newPeopleTestedBySpecimenDate%22,%22cumPeopleTestedBySpecimenDate%22:%22cumPeopleTestedBySpecimenDate%22%7D&format=csv")

pos <- data1 %>%
  mutate(date = lubridate::ymd(date)) %>%
  arrange(date) %>%
  mutate(pos =  newCasesBySpecimenDate / newPeopleTestedBySpecimenDate, 
         cumpos = cumCasesBySpecimenDate / cumPeopleTestedBySpecimenDate, 
         sevenDayPos = zoo::rollmean(pos, k = 7, align = "center", na.pad = TRUE)) %>%
  filter(date < today() - days(5))

pos %>%
  left_join(pos1) %>%
  arrange(desc(date)) %>%
  filter(date >= "2020-09-01") %>%
  mutate(post = 100 * cumpos) %>%
  select(date, post, uniqueCasePositivityBySpecimenDateRollingSum) %>%
  
  
  ggplot(aes(date, uniqueCasePositivityBySpecimenDateRollingSum)) +
  geom_line() +
  geom_smooth() +
  geom_line(aes(date, post))
```

![](care-home-deaths_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Cases

``` r
cases <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=cumCasesByPublishDate&metric=cumCasesBySpecimenDate&metric=newCasesByPublishDate&metric=newCasesBySpecimenDate&metric=newCasesBySpecimenDateDirection&format=csv")

cases %>%
  group_by(areaName) %>%
  mutate(total = sum(newCasesBySpecimenDate, na.rm = TRUE), 
         daily_pub_change = -cumCasesByPublishDate + lag(cumCasesByPublishDate), 
         daily_diff = daily_pub_change - lag(newCasesByPublishDate)) %>%
  filter(date >= "2020-09-01") %>%
  summarise(check = max(cumCasesByPublishDate, na.rm = TRUE),
            tot = sum(newCasesByPublishDate, na.rm = TRUE), 
            total) %>%
  mutate_if(is.numeric, sum) %>%
  mutate(diff = check - tot)
```

    ## # A tibble: 500 x 5
    ## # Groups:   areaName [4]
    ##    areaName     check       tot     total     diff
    ##    <chr>        <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 England  285850375 249743500 285850375 36106875
    ##  2 England  285850375 249743500 285850375 36106875
    ##  3 England  285850375 249743500 285850375 36106875
    ##  4 England  285850375 249743500 285850375 36106875
    ##  5 England  285850375 249743500 285850375 36106875
    ##  6 England  285850375 249743500 285850375 36106875
    ##  7 England  285850375 249743500 285850375 36106875
    ##  8 England  285850375 249743500 285850375 36106875
    ##  9 England  285850375 249743500 285850375 36106875
    ## 10 England  285850375 249743500 285850375 36106875
    ## # … with 490 more rows

``` r
cases %>%
  filter(str_detect(areaCode, "^N"))
```

    ## # A tibble: 367 x 9
    ##    date       areaType areaCode areaName cumCasesByPubli… cumCasesBySpeci…
    ##    <date>     <chr>    <chr>    <chr>               <dbl>            <dbl>
    ##  1 2021-01-03 nation   N920000… Norther…            78072               NA
    ##  2 2021-01-02 nation   N920000… Norther…            76410               NA
    ##  3 2021-01-01 nation   N920000… Norther…            72834               NA
    ##  4 2020-12-31 nation   N920000… Norther…            72834               NA
    ##  5 2020-12-30 nation   N920000… Norther…            70905            72834
    ##  6 2020-12-29 nation   N920000… Norther…            68762            71255
    ##  7 2020-12-28 nation   N920000… Norther…            67196            68921
    ##  8 2020-12-27 nation   N920000… Norther…            65562            67483
    ##  9 2020-12-26 nation   N920000… Norther…            65562            66360
    ## 10 2020-12-25 nation   N920000… Norther…            64564            65614
    ## # … with 357 more rows, and 3 more variables: newCasesByPublishDate <dbl>,
    ## #   newCasesBySpecimenDate <dbl>, newCasesBySpecimenDateDirection <chr>

### Reporting cases and deaths

For both cases and deaths we present two figures - the number of each
**reported** each day, and the number of each by the date of event.

If we take England an example, PHE receives positive test results from
all the testing labs, public and private overnight each day. The number
of cases reported is the difference between the number of cases in the
database midnight on the day of report
