MSOA case maps
================
2021-01-04

## 

``` r
library(data.table)
library(tmap)
library(ggmap)
library(geojsonio)
library(tidyverse)
```

``` r
msoa <- fread("https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&metric=newCasesBySpecimenDateRollingRate&metric=newCasesBySpecimenDateChangePercentage&metric=newCasesBySpecimenDateDirection&format=csv")
msoa1 <- msoa[date == max(date), ]
```

``` r
skimr::skim(msoa1)
```

|                                                  |       |
|:-------------------------------------------------|:------|
| Name                                             | msoa1 |
| Number of rows                                   | 6791  |
| Number of columns                                | 13    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |       |
| Column type frequency:                           |       |
| character                                        | 10    |
| Date                                             | 1     |
| numeric                                          | 2     |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |       |
| Group variables                                  | None  |

Data summary

**Variable type: character**

| skim\_variable                  | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:--------------------------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| regionCode                      |          0 |              1 |   9 |   9 |     0 |         9 |          0 |
| regionName                      |          0 |              1 |   6 |  24 |     0 |         9 |          0 |
| UtlaCode                        |          0 |              1 |   9 |   9 |     0 |       149 |          0 |
| UtlaName                        |          0 |              1 |   4 |  35 |     0 |       149 |          0 |
| LtlaCode                        |          0 |              1 |   9 |   9 |     0 |       315 |          0 |
| LtlaName                        |          0 |              1 |   4 |  35 |     0 |       315 |          0 |
| areaType                        |          0 |              1 |   4 |   4 |     0 |         1 |          0 |
| areaCode                        |          0 |              1 |   9 |   9 |     0 |      6791 |          0 |
| areaName                        |          0 |              1 |   3 |  48 |     0 |      6728 |          0 |
| newCasesBySpecimenDateDirection |          0 |              1 |   0 |   4 |    26 |         4 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-12-29 | 2020-12-29 | 2020-12-29 |         1 |

**Variable type: numeric**

| skim\_variable                         | n\_missing | complete\_rate |   mean |     sd |    p0 |   p25 |   p50 |   p75 |   p100 | hist  |
|:---------------------------------------|-----------:|---------------:|-------:|-------:|------:|------:|------:|------:|-------:|:------|
| newCasesBySpecimenDateRollingRate      |         26 |              1 | 465.88 | 315.42 |  28.7 | 231.4 | 363.5 | 631.9 | 1893.2 | ▇▅▂▁▁ |
| newCasesBySpecimenDateChangePercentage |         26 |              1 |  45.54 |  86.58 | -84.4 |  -5.6 |  25.0 |  70.6 | 1400.0 | ▇▁▁▁▁ |

``` r
shp <- "https://opendata.arcgis.com/datasets/826dc85fb600440889480f4d9dbb1a24_0.geojson"

palette <- rev(viridis::viridis(15))
palette1 <- viridis::viridis(4, direction = -1)
credits <- "Contains ordnance survey data © \nCrown copyright and database right 2020"


## Download the data and convert to spatial polygon data frame
geodata <- geojson_read(shp, what = "sp")

# glimpse(geodata)
## Select England wards
seng <- subset(geodata, substr(msoa11cd, 1, 1) == "E")

## Join data to plot
seng@data <- left_join(seng@data, msoa1, by=c("msoa11cd" = "areaCode"))
```

``` r
u <- tm_shape(seng) +
  tm_fill("newCasesBySpecimenDateDirection", style = "kmeans", palette = palette1 , n = 10,
          title = "Direction") +
  tm_credits( credits, size = 0.5, align = "right") +
  tm_layout(legend.outside = TRUE, 
            frame = FALSE) +
  tm_compass(position = c("left", "center")) + 
  tm_scale_bar(position = c("left", "center"))

u
```

    ## Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0

![](msoa-map_files/figure-gfm/plot-change-1.png)<!-- -->

``` r
v <- tm_shape(seng) +
  tm_fill("newCasesBySpecimenDateChangePercentage", style = "kmeans", palette = palette , n = 10,
          title = "% change") +
  tm_credits( credits, size = 0.5, align = "right") +
  tm_layout(legend.outside = TRUE, 
            frame = FALSE) +
  tm_compass(position = c("left", "center")) + 
  tm_scale_bar(position = c("left", "center"))

v
```

![](msoa-map_files/figure-gfm/plot-percentage-change-1.png)<!-- -->

``` r
w <- tm_shape(seng) +
  tm_fill("newCasesBySpecimenDateRollingRate", style = "fisher", palette = palette , n = 10,
          title = "Rate") +
  tm_credits( credits, size = 0.5, align = "right") +
  tm_layout(legend.outside = TRUE, 
            frame = FALSE) +
  tm_compass(position = c("left", "center")) + 
  tm_scale_bar(position = c("left", "center"))

w
```

![](msoa-map_files/figure-gfm/plot-rate-1.png)<!-- -->
