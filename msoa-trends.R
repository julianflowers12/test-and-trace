library(tmap)
library(ggmap)
library(geojsonio)


shp <- "https://opendata.arcgis.com/datasets/826dc85fb600440889480f4d9dbb1a24_0.geojson"


msoa <- fread("https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&metric=newCasesBySpecimenDateRollingRate&format=csv")
msoa1 <- msoa[date == max(date), ]

dim(msoa1)

palette <- rev(viridis::plasma(15))
credits <- "Contains ordnance survey data (c) \nCrown copyright and database right 2020"


## Download the data and convert to spatial polygon data frame
geodata <- geojson_read(shp, what = "sp")

glimpse(geodata)
## Select England wards
seng <- subset(geodata, substr(msoa11cd, 1, 1) == "E")

## Join data to plot
seng@data <- left_join(seng@data, msoa1, by=c("msoa11cd" = "areaCode"))

tm_shape(seng) +
  tm_fill("newCasesBySpecimenDateRollingRate", style = "kmeans" , n = 10,
          palette = palette, title = "Self harm rate") +
  tm_credits( credits, size = 0.5, align = "right") +
  tm_layout(legend.outside = TRUE, 
            frame = FALSE) +
  tm_compass(position = c("left", "center")) + 
  tm_scale_bar(position = c("left", "center"))

## Map

tm_shape(seng) +
  tm_fill("value", style = "kmeans" , n = 10,
          palette = palette, title = "Self harm rate") +
  tm_credits( credits, size = 0.5, align = "right") +
  tm_layout(legend.outside = TRUE, 
            frame = FALSE) +
  tm_compass(position = c("left", "center")) + 
  tm_scale_bar(position = c("left", "center")) 
