#install.packages("umap")
library(umap)
library(tidyverse)
library(fingertipsR)
library(dbscan)
library(Rtsne)

data <- fingertips_data(ProfileID = 19, AreaTypeID = 102)

data <- data  %>% 
    select(IndicatorID, IndicatorName, AreaName, Value, Age, Sex, TimeperiodSortable)  %>% 
    mutate(index = paste(IndicatorName, Age, Sex))  %>% 
    group_by(IndicatorName, Age, Sex)  %>% 
    filter(TimeperiodSortable == max(TimeperiodSortable)) 

data_wide <- data  %>% 
    ungroup()  %>% 
    select(AreaName, index, Value)  %>% 
    pivot_wider(names_from = "index", values_from = "Value")

data_wide_1 <- data_wide  %>% 
    mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)) %>%
    mutate_if(is.numeric, scale)

data_wide_1 <- select_if(data_wide_1, function(x) mean(is.na(x)) < 1)

which(is.na(data_wide_1))  

## umap
umap <- umap(data_wide_1[, -1]) 

umap$layout  %>% pairs()

cluster <- umap$layout  %>% hdbscan(., minPts = 3)

data_wide_2 <- data.frame(data_wide_1, cluster = cluster$cluster)

glimpse(data_wide_2)

data_wide_2 %>% select(AreaName, cluster)  %>% arrange(cluster)

## rtsne

rtsne <- Rtsne::Rtsne(data_wide_1[, -1])

rtsne$Y %>% pairs()

clust_tsne <- rtsne$Y  %>% hdbscan(., minPts = 3)

data_wide_2 <- data.frame(data_wide_2, tsne = clust_tsne$cluster)

glimpse(data_wide_2)

data_wide_2 %>% select(AreaName, cluster, tsne)  %>% arrange(cluster)  %>% 
     ggplot(aes(AreaName, cluster)) +
     geom_point() +
     facet_grid(~tsne)

str(umap)

umap$knn$index