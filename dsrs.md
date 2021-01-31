DSRs
================
Julian Flowers
2021-01-27

## Calculate case rate dsrs for local authorities

``` r
URL <- "https://github.com/julianflowers12/test-and-trace/blob/master/create_case_dsr.R?raw=TRUE"

source_url(URL)

dsrs <- case_dsrs_las(area = "utla")
```

## Comparisons

``` r
dsrs$dsr %>%
  filter(str_detect(areaName, "Camb")) %>%
  ggplot(aes(date, value)) +
  geom_line()
```

![](dsrs_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Get tiers

``` r
## get tier information

tiers <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=alertLevel&format=csv")

tiers <- tiers %>%
  filter(date == max(date))
```

## DSRs vs tiers

``` r
dsr_tiers <- dsrs$dsr %>%
  left_join(tiers, by = "areaName")

dsr_1 <- dsr_tiers %>%
  filter(date.x == max(date.x)) %>%
  ggplot(aes(reorder(areaName, value), value, fill = alertLevelName)) +
  geom_col() +
  labs(y = "Standardised case rate", 
       x = "Date", 
       title = "Age standardised 7-day rolling case rates per 100,000") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 8))

dsr_1# library(plotly)
```

![](dsrs_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# ggplotly(dsr_1)
```

``` r
dsr_tiers %>%
    filter(date.x == max(date.x), !is.na(alertLevel)) %>%
    ggplot() +
    geom_density(aes(value, fill = alertLevelName), alpha = .4)
```

![](dsrs_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
dsr_tiers %>%
  group_by(areaName) %>%
  filter(date.x == max(date.x), !is.na(alertLevel)) %>%
  ggplot(aes(reorder(areaName, -value), value)) +
  geom_point(aes(colour = alertLevelName), size = 1) +
  geom_linerange(aes(ymax = uppercl, ymin = lowercl, colour = alertLevelName)) +
  coord_flip() +
  theme(axis.text = element_text(size = 6), 
        panel.background = element_blank(), 
        legend.position = "") +
  labs(x = "", y = "Rate") +
  viridis::scale_colour_viridis(discrete = TRUE, option = "D", direction = -1, name = "") +
  facet_wrap(~alertLevel, nrow = 1)
```

![](dsrs_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Cluster analysis of ltla covid data

``` r
library(Rtsne)
library(dbscan)
set.seed(123)
cl_data <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDateChangePercentage&metric=newCasesBySpecimenDateRollingRate&metric=uniqueCasePositivityBySpecimenDateRollingSum&metric=newDeaths60DaysByDeathDateRollingRate&format=csv")


cl_data_1 <- cl_data %>%
  filter(date == max(date), 
         str_detect(areaCode, "^E")) 

cl_data_2 <- cl_data_1 %>%
  select(5:8) %>%
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)) %>%
  mutate_if(is.numeric, scale) 

tsne <- cl_data_2 %>%
  as.matrix() %>%
  Rtsne::Rtsne() 

clust <- dbscan::hdbscan(tsne$Y, minPts = 9, gen_hdbscan_tree = TRUE)

data.frame(tsne$Y, clust$cluster) %>%
  ggplot(aes(X1, X2, colour = factor(clust.cluster))) +
  geom_point() +
  viridis::scale_color_viridis(discrete = TRUE)
```

![](dsrs_files/figure-gfm/clustering-1.png)<!-- -->

``` r
df <- data.frame(cl_data_1, cluster = clust$cluster)

df %>%
  select(areaName, cluster) %>%
  arrange(cluster) %>%
  reactable::reactable(filterable = TRUE, searchable = TRUE)
```

<!--html_preserve-->

<div id="htmlwidget-7dfe9b1cc3a8846f5efa" class="reactable html-widget"
style="width:auto;height:auto;">

</div>

<script type="application/json" data-for="htmlwidget-7dfe9b1cc3a8846f5efa">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"areaName":["Warwick","Peterborough","Manchester","Staffordshire Moorlands","Bolsover","Bassetlaw","Craven","Charnwood","Bath and North East Somerset","Selby","Kettering","Trafford","North East Lincolnshire","Vale of White Horse","North Lincolnshire","South Derbyshire","Stafford","Brentwood","Lichfield","Blaby","Tendring","Wyre","Knowsley","Melton","Torridge","Stoke-on-Trent","Southampton","Blackpool","South Staffordshire","Northumberland","Southend-on-Sea","Tameside","St Albans","Richmondshire","West Lancashire","Coventry","East Suffolk","South Somerset","Castle Point","South Gloucestershire","Wellingborough","Tewkesbury","Kingston upon Thames","Wigan","Folkestone and Hythe","Cheshire East","Fenland","East Lindsey","South Holland","Tonbridge and Malling","Adur","Richmond upon Thames","New Forest","Broxtowe","Leicester","Rutland","West Devon","Mid Devon","Havering","East Riding of Yorkshire","Guildford","Stockton-on-Tees","Mole Valley","South Ribble","King's Lynn and West Norfolk","South Lakeland","South Kesteven","Mendip","Colchester","Kingston upon Hull, City of","Harborough","Maldon","Camden","Daventry","Basildon","Boston","Sevenoaks","Cotswold","South Norfolk","Derbyshire Dales","Sunderland","Salford","Rugby","Barrow-in-Furness","Central Bedfordshire","Waverley","Chesterfield","Amber Valley","Gosport","Telford and Wrekin","Forest of Dean","Huntingdonshire","Copeland","Horsham","Eastbourne","Redcar and Cleveland","Sefton","Milton Keynes","Somerset West and Taunton","North Warwickshire","Wiltshire","Bury","Nottingham","North Tyneside","Middlesbrough","Nuneaton and Bedworth","Preston","Thurrock","Kensington and Chelsea","Corby","York","Babergh","Eastleigh","East Hampshire","Dover","Brighton and Hove","North Kesteven","Uttlesford","West Lindsey","Tunbridge Wells","Mid Sussex","Runnymede","Oadby and Wigston","Scarborough","Mid Suffolk","Teignbridge","Exeter","Torbay","Swindon","North Somerset","Gateshead","Malvern Hills","Wokingham","Gedling","Cornwall and Isles of Scilly","Ryedale","Bristol, City of","West Oxfordshire","Cheltenham","Shropshire","North Devon","South Northamptonshire","North Norfolk","South Hams","Stroud","Winchester","Wyre Forest","Herefordshire, County of","Sedgemoor","Hambleton","Harrogate","Plymouth","Stratford-on-Avon","East Devon","East Cambridgeshire","Stockport","Wakefield","Sheffield","North East Derbyshire","Bradford","East Northamptonshire","Calderdale","County Durham","Rushcliffe","Chorley","Rochdale","Rotherham","Barnsley","Darlington","Fylde","Kirklees","Lincoln","Bolton","Leeds","North West Leicestershire","High Peak","Newark and Sherwood","South Tyneside","Newcastle-under-Lyme","Dorset","South Cambridgeshire","Doncaster","West Berkshire","South Oxfordshire","Oldham","Hinckley and Bosworth","Test Valley","Newcastle upon Tyne","Gloucester","West Suffolk","Cambridge","Fareham","Ipswich","Canterbury","Hart","Isle of Wight","Ashford","Worthing","Eden","Tandridge","Dacorum","Broadland","Wychavon","Lewes","Arun","Havant","Rother","Allerdale","Hastings","Chiltern","Wealden","Braintree","Basingstoke and Deane","Surrey Heath","Bromsgrove","Maidstone","Chelmsford","Breckland","Cherwell","Hartlepool","Windsor and Maidenhead","Chichester","Three Rivers","Ribble Valley","Elmbridge","Thanet","Swale","Rochford","Islington","Lambeth","Redditch","Portsmouth","Hyndburn","South Bucks","Northampton","North Hertfordshire","Crawley","Wycombe","Bexley","Mansfield","Luton","Hounslow","Halton","Spelthorne","East Hertfordshire","Redbridge","Enfield","Sandwell","Sutton","Hammersmith and Fulham","Croydon","Tamworth","Dudley","Medway","Epsom and Ewell","Hackney and City of London","Welwyn Hatfield","Wolverhampton","Watford","Broxbourne","Woking","Norwich","Carlisle","Derby","Wandsworth","Lancaster","Birmingham","Burnley","Westminster","Blackburn with Darwen","Southwark","Rushmoor","Pendle","Haringey","Slough","Bedford","Hertsmere","Tower Hamlets","Dartford","Greenwich","Bracknell Forest","Worcester","Bournemouth, Christchurch and Poole","Gravesham","Lewisham","Reigate and Banstead","Cheshire West and Chester","Rossendale","Ealing","Erewash","Harrow","Warrington","Barking and Dagenham","Wirral","Liverpool","Epping Forest","East Staffordshire","Stevenage","Reading","Hillingdon","Aylesbury Vale","Brent","Harlow","Bromley","Barnet","St. Helens","Waltham Forest","Walsall","Cannock Chase","Oxford","Merton","Ashfield","Solihull","Newham","Great Yarmouth"],"cluster":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6]},"columns":[{"accessor":"areaName","name":"areaName","type":"character"},{"accessor":"cluster","name":"cluster","type":"numeric"}],"filterable":true,"searchable":true,"defaultPageSize":10,"paginationType":"numbers","showPageInfo":true,"minRows":1,"dataKey":"30120204f5f75a6dd184466a0ea747b5","key":"30120204f5f75a6dd184466a0ea747b5"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->

``` r
df %>%
  mutate_at(.vars = 5:8, scale) %>%
  group_by(cluster) %>%
  summarise(across(5:8, mean)) %>% 
  rename(change = newCasesBySpecimenDateChangePercentage, case_rate = newCasesBySpecimenDateRollingRate, pos_rate = uniqueCasePositivityBySpecimenDateRollingSum, deaths = newDeaths60DaysByDeathDateRollingRate) %>%
  pivot_longer(names_to = "metric", values_to = "values", cols = 2:5) %>%
  ggplot(aes(metric, values)) +
  geom_point() +
  geom_line(aes(group = cluster)) +
  geom_hline(yintercept = 0, colour = "red") +
  coord_polar() +
  facet_wrap(~cluster)
```

![](dsrs_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
