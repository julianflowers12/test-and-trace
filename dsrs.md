DSRs
================
Julian Flowers
2021-01-03

## Calculate case rate dsrs for local authorities

``` r
URL <- "https://github.com/julianflowers12/test-and-trace/blob/master/create_case_dsr.R?raw=TRUE"

source_url(URL)

test <- case_dsrs_las(area = "ltla")

tail(test$dsr)
```

    ## # A tibble: 6 x 10
    ## # Groups:   areaName, date [6]
    ##   areaName date       total_count total_pop value lowercl uppercl confidence
    ##   <chr>    <date>           <int>     <dbl> <dbl>   <dbl>   <dbl> <chr>     
    ## 1 York     2020-12-24         509    210618  247.    226.    270. 95%       
    ## 2 York     2020-12-25         499    210618  243.    222.    266. 95%       
    ## 3 York     2020-12-26         562    210618  275.    252.    299. 95%       
    ## 4 York     2020-12-27         627    210618  308.    284.    334. 95%       
    ## 5 York     2020-12-28         679    210618  335.    310.    361. 95%       
    ## 6 York     2020-12-29         802    210618  393.    365.    421. 95%       
    ## # … with 2 more variables: statistic <chr>, method <chr>

### Get tiers

``` r
## get tier information

tiers <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=alertLevel&format=csv")

tiers <- tiers %>%
  filter(date == max(date))
```

## DSRs vs tiers

``` r
dsr_tiers <- test$dsr %>%
  left_join(tiers, by = "areaName")

dsr_1 <- dsr_tiers %>%
  filter(date.x == max(date.x)) %>%
  ggplot(aes(reorder(areaName, value), value, fill = alertLevelName)) +
  geom_col() +
  labs(y = "Standardised case rate", 
       x = "Date", 
       title = "Age standardised 7-day rolling case rates per 100,000") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 4))

dsr_1
```

![](dsrs_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
# library(plotly)
# ggplotly(dsr_1)
```

``` r
dsr_tiers %>%
    filter(date.x == max(date.x), !is.na(alertLevel)) %>%
    ggplot() +
    geom_density(aes(value, fill = alertLevelName), alpha = .4)
```

![](dsrs_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

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

![](dsrs_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### Cluster analysis of ltla covid data

``` r
library(Rtsne)
library(dbscan)
set.seed(123)
cl_data <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDateChangePercentage&metric=newCasesBySpecimenDateRollingRate&metric=uniqueCasePositivityBySpecimenDateRollingSum&metric=newDeaths60DaysByDeathDateRollingRate&format=csv")


cl_data_1 <- cl_data %>%
  filter(date == max(date), 
         str_detect(areaCode, "^E")) %>%
  left_join(tiers, by = "areaName")

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
  geom_point()
```

![](dsrs_files/figure-gfm/clustering-1.png)<!-- -->

``` r
df <- data.frame(cl_data_1, cluster = clust$cluster)
df %>%
  select(areaName, cluster, alertLevelName) %>%
  arrange(cluster) %>%
  knitr::kable()
```

| areaName                            | cluster | alertLevelName |
|:------------------------------------|--------:|:---------------|
| Sandwell                            |       0 | Stay at Home   |
| North Hertfordshire                 |       0 | Stay at Home   |
| North East Derbyshire               |       0 | Stay at Home   |
| East Suffolk                        |       0 | Stay at Home   |
| Wakefield                           |       0 | Very High      |
| Gosport                             |       0 | Stay at Home   |
| East Staffordshire                  |       0 | Stay at Home   |
| County Durham                       |       0 | Stay at Home   |
| South Cambridgeshire                |       0 | Stay at Home   |
| Dudley                              |       0 | Stay at Home   |
| South Staffordshire                 |       0 | Stay at Home   |
| Havant                              |       0 | Stay at Home   |
| Mansfield                           |       0 | Stay at Home   |
| Babergh                             |       0 | Stay at Home   |
| North Warwickshire                  |       0 | Stay at Home   |
| Redbridge                           |       0 | Stay at Home   |
| Norwich                             |       0 | Stay at Home   |
| Tendring                            |       0 | Stay at Home   |
| Wyre                                |       0 | Stay at Home   |
| Selby                               |       0 | Very High      |
| Bury                                |       0 | Stay at Home   |
| Rother                              |       0 | Stay at Home   |
| Walsall                             |       0 | Stay at Home   |
| South Tyneside                      |       0 | Stay at Home   |
| Tameside                            |       0 | Stay at Home   |
| Peterborough                        |       0 | Stay at Home   |
| Staffordshire Moorlands             |       0 | Stay at Home   |
| Folkestone and Hythe                |       0 | Stay at Home   |
| Stoke-on-Trent                      |       0 | Stay at Home   |
| Ribble Valley                       |       0 | Stay at Home   |
| Spelthorne                          |       0 | Stay at Home   |
| Eastbourne                          |       0 | Stay at Home   |
| Coventry                            |       0 | Stay at Home   |
| Medway                              |       0 | Stay at Home   |
| Newcastle upon Tyne                 |       0 | Stay at Home   |
| Canterbury                          |       0 | Stay at Home   |
| Hyndburn                            |       0 | Stay at Home   |
| Enfield                             |       0 | Stay at Home   |
| Oadby and Wigston                   |       0 | Stay at Home   |
| Southampton                         |       0 | Stay at Home   |
| Luton                               |       0 | Stay at Home   |
| Stafford                            |       0 | Stay at Home   |
| North Somerset                      |       0 | Very High      |
| Ashford                             |       0 | Stay at Home   |
| Malvern Hills                       |       0 | Very High      |
| Crawley                             |       0 | Stay at Home   |
| Eden                                |       0 | Stay at Home   |
| Derby                               |       0 | Stay at Home   |
| West Berkshire                      |       0 | Stay at Home   |
| Dorset                              |       0 | Very High      |
| Wychavon                            |       0 | Very High      |
| Burnley                             |       0 | Stay at Home   |
| Torridge                            |       0 | Very High      |
| Southend-on-Sea                     |       0 | Stay at Home   |
| North East Lincolnshire             |       0 | Very High      |
| Doncaster                           |       0 | Very High      |
| Lewes                               |       0 | Stay at Home   |
| Blackburn with Darwen               |       0 | Stay at Home   |
| St Albans                           |       0 | Stay at Home   |
| Pendle                              |       0 | Stay at Home   |
| Wellingborough                      |       0 | Stay at Home   |
| Lincoln                             |       0 | Stay at Home   |
| Maldon                              |       0 | Stay at Home   |
| Telford and Wrekin                  |       0 | Very High      |
| South Bucks                         |       0 | Stay at Home   |
| Wolverhampton                       |       0 | Stay at Home   |
| Fylde                               |       0 | Stay at Home   |
| Northampton                         |       0 | Stay at Home   |
| Broxtowe                            |       0 | Stay at Home   |
| Gravesham                           |       0 | Stay at Home   |
| Cheshire East                       |       0 | Stay at Home   |
| Kingston upon Thames                |       0 | Stay at Home   |
| Braintree                           |       0 | Stay at Home   |
| Chelmsford                          |       0 | Stay at Home   |
| St. Helens                          |       0 | Very High      |
| Redditch                            |       0 | Very High      |
| East Lindsey                        |       0 | Stay at Home   |
| Three Rivers                        |       0 | Stay at Home   |
| Leicester                           |       0 | Stay at Home   |
| Stratford-on-Avon                   |       0 | Stay at Home   |
| Torbay                              |       0 | Very High      |
| Bolton                              |       0 | Stay at Home   |
| Bromsgrove                          |       0 | Very High      |
| East Hertfordshire                  |       0 | Stay at Home   |
| Blaby                               |       0 | Stay at Home   |
| Blackpool                           |       0 | Stay at Home   |
| Hastings                            |       0 | Stay at Home   |
| Basingstoke and Deane               |       0 | Stay at Home   |
| Tower Hamlets                       |       0 | Stay at Home   |
| Aylesbury Vale                      |       0 | Stay at Home   |
| Cambridge                           |       0 | Stay at Home   |
| West Devon                          |       0 | Very High      |
| Sevenoaks                           |       0 | Stay at Home   |
| Ashfield                            |       0 | Stay at Home   |
| East Northamptonshire               |       0 | Stay at Home   |
| Cornwall and Isles of Scilly        |       0 | Very High      |
| Hertsmere                           |       0 | Stay at Home   |
| Teignbridge                         |       0 | Very High      |
| Colchester                          |       0 | Stay at Home   |
| Hartlepool                          |       0 | Stay at Home   |
| South Lakeland                      |       0 | Stay at Home   |
| Lichfield                           |       0 | Stay at Home   |
| Amber Valley                        |       0 | Stay at Home   |
| Mole Valley                         |       0 | Stay at Home   |
| Lancaster                           |       0 | Stay at Home   |
| Gloucester                          |       0 | Stay at Home   |
| Boston                              |       0 | Stay at Home   |
| Watford                             |       0 | Stay at Home   |
| New Forest                          |       0 | Stay at Home   |
| Adur                                |       0 | Stay at Home   |
| Cannock Chase                       |       0 | Stay at Home   |
| Somerset West and Taunton           |       0 | Stay at Home   |
| Gedling                             |       0 | Stay at Home   |
| East Hampshire                      |       0 | Stay at Home   |
| Wiltshire                           |       0 | Very High      |
| Cheshire West and Chester           |       0 | Stay at Home   |
| Solihull                            |       0 | Stay at Home   |
| Warrington                          |       0 | Stay at Home   |
| Wokingham                           |       0 | Stay at Home   |
| Nuneaton and Bedworth               |       0 | Stay at Home   |
| Darlington                          |       0 | Stay at Home   |
| Tunbridge Wells                     |       0 | Stay at Home   |
| Sutton                              |       0 | Stay at Home   |
| Central Bedfordshire                |       0 | Stay at Home   |
| Carlisle                            |       0 | Stay at Home   |
| South Hams                          |       0 | Very High      |
| Rochdale                            |       0 | Stay at Home   |
| Guildford                           |       0 | Stay at Home   |
| Uttlesford                          |       0 | Stay at Home   |
| Dover                               |       0 | Stay at Home   |
| Oxford                              |       0 | Stay at Home   |
| Swale                               |       0 | Stay at Home   |
| Plymouth                            |       0 | Very High      |
| Thanet                              |       0 | Stay at Home   |
| Harborough                          |       0 | Stay at Home   |
| Reigate and Banstead                |       0 | Stay at Home   |
| Bedford                             |       0 | Stay at Home   |
| Birmingham                          |       0 | Stay at Home   |
| Rugby                               |       0 | Stay at Home   |
| Middlesbrough                       |       0 | Stay at Home   |
| Nottingham                          |       0 | Stay at Home   |
| Newham                              |       0 | Stay at Home   |
| Waverley                            |       0 | Stay at Home   |
| Wealden                             |       0 | Stay at Home   |
| Cherwell                            |       0 | Stay at Home   |
| Rossendale                          |       0 | Stay at Home   |
| Stockport                           |       0 | Stay at Home   |
| Windsor and Maidenhead              |       0 | Stay at Home   |
| Rushmoor                            |       0 | Stay at Home   |
| Chichester                          |       0 | Stay at Home   |
| South Northamptonshire              |       0 | Stay at Home   |
| Derbyshire Dales                    |       0 | Stay at Home   |
| West Suffolk                        |       0 | Stay at Home   |
| North Devon                         |       0 | Very High      |
| Surrey Heath                        |       0 | Stay at Home   |
| Daventry                            |       0 | Stay at Home   |
| Broxbourne                          |       1 | Stay at Home   |
| Bromley                             |       1 | Stay at Home   |
| Tandridge                           |       1 | Stay at Home   |
| Brentwood                           |       1 | Stay at Home   |
| Bexley                              |       1 | Stay at Home   |
| Epping Forest                       |       1 | Stay at Home   |
| Thurrock                            |       1 | Stay at Home   |
| Castle Point                        |       1 | Stay at Home   |
| Waltham Forest                      |       1 | Stay at Home   |
| Havering                            |       1 | Stay at Home   |
| Barking and Dagenham                |       1 | Stay at Home   |
| Hounslow                            |       1 | Stay at Home   |
| Rochford                            |       1 | Stay at Home   |
| Epsom and Ewell                     |       1 | Stay at Home   |
| Croydon                             |       1 | Stay at Home   |
| Bracknell Forest                    |       1 | Stay at Home   |
| Greenwich                           |       1 | Stay at Home   |
| Lewisham                            |       1 | Stay at Home   |
| Basildon                            |       1 | Stay at Home   |
| Ealing                              |       1 | Stay at Home   |
| Haringey                            |       1 | Stay at Home   |
| Dartford                            |       1 | Stay at Home   |
| Slough                              |       1 | Stay at Home   |
| Harlow                              |       1 | Stay at Home   |
| Hillingdon                          |       1 | Stay at Home   |
| Harrow                              |       1 | Stay at Home   |
| Brent                               |       1 | Stay at Home   |
| Merton                              |       1 | Stay at Home   |
| Barnet                              |       1 | Stay at Home   |
| Maidstone                           |       1 | Stay at Home   |
| Milton Keynes                       |       1 | Stay at Home   |
| Lambeth                             |       2 | Stay at Home   |
| Hackney and City of London          |       2 | Stay at Home   |
| Islington                           |       2 | Stay at Home   |
| Wandsworth                          |       2 | Stay at Home   |
| Hammersmith and Fulham              |       2 | Stay at Home   |
| Westminster                         |       2 | Stay at Home   |
| Richmond upon Thames                |       2 | Stay at Home   |
| Camden                              |       2 | Stay at Home   |
| Tonbridge and Malling               |       2 | Stay at Home   |
| Southwark                           |       2 | Stay at Home   |
| Kensington and Chelsea              |       2 | Stay at Home   |
| Welwyn Hatfield                     |       3 | Stay at Home   |
| Runnymede                           |       3 | Stay at Home   |
| Dacorum                             |       3 | Stay at Home   |
| Wycombe                             |       3 | Stay at Home   |
| Portsmouth                          |       3 | Stay at Home   |
| Reading                             |       3 | Stay at Home   |
| Stevenage                           |       3 | Stay at Home   |
| Woking                              |       3 | Stay at Home   |
| Elmbridge                           |       3 | Stay at Home   |
| Kettering                           |       4 | Stay at Home   |
| Vale of White Horse                 |       4 | Stay at Home   |
| Fenland                             |       4 | Stay at Home   |
| Broadland                           |       4 | Stay at Home   |
| South Gloucestershire               |       4 | Very High      |
| Trafford                            |       4 | Stay at Home   |
| North Tyneside                      |       4 | Stay at Home   |
| Bath and North East Somerset        |       4 | Very High      |
| South Somerset                      |       4 | Stay at Home   |
| Bristol, City of                    |       4 | Very High      |
| North Norfolk                       |       4 | Stay at Home   |
| Erewash                             |       4 | Stay at Home   |
| Northumberland                      |       4 | Stay at Home   |
| West Oxfordshire                    |       4 | Stay at Home   |
| Leeds                               |       4 | Very High      |
| Rutland                             |       4 | Very High      |
| Herefordshire, County of            |       4 | Very High      |
| Tewkesbury                          |       5 | Stay at Home   |
| Kirklees                            |       5 | Very High      |
| Manchester                          |       5 | Stay at Home   |
| Calderdale                          |       5 | Very High      |
| Bradford                            |       5 | Very High      |
| Mid Devon                           |       5 | Very High      |
| Bolsover                            |       5 | Stay at Home   |
| Exeter                              |       5 | Very High      |
| South Ribble                        |       5 | Stay at Home   |
| Hinckley and Bosworth               |       5 | Stay at Home   |
| Chesterfield                        |       5 | Stay at Home   |
| Charnwood                           |       6 | Stay at Home   |
| South Derbyshire                    |       6 | Stay at Home   |
| Forest of Dean                      |       6 | Stay at Home   |
| North Lincolnshire                  |       6 | Very High      |
| Newcastle-under-Lyme                |       6 | Stay at Home   |
| Newark and Sherwood                 |       6 | Stay at Home   |
| Tamworth                            |       6 | Stay at Home   |
| Bassetlaw                           |       6 | Stay at Home   |
| Warwick                             |       6 | Stay at Home   |
| Rotherham                           |       6 | Very High      |
| Sheffield                           |       6 | Very High      |
| Scarborough                         |       6 | Very High      |
| Barnsley                            |       6 | Very High      |
| King’s Lynn and West Norfolk        |       6 | Stay at Home   |
| Melton                              |       6 | Stay at Home   |
| High Peak                           |       6 | Stay at Home   |
| Stroud                              |       6 | Stay at Home   |
| South Kesteven                      |       6 | Stay at Home   |
| North Kesteven                      |       6 | Stay at Home   |
| East Riding of Yorkshire            |       6 | Very High      |
| Cheltenham                          |       6 | Stay at Home   |
| East Devon                          |       6 | Very High      |
| West Lindsey                        |       6 | Stay at Home   |
| Mid Sussex                          |       6 | Stay at Home   |
| Cotswold                            |       6 | Stay at Home   |
| Rushcliffe                          |       6 | Stay at Home   |
| South Holland                       |       6 | Stay at Home   |
| Salford                             |       6 | Stay at Home   |
| Mendip                              |       6 | Stay at Home   |
| Preston                             |       6 | Stay at Home   |
| Wyre Forest                         |       6 | Very High      |
| Oldham                              |       6 | Stay at Home   |
| Kingston upon Hull, City of         |       6 | Very High      |
| North West Leicestershire           |       6 | Stay at Home   |
| Stockton-on-Tees                    |       6 | Stay at Home   |
| Craven                              |       7 | Very High      |
| York                                |       7 | Very High      |
| Ipswich                             |       7 | Stay at Home   |
| Richmondshire                       |       7 | Very High      |
| Huntingdonshire                     |       7 | Stay at Home   |
| Chorley                             |       7 | Stay at Home   |
| Harrogate                           |       7 | Very High      |
| Winchester                          |       7 | Stay at Home   |
| Fareham                             |       7 | Stay at Home   |
| Halton                              |       7 | Very High      |
| Swindon                             |       7 | Stay at Home   |
| Mid Suffolk                         |       7 | Stay at Home   |
| Knowsley                            |       7 | Very High      |
| Gateshead                           |       7 | Stay at Home   |
| Hart                                |       7 | Stay at Home   |
| Worthing                            |       7 | Stay at Home   |
| Chiltern                            |       7 | Stay at Home   |
| Wigan                               |       7 | Stay at Home   |
| Allerdale                           |       7 | Stay at Home   |
| Test Valley                         |       7 | Stay at Home   |
| Arun                                |       7 | Stay at Home   |
| Liverpool                           |       7 | Very High      |
| Worcester                           |       7 | Very High      |
| Eastleigh                           |       7 | Stay at Home   |
| Corby                               |       7 | Stay at Home   |
| Horsham                             |       7 | Stay at Home   |
| Shropshire                          |       7 | Very High      |
| South Oxfordshire                   |       7 | Stay at Home   |
| Breckland                           |       7 | Stay at Home   |
| Barrow-in-Furness                   |       7 | Stay at Home   |
| Redcar and Cleveland                |       7 | Stay at Home   |
| Sunderland                          |       7 | Stay at Home   |
| South Norfolk                       |       7 | Stay at Home   |
| Copeland                            |       7 | Stay at Home   |
| West Lancashire                     |       7 | Stay at Home   |
| Isle of Wight                       |       7 | Stay at Home   |
| Ryedale                             |       7 | Very High      |
| Bournemouth, Christchurch and Poole |       7 | Stay at Home   |
| Sedgemoor                           |       7 | Stay at Home   |
| Great Yarmouth                      |       7 | Stay at Home   |
| Brighton and Hove                   |       7 | Stay at Home   |
| Wirral                              |       7 | Very High      |
| Hambleton                           |       7 | Very High      |
| East Cambridgeshire                 |       7 | Stay at Home   |
| Sefton                              |       7 | Very High      |

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

![](dsrs_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
