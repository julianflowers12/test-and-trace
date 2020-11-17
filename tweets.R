library(pacman)
p_load(myScrapers, spacyr, rtweet, tidyverse, quanteda, tidytext, textfeatures, text2vec, lubridate)


### coronavirus.data.gov.uk tweets


covid <- "coronavirus.data.gov.uk"
covid_tweets <- search_tweets(q = covid, n = 18000)

dim(covid_tweets)

cov_tweets <- covid_tweets %>%
  mutate(date = ymd(str_sub(created_at, 1, 10))) %>%
  select(user_id, date, screen_name, text, retweet_count, favorite_count, 
         favorite_count, followers_count, friends_count, country)


cov_text <- cov_tweets %>%
  select(text) %>%
  distinct()

corp <- corpus(cov_text)

dfm <- dfm(corp, remove_punct = TRUE, remove = stopwords("en"), remove_url = TRUE, remove_symbols = TRUE, stem = TRUE)
dfm <- dfm_trim(dfm, min_termfreq = 5, min_docfreq = 2)


tstat <- textstat_simil(dfm, method = "correlation", margin = "documents")

# hierarchical clustering - get distances on normalized dfm
tstat_dist <- textstat_dist(dfm_weight(dfm, scheme = "prop")) 

tstat_dist@x <- tstat_dist@x %>%
  na.omit()
# hiarchical clustering the distance object
pres_cluster <- hclust(as.dist(tstat_dist))
# label with document names
pres_cluster$labels <- docnames(dfm)
# plot as a dendrogram
plot(pres_cluster, xlab = "", sub = "", main = "Euclidean Distance on Normalized Token Frequency")

