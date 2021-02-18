## pilot STP ICS plan analysis
## based on 3 plans

### load libraries
library(pacman)
p_load(tidyverse, readtext, quanteda, myScrapers)


### extract and import files
dir <- tempdir()

unzip("~/Downloads/fwdofficialaisoftwareanalysisoficsplans.zip", exdir = dir)

pdfs <- list.files(dir, pattern = "pdf")

files <- readtext(paste0(dir, "/", pdfs))

### explore files

corp1 <- corpus(files, text_field = "text")

textplot_xray(kwic(corp1, phrase("blue*")))

corpus <- files %>%
  mutate(pmid = row_number()) %>%
  rename(absText = text,
         title = doc_id ) %>%
  create_abstract_corpus() 

dfm <- corpus$corpus %>%
  filter(!str_detect(word, "[[:digit:]]")) %>%
  cast_dfm(pmid, word, n) %>%
  as.dfm()


### word clouds

textplot_wordcloud(dfm, comparison = TRUE, min_count = 2, color = viridis::viridis(5, begin = .2))



## Search for key words
### create dictionary

l <- list("Built Environment, Natural Environment, Nature, Green space, 
           Blue space, Parks, Open space, Built and Natural Environment, 
           Environment(al), Wider determinants, Social determinants, Housing, Homelessness, 
           Rough sleeping, Good home, Affordable housing, Private rented sector, 
           Social housing, Accommodation, Sustainable housing, Fuel poverty, Education, 
           Employment, Place, Neighbourhood, Place based, Deprivation, Deprived communities, Air quality, Air pollution, Transport, 
           Public transport, Traffic, Congestion, Walking, Cycling, Walking and cycling, Active travel")

dictionary <- create_lookup(
  
  natural_environment = c("natur*", "green*", "blue*", "open*"), 
  built_environment = "buil*", 
  determinants = "determin*", 
  housing = c("hous", "hom*", "afford"), 
  fuel_poverty = "fuel", 
  place_based = c("plac", "neighbour*"), 
  deprivation = "depriv*", 
  airquality = c("pollut*", "air_qual*"), 
  transport = c("traffic", "walk*", "cycl*"), 
  homelessness = c("homeless", "rough"), 
  employment = "employ*"
)


lu <- dfm_lookup(dfm, dictionary = dictionary) %>%
  convert(., to = "data.frame") %>%
  pivot_longer(names_to = "category", values_to = "count", cols = 2:ncol(.)) 

lu %>%
  gt::gt()

lu %>%
  ggplot(aes(category, fct_rev(doc_id), fill = count)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 30, low = "white", mid = "orange", high = "red") +
  coord_equal() +
  #viridis::scale_fill_viridis(direction = -1) +
  labs(y = "doc_id")


### synonyms / word vectors


library(text2vec)

tok <- tokens(corp1)
feats <- dfm(tok, verbose = TRUE) %>%
  dfm_trim(min_termfreq = 2) %>%
  featnames()

fcm <- fcm(tok, context = "window", count = "weighted", weights = 1 / (1:5), tri = TRUE)
glove <- GlobalVectors$new(rank = 50, x_max = 10)
wv_main <- glove$fit_transform(fcm, n_iter = 10,
                               convergence_tol = 0.01, n_threads = 8)
wv_context <- glove$components
word_vectors <- wv_main + t(wv_context)
test <- word_vectors["homeless", , drop = FALSE] +
  word_vectors["environment", , drop = FALSE]
library("quanteda.textstats")
cos_sim <- textstat_simil(x = as.dfm(word_vectors), y = as.dfm(test),
                          method = "cosine")
head(sort(cos_sim[, 1], decreasing = TRUE), 20)
