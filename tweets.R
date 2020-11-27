library(pacman)
p_load(myScrapers, spacyr, rtweet, tidyverse, quanteda, tidytext, textfeatures, text2vec, lubridate)


### coronavirus.data.gov.uk tweets


covid <- "coronavirus.data.gov.uk"
covid_tweets <- search_tweets(q = covid, n = 18000)

dim(covid_tweets)

cov_tweets1 <- covid_tweets %>%
  mutate(date = ymd(str_sub(created_at, 1, 10))) %>%
  select(status_id, user_id, date, screen_name, text, retweet_count, favorite_count, 
         favorite_count, followers_count, friends_count, country, created_at)


cov_text <- cov_tweets1 %>%
  select(status_id, text, created_at) %>%
  distinct()

 str(cov_text) 

 sent <- textfeatures::textfeatures(cov_text$text)

 dim(sent)

cov_text1 <- cov_text %>% cbind(sent) %>% data.frame()

class(tokens)

cov_text1 %>% filter(sent_vader == min(sent_vader))
glimpse(cov_text1)

 head(cov_text1)

 dim(sent)

 sent  %>% 
    ggplot() +
    geom_density(aes(sent_vader))

  str(cov_text)


tokens <- cov_text %>% mutate(text = tolower(text),
                              text = tm::removePunctuation(text),
                              text = tm::removeNumbers(text),
                              text = tm::removeWords(text, 
                              c(stopwords("en"), "https?", "t.co", "\\n")))


corp <- corpus(tokens, text_field = "text")
dfm <- dfm(corp)
stm <- convert(dfm, to = "stm")
stm_topics <- stm::stm(K = 10, stm$documents, stm$vocab, data = stm$meta, init.type = "Spectral")

plot(stm_topics, n = 7, text.cex = .6)

#####
 str(tokens)                           
tokens <- word_tokenizer(tokens$text)
it <- itoken(tokens, ids = cov_text$status_id, progressbar = FALSE)
v <- create_vocabulary(it,  ngram = c(ngram_min = 1, ngram_max = 2) , stopwords = tidytext::stop_words$word)
v <- prune_vocabulary(v, term_count_min = 2, doc_proportion_max = 0.2)


vectorizer <- vocab_vectorizer(v)
dtm <- create_dtm(it, vectorizer, type = "dgTMatrix")

lda_model = LDA$new(n_topics = 20, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr = 
  lda_model$fit_transform(x = dtm, n_iter = 1000, 
                          convergence_tol = 0.001, n_check_convergence = 25, 
                          progressbar = FALSE)

lda_model$get_top_words(n = 10, lambda = .2)


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

wiki_toks <- tokens_remove(tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE, remove_numbers = TRUE ), stopwords("english"))

feats <- dfm(wiki_toks, verbose = TRUE) %>%
    dfm_trim(min_termfreq = 5) %>%
    featnames()

wiki_fcm <- fcm(wiki_toks, context = "window", count = "weighted", weights = 1 / (1:5), tri = TRUE)

library("text2vec")

glove <- GlobalVectors$new(rank = 50, x_max = 10)
wv_main <- glove$fit_transform(wiki_fcm, n_iter = 10,
                               convergence_tol = 0.01, n_threads = 8)

wv_context <- glove$components
dim(wv_context)   
word_vectors <- wv_main + t(wv_context)   

covid <- word_vectors["official", , drop = FALSE]
cos_sim <- textstat_simil(x = as.dfm(word_vectors), y = as.dfm(covid),
                          method = "cosine")
head(sort(cos_sim[, 1], decreasing = TRUE), 20)



######
glimpse(cov_text1)
cov_text2 <- cov_text1  %>% 
     mutate(pmid = row_number(), 
     title = "A") %>%
     rename(absText = text)
corp1 <- myScrapers::create_abstract_corpus(cov_text2)

clust1 <- myScrapers::create_abstract_cluster(corp1$corpus)