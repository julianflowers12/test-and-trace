library(RSQLite)
library(rtweet)
library(tm)
library(dplyr)
library(knitr)
library(wordcloud)
library(lubridate)
library(ggplot2)

conn <- dbConnect(RSQLite::SQLite(), "tweet.db")

dbExecute(conn, "CREATE TABLE Tweet_Data(
                  Tweet_ID INTEGER PRIMARY KEY,
                  User TEXT,
                  Tweet_Content TEXT,
                  Date_Created INTEGER)")

normalise_text <- function(text){
  
  text = iconv(text, "latin1", "ASCII", sub = "")
  
  text = tolower(text)
  
  text = gsub("<.*?>", " ", text)
  
  text = gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", "", text)
  
  text = gsub("[^[:alnum:]]", " ", text)
  
  text = removeWords(text,c("rt","gt",stopwords("en")))
  
  text = stripWhitespace(text)                                 
  text = gsub("^\\s+|\\s+$", "", text)                         
  
  return(text)

  
}



tok <- read_rds("/Users/julianflowers/.rtweet_token1.rds")
tweet_stream <- rtweet::stream_tweets(keys, token = tok)
keys <- "coronavirus.data.gov.uk"



usethis::edit_r_environ()
transform_and_clean_tweets <- function(filename, remove_rts = TRUE){
  
  # Import the normalize_text function
  source("normalize_text.R")
  
  # Parse the .json file given by the Twitter API into an R data frame
  df <- parse_stream(filename)
  # If remove_rst = TRUE, filter out all the retweets from the stream
  if(remove_rts == TRUE){
    df <- filter(df,df$is_retweet == FALSE)
  }
  # Keep only the tweets that are in English
  df <- filter(df, df$lang == "en")
  
  small_df <- df[,c("screen_name","text","created_at")]
  names(small_df) <- c("User","Tweet_Content","Date_Created")
  # Finally normalize the tweet text
  small_df$Tweet_Content <- sapply(small_df$Tweet_Content, normalize_text)
  # Return the processed data frame
  return(small_df)
}


hour_counter <- 0

# Initialize a while loop that stops when the number of hours you want to stream tweets for is exceeded
while(hour_counter <= 12){
  # Set the stream time to 2 hours each iteration (7200 seconds)
  streamtime <- 3600
  # Create the file name where the 2 hour stream will be stored. Note that the Twitter API outputs a .json file.
  filename <- paste0("nlp_stream_",format(Sys.time(),'%d_%m_%Y__%H_%M_%S'),".json")
  # Stream Tweets containing the desired keys for the specified amount of time
  stream_tweets(q = keys, timeout = streamtime, file_name = filename)
  # Clean the streamed tweets and select the desired fields
  clean_stream <- transform_and_clean_tweets(filename, remove_rts = TRUE)
  # Append the streamed tweets to the Tweet_Data table in the SQLite database
  dbWriteTable(conn, "Tweet_Data", clean_stream, append = T)
  # Delete the .json file from this 2-hour stream
  file.remove(filename)
  # Add the hours to the tally
  hour_counter <- hour_counter + 2
}

data_test <- dbGetQuery(conn, "SELECT * FROM Tweet_Data LIMIT 20")
unique_rows <- dbGetQuery(conn, "SELECT COUNT() AS Total FROM Tweet_Data")
kable(data_test)
