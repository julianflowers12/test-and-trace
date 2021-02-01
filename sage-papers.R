## sage publications

library(myScrapers)
library(readtext)
library(quanteda)
library(spacyr)
library(reticulate)

source("entities2.R")

spacy_initialize()

url <- paste0("https://www.gov.uk/search/all?keywords=sage&order=relevance&page=", 1:73)

links <- map(url, get_page_links)

links_df <- enframe(links) %>%
  unnest("value")

links_df <- links_df %>%
  filter(str_detect(value, "sage") & str_detect(value, "covid") ) %>%
  mutate(link = paste0("https://www.gov.uk", value), 
         links = map(link, get_page_docs)) %>%
  unnest("links")

links_text <- links_df %>%
  mutate(text = map(links, readtext)) %>%
  unnest("text") %>%
  distinct() 

links_text <- links_text %>%
  filter(nchar(text) > 2)

corp <- corpus(links_text, text_field = "text", docid_field = "doc_id", unique_docnames = FALSE)

mortality <- kwic(corp, pattern = "mortality|death", valuetype = "regex", window = 10) %>%
  data.frame()

date <- kwic(corp, pattern = "\\d{2}.[[:alpha:]].*\\d{4}", valuetype = "regex", window = 10) %>%
  data.frame()

date %>%
  reactable::reactable()

safe_np <- safely(extract_np)
safe_date <- safely(extract_date)

test <- links_text %>%
  mutate(test = map(text, safe_np), 
         date = map(text, safe_date))

test <- links_text %>%
  mutate(date = map(text, safe_date))

t1 <- test %>%
  unnest("test") %>%
  filter(!test == "NULL") %>% 
  mutate(test1 = map(test, "text")) %>%
  unnest("test1")

t1 <- t1 %>%
  unnest("date") %>%
  filter(!date == "NULL") %>% 
  mutate(date1 = map(date, "text")) %>%
  unnest("date1")

t2 <- t1 %>%
  select(doc_id, test1, date1) %>%
  data.table::setDT()

t2[str_detect(date1, "17th Decem") & str_detect(test1, "death|mort"),]

%>% View()
  unnest_auto("test")

View()

https://www.gov.uk/search/all?keywords=sage&order=relevancesage <- googlesearchR("site:GOV.uk Scientific Advisory Committee on Emergencies reports mortality")

sage1 <- sage %>%
  .[3] %>%
  as.character() %>%
  get_page_links() %>%
  enframe()

View(sage1)
