## use spacyr to redact names

redact_names <- function(df, x){
  
  require(spacyr)
  require(dplyr)
  df <- df %>%
    mutate(doc_id = paste0("text", row_number()))
  anno <- spacy_extract_entity(x)
  persons <- anno %>%
    filter(str_detect(ent_type, "PERSON")) 
  redacted <- persons %>%
    left_join(df) %>%
    mutate(redacted = str_replace_all(feedback, text, "\\[redacted\\]"))
  
  out <- list(redacted = redacted, persons = persons)
  
}


