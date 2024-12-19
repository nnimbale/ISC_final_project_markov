# training.R
source("text_preprocess.R")

punctuation_transition <- function(text) {
  sentences <- unlist(strsplit(text, "(?<=[,\\.\\?!])\\s*", perl = TRUE))
  last_words <- c()
  punctuations <- c()
  for (sentence in sentences) {
    match <- regmatches(sentence, gregexpr("\\b\\w+\\b[,\\.\\?!]?", sentence))[[1]]
    if (length(match) > 0) {
      last_part <- match[length(match)]
      last_word <- gsub("[,\\.\\?!]", "", last_part)  # Remove punctuation
      punctuation <- gsub(".*(,|\\.|\\?|!)$", "\\1", last_part)  # Extract punctuation
      
      if (punctuation %in% c(",", ".", "?", "!")) {
        last_words <- c(last_words, last_word)
        punctuations <- c(punctuations, punctuation)
      }
    }
  }
  
  df <- data.frame(
    last_word = last_words,
    punctuation = punctuations,
    stringsAsFactors = FALSE
  )
  
  prob_df <- df %>% 
    group_by(last_word, punctuation) %>%  
    summarise(count = n(), .groups = 'drop') %>% 
    group_by(last_word) %>% 
    mutate(probability = count/sum(count)) %>% 
    ungroup() %>% 
    select(last_word, punctuation, probability)
  
  return(prob_df)
}

generate_n_grams <- function(tokens, n=2){
  n_grams <- lapply(1:(length(tokens)-n+1), function(i) tokens[i:(i+n-1)])
  
  n_grams <- lapply(n_grams, function(x) list(x[1:(n-1)], x[n]))
  
  n_grams <- lapply(n_grams, function(n_gram) {
    c(paste(n_gram[[1]], collapse = " "),
      n_gram[[2]])
  })
  
  return(n_grams)
}

generate_transition_prob <- function(n_grams){
  df <- do.call(rbind, lapply(n_grams, function(x) {
    data.frame(previous = x[1], current = x[2])
  }))
  transition_prob <- df %>% 
    group_by(previous, current) %>%  
    summarise(count = n(), .groups = 'drop') %>% 
    group_by(previous) %>%
    mutate(prob = count/sum(count)) %>% 
    ungroup() %>% 
    select(previous, current, prob)
  return(transition_prob)
}



train_model <- function(directory, n=2){
  txt_files <- list.files(path = directory, pattern = "\\.txt$", full.names = TRUE)
  file_contents <- read_txt_files(txt_files)
  cleaned_text <- clean_text(file_contents)
  punctuations_df <- punctuation_transition(cleaned_text)
  modified_text <- replace_punctuation_with_tag(cleaned_text)
  tokens_modified <- tokenize_words(modified_text) 
  n_grams_modified <- generate_n_grams(tokens_modified, n)
  transition_prob_modified <- generate_transition_prob(n_grams_modified)
  model <- list(punctuations_df = punctuations_df, transition_df = transition_prob_modified)
  return(model)
}