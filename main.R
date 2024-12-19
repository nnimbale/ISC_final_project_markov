# main.R
source("model_training.R")

generate_text <- function(model, len=50, feed = "", n = 2){
  transition_df <- model$transition_df
  punctuations_df <- model$punctuations_df
  if (feed != ""){
    feed <- tolower(feed)
    feed <- unlist(strsplit(feed, "\\s+"))
    if (length(feed) >= len){
      stop("The length of the feed is longer than the length of the text requested")
    }
    else if (length(feed) < n-1){
      stop("The length of the feed is shorter than the n-1")
    }
    else{
      first_ngram <- paste(tail(feed, n - 1), collapse = " ")
    }
  }

  else {
    first_ngram <- sample(transition_df$previous, 1)
  }
  
  first_ngram <- unlist(strsplit(first_ngram, "\\s+"))
  
  text <- character(0)
  text <- c(text, first_ngram)
  
  word_count <- length(text)
  i <- length(text) + 1 
  
  while (word_count < len){
    
    previous_ngram <- tail(text, n-1)
    previous_ngram <- paste(previous_ngram, collapse = " ")
    previous_ngram <- as.character(previous_ngram)
    
    next_words <- transition_df %>% 
      filter(previous == previous_ngram) 
    
    if (nrow(next_words) == 0){
      stop("You input some words not included in the training data")
    }
    current_word <- sample(next_words$current, 1, prob = next_words$prob)
    if (current_word != "</s>") {
      word_count <- word_count + 1
    }
    text <- c(text, current_word)
    i <- i + 1
  }
  for (i in 1:length(text)){
    if (text[i] == "</s>"){
      prev_word <- text[i-1]
      available_punctuation <- punctuations_df%>%
        filter(last_word %in% prev_word)
      punctuation <- sample(available_punctuation$punctuation, 1, prob = available_punctuation$probability)
      text[i] <- punctuation
    }
  }
  
  
  return(text)
}

capitalize_first_letter <- function(text){
  text <- paste0(toupper(substr(text, 1, 1)), 
                 substr(text, 2, nchar(text)))
  return(text)
}

readable_text <- function(text){
  text[1] <- capitalize_first_letter(text[1])
  for (i in 2:length(text)){
    if (text[i-1] %in% c(".", "?", "!")){
      text[i] <- capitalize_first_letter(text[i])
    }
  }
  for (i in 2:length(text)){
    if (text[i] %in% c(".", "?", "!", ",")){
      text[i - 1] <- paste0(text[i - 1], text[i])
      text[i] <- ""
    }
  }
  text <- paste(text, collapse = " ")
  text <- gsub("\\s+", " ", text)
  return(text)
}

main <- function(){
  while(TRUE){
    n <- as.integer(readline("What is the n in n-gram: "))
    feed <- readline("Any context: ")
    length <- as.integer(readline("Enter the total length of text you want to generate: "))
    pattern <- paste0("*", "n_", n, "\\.rds")
    models <- list.files(path = directory, pattern = pattern, full.names = TRUE)
    cat("The following models are available:\n")
    for (i in 1:length(models)){
      cat(models[i], "\n")
    }
    new_model <- readline("Do you want to train a new model? (y/n): ")
    
    if (new_model == "y") {
      model <- train_model(directory, n)
      model_name <- readline("Enter a name for the model: ")
      model_name <- paste(model_name, "n", n, sep = "_")
      model_path <- paste0(directory, "/" , model_name, ".rds")
      
      saveRDS(model, model_path)
      cat("Model saved as:", model_path, "\n")
      
      sample <- generate_text(model, length, n, feed = feed)
      cat(generate_readable_text(sample))
    }else {      
      model_name <- readline("Please enter the name of the model (including extension): ")
      # load the model
      model_path <- paste0(directory, "/" , model_name)
      model <- readRDS(model_path)
      sample <- generate_text(model, length, n, feed = feed)
      cat(generate_readable_text(sample))
    }
    exit <- readline("Do you want to exit? (y/n): ")
    if (exit == "y"){
      break
    }
  }
}

main()

