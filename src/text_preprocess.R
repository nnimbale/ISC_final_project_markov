# preprocess.R
setwd("/Users/ramyakbilas/Downloads/S610_Markov_Model") 
library(dplyr)
library(readr)

#load_train_data <- function(directory){
#  booklist <- list.files(directory, full.names=TRUE)
#  train_data <- paste(train, collapse = " ")
#  return(train_data)
#}

read_txt_files <- function(files) {
  file_contents <- lapply(files, readr::read_file)
  names(file_contents) <- basename(files)  
  return(file_contents)
}

clean_text <- function(text) {
  text <- tolower(text)
  text <- gsub("[^[:alnum:],.!?\"\']", " ", text)
  text <- gsub("\n", " ", text)
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  return(text)
}


tokenize_words <- function(text) {
  tokens <- unlist(strsplit(text, "\\s+"))
  return(tokens)
}


replace_punctuation_with_tag <- function(text) {
  text <- gsub("[,\\.\\?!]", " </s> ", text)
  text <- gsub("\\s+", " ", text)
  return(text)
}
