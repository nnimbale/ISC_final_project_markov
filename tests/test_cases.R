# Functions for Markov Chain training and text generation
# Author: Naman Nimbale and Ramyak Bilas

setwd("D:\\Studies\\Semesters\\Fall_24\\ISC\\Project\\ISC_final_project_markov") # Change your respective WD path

# Source required scripts
source("./src/preprocessing.R")
source("./src/train_model.R")

# Load required libraries
library(testthat)
library(dplyr)

# Load the pre-trained model from an RDS file
# Extract the transition probabilities and last word probabilities
model <- readRDS("./models/sh_model.rds")
transition_prob <- model$transition_prob
last_word <- model$last_word

# Test: Ensure all transition probabilities are valid
test_that("Transition probabilities are between 0 and 1", {
  # Check that all probabilities fall within the valid range [0, 1]
  expect_true(all(transition_prob$prob >= 0 & transition_prob$prob <= 1))
})

# Test: Ensure the sum of probabilities for each previous state is approximately 1
test_that("Sum of probabilities for each previous state is approximately 1", {
  # Group by the "previous" state and calculate the sum of probabilities
  summed_probs <- transition_prob %>%
    group_by(previous) %>%
    summarise(sum_prob = sum(prob))
  
  # Verify that the sum is close to 1 (considering floating-point precision limits)
  expect_true(all(abs(summed_probs$sum_prob - 1) < .Machine$double.eps^0.5))
})

# Test: Ensure all last word probabilities are valid
test_that("Last word probabilities are between 0 and 1", {
  # Check that all last word probabilities fall within the valid range [0, 1]
  expect_true(all(last_word$prob >= 0 & last_word$prob <= 1))
})

# Test: Ensure the sum of last word probabilities is approximately 1
test_that("Sum of last word probabilities is approximately 1", {
  # Group by each "last_word" and calculate the sum of probabilities
  last_word_probs <- last_word %>%
    group_by(last_word) %>%
    summarise(sum_prob = sum(prob))
  
  # Verify that the sum is close to 1 (considering floating-point precision limits)
  expect_true(all(abs(last_word_probs$sum_prob - 1) < .Machine$double.eps^0.5))
})

# Test: Validate the word tokenizer function
test_that("tokenize_words splits text into words accurately", {
  # Input text and expected tokenized output
  test_text <- "This is another test."
  expected_output <- c("This", "is", "another", "test.")
  
  # Check if the function output matches the expected tokens
  expect_equal(tokenize_words(test_text), expected_output)
})

# Test: Validate the sentence tokenizer function
test_that("tokenize_sentences correctly splits text into sentences", {
  # Input text and expected tokenized sentences
  test_text <- "Testing sentence tokenizer. Does it work? Yes!"
  expected_output <- c("Testing sentence tokenizer.", "Does it work?", "Yes!")
  
  # Check if the function output matches the expected sentences
  expect_equal(tokenize_sentences(test_text), expected_output)
})

# Test: Validate the n-gram generator function
test_that("generate_n_grams creates correct n-grams", {
  # Input tokens and expected 2-grams output
  tokens <- c("This", "is", "a", "simple", "test")
  n <- 3
  expected_output <- list(c("This", "is", "a"), c("is", "a", "simple"), c("a", "simple", "test"))
  
  # Check if the function output matches the expected n-grams
  expect_equal(generate_n_grams(tokens, n), expected_output)
})

# Additional test case: Check behavior of n-gram generator with empty token input
test_that("generate_n_grams handles empty token inputs gracefully", {
  # Input an empty token list and expected output
  tokens <- c()
  n <- 2
  expected_output <- list()
  
  # Verify the function handles this edge case correctly
  expect_equal(generate_n_grams(tokens, n), expected_output)
})

# Additional test case: Check behavior of n-gram generator with a single token
test_that("generate_n_grams handles single-token input", {
  # Input a single token and expected output
  tokens <- c("Single")
  n <- 2
  expected_output <- list()
  
  # Verify the function handles this edge case correctly
  expect_equal(generate_n_grams(tokens, n), expected_output)
})

# Test: Validate word tokenizer with special characters and punctuation
test_that("tokenize_words handles special characters and punctuation", {
  # Input text with special characters and expected tokenized output
  test_text <- "Hello, world! Let's test special characters: @, #, $, %."
  expected_output <- c("Hello,", "world!", "Let's", "test", "special", "characters:", "@,", "#,", "$,", "%.")
  
  # Verify the tokenizer correctly handles special characters
  expect_equal(tokenize_words(test_text), expected_output)
})

# Test: Validate sentence tokenizer with multiple spaces and newline characters
test_that("tokenize_sentences works with multiple spaces and newline characters", {
  # Input text with irregular spacing and newline characters
  test_text <- "First sentence.  Second sentence.\nThird sentence."
  expected_output <- c("First sentence.", "Second sentence.", "Third sentence.")
  
  # Verify the tokenizer correctly splits sentences
  expect_equal(tokenize_sentences(test_text), expected_output)
})