# Test cases for functions for Markov Chain training and text generation
# Author: Naman Nimbale

setwd("D:\\Studies\\Semesters\\Fall_24\\ISC\\Project\\ISC_final_project_markov") # Change your respective WD path

# Source required scripts
source("./src/text_preprocess.R")
source("./src/model_training.R")

# Load required libraries
library(testthat)
library(dplyr)

# Load the pre-trained model from an RDS file
# Extract the transition probabilities and last word probabilities
model <- readRDS("./models/model3_n_4.rds")
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


# Test: Validate the word tokenizer function
test_that("tokenize_words splits text into words accurately", {
  # Input text and expected tokenized output
  test_text <- "This is another test."
  expected_output <- c("This", "is", "another", "test.")
  
  # Check if the function output matches the expected tokens
  expect_equal(tokenize_words(test_text), expected_output)
})

# Test: Validate the punctuation transition function
test_that("punctuation_transition correctly splits text into punctuations", {
  # Input text and expected punctuation transition
  test_text <- "Testing punctuation transition. Does it work? Yes!"
  expected_output <- tibble::tibble(
    last_word = c("Yes", "transition", "work"),
    punctuation = c("!", ".", "?"),
    probability = c(1, 1, 1))
  
  # Check if the function output matches the expected sentences
  expect_equal(punctuation_transition(test_text), expected_output)
})


# Test: Validate the n-gram generator function
test_that("generate_n_grams creates correct n-grams", {
  # Input tokens and expected 3-grams output
  tokens <- c("This", "is", "a", "simple", "test")
  n <- 3
  expected_output <- list(c("This is", "a"), c("is a", "simple"), c("a simple", "test"))
  
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

# Check behavior of n-gram generator with a single token
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


# Test cases for generate_transition_prob
test_that("generate_transition_prob calculates correct transition probabilities", {
  n_grams <- list(c("this is", "a"), c("is a", "test"), c("this is", "another"))
  expected <- tibble::tibble(
    previous = c("is a", "this is", "this is"),
    current = c("test", "a", "another" ),
    prob = c(1, 0.5, 0.5)
  )
  result <- generate_transition_prob(n_grams)
  expect_equal(result, expected)
})

test_that("generate_transition_prob handles empty input", {
  n_grams <- list()
  expected_df <- data.frame(previous = character(0), current = character(0), prob = numeric(0))
  result <- generate_transition_prob(n_grams)
  expect_equal(result, expected_df)
})

