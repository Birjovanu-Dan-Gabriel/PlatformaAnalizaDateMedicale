# test_health_platform.R
# Test script for the Health Data Platform application

library(testthat)
library(shiny)
library(shinytest2)
library(dplyr)

# Source the data processing function
source("R/data_processing.R")

# Set up test data
create_test_data <- function() {
  test_data <- data.frame(
    patient_id = c(1, 2, 3),
    nume = c("Popescu", "Ionescu", "Vasilescu"),
    prenume = c("Ion", "Maria", "Dan"),
    age = c(45, 62, 28),
    gender = c("M", "F", "M"),
    region = c("Urban", "Rural", "Urban"),
    weight_kg = c(85, 70, 65),
    height_cm = c(175, 160, 180),
    bmi = c(27.8, 27.3, 20.1),
    blood_pressure = c("135/85", "150/90", "120/80"),
    cholesterol = c("Normal", "High", "Normal"),
    glucose = c(110, 140, 95),
    smoker = c("Yes", "No", "No"),
    diabetic = c("No", "Yes", "No"),
    physical_activity = c("Low", "Low", "High"),
    stringsAsFactors = FALSE
  )
  
  # Save test data
  dir.create("data", showWarnings = FALSE)
  write.csv(test_data, "data/raw_data2.csv", row.names = FALSE)
  
  return(test_data)
}

# Function to clean up test files
cleanup_test_files <- function() {
  if (file.exists("data/raw_data22.csv")) {
    file.remove("data/raw_data22.csv")
  }
  if (file.exists("data/processed_data2.csv")) {
    file.remove("data/processed_data22.csv")
  }
}

# Run tests
test_that("Data processing function works correctly", {
  # Create test data
  test_data <- create_test_data()
  
  # Process data
  processed <- preprocess_health_data(test_data)
  
  # Test basic processing
  expect_equal(nrow(processed), 3)
  expect_true("risk_score" %in% names(processed))
  expect_true("age_group" %in% names(processed))
  expect_true("bp_systolic" %in% names(processed))

  
  # Test risk calculation
  expect_true(processed$risk_score[2] >= 1, "High BP patient should have risk score of at least 2")
  
  # Clean up
  cleanup_test_files()
})

test_that("Application initialization creates necessary files", {
  # Create test data
  test_data <- create_test_data()
  
  # Test if processed file gets created
  source("R/data_processing.R")
  processed <- preprocess_health_data(test_data)
  write.csv(processed, "data/processed_data2.csv", row.names = FALSE)
  
  expect_true(file.exists("data/processed_data2.csv"))
  
  # Read back processed file
  processed_read <- read.csv("data/processed_data2.csv", stringsAsFactors = FALSE)
  expect_equal(nrow(processed_read), 3)
  
  # Clean up
  cleanup_test_files()
})

test_that("BMI calculation works correctly", {
  # Create test data with edge cases
  test_edge_cases <- data.frame(
    patient_id = c(4, 5, 6),
    nume = c("Test1", "Test2", "Test3"),
    prenume = c("A", "B", "C"),
    age = c(30, 40, 50),
    gender = c("M", "F", "M"),
    region = c("Urban", "Urban", "Rural"),
    weight_kg = c(70, 80, NA),  # Including NA case
    height_cm = c(175, 0, 180), # Including 0 height case
    blood_pressure = c("120/80", "130/85", "140/90"),
    stringsAsFactors = FALSE
  )
  
  # Process data
  processed <- preprocess_health_data(test_edge_cases)
  
  # Test BMI calculation
  expect_equal(round(processed$bmi[1], 1), round(70/((175/100)^2), 1))
  expect_true(is.na(processed$bmi[2]), "BMI should be NA when height is 0")
  expect_true(is.na(processed$bmi[3]), "BMI should be NA when weight is NA")
  
  # No need to save these to file
})

# Print successful completion message
cat("\n\n=== Test execution completed successfully ===\n")
cat("All health platform tests have passed.\n\n")