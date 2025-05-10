# În tests/test_analysis.R
library(testthat)

test_that("Datele sunt curățate corect", {
  expect_equal(sum(is.na(clean_data$age)), 0)
  expect_true(all(clean_data$age >= 0))
  expect_equal(sum(is.na(clean_data$glocose)), 0)
  expect_true(all(clean_data$glucose >= 0))
})

test_that("Calcul factor de risc", {
  expect_true(max(clean_data$risk_factor) <= 3)
})
