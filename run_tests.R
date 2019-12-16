if (!requireNamespace("shinyBS", quietly = TRUE)) {
  install.packages("shinyBS")
}

if (!requireNamespace("testthat", quietly = TRUE)) {
  install.packages("testthat")
}

if (!requireNamespace("shinytest", quietly = TRUE)) {
  install.packages("shinytest")
}

suppressPackageStartupMessages({
library("shiny")
library("testthat")
library("shinytest")
})

test_that("Application works", {
  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  expect_pass(testApp(".", compareImages = FALSE))
})
