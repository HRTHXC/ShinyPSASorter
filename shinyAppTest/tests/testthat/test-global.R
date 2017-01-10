context("Test global.R functions")


test_that("Test global.R", {
  ## Load functions
  setwd("../..")
  source("global.R")

  ## Test x()
  expect_equal(x("foo"), "foo")
  expect_error(x(), 'argument "x" is missing, with no default')

})
