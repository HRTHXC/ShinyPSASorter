#!/bin/env Rscript

suppressPackageStartupMessages(library(testthat))


## function to run testthat tests on code in a 'tests/testthat' directory
test_code <- function(path, package = NULL, filter = NULL, reporter="summary") {

  if(!is.null(package)) {
    for(i in package) {
      suppressPackageStartupMessages(require(i, character.only = TRUE))
    }
  }

  test_path <- "testthat"
  if(!file_test("-d", test_path)) {
    stop("No tests found for ", path, call. = FALSE)
  }

  reporter <- testthat:::find_reporter(reporter)
  env <- testthat:::test_pkg_env(test_path)
  testthat:::with_top_env(env, {
    df <- test_dir(test_path, reporter = reporter, env = env,
                   filter = filter)
  })
  if (!is.null(reporter$failed)) {
    stop("Test failures", call. = FALSE)
  }
  invisible(df)
}


packages <- c("RCurl")

## test_dir could be an option here
test_code(".", package=packages)
