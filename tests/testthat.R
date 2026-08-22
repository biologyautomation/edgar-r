# Tests runner. R CMD check invokes this file when running the test
# suite.
suppressMessages({
  library(testthat)
  library(ExperimentalDesignGeneratorandRandomiser)
})
test_check("ExperimentalDesignGeneratorandRandomiser")
