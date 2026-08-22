# Cross-language parity tests for the nine EDGAR designs.
#
# For each design, we generate a design with the same parameters and
# seed=42 as the upstream Python implementation, and verify the R
# output is byte-identical to the Python output. This is possible
# because the R port reimplements CPython's Mersenne Twister seeding
# and Fisher-Yates shuffle, so the same seed produces the same RNG
# stream.

# Helper: load golden Python output from inst/extdata or tests/testthat
load_golden <- function(key) {
  path <- testthat::test_path("golden_designs.json")
  if (!file.exists(path)) testthat::skip("golden_designs.json not available")
  all_golden <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
  all_golden[[key]]
}

# Helper: compare an R data frame against a golden list-of-rows
compare_to_golden <- function(df, golden_rows, columns) {
  expect_equal(names(df), columns)
  expect_equal(nrow(df), length(golden_rows))
  for (i in seq_len(nrow(df))) {
    row <- df[i, , drop = TRUE]
    golden <- golden_rows[[i]]
    for (col in columns) {
      # The R data frame may store integers as integers or doubles.
      # Compare by character to avoid type coercion issues.
      actual <- as.character(row[[col]])
      expected <- as.character(golden[[col]])
      expect_equal(actual, expected,
                   info = sprintf("Row %d, column '%s': R=%s, Python=%s",
                                  i, col, actual, expected))
    }
  }
}

test_that("cr_eq byte-identical to Python for seed=42", {
  skip_if_not_installed("jsonlite")
  golden <- load_golden("cr_eq")
  res <- design_cr(treatment_count = 4, reps_per_treatment = 3, seed = 42)
  expect_equal(res$design_name, golden$design_name)
  expect_equal(total_units(res), golden$total_units)
  compare_to_golden(as.data.frame(res), golden$rows, golden$columns)
})

test_that("cr_uneq byte-identical to Python for seed=42", {
  skip_if_not_installed("jsonlite")
  golden <- load_golden("cr_uneq")
  res <- design_cr_unequal(treatment_count = 3,
                           per_treatment_reps = c(2L, 3L, 1L),
                           seed = 42)
  expect_equal(res$design_name, golden$design_name)
  expect_equal(total_units(res), golden$total_units)
  compare_to_golden(as.data.frame(res), golden$rows, golden$columns)
})

test_that("rcb byte-identical to Python for seed=42", {
  skip_if_not_installed("jsonlite")
  golden <- load_golden("rcb")
  res <- design_rcb(treatment_count = 4, block_count = 3, seed = 42)
  expect_equal(res$design_name, golden$design_name)
  expect_equal(total_units(res), golden$total_units)
  compare_to_golden(as.data.frame(res), golden$rows, golden$columns)
})

test_that("rcb_uneq byte-identical to Python for seed=42", {
  skip_if_not_installed("jsonlite")
  golden <- load_golden("rcb_uneq")
  res <- design_rcb_unequal(treatment_count = 3, block_count = 2,
                            reps_per_treatment_per_block = c(1L, 2L, 1L),
                            seed = 42)
  expect_equal(res$design_name, golden$design_name)
  expect_equal(total_units(res), golden$total_units)
  compare_to_golden(as.data.frame(res), golden$rows, golden$columns)
})

test_that("two_factor_rcb byte-identical to Python for seed=42", {
  skip_if_not_installed("jsonlite")
  golden <- load_golden("two_factor_rcb")
  res <- design_two_factor_rcb(factor_a_count = 2, factor_b_count = 3,
                               block_count = 2, seed = 42)
  expect_equal(res$design_name, golden$design_name)
  expect_equal(total_units(res), golden$total_units)
  compare_to_golden(as.data.frame(res), golden$rows, golden$columns)
})

test_that("latin byte-identical to Python for seed=42 (multi-square)", {
  skip_if_not_installed("jsonlite")
  golden <- load_golden("latin")
  res <- design_latin(treatment_count = 3, row_count = 6, column_count = 6,
                      seed = 42)
  expect_equal(res$design_name, golden$design_name)
  expect_equal(total_units(res), golden$total_units)
  compare_to_golden(as.data.frame(res), golden$rows, golden$columns)
})

test_that("split_plot byte-identical to Python for seed=42", {
  skip_if_not_installed("jsonlite")
  golden <- load_golden("split_plot")
  res <- design_split_plot(block_count = 2, main_treatment_count = 2,
                           sub_treatment_count = 3, seed = 42)
  expect_equal(res$design_name, golden$design_name)
  expect_equal(total_units(res), golden$total_units)
  compare_to_golden(as.data.frame(res), golden$rows, golden$columns)
})

test_that("variable_blocks byte-identical to Python for seed=42", {
  skip_if_not_installed("jsonlite")
  golden <- load_golden("variable_blocks")
  res <- design_variable_blocks(treatment_count = 4, replicates = 3,
                                block_count = 5, seed = 42)
  expect_equal(res$design_name, golden$design_name)
  expect_equal(total_units(res), golden$total_units)
  compare_to_golden(as.data.frame(res), golden$rows, golden$columns)
})

test_that("alpha byte-identical to Python for seed=42 (default structure)", {
  skip_if_not_installed("jsonlite")
  golden <- load_golden("alpha")
  res <- design_alpha(treatment_count = 20, reps = 4,
                      blocks_per_replicate = 5, seed = 42)
  expect_equal(res$design_name, golden$design_name)
  expect_equal(total_units(res), golden$total_units)
  compare_to_golden(as.data.frame(res), golden$rows, golden$columns)
})

test_that("alpha with repeated controls byte-identical to Python for seed=42", {
  skip_if_not_installed("jsonlite")
  golden <- load_golden("alpha_controls")
  res <- design_alpha(treatment_count = 24, reps = 3,
                      blocks_per_replicate = 6,
                      repeated_controls = 2, seed = 42)
  expect_equal(res$design_name, golden$design_name)
  expect_equal(total_units(res), golden$total_units)
  compare_to_golden(as.data.frame(res), golden$rows, golden$columns)
})
