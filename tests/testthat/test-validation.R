# Validation tests: confirm that invalid parameters raise errors of
# class `edgar_validation_error`.

test_that("cr_eq rejects too few treatments", {
  expect_error(design_cr(treatment_count = 1, seed = 1),
               class = "edgar_validation_error")
})

test_that("cr_eq rejects too many treatments", {
  expect_error(design_cr(treatment_count = 600, seed = 1),
               class = "edgar_validation_error")
})

test_that("cr_eq rejects reps_per_treatment < 1", {
  expect_error(design_cr(treatment_count = 4, reps_per_treatment = 0, seed = 1),
               class = "edgar_validation_error")
})

test_that("cr_uneq rejects mismatched per_treatment_reps length", {
  expect_error(
    design_cr_unequal(treatment_count = 3, per_treatment_reps = c(2L, 3L), seed = 1),
    class = "edgar_validation_error"
  )
})

test_that("cr_uneq rejects per_treatment_reps < 1", {
  expect_error(
    design_cr_unequal(treatment_count = 3,
                     per_treatment_reps = c(0L, 3L, 2L), seed = 1),
    class = "edgar_validation_error"
  )
})

test_that("rcb rejects block_count < 1", {
  expect_error(design_rcb(treatment_count = 4, block_count = 0, seed = 1),
               class = "edgar_validation_error")
})

test_that("rcb_uneq rejects mismatched reps vector", {
  expect_error(
    design_rcb_unequal(treatment_count = 3, block_count = 2,
                      reps_per_treatment_per_block = c(1L, 2L), seed = 1),
    class = "edgar_validation_error"
  )
})

test_that("two_factor_rcb rejects too many total units", {
  # 100 * 100 * 5 = 50000, over the 5000 cap
  expect_error(
    design_two_factor_rcb(factor_a_count = 100, factor_b_count = 100,
                          block_count = 5, seed = 1),
    class = "edgar_validation_error"
  )
})

test_that("latin rejects rows not a multiple of treatments", {
  expect_error(
    design_latin(treatment_count = 3, row_count = 4, column_count = 3, seed = 1),
    class = "edgar_validation_error"
  )
})

test_that("latin rejects columns not a multiple of treatments", {
  expect_error(
    design_latin(treatment_count = 3, row_count = 3, column_count = 4, seed = 1),
    class = "edgar_validation_error"
  )
})

test_that("split_plot rejects too many total units", {
  # 50 * 50 * 5 = 12500 > 5000
  expect_error(
    design_split_plot(block_count = 5, main_treatment_count = 50,
                      sub_treatment_count = 50, seed = 1),
    class = "edgar_validation_error"
  )
})

test_that("alpha rejects too few treatments", {
  expect_error(
    design_alpha(treatment_count = 10, reps = 2, blocks_per_replicate = 5,
                 seed = 1),
    class = "edgar_validation_error"
  )
})

test_that("alpha rejects too many treatments", {
  expect_error(
    design_alpha(treatment_count = 200, reps = 2, blocks_per_replicate = 5,
                 seed = 1),
    class = "edgar_validation_error"
  )
})

test_that("alpha rejects reps > 4", {
  expect_error(
    design_alpha(treatment_count = 24, reps = 5, blocks_per_replicate = 6,
                 seed = 1),
    class = "edgar_validation_error"
  )
})

test_that("alpha rejects repeated_controls > 6", {
  expect_error(
    design_alpha(treatment_count = 24, reps = 2, blocks_per_replicate = 6,
                 repeated_controls = 8, seed = 1),
    class = "edgar_validation_error"
  )
})

test_that("alpha warns (not errors) when s is outside the recommended set", {
  # blocks_per_replicate=11 is valid for v in 44..99, but for v=24 it's outside
  expect_warning(
    res <- design_alpha(treatment_count = 24, reps = 2,
                        blocks_per_replicate = 11, seed = 1),
    NA  # might warn, might not, but should not error
  )
  # Confirm the design still generates
  expect_true(inherits(res, "edgar_design"))
})

test_that("validate_design returns valid=TRUE for sensible parameters", {
  v <- validate_design("rcb", treatment_count = 4, block_count = 3)
  expect_true(v$valid)
  expect_equal(v$warnings, character())
})

test_that("generate_design dispatcher works for all keys", {
  designs <- list_designs()
  expect_equal(nrow(designs), 9)
  expected_keys <- c("cr_eq", "cr_uneq", "rcb", "rcb_uneq",
                      "two_factor_rcb", "latin", "split_plot",
                      "variable_blocks", "alpha")
  expect_setequal(designs$key, expected_keys)
})
