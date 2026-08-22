# Tests for the S3 edgar_design class methods.

test_that("print.edgar_design produces expected summary", {
  res <- design_rcb(treatment_count = 3, block_count = 2, seed = 1)
  out <- capture.output(print(res))
  expect_true(any(grepl("<edgar_design>", out)))
  expect_true(any(grepl("Randomised complete block design", out)))
  expect_true(any(grepl("total_units", out)))
  expect_true(any(grepl("seed", out)))
})

test_that("as.data.frame.edgar_design returns the underlying rows", {
  res <- design_rcb(treatment_count = 3, block_count = 2, seed = 1)
  df <- as.data.frame(res)
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 6)
  expect_equal(names(df), c("Unit", "Block", "Plot", "Variety"))
})

test_that("total_units returns nrow of rows", {
  res <- design_cr(treatment_count = 5, reps_per_treatment = 2, seed = 1)
  expect_equal(total_units(res), 10)
})

test_that("has_layout returns TRUE for designs with layout", {
  res <- design_rcb(treatment_count = 3, block_count = 2, seed = 1)
  expect_true(has_layout(res))
})

test_that("has_layout returns FALSE for cr_eq", {
  res <- design_cr(treatment_count = 3, reps_per_treatment = 2, seed = 1)
  expect_false(has_layout(res))
})

test_that("as_layout_frames returns a list of data frames", {
  res <- design_rcb(treatment_count = 3, block_count = 2, seed = 1)
  frames <- as_layout_frames(res)
  expect_type(frames, "list")
  expect_equal(length(frames), 1)  # one section for RCB
  expect_equal(names(frames), "Design layout")
  expect_equal(names(frames[[1]])[1], "Plot")
})

test_that("write_edgar_csv returns CSV with metadata header", {
  res <- design_rcb(treatment_count = 3, block_count = 2, seed = 1)
  csv <- write_edgar_csv(res, file = "")
  expect_type(csv, "character")
  expect_true(grepl("Edgar II", csv, fixed = TRUE))
  expect_true(grepl("Randomised complete block design", csv, fixed = TRUE))
  expect_true(grepl("Seed:", csv, fixed = TRUE))
})

test_that("write_edgar_csv writes a file when given a path", {
  res <- design_cr(treatment_count = 3, reps_per_treatment = 2, seed = 1)
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write_edgar_csv(res, file = tmp)
  expect_true(file.exists(tmp))
  contents <- readLines(tmp)
  expect_true(grepl("Edgar II", contents[1], fixed = TRUE))
})

test_that("write_edgar_json returns valid JSON when jsonlite is available", {
  skip_if_not_installed("jsonlite")
  res <- design_rcb(treatment_count = 3, block_count = 2, seed = 1)
  json <- write_edgar_json(res, file = "")
  parsed <- jsonlite::fromJSON(json)
  expect_equal(parsed$design_name, "Randomised complete block design")
  expect_equal(parsed$total_units, 6)
  # jsonlite simplifies list-of-objects to a data frame when all rows
  # share the same keys. The number of rows equals the length of the
  # first column vector.
  expect_equal(nrow(parsed$rows), 6)
})

test_that("write_edgar_xlsx creates a workbook when openxlsx is available", {
  skip_if_not_installed("openxlsx")
  res <- design_rcb(treatment_count = 3, block_count = 2, seed = 1)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  write_edgar_xlsx(res, file = tmp)
  expect_true(file.exists(tmp))
})

test_that("new_edgar_design accepts data.frame rows directly", {
  df <- data.frame(Unit = 1:2, Variety = c("A", "B"), stringsAsFactors = FALSE)
  obj <- ExperimentalDesignGeneratorandRandomiser:::new_edgar_design(
    design_name = "Test design",
    parameters = list(),
    seed = 0L,
    rows = df
  )
  expect_s3_class(obj, "edgar_design")
  expect_equal(as.data.frame(obj), df)
})
