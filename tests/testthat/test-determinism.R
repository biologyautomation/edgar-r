# Determinism and reproducibility tests.
#
# Verifies that the same seed produces the same design, that different
# seeds produce different designs, and that calling a design generator
# does not pollute the global R RNG state.

test_that("same seed produces identical design", {
  res1 <- design_rcb(treatment_count = 4, block_count = 3, seed = 42)
  res2 <- design_rcb(treatment_count = 4, block_count = 3, seed = 42)
  expect_equal(as.data.frame(res1), as.data.frame(res2))
})

test_that("different seeds produce different designs (with high probability)", {
  res1 <- design_rcb(treatment_count = 4, block_count = 3, seed = 42)
  res2 <- design_rcb(treatment_count = 4, block_count = 3, seed = 43)
  expect_false(identical(as.data.frame(res1)$Variety,
                         as.data.frame(res2)$Variety))
})

test_that("seed=0 is the default and reproducible", {
  res1 <- design_rcb(treatment_count = 4, block_count = 3)
  res2 <- design_rcb(treatment_count = 4, block_count = 3)
  expect_equal(as.data.frame(res1), as.data.frame(res2))
})

test_that("generating a design does not change the global RNG state", {
  seed_before <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else NULL
  # Generate three designs in a row
  res1 <- design_rcb(treatment_count = 4, block_count = 3, seed = 100)
  res2 <- design_alpha(treatment_count = 24, reps = 2, seed = 200,
                       blocks_per_replicate = 6)
  res3 <- design_variable_blocks(treatment_count = 4, replicates = 3,
                                 block_count = 5, seed = 300)
  seed_after <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else NULL
  expect_equal(seed_before, seed_after)
  # And runif continues unaffected
  expect_equal(length(runif(5)), 5)
})

test_that("alpha structural proposal helper returns valid structures", {
  proposals <- propose_alpha_structures(24)
  expect_true(is.data.frame(proposals))
  expect_true(nrow(proposals) > 0)
  # All proposals must satisfy min <= 24 <= max
  for (i in seq_len(nrow(proposals))) {
    expect_true(proposals$min_treatments[i] <= 24)
    expect_true(24 <= proposals$max_treatments[i])
  }
  # s in 5..15
  expect_true(all(proposals$s >= 5))
  expect_true(all(proposals$s <= 15))
  # k = ceil(24 / s)
  for (i in seq_len(nrow(proposals))) {
    s <- proposals$s[i]
    expected_k <- as.integer(-(-24 %/% s))
    expect_equal(proposals$k[i], expected_k)
  }
})

test_that("alpha structural proposal rejects too-few treatments", {
  expect_equal(nrow(propose_alpha_structures(10)), 0)
  expect_equal(nrow(propose_alpha_structures(5)), 0)
})

test_that("alpha structural proposal handles high treatment counts", {
  # 100 treatments: should have at least one proposal
  proposals <- propose_alpha_structures(100)
  expect_true(nrow(proposals) >= 1)
  # 101 treatments: no proposals (out of range)
  expect_equal(nrow(propose_alpha_structures(101)), 0)
})

test_that("choose_design is an alias for propose_alpha_structures", {
  expect_equal(choose_design(24), propose_alpha_structures(24))
})
