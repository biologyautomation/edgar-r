# Structural invariant tests for each design.
#
# These complement the byte-identical parity tests by verifying
# structural properties of the designs that any correct implementation
# must satisfy: replication counts, block structure, Latin square
# orthogonality, alpha resolvability, etc.

test_that("cr_eq produces the correct number of treatments and replicates", {
  res <- design_cr(treatment_count = 5, reps_per_treatment = 3, seed = 11)
  df <- as.data.frame(res)
  expect_equal(nrow(df), 15)
  expect_equal(names(df), c("Plot", "Variety"))
  expect_equal(sort(unique(df$Variety)), as.character(1:5))
  for (t in as.character(1:5)) {
    expect_equal(sum(df$Variety == t), 3,
                 info = sprintf("Treatment %s appears 3 times", t))
  }
})

test_that("cr_uneq produces the correct per-treatment replication counts", {
  res <- design_cr_unequal(treatment_count = 4,
                           per_treatment_reps = c(2L, 3L, 1L, 4L),
                           seed = 1)
  df <- as.data.frame(res)
  expect_equal(nrow(df), 10)
  expect_equal(sum(df$Variety == "1"), 2)
  expect_equal(sum(df$Variety == "2"), 3)
  expect_equal(sum(df$Variety == "3"), 1)
  expect_equal(sum(df$Variety == "4"), 4)
})

test_that("rcb has one block per level of block_factor", {
  res <- design_rcb(treatment_count = 4, block_count = 3, seed = 5)
  df <- as.data.frame(res)
  expect_equal(nrow(df), 12)
  expect_equal(names(df), c("Unit", "Block", "Plot", "Variety"))
  expect_equal(sort(unique(df$Block)), 1:3)
  for (b in 1:3) {
    blk <- df[df$Block == b, ]
    expect_equal(nrow(blk), 4)
    expect_equal(sort(as.character(blk$Variety)), as.character(1:4))
  }
})

test_that("rcb_uneq produces correct per-block plot counts", {
  res <- design_rcb_unequal(treatment_count = 3, block_count = 2,
                            reps_per_treatment_per_block = c(1L, 2L, 1L),
                            seed = 99)
  df <- as.data.frame(res)
  expect_equal(nrow(df), 8)
  for (b in 1:2) {
    blk <- df[df$Block == b, ]
    expect_equal(nrow(blk), 4)
    expect_equal(sum(blk$Variety == "1"), 1)
    expect_equal(sum(blk$Variety == "2"), 2)
    expect_equal(sum(blk$Variety == "3"), 1)
  }
})

test_that("two_factor_rcb includes all combinations per block", {
  res <- design_two_factor_rcb(factor_a_count = 2, factor_b_count = 3,
                               block_count = 2, seed = 7)
  df <- as.data.frame(res)
  expect_equal(nrow(df), 12)
  for (b in 1:2) {
    blk <- df[df$Block == b, ]
    combos <- paste(blk$Treatment1, blk$Treatment2, sep = ",")
    expect_equal(length(combos), 6)
    expect_equal(length(unique(combos)), 6)
  }
})

test_that("latin is orthogonal: each treatment appears once per row and column", {
  res <- design_latin(treatment_count = 4, row_count = 4, column_count = 4,
                      seed = 2024)
  df <- as.data.frame(res)
  expect_equal(nrow(df), 16)
  # Each treatment appears 4 times in total
  for (t in as.character(1:4)) {
    expect_equal(sum(df$Variety == t), 4)
  }
  # Each row: each treatment appears once
  for (r in 1:4) {
    sub <- df[df$Row == r, ]
    expect_equal(sort(as.character(sub$Variety)), as.character(1:4))
  }
  # Each column: each treatment appears once
  for (c in 1:4) {
    sub <- df[df$Column == c, ]
    expect_equal(sort(as.character(sub$Variety)), as.character(1:4))
  }
})

test_that("split_plot has correct main/sub plot counts", {
  res <- design_split_plot(block_count = 3, main_treatment_count = 2,
                           sub_treatment_count = 4, seed = 13)
  df <- as.data.frame(res)
  expect_equal(nrow(df), 24)
  expect_equal(names(df), c("Unit", "Block", "Main plot", "MainTreat",
                            "Sub-plot", "SubTreat"))
  for (b in 1:3) {
    blk <- df[df$Block == b, ]
    expect_equal(nrow(blk), 8)
    expect_equal(length(unique(blk$MainTreat)), 2)
    for (mp in 1:2) {
      main <- blk[blk$`Main plot` == mp, ]
      expect_equal(length(unique(main$SubTreat)), 4)
    }
  }
})

test_that("variable_blocks honours requested block sizes", {
  res <- design_variable_blocks(treatment_count = 4, replicates = 3,
                                block_count = 5, seed = 8)
  df <- as.data.frame(res)
  # Default block_sizes = treatment_count*replicates/block_count distributed
  # 4*3=12, /5=2 (base), remainder=2, so [3,3,2,2,2]
  expect_equal(nrow(df), 12)
  block_sizes <- vapply(1:5, function(b) sum(df$Block == b), integer(1))
  expect_equal(block_sizes, c(3L, 3L, 2L, 2L, 2L))
  # Each treatment appears 3 times total
  for (t in as.character(1:4)) {
    expect_equal(sum(df$Variety == t), 3)
  }
})

test_that("variable_blocks with controls places controls in every block", {
  res <- design_variable_blocks(treatment_count = 4, replicates = 2,
                                block_count = 3,
                                control_flags = c(TRUE, FALSE, FALSE, FALSE),
                                block_sizes = c(3L, 2L, 3L),
                                seed = 42)
  df <- as.data.frame(res)
  expect_equal(nrow(df), 8)
  # Control treatment "1" must appear in every block
  for (b in 1:3) {
    blk <- df[df$Block == b, ]
    expect_true("1" %in% as.character(blk$Variety))
  }
})

test_that("alpha is resolvable: each replicate has all treatments", {
  res <- design_alpha(treatment_count = 24, reps = 2,
                      blocks_per_replicate = 6, seed = 100)
  df <- as.data.frame(res)
  expect_equal(nrow(df), 48)  # 24*2
  for (r in 1:2) {
    rep <- df[df$Rep == r, ]
    # Sort numerically, not lexically, because treatment names are
    # character strings of integers ("1".."24").
    sorted_treatments <- sort(unique(as.integer(as.character(rep$Variety))))
    expect_equal(sorted_treatments, 1:24)
  }
})

test_that("alpha produces the correct block count per replicate", {
  res <- design_alpha(treatment_count = 30, reps = 3,
                      blocks_per_replicate = 6, seed = 200)
  df <- as.data.frame(res)
  for (r in 1:3) {
    rep <- df[df$Rep == r, ]
    expect_equal(length(unique(rep$Block)), 6)
  }
})

test_that("alpha with controls places controls in every block of every rep", {
  res <- design_alpha(treatment_count = 24, reps = 2,
                      blocks_per_replicate = 6,
                      repeated_controls = 2, seed = 31)
  df <- as.data.frame(res)
  for (r in 1:2) {
    rep <- df[df$Rep == r, ]
    for (b in 1:6) {
      blk <- rep[rep$Block == b, ]
      expect_true("C1" %in% as.character(blk$Variety))
      expect_true("C2" %in% as.character(blk$Variety))
    }
  }
})
