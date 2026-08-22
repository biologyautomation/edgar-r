# Test the native py_random Mersenne Twister implementation against
# CPython's `random.Random` reference output. These tests verify
# byte-identical cross-language reproducibility for the RNG primitives.
#
# The RNG helpers (`edgar_py_random`, `edgar_py_random_float`, etc.)
# are internal to the package; tests access them via `:::`.

test_that("py_random produces Python-identical floats for seeds 0..100", {
  expect_equal(ExperimentalDesignGeneratorandRandomiser:::edgar_py_random_float(
                 ExperimentalDesignGeneratorandRandomiser:::edgar_py_random(42)),
               0.6394267984578837)
  expect_equal(ExperimentalDesignGeneratorandRandomiser:::edgar_py_random_float(
                 ExperimentalDesignGeneratorandRandomiser:::edgar_py_random(0)),
               0.8444218515250481)
  expect_equal(ExperimentalDesignGeneratorandRandomiser:::edgar_py_random_float(
                 ExperimentalDesignGeneratorandRandomiser:::edgar_py_random(1)),
               0.13436424411240122)
  expect_equal(ExperimentalDesignGeneratorandRandomiser:::edgar_py_random_float(
                 ExperimentalDesignGeneratorandRandomiser:::edgar_py_random(100)),
               0.1456692551041303)
})

test_that("py_random treats negative seeds as their absolute value (CPython behaviour)", {
  expect_equal(ExperimentalDesignGeneratorandRandomiser:::edgar_py_random_float(
                 ExperimentalDesignGeneratorandRandomiser:::edgar_py_random(-1)),
               0.13436424411240122)
  expect_equal(ExperimentalDesignGeneratorandRandomiser:::edgar_py_random_float(
                 ExperimentalDesignGeneratorandRandomiser:::edgar_py_random(-42)),
               0.6394267984578837)
})

test_that("py_shuffle matches Python's random.shuffle", {
  r <- ExperimentalDesignGeneratorandRandomiser:::edgar_py_random(42)
  expect_equal(ExperimentalDesignGeneratorandRandomiser:::edgar_py_shuffle(r, 0:9),
               c(7L, 3L, 2L, 8L, 5L, 6L, 9L, 4L, 0L, 1L))
  r <- ExperimentalDesignGeneratorandRandomiser:::edgar_py_random(42)
  expect_equal(ExperimentalDesignGeneratorandRandomiser:::edgar_py_shuffle(r, 0:99), c(
    42L, 41L, 91L, 9L, 65L, 50L, 1L, 70L, 15L, 78L, 73L, 10L, 55L, 56L,
    72L, 45L, 48L, 92L, 76L, 37L, 30L, 21L, 32L, 96L, 80L, 49L, 83L,
    26L, 87L, 33L, 8L, 47L, 59L, 63L, 74L, 44L, 98L, 52L, 85L, 12L,
    36L, 23L, 39L, 40L, 18L, 66L, 61L, 60L, 7L, 34L, 99L, 46L, 2L,
    51L, 16L, 38L, 58L, 68L, 22L, 62L, 24L, 5L, 6L, 67L, 82L, 19L,
    79L, 43L, 90L, 20L, 0L, 95L, 57L, 93L, 53L, 89L, 25L, 71L, 84L,
    77L, 64L, 29L, 27L, 88L, 97L, 4L, 54L, 75L, 11L, 69L, 86L, 13L,
    17L, 28L, 31L, 35L, 94L, 3L, 14L, 81L
  ))
})

test_that("py_getrandbits matches Python for k=8", {
  r <- ExperimentalDesignGeneratorandRandomiser:::edgar_py_random(42)
  bits <- integer(10)
  for (i in 1:10) bits[i] <- ExperimentalDesignGeneratorandRandomiser:::edgar_py_getrandbits(r, 8L)
  expect_equal(bits, c(163L, 28L, 6L, 189L, 70L, 62L, 57L, 35L, 188L, 26L))
})

test_that("py_randint matches Python's random.randint", {
  r <- ExperimentalDesignGeneratorandRandomiser:::edgar_py_random(42)
  ints <- integer(20)
  for (i in 1:20) ints[i] <- ExperimentalDesignGeneratorandRandomiser:::edgar_py_randint(r, 0L, 100L)
  expect_equal(ints, c(81L, 14L, 3L, 94L, 35L, 31L, 28L, 17L, 94L, 13L,
                       86L, 94L, 69L, 11L, 75L, 54L, 4L, 3L, 11L, 27L))
})

test_that("make_rng and seeded_shuffle produce Python-identical output", {
  rng <- make_rng(42)
  expect_equal(seeded_shuffle(rng, c("a", "b", "c", "d", "e")),
               c("d", "b", "c", "e", "a"))
})

test_that("isolated RNG does not modify global .Random.seed", {
  seed_before <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else NULL
  rng <- make_rng(123)
  discard <- seeded_shuffle(rng, 1:50)
  seed_after <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else NULL
  expect_equal(seed_before, seed_after)
})
