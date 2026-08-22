# High-level wrappers around the native py_random Mersenne Twister.
#
# These mirror the public API of the upstream Python `randomisation.py`
# module: `make_rng(seed)`, `seeded_shuffle(rng, items)`,
# `seeded_sample(rng, items, k)`, `seeded_randint(rng, a, b)`.
#
# Every call to `make_rng()` creates a fresh, isolated RNG that never
# touches `.Random.seed`, so the user's global R RNG state is preserved
# across design generation. This is required by the R port specification:
# a call such as `generate_design("rcb", ..., seed = 42)` must not change
# the random numbers produced by unrelated user code afterwards.

#' Create a fresh seeded RNG.
#'
#' @param seed Integer seed. Negative seeds behave as in CPython (the
#'   absolute value is used internally).
#' @return An environment usable with `seeded_shuffle()`,
#'   `seeded_sample()`, `seeded_randint()`.
#' @export
make_rng <- function(seed = 0L) {
  edgar_py_random(seed)
}

#' Return a new vector with `items` shuffled by `rng`.
#'
#' The input is not modified. Mirrors CPython's `random.shuffle()` so
#' the same seed produces the same permutation as the upstream Python
#' implementation.
#'
#' @param rng An RNG returned by `make_rng()`.
#' @param items An atomic vector (character, integer, numeric, logical).
#' @return A new vector of the same type and length, shuffled.
#' @export
seeded_shuffle <- function(rng, items) {
  edgar_py_shuffle(rng, items)
}

#' Return k random items from `items`, without replacement.
#'
#' @param rng An RNG returned by `make_rng()`.
#' @param items An atomic vector.
#' @param k Number of items to draw.
#' @return A new vector of length k.
#' @export
seeded_sample <- function(rng, items, k) {
  edgar_py_sample(rng, items, k)
}

#' Return a random integer between a and b inclusive.
#'
#' @param rng An RNG returned by `make_rng()`.
#' @param a Lower bound (integer).
#' @param b Upper bound (integer).
#' @return An integer.
#' @export
seeded_randint <- function(rng, a, b) {
  edgar_py_randint(rng, a, b)
}

#' Run a block of code with an isolated seeded RNG, restoring the
#' caller's `.Random.seed` afterwards. This is the safety net for any
#' internal code that calls base R `sample()` or `runif()` directly; the
#' `make_rng()` / `seeded_shuffle()` path does not touch the global
#' state, so this wrapper is provided for completeness and for users
#' who want to call base R sampling under a known seed.
#'
#' @param seed Integer seed.
#' @param expr R expression to evaluate.
#' @return The value of `expr`.
#' @export
with_edgar_seed <- function(seed, expr) {
  old_kind <- RNGkind("Mersenne-Twister")
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (exists(".Random.seed", envir = .GlobalEnv)) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else {
      suppressWarnings(rm(".Random.seed", envir = .GlobalEnv))
    }
  }, add = TRUE)
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  set.seed(seed, kind = "Mersenne-Twister")
  expr
}
