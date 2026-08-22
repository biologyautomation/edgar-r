# Native R re-implementation of CPython's `random.Random(seed)`.
#
# Implements the Mersenne Twister (MT19937) with CPython's specific
# `init_by_array` seeding algorithm, so that an integer seed produces
# the *same* stream of pseudo-random numbers as Python's
# `random.Random(seed)`. This is what lets the R port produce designs
# that are byte-identical to the upstream Python `edgar-design`
# package for the same seed.
#
# The audit of CPython's `Modules/_randommodule.c` and `Lib/random.py`
# is summarised in the file `edgar_upstream_audit.md` at the repository
# root. The key non-obvious requirement is that the int seed must be
# decomposed into one or more 32-bit little-endian words before being
# mixed into the MT state via `init_by_array`.
#
# Arithmetic note: R doubles can represent every integer in [0, 2^53]
# exactly, but the MT recurrence multiplies two 32-bit quantities
# (product up to 2^64). To stay exact we implement 32-bit modular
# multiplication via 16-bit high/low splitting (see `mul32`).

# Mersenne Twister constants (matches CPython's _randommodule.c)
.EDGAR_MT_N <- 624L
.EDGAR_MT_M <- 397L
.EDGAR_MT_MATRIX_A <- 2567483615   # 0x9908b0df
.EDGAR_MT_UPPER_MASK <- 2147483648 # 0x80000000
.EDGAR_MT_LOWER_MASK <- 2147483647 # 0x7fffffff
.EDGAR_MT_TEMP_B <- 2636928640     # 0x9d2c5680
.EDGAR_MT_TEMP_C <- 4022730752    # 0xefc60000

# ---- uint32 helpers (operate on doubles in [0, 2^32)) ----------------------

edgar_mask32 <- function(x) {
  x <- x %% 4294967296
  ifelse(x < 0, x + 4294967296, x)
}

edgar_xor32 <- function(a, b) {
  a <- edgar_mask32(a); b <- edgar_mask32(b)
  ah <- a %/% 65536; al <- a %% 65536
  bh <- b %/% 65536; bl <- b %% 65536
  edgar_mask32(bitwXor(ah, bh) * 65536 + bitwXor(al, bl))
}

edgar_and32 <- function(a, b) {
  a <- edgar_mask32(a); b <- edgar_mask32(b)
  ah <- a %/% 65536; bh <- b %/% 65536
  al <- a %% 65536; bl <- b %% 65536
  edgar_mask32(bitwAnd(ah, bh) * 65536 + bitwAnd(al, bl))
}

edgar_shiftr32 <- function(x, n) floor(edgar_mask32(x) / 2^n)
edgar_shiftl32 <- function(x, n) edgar_mask32(edgar_mask32(x) * 2^n)

# 32-bit multiplication truncated to the low 32 bits, via 16-bit splitting
# so the intermediate values stay below 2^53 and remain exactly representable
# in R doubles.
edgar_mul32 <- function(a, b) {
  a <- edgar_mask32(a); b <- edgar_mask32(b)
  ah <- a %/% 65536; al <- a %% 65536
  bh <- b %/% 65536; bl <- b %% 65536
  cross <- (ah * bl + al * bh) %% 4294967296
  low_low <- al * bl
  edgar_mask32(cross * 65536 + low_low)
}

# ---- the Mersenne Twister itself -------------------------------------------

#' Constructor: returns an environment that owns the MT state. Pass by
#' reference lets `genrand_uint32` advance the state without copying.
#'
#' @param seed Integer (or numeric coercible to one). Negative seeds are
#'   silently treated as their absolute value, matching CPython's behaviour
#'   where `random.Random(-1)` and `random.Random(1)` produce identical
#'   streams.
#' @return An environment with `$state` (numeric vector of 624 uint32s) and
#'   `$index` (1-based pointer into `$state`).
edgar_py_random <- function(seed) {
  self <- new.env(parent = emptyenv())
  self$state <- numeric(.EDGAR_MT_N)
  self$index <- 1L

  # init_genrand(19650218) - the first step of init_by_array.
  self$state[1] <- edgar_mask32(19650218)
  for (i in seq_len(.EDGAR_MT_N - 1L) + 1L) {
    prev <- self$state[i - 1L]
    combined <- edgar_xor32(prev, edgar_shiftr32(prev, 30))
    self$state[i] <- edgar_mask32(edgar_mul32(1812433253, combined) + (i - 1L))
  }

  # Decompose the seed into uint32 little-endian words, the way CPython
  # does before calling init_by_array. abs() mirrors CPython's sign
  # handling for negative ints.
  seed <- abs(as.numeric(seed)[1])
  if (is.na(seed)) {
    stop("seed must be coercible to numeric", call. = FALSE)
  }
  if (seed == 0) {
    key <- 0
  } else {
    key <- c()
    a <- seed
    while (a > 0) {
      key <- c(key, a %% 4294967296)
      a <- a %/% 4294967296
    }
  }
  key <- edgar_mask32(key)

  # init_by_array(key)
  i <- 2L
  j <- 1L
  keylen <- length(key)
  n_iter <- max(.EDGAR_MT_N, keylen)
  for (k in seq_len(n_iter)) {
    mt_im1 <- self$state[i - 1L]
    combined <- edgar_xor32(mt_im1, edgar_shiftr32(mt_im1, 30))
    self$state[i] <- edgar_mask32(
      edgar_xor32(self$state[i], edgar_mul32(combined, 1664525)) +
        key[j] + (j - 1L)
    )
    i <- i + 1L
    j <- j + 1L
    if (i > .EDGAR_MT_N) { self$state[1] <- self$state[.EDGAR_MT_N]; i <- 2L }
    if (j > keylen) j <- 1L
  }
  for (k in seq_len(.EDGAR_MT_N - 1L)) {
    mt_im1 <- self$state[i - 1L]
    combined <- edgar_xor32(mt_im1, edgar_shiftr32(mt_im1, 30))
    self$state[i] <- edgar_mask32(
      edgar_xor32(self$state[i], edgar_mul32(combined, 1566083941)) -
        (i - 1L)
    )
    i <- i + 1L
    if (i > .EDGAR_MT_N) { self$state[1] <- self$state[.EDGAR_MT_N]; i <- 2L }
  }
  self$state[1] <- .EDGAR_MT_UPPER_MASK # 0x80000000 sentinel
  self$index <- .EDGAR_MT_N + 1L        # force twist on first draw
  self
}

# Generate one 32-bit unsigned integer from `self`. Mirrors
# `genrand_uint32` in `_randommodule.c`, including the tempering step.
edgar_genrand_uint32 <- function(self) {
  if (self$index > .EDGAR_MT_N) {
    mt <- self$state
    # Twist loop 1: kk in [0, N - M)
    for (kk in seq_len(.EDGAR_MT_N - .EDGAR_MT_M)) {
      y <- edgar_and32(mt[kk], .EDGAR_MT_UPPER_MASK) +
        edgar_and32(mt[kk + 1L], .EDGAR_MT_LOWER_MASK)
      mt[kk] <- edgar_xor32(
        edgar_xor32(mt[kk + .EDGAR_MT_M], edgar_shiftr32(y, 1)),
        ifelse(edgar_and32(y, 1) == 1, .EDGAR_MT_MATRIX_A, 0)
      )
    }
    # Twist loop 2: kk in [N - M, N - 1)
    for (kk in (seq_len(.EDGAR_MT_M - 1L) + (.EDGAR_MT_N - .EDGAR_MT_M))) {
      y <- edgar_and32(mt[kk], .EDGAR_MT_UPPER_MASK) +
        edgar_and32(mt[kk + 1L], .EDGAR_MT_LOWER_MASK)
      mt[kk] <- edgar_xor32(
        edgar_xor32(mt[kk + (.EDGAR_MT_M - .EDGAR_MT_N)],
                    edgar_shiftr32(y, 1)),
        ifelse(edgar_and32(y, 1) == 1, .EDGAR_MT_MATRIX_A, 0)
      )
    }
    # Final word
    y <- edgar_and32(mt[.EDGAR_MT_N], .EDGAR_MT_UPPER_MASK) +
      edgar_and32(mt[1L], .EDGAR_MT_LOWER_MASK)
    mt[.EDGAR_MT_N] <- edgar_xor32(
      edgar_xor32(mt[.EDGAR_MT_M], edgar_shiftr32(y, 1)),
      ifelse(edgar_and32(y, 1) == 1, .EDGAR_MT_MATRIX_A, 0)
    )
    self$state <- mt
    self$index <- 1L
  }
  y <- self$state[self$index]
  self$index <- self$index + 1L

  # Tempering
  y <- edgar_xor32(y, edgar_shiftr32(y, 11))
  y <- edgar_xor32(y, edgar_and32(edgar_shiftl32(y, 7), .EDGAR_MT_TEMP_B))
  y <- edgar_xor32(y, edgar_and32(edgar_shiftl32(y, 15), .EDGAR_MT_TEMP_C))
  y <- edgar_xor32(y, edgar_shiftr32(y, 18))
  y
}

# CPython's `random.random()` - a float in [0, 1) with 53 bits of precision.
edgar_py_random_float <- function(self) {
  a <- edgar_shiftr32(edgar_genrand_uint32(self), 5) # 27 bits
  b <- edgar_shiftr32(edgar_genrand_uint32(self), 6) # 26 bits
  (a * 67108864.0 + b) * (1.0 / 9007199254740992.0)  # 2^-53
}

# CPython's `getrandbits(k)` - returns an integer in [0, 2^k).
edgar_py_getrandbits <- function(self, k) {
  if (k <= 0L) stop("k must be positive", call. = FALSE)
  words <- ((k - 1L) %/% 32L) + 1L
  result <- 0
  remaining_bits <- k
  for (i in seq_len(words)) {
    r <- edgar_genrand_uint32(self)
    if (remaining_bits < 32L) {
      r <- edgar_shiftr32(r, 32 - remaining_bits)
    }
    result <- result * 4294967296 + r
    result <- result %% (2 ^ k)
    remaining_bits <- remaining_bits - 32L
  }
  result
}

# CPython's `_randbelow(n)` - rejection sampling on `getrandbits(k)`
# where `k = n.bit_length()`. Used by `shuffle` and `randint`.
edgar_py_randbelow <- function(self, n) {
  if (n <= 0L) stop("n must be positive", call. = FALSE)
  k <- 0L
  tmp <- n
  while (tmp > 0) {
    k <- k + 1L
    tmp <- tmp %/% 2L
  }
  repeat {
    r <- edgar_py_getrandbits(self, k)
    if (r < n) return(r)
  }
}

# CPython's `random.shuffle(x)` - in-place Fisher-Yates with `_randbelow`.
edgar_py_shuffle <- function(self, x) {
  if (length(x) <= 1L) return(x)
  for (i in seq(length(x), 2L, by = -1L)) {
    j <- edgar_py_randbelow(self, i) + 1L
    tmp <- x[i]
    x[i] <- x[j]
    x[j] <- tmp
  }
  x
}

# CPython's `random.randint(a, b)` - integer between a and b inclusive.
edgar_py_randint <- function(self, a, b) {
  a + edgar_py_randbelow(self, b - a + 1L)
}

# CPython's `random.sample(population, k)` - k distinct items in random order.
# Implements the partial Fisher-Yates that CPython uses for small k.
edgar_py_sample <- function(self, population, k) {
  n <- length(population)
  if (k < 0L || k > n) stop("sample size out of range", call. = FALSE)
  if (k == 0L) return(population[0L])  # length-0 vector of correct type
  # Use partial Fisher-Yates: shuffle the first k indices.
  pool <- population
  for (i in seq_len(k)) {
    j <- edgar_py_randbelow(self, n - i + 1L) + i
    tmp <- pool[i]
    pool[i] <- pool[j]
    pool[j] <- tmp
  }
  pool[seq_len(k)]
}
