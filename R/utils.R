# Small internal utility helpers shared across the package.

# Null-coalescing operator. Returns `y` when `x` is `NULL` (or length 0).
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

# Integer ceiling division, matching Python's `-(-a // b)` idiom.
edgar_ceil_div <- function(a, b) {
  as.integer(-(-a %/% b))
}

# Bit length of a non-negative integer (matches Python's `int.bit_length()`).
edgar_bit_length <- function(n) {
  if (n <= 0L) return(0L)
  bl <- 0L
  while (n > 0) {
    bl <- bl + 1L
    n <- n %/% 2L
  }
  bl
}

# Format a POSIXct timestamp the way the upstream Python CSV exporter
# does: `"%Y-%m-%d %H:%M:%S"`.
edgar_format_timestamp <- function(t = Sys.time()) {
  format(t, "%Y-%m-%d %H:%M:%S")
}

# Coerce a list of flat named lists (one per row) into a data.frame
# while preserving column order, types, and empty-cell handling. Used by
# the design builders.
edgar_rows_to_dataframe <- function(rows) {
  if (length(rows) == 0L) {
    return(data.frame(row_num = integer(0), stringsAsFactors = FALSE))
  }
  # The set of column names is the keys of the first row plus any
  # columns that appear only in later rows, preserving first-seen order.
  headers <- character()
  seen <- character()
  for (r in rows) {
    new_cols <- setdiff(names(r), seen)
    if (length(new_cols)) {
      headers <- c(headers, new_cols)
      seen <- c(seen, new_cols)
    }
  }
  cols <- vector("list", length(headers))
  names(cols) <- headers
  for (h in headers) {
    vals <- lapply(rows, function(r) {
      v <- r[[h]]
      if (is.null(v)) "" else v
    })
    # If every value is integer, return integer; if all numeric, numeric;
    # otherwise character. The upstream Python code only ever emits ints
    # and strings, but we keep the typing flexible.
    any_int <- all(vapply(vals, function(v) is.integer(v) || (is.numeric(v) && v == round(v) && !is.na(v)), logical(1)))
    if (any_int) {
      cols[[h]] <- as.integer(unlist(vals))
    } else {
      cols[[h]] <- as.character(unlist(vals))
    }
  }
  do.call(data.frame, c(cols, list(stringsAsFactors = FALSE, check.names = FALSE)))
}
