# Validators ported from the upstream Python `edgar_design/validation.py`.
#
# Each validator either raises an error of class `edgar_validation_error`
# or returns character(0) / a list of warnings. The error messages match
# the upstream Python messages so golden tests against the Python API
# can compare strings.

# Error class. Inheriting from `error` and `simpleError` keeps
# `tryCatch()` ergonomics familiar to R users while still being
# distinguishable via `inherits()`.
edgar_validation_error <- function(message) {
  structure(
    list(message = message, call = NULL),
    class = c("edgar_validation_error", "simpleError", "error", "condition")
  )
}

# Raise an edgar validation error. Helper used internally.
edgar_stop <- function(message) {
  cond <- edgar_validation_error(message)
  cond$call <- NULL
  stop(cond)
}

#' Validate treatment counts. Mirrors `validate_treatment_count`.
#' @param value Integer. Number of treatments.
#' @param min_val Minimum (default 2).
#' @param max_val Maximum (default 500).
#' @param context Optional context string for the error message.
#' @return Invisible TRUE; otherwise raises an `edgar_validation_error`.
validate_treatment_count <- function(value, min_val = 2L, max_val = 500L, context = "") {
  ctx <- if (nchar(context)) paste0(" (", context, ")") else ""
  if (value < min_val) {
    edgar_stop(sprintf(
      "Number of treatments must be at least %d. Got %d.%s",
      min_val, value, ctx
    ))
  }
  if (value > max_val) {
    edgar_stop(sprintf(
      "Number of treatments must be at most %d. Got %d.%s",
      max_val, value, ctx
    ))
  }
  invisible(TRUE)
}

# Validate total experimental unit count.
validate_total_units <- function(total, max_val = 5000L, context = "") {
  ctx <- if (nchar(context)) paste0(" (", context, ")") else ""
  if (total > max_val) {
    edgar_stop(sprintf(
      "Total number of experimental units must not exceed %d. Got %d.%s",
      max_val, total, ctx
    ))
  }
  if (total < 1L) {
    edgar_stop(sprintf(
      "Total number of experimental units must be at least 1. Got %d.%s",
      total, ctx
    ))
  }
  invisible(TRUE)
}

# Validate replicates-per-treatment.
validate_reps_per_treatment <- function(value, min_val = 1L, max_val = 100L) {
  if (value < min_val) {
    edgar_stop(sprintf("Replicates per treatment must be at least %d. Got %d.", min_val, value))
  }
  if (value > max_val) {
    edgar_stop(sprintf("Replicates per treatment must be at most %d. Got %d.", max_val, value))
  }
  invisible(TRUE)
}

# Validate a per-treatment replication vector. Length must match
# `treatment_count`; each value must be in [min_val, max_val].
validate_per_treatment_reps <- function(reps, treatment_count, min_val = 1L, max_val = 100L) {
  if (length(reps) != treatment_count) {
    edgar_stop(sprintf(
      "Per-treatment replicate list must have exactly %d elements. Got %d.",
      treatment_count, length(reps)
    ))
  }
  for (i in seq_along(reps)) {
    if (reps[i] < min_val) {
      edgar_stop(sprintf(
        "Replicates for treatment %d must be at least %d. Got %d.",
        i, min_val, reps[i]
      ))
    }
    if (reps[i] > max_val) {
      edgar_stop(sprintf(
        "Replicates for treatment %d must be at most %d. Got %d.",
        i, max_val, reps[i]
      ))
    }
  }
  invisible(TRUE)
}

# Validate block count.
validate_block_count <- function(value, min_val = 1L, max_val = 100L) {
  if (value < min_val) {
    edgar_stop(sprintf("Number of blocks must be at least %d. Got %d.", min_val, value))
  }
  if (value > max_val) {
    edgar_stop(sprintf("Number of blocks must be at most %d. Got %d.", max_val, value))
  }
  invisible(TRUE)
}

# Validate Latin square dimensions: rows and columns must be integer
# multiples of the treatment count.
validate_latin_dimensions <- function(treatment_count, row_count, column_count) {
  if (row_count %% treatment_count != 0) {
    edgar_stop(sprintf(
      "Number of rows (%d) must be a multiple of the number of treatments (%d).",
      row_count, treatment_count
    ))
  }
  if (column_count %% treatment_count != 0) {
    edgar_stop(sprintf(
      "Number of columns (%d) must be a multiple of the number of treatments (%d).",
      column_count, treatment_count
    ))
  }
  invisible(TRUE)
}

# Validate or default treatment names. Returns the resolved names vector.
# Padding continues from `len(result) + 1` (current length, not original
# input length), matching the upstream Python quirk where `["A"]` with
# count=3 becomes `["A", "2", "3"]`.
validate_treatment_names <- function(names, count) {
  if (is.null(names)) {
    return(as.character(seq_len(count)))
  }
  if (length(names) == 0L) {
    return(as.character(seq_len(count)))
  }
  result <- as.character(names[seq_len(min(length(names), count))])
  while (length(result) < count) {
    result <- c(result, as.character(length(result) + 1L))
  }
  result
}

# Validate the variable-blocks design. Returns a character vector of
# soft warnings rather than raising. `total_needed` here is literally
# computed as `n_controls * replicates + n_non_controls * replicates`
# (which simplifies to `treatment_count * replicates`) so the message
# numbers match the upstream Python output.
edgar_validate_variable_blocks <- function(treatment_count, replicates, block_sizes, control_flags) {
  warnings <- character()
  if (is.null(control_flags)) {
    control_flags <- rep(FALSE, treatment_count)
  }
  n_controls <- sum(control_flags)
  n_non_controls <- treatment_count - n_controls
  control_plots <- n_controls * replicates
  non_control_plots <- n_non_controls * replicates
  total_needed <- control_plots + non_control_plots
  total_available <- sum(block_sizes)
  if (total_available != total_needed) {
    warnings <- c(warnings, sprintf(
      "Total plots available (%d) does not match total plots needed (%d). Adjust block sizes or replicates.",
      total_available, total_needed
    ))
  }
  for (i in seq_along(block_sizes)) {
    if (block_sizes[i] < n_controls) {
      warnings <- c(warnings, sprintf(
        "Block %d has size %d but must accommodate %d control treatments.",
        i, block_sizes[i], n_controls
      ))
    }
  }
  warnings
}

# Validate an alpha design. Raises hard errors for the four hard
# constraints; returns soft warnings for `blocks_per_replicate` outside
# the recommended set.
edgar_validate_alpha_design <- function(treatment_count, repeated_controls, reps, blocks_per_replicate) {
  warnings <- character()
  if (treatment_count < 20L) {
    edgar_stop(sprintf("Alpha design requires at least 20 treatments. Got %d.", treatment_count))
  }
  if (treatment_count > 100L) {
    edgar_stop(sprintf("Alpha design supports at most 100 treatments. Got %d.", treatment_count))
  }
  if (reps > 4L) {
    edgar_stop(sprintf("Alpha design supports at most 4 replicates. Got %d.", reps))
  }
  if (repeated_controls > 6L) {
    edgar_stop(sprintf("Alpha design supports at most 6 repeated controls. Got %d.", repeated_controls))
  }
  valid_s <- get_valid_alpha_s(treatment_count)
  if (length(valid_s) > 0L && !(blocks_per_replicate %in% valid_s)) {
    warnings <- c(warnings, sprintf(
      "blocks_per_replicate=%d is not in the recommended set %s for %d treatments. The design may not be resolvable.",
      blocks_per_replicate,
      paste("{", paste(valid_s, collapse = ", "), "}", sep = ""),
      treatment_count
    ))
  }
  warnings
}

# Implementation that uses `max_v = s * ceil(v/s)`, mirroring the
# `get_valid_alpha_s` defined in `validation.py`.
get_valid_alpha_s <- function(treatment_count) {
  valid <- integer()
  for (s in 5:15) {
    k <- edgar_ceil_div(treatment_count, s)
    min_v <- 4 * s
    max_v <- s * k
    if (min_v <= treatment_count && treatment_count <= max_v) {
      valid <- c(valid, as.integer(s))
    }
  }
  valid
}

# Validate a split-plot design.
edgar_validate_split_plot <- function(block_count, main_treatment_count, sub_treatment_count, max_units = 5000L) {
  total <- block_count * main_treatment_count * sub_treatment_count
  validate_total_units(total, max_units, context = "Split plot design")
}

# Validate a two-factor RCB design.
edgar_validate_two_factor_rcb <- function(factor_a_count, factor_b_count, block_count, max_units = 5000L) {
  total <- factor_a_count * factor_b_count * block_count
  validate_total_units(total, max_units, context = "Two-factor RCB design")
}
