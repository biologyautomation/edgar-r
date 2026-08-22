#' Public dispatcher: generate a design by key.
#'
#' @param type Design key (`cr_eq`, `cr_uneq`, `rcb`, `rcb_uneq`,
#'   `two_factor_rcb`, `latin`, `split_plot`, `variable_blocks`, `alpha`).
#' @param ... Design-specific parameters. Forwarded to the design
#'   function.
#' @param seed Integer seed. Default `0L`. Same seed produces the same
#'   design, byte-identical to the upstream Python implementation.
#' @return An `edgar_design` S3 object.
#' @examples
#' res <- generate_design("rcb", treatment_count = 4, block_count = 3, seed = 42)
#' print(res)
#' @export
generate_design <- function(type, ..., seed = 0L) {
  entry <- get_design(type)
  do.call(entry$func, c(list(...), list(seed = seed)))
}

#' Public dispatcher: validate design parameters by key.
#'
#' Runs the design's validator against the supplied parameters. Returns
#' a list with `valid` (logical), `warnings` (character vector). Hard
#' validation failures raise an `edgar_validation_error` condition.
#' @param type Design key.
#' @param ... Design-specific parameters.
#' @return A list with elements `valid` (TRUE on success) and
#'   `warnings` (character vector, possibly empty).
#' @export
validate_design <- function(type, ...) {
  entry <- get_design(type)
  do.call(entry$validate, list(...))
}

#' Convenience wrapper to fetch a design's metadata (for programmatic
#' introspection by tests or downstream packages).
#' @param key Design key.
#' @return A list with `key`, `name`, `description`, `has_layout`,
#'   `has_blocks`, `default_params`, `param_specs`.
#' @export
design_info <- function(key) {
  get_design(key)$info
}
