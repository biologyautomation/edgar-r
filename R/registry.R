# Design registry.
#
# The registry is a singleton environment mapping the nine EDGAR design
# keys to a list with `func` (the design function), `validate` (the
# design's parameter validator), and `info` (metadata for
# `list_designs()`). Designs self-register on first load via
# `register_design()`; the lazy-load guard `_ensure_loaded()` is called
# by every public dispatcher.

# Singleton registry environment.
edgar_registry <- new.env(parent = emptyenv())
edgar_registry$designs <- list()
edgar_registry$loaded <- FALSE

#' Register a design.
#'
#' @param key Design key (e.g. `"cr_eq"`).
#' @param name Human-readable design name.
#' @param description One-paragraph description.
#' @param has_layout Logical: does the design carry a layout view?
#' @param has_blocks Logical: does the design have a block factor?
#' @param default_params Named list of default parameters.
#' @param param_specs List of parameter specifications (for documentation).
#' @param func The design-generating function.
#' @param validate The validation function for this design.
#' @return Invisible TRUE; the function is registered for side effect.
register_design <- function(key, name, description, has_layout, has_blocks,
                            default_params, param_specs, func, validate) {
  info <- list(
    key = key,
    name = name,
    description = description,
    has_layout = has_layout,
    has_blocks = has_blocks,
    default_params = default_params,
    param_specs = param_specs
  )
  edgar_registry$designs[[key]] <- list(
    func = func,
    validate = validate,
    info = info
  )
  invisible(TRUE)
}

#' List all registered designs with their metadata.
#' @return A data frame with columns `key`, `name`, `has_layout`,
#'   `has_blocks`. The full `info` lists are stored in the
#'   `attribute `"info"`.
#' @examples
#' list_designs()
#' @export
list_designs <- function() {
  .edgar_ensure_loaded()
  designs <- edgar_registry$designs
  keys <- names(designs)
  info_list <- lapply(designs, function(d) d$info)
  df <- data.frame(
    key = keys,
    name = vapply(info_list, function(i) i$name, character(1)),
    has_layout = vapply(info_list, function(i) isTRUE(i$has_layout), logical(1)),
    has_blocks = vapply(info_list, function(i) isTRUE(i$has_blocks), logical(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  attr(df, "info") <- info_list
  df
}

# Get a design entry by key.
get_design <- function(key) {
  .edgar_ensure_loaded()
  if (!(key %in% names(edgar_registry$designs))) {
    available <- paste(names(edgar_registry$designs), collapse = ", ")
    stop(sprintf("Unknown design '%s'. Available: %s", key, available),
         call. = FALSE)
  }
  edgar_registry$designs[[key]]
}

# Lazy-load guard. Triggered by `list_designs()`, `get_design()`, and
# `generate_design()`. The body imports each design module, which
# self-registers via `register_design()` at the top level.
.edgar_ensure_loaded <- function() {
  if (isTRUE(edgar_registry$loaded)) return(invisible(TRUE))
  edgar_registry$loaded <- TRUE
  # Importing the modules triggers their top-level `register_design()`
  # calls. The order does not matter; keys are unique.
  # These calls are wrapped in `requireNamespace()`-free direct
  # references because the design functions live inside this package.
  edgar_register_cr_eq()
  edgar_register_cr_uneq()
  edgar_register_rcb()
  edgar_register_rcb_uneq()
  edgar_register_two_factor_rcb()
  edgar_register_latin()
  edgar_register_split_plot()
  edgar_register_variable_blocks()
  edgar_register_alpha()
  invisible(TRUE)
}
