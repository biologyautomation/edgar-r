# Two-Factor Randomised Complete Block design.
#
# Native R port of the upstream Python `two_factor_rcb.py`. All
# combinations of factor A and factor B appear once per block.

validate_two_factor_rcb <- function(factor_a_count = 2L, factor_b_count = 2L,
                                    block_count = 2L, ...) {
  edgar_validate_two_factor_rcb(factor_a_count, factor_b_count, block_count)
  list(valid = TRUE, warnings = character())
}

#' Generate a `two_factor_rcb` design.
#' @param experiment_name Optional experiment name.
#' @param factor_a Header for factor A (default "Treatment1").
#' @param factor_a_count Number of levels of factor A (2..100).
#' @param factor_b Header for factor B (default "Treatment2").
#' @param factor_b_count Number of levels of factor B (2..100).
#' @param block_factor Block column header (default "Block").
#' @param block_count Number of blocks (1..100).
#' @param unit_label Unit column header (default "Plot").
#' @param factor_a_names Optional character vector of factor A level names.
#' @param factor_b_names Optional character vector of factor B level names.
#' @param seed Integer seed.
#' @return An `edgar_design` S3 object.
#' @export
design_two_factor_rcb <- function(experiment_name = "",
                                  factor_a = "Treatment1",
                                  factor_a_count = 2L,
                                  factor_b = "Treatment2",
                                  factor_b_count = 2L,
                                  block_factor = "Block",
                                  block_count = 2L,
                                  unit_label = "Plot",
                                  factor_a_names = NULL,
                                  factor_b_names = NULL,
                                  seed = 0L) {
  edgar_validate_two_factor_rcb(factor_a_count, factor_b_count, block_count)
  a_names <- validate_treatment_names(factor_a_names, factor_a_count)
  b_names <- validate_treatment_names(factor_b_names, factor_b_count)

  # Build all treatment combinations. Outer loop = a_names, inner = b_names.
  combos <- vector("list", factor_a_count * factor_b_count)
  idx <- 1L
  for (a in a_names) {
    for (b in b_names) {
      combos[[idx]] <- c(a, b)
      idx <- idx + 1L
    }
  }

  rng <- make_rng(seed)
  total <- length(combos) * block_count
  rows <- vector("list", total)
  unit <- 1L
  for (block in seq_len(block_count)) {
    block_combos <- seeded_shuffle(rng, combos)
    for (plot in seq_along(block_combos)) {
      a_val <- block_combos[[plot]][1L]
      b_val <- block_combos[[plot]][2L]
      rows[[unit]] <- stats::setNames(
        list(unit, block, plot, a_val, b_val),
        c("Unit", block_factor, unit_label, factor_a, factor_b)
      )
      unit <- unit + 1L
    }
  }

  # Layout: one section per block. Header = [Plot, {factor_a}, {factor_b}].
  sections <- vector("list", block_count)
  headers <- vector("list", block_count)
  labels <- character(block_count)
  # Group rows by block
  blocks_data <- split(rows, vapply(rows, function(r) r[[block_factor]], integer(1)))
  for (b in seq_len(block_count)) {
    blk_rows <- blocks_data[[b]] %||% list()
    header <- c(unit_label, factor_a, factor_b)
    section <- list(header)
    for (row in blk_rows) {
      section <- c(section, list(c(row[[unit_label]], row[[factor_a]], row[[factor_b]])))
    }
    sections[[b]] <- section
    headers[[b]] <- header
    labels[b] <- paste(block_factor, b)
  }

  new_edgar_design(
    design_name = "Randomised complete block design with two factors",
    parameters = list(
      experiment_name = experiment_name,
      factor_a = factor_a,
      factor_a_count = factor_a_count,
      factor_b = factor_b,
      factor_b_count = factor_b_count,
      block_factor = block_factor,
      block_count = block_count,
      unit_label = unit_label
    ),
    seed = seed,
    rows = rows,
    layout = sections,
    layout_headers = headers,
    layout_section_labels = labels,
    warnings = character()
  )
}

edgar_register_two_factor_rcb <- function() {
  register_design(
    key = "two_factor_rcb",
    name = "Two-Factor RCB",
    description = paste0(
      "A randomised complete block design where every combination of ",
      "two treatment factors appears once per block, randomised within ",
      "each block."
    ),
    has_layout = TRUE,
    has_blocks = TRUE,
    default_params = list(
      factor_a_count = 2L,
      factor_b_count = 2L,
      block_count = 2L,
      factor_a = "Treatment1",
      factor_b = "Treatment2",
      block_factor = "Block",
      unit_label = "Plot",
      experiment_name = ""
    ),
    param_specs = list(
      list(name = "factor_a_count", type = "integer", min = 2, max = 100),
      list(name = "factor_b_count", type = "integer", min = 2, max = 100),
      list(name = "block_count", type = "integer", min = 1, max = 100),
      list(name = "factor_a", type = "character"),
      list(name = "factor_b", type = "character"),
      list(name = "block_factor", type = "character"),
      list(name = "unit_label", type = "character"),
      list(name = "factor_a_names", type = "character", nullable = TRUE),
      list(name = "factor_b_names", type = "character", nullable = TRUE),
      list(name = "experiment_name", type = "character"),
      list(name = "seed", type = "integer", default = 0L)
    ),
    func = design_two_factor_rcb,
    validate = validate_two_factor_rcb
  )
}
