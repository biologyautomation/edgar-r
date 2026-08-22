# Randomised Complete Block design.
#
# Native R port of the upstream Python `randomised_complete_block.py`.

validate_rcb <- function(treatment_count = 2L, block_count = 2L, ...) {
  validate_treatment_count(treatment_count)
  validate_block_count(block_count)
  total <- treatment_count * block_count
  validate_total_units(total)
  list(valid = TRUE, warnings = character())
}

#' Generate an `rcb` design.
#' @inheritParams design_cr
#' @param block_factor Block column header (default "Block").
#' @param block_count Number of blocks (1..100).
#' @param seed Integer seed. Default 0L.
#' @return An `edgar_design` S3 object.
#' @examples
#' res <- design_rcb(treatment_count = 4, block_count = 3, seed = 42)
#' print(res)
#' @export
design_rcb <- function(experiment_name = "",
                       treatment_factor = "Variety",
                       treatment_count = 2L,
                       block_factor = "Block",
                       block_count = 2L,
                       unit_label = "Plot",
                       treatment_names = NULL,
                       seed = 0L) {
  validate_treatment_count(treatment_count)
  validate_block_count(block_count)
  total <- treatment_count * block_count
  validate_total_units(total)
  names_ <- validate_treatment_names(treatment_names, treatment_count)

  rng <- make_rng(seed)
  rows <- vector("list", total)
  unit <- 1L
  for (block in seq_len(block_count)) {
    block_treatments <- seeded_shuffle(rng, names_)
    for (plot in seq_along(block_treatments)) {
      rows[[unit]] <- stats::setNames(
        list(unit, block, plot, block_treatments[plot]),
        c("Unit", block_factor, unit_label, treatment_factor)
      )
      unit <- unit + 1L
    }
  }

  layout <- edgar_build_rcb_layout(rows, block_count, treatment_count,
                                  block_factor, unit_label, treatment_factor)

  new_edgar_design(
    design_name = "Randomised complete block design",
    parameters = list(
      experiment_name = experiment_name,
      treatment_factor = treatment_factor,
      treatment_count = treatment_count,
      block_factor = block_factor,
      block_count = block_count,
      unit_label = unit_label
    ),
    seed = seed,
    rows = rows,
    layout = layout$layout,
    layout_headers = layout$headers,
    layout_section_labels = layout$labels,
    warnings = character()
  )
}

# Build the layout view for RCB designs: a single section whose header
# row is `[Plot, Block 1, Block 2, ..., Block n]` and whose data rows
# are plot positions 1..treatment_count with the per-block treatment at
# that plot position.
edgar_build_rcb_layout <- function(rows, block_count, treatment_count,
                                   block_factor, unit_label, treatment_factor) {
  blocks_data <- vector("list", block_count)
  for (row in rows) {
    blk <- row[[block_factor]]
    if (is.null(blocks_data[[blk]])) blocks_data[[blk]] <- list()
    blocks_data[[blk]] <- c(blocks_data[[blk]], list(row))
  }

  header <- c(unit_label, paste(block_factor, seq_len(block_count)))
  section <- list(header)
  for (p in seq_len(treatment_count)) {
    row_data <- as.character(p)
    for (b in seq_len(block_count)) {
      blk_rows <- blocks_data[[b]] %||% list()
      if (p <= length(blk_rows)) {
        row_data <- c(row_data, blk_rows[[p]][[treatment_factor]])
      } else {
        row_data <- c(row_data, "")
      }
    }
    section <- c(section, list(row_data))
  }

  list(
    layout = list(section),
    headers = list(header),
    labels = "Design layout"
  )
}

edgar_register_rcb <- function() {
  register_design(
    key = "rcb",
    name = "Randomised Complete Block",
    description = paste0(
      "A randomised complete block design where each block contains ",
      "one replicate of every treatment, randomised within the block."
    ),
    has_layout = TRUE,
    has_blocks = TRUE,
    default_params = list(
      treatment_count = 2L,
      block_count = 2L,
      unit_label = "Plot",
      treatment_factor = "Variety",
      block_factor = "Block",
      experiment_name = ""
    ),
    param_specs = list(
      list(name = "treatment_count", type = "integer", min = 2, max = 500),
      list(name = "block_count", type = "integer", min = 1, max = 100),
      list(name = "unit_label", type = "character"),
      list(name = "treatment_factor", type = "character"),
      list(name = "block_factor", type = "character"),
      list(name = "treatment_names", type = "character", nullable = TRUE),
      list(name = "experiment_name", type = "character"),
      list(name = "seed", type = "integer", default = 0L)
    ),
    func = design_rcb,
    validate = validate_rcb
  )
}
