# Randomised Complete Block design with unequal replication per treatment.
#
# Native R port of the upstream Python
# `randomised_complete_block_unequal.py`.

validate_rcb_uneq <- function(treatment_count = 2L, block_count = 2L,
                              reps_per_treatment_per_block = NULL, ...) {
  if (is.null(reps_per_treatment_per_block)) {
    reps_per_treatment_per_block <- rep(2L, treatment_count)
  }
  validate_treatment_count(treatment_count)
  validate_block_count(block_count)
  validate_per_treatment_reps(reps_per_treatment_per_block, treatment_count)
  plots_per_block <- sum(reps_per_treatment_per_block)
  total <- plots_per_block * block_count
  validate_total_units(total)
  list(valid = TRUE, warnings = character())
}

#' Generate an `rcb_uneq` design.
#' @inheritParams design_rcb
#' @param reps_per_treatment_per_block Integer vector of length
#'   `treatment_count`. Each entry is the number of times the
#'   corresponding treatment appears within each block. Defaults to
#'   `rep(2, treatment_count)`.
#' @return An `edgar_design` S3 object.
#' @export
design_rcb_unequal <- function(experiment_name = "",
                               treatment_factor = "Variety",
                               treatment_count = 2L,
                               block_factor = "Block",
                               block_count = 2L,
                               unit_label = "Plot",
                               reps_per_treatment_per_block = NULL,
                               treatment_names = NULL,
                               seed = 0L) {
  if (is.null(reps_per_treatment_per_block)) {
    reps_per_treatment_per_block <- rep(2L, treatment_count)
  }
  validate_treatment_count(treatment_count)
  validate_block_count(block_count)
  validate_per_treatment_reps(reps_per_treatment_per_block, treatment_count)
  plots_per_block <- sum(reps_per_treatment_per_block)
  total <- plots_per_block * block_count
  validate_total_units(total)
  names_ <- validate_treatment_names(treatment_names, treatment_count)

  rng <- make_rng(seed)
  rows <- vector("list", total)
  unit <- 1L
  for (block in seq_len(block_count)) {
    block_treatments <- character()
    for (i in seq_len(treatment_count)) {
      block_treatments <- c(block_treatments,
                            rep(names_[i], reps_per_treatment_per_block[i]))
    }
    block_treatments <- seeded_shuffle(rng, block_treatments)
    for (plot in seq_along(block_treatments)) {
      rows[[unit]] <- stats::setNames(
        list(unit, block, plot, block_treatments[plot]),
        c("Unit", block_factor, unit_label, treatment_factor)
      )
      unit <- unit + 1L
    }
  }

  layout <- edgar_build_rcb_layout(rows, block_count, plots_per_block,
                                   block_factor, unit_label, treatment_factor)

  new_edgar_design(
    design_name = "Randomised complete block design, unequal replication",
    parameters = list(
      experiment_name = experiment_name,
      treatment_factor = treatment_factor,
      treatment_count = treatment_count,
      block_factor = block_factor,
      block_count = block_count,
      reps_per_treatment_per_block = as.integer(reps_per_treatment_per_block),
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

edgar_register_rcb_uneq <- function() {
  register_design(
    key = "rcb_uneq",
    name = "RCB, Unequal Replication",
    description = paste0(
      "A randomised complete block design where the number of ",
      "replicates per treatment within each block may differ."
    ),
    has_layout = TRUE,
    has_blocks = TRUE,
    default_params = list(
      treatment_count = 2L,
      block_count = 2L,
      reps_per_treatment_per_block = list(2L, 2L),
      unit_label = "Plot",
      treatment_factor = "Variety",
      block_factor = "Block",
      experiment_name = ""
    ),
    param_specs = list(
      list(name = "treatment_count", type = "integer", min = 2, max = 500),
      list(name = "block_count", type = "integer", min = 1, max = 100),
      list(name = "reps_per_treatment_per_block", type = "integer", nullable = TRUE),
      list(name = "unit_label", type = "character"),
      list(name = "treatment_factor", type = "character"),
      list(name = "block_factor", type = "character"),
      list(name = "treatment_names", type = "character", nullable = TRUE),
      list(name = "experiment_name", type = "character"),
      list(name = "seed", type = "integer", default = 0L)
    ),
    func = design_rcb_unequal,
    validate = validate_rcb_uneq
  )
}
