# Completely Randomised design with unequal replication per treatment.
#
# Native R port of the upstream Python `completely_randomised_unequal.py`.
# Each treatment can appear a different number of times.

validate_cr_uneq <- function(treatment_count = 2L,
                             per_treatment_reps = NULL, ...) {
  if (is.null(per_treatment_reps)) {
    per_treatment_reps <- rep(2L, treatment_count)
  }
  validate_treatment_count(treatment_count)
  validate_per_treatment_reps(per_treatment_reps, treatment_count)
  total <- sum(per_treatment_reps)
  validate_total_units(total)
  list(valid = TRUE, warnings = character())
}

#' Generate a `cr_uneq` design.
#' @param experiment_name Optional experiment name.
#' @param treatment_factor Treatment column header (default "Variety").
#' @param treatment_count Number of treatments (2..500).
#' @param per_treatment_reps Integer vector of length `treatment_count`
#'   with the replication count per treatment. Defaults to
#'   `rep(2, treatment_count)` (NOT a hardcoded `[2, 2]`).
#' @param unit_label Unit column header (default "Plot").
#' @param treatment_names Optional character vector of treatment names.
#' @param seed Integer seed. Default 0L.
#' @return An `edgar_design` S3 object.
#' @export
design_cr_unequal <- function(experiment_name = "",
                              treatment_factor = "Variety",
                              treatment_count = 2L,
                              per_treatment_reps = NULL,
                              unit_label = "Plot",
                              treatment_names = NULL,
                              seed = 0L) {
  if (is.null(per_treatment_reps)) {
    per_treatment_reps <- rep(2L, treatment_count)
  }
  validate_treatment_count(treatment_count)
  validate_per_treatment_reps(per_treatment_reps, treatment_count)
  total <- sum(per_treatment_reps)
  validate_total_units(total)
  names_ <- validate_treatment_names(treatment_names, treatment_count)

  # Build treatment list (unrandomised)
  treatments <- character()
  for (i in seq_len(treatment_count)) {
    treatments <- c(treatments, rep(names_[i], per_treatment_reps[i]))
  }

  rng <- make_rng(seed)
  randomised <- seeded_shuffle(rng, treatments)

  rows <- vector("list", length(randomised))
  for (plot_num in seq_along(randomised)) {
    rows[[plot_num]] <- stats::setNames(
      list(plot_num, randomised[[plot_num]]),
      c(unit_label, treatment_factor)
    )
  }

  new_edgar_design(
    design_name = "Completely randomised design, unequal replication",
    parameters = list(
      experiment_name = experiment_name,
      treatment_factor = treatment_factor,
      treatment_count = treatment_count,
      per_treatment_reps = as.integer(per_treatment_reps),
      unit_label = unit_label
    ),
    seed = seed,
    rows = rows,
    layout = NULL,
    layout_headers = NULL,
    layout_section_labels = NULL,
    warnings = character()
  )
}

edgar_register_cr_uneq <- function() {
  register_design(
    key = "cr_uneq",
    name = "Completely Randomised, Unequal Replication",
    description = paste0(
      "A completely randomised design with a different number of ",
      "replicates per treatment. No blocking factor."
    ),
    has_layout = FALSE,
    has_blocks = FALSE,
    default_params = list(
      treatment_count = 2L,
      per_treatment_reps = list(2L, 2L),
      unit_label = "Plot",
      treatment_factor = "Variety",
      experiment_name = ""
    ),
    param_specs = list(
      list(name = "treatment_count", type = "integer", min = 2, max = 500),
      list(name = "per_treatment_reps", type = "integer", nullable = TRUE),
      list(name = "unit_label", type = "character"),
      list(name = "treatment_factor", type = "character"),
      list(name = "treatment_names", type = "character", nullable = TRUE),
      list(name = "experiment_name", type = "character"),
      list(name = "seed", type = "integer", default = 0L)
    ),
    func = design_cr_unequal,
    validate = validate_cr_uneq
  )
}
