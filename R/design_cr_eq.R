# Completely Randomised design with equal replication per treatment.
#
# Native R port of the upstream Python `completely_randomised_equal.py`.
# Produces a flat randomised layout where every treatment appears
# exactly `reps_per_treatment` times, with no block factor.

# Validate `cr_eq` parameters.
validate_cr_eq <- function(treatment_count = 2L, reps_per_treatment = 2L, ...) {
  validate_treatment_count(treatment_count)
  validate_reps_per_treatment(reps_per_treatment)
  total <- treatment_count * reps_per_treatment
  validate_total_units(total)
  list(valid = TRUE, warnings = character())
}

#' Generate a `cr_eq` design.
#' @param experiment_name Optional experiment name (string).
#' @param treatment_factor Treatment column header (default "Variety").
#' @param treatment_count Number of treatments (2..500).
#' @param reps_per_treatment Replicates per treatment (1..100).
#' @param unit_label Unit column header (default "Plot").
#' @param treatment_names Optional character vector of treatment names.
#'   Defaults to `as.character(1:treatment_count)`; shorter vectors are
#'   padded with sequential numbers, matching the upstream behaviour.
#' @param seed Integer seed. Default 0L.
#' @return An `edgar_design` S3 object.
#' @examples
#' res <- design_cr(treatment_count = 4, reps_per_treatment = 2, seed = 42)
#' print(res)
#' @export
design_cr <- function(experiment_name = "",
                      treatment_factor = "Variety",
                      treatment_count = 2L,
                      reps_per_treatment = 2L,
                      unit_label = "Plot",
                      treatment_names = NULL,
                      seed = 0L) {
  # Validate
  validate_treatment_count(treatment_count)
  validate_reps_per_treatment(reps_per_treatment)
  total <- treatment_count * reps_per_treatment
  validate_total_units(total)
  names_ <- validate_treatment_names(treatment_names, treatment_count)

  # Build treatment list (unrandomised)
  treatments <- character()
  for (i in seq_len(treatment_count)) {
    treatments <- c(treatments, rep(names_[i], reps_per_treatment))
  }

  # Randomise with isolated seeded RNG
  rng <- make_rng(seed)
  randomised <- seeded_shuffle(rng, treatments)

  # Build rows
  rows <- vector("list", length(randomised))
  for (plot_num in seq_along(randomised)) {
    rows[[plot_num]] <- stats::setNames(
      list(plot_num, randomised[[plot_num]]),
      c(unit_label, treatment_factor)
    )
  }

  new_edgar_design(
    design_name = "Completely randomised design, equal replication",
    parameters = list(
      experiment_name = experiment_name,
      treatment_factor = treatment_factor,
      treatment_count = treatment_count,
      reps_per_treatment = reps_per_treatment,
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

# Self-register the design. Called from `.edgar_ensure_loaded()`.
edgar_register_cr_eq <- function() {
  register_design(
    key = "cr_eq",
    name = "Completely Randomised, Equal Replication",
    description = paste0(
      "A completely randomised design with the same number of ",
      "replicates per treatment. No blocking factor."
    ),
    has_layout = FALSE,
    has_blocks = FALSE,
    default_params = list(
      treatment_count = 2L,
      reps_per_treatment = 2L,
      unit_label = "Plot",
      treatment_factor = "Variety",
      experiment_name = ""
    ),
    param_specs = list(
      list(name = "treatment_count", type = "integer", min = 2, max = 500),
      list(name = "reps_per_treatment", type = "integer", min = 1, max = 100),
      list(name = "unit_label", type = "character"),
      list(name = "treatment_factor", type = "character"),
      list(name = "treatment_names", type = "character", nullable = TRUE),
      list(name = "experiment_name", type = "character"),
      list(name = "seed", type = "integer", default = 0L)
    ),
    func = design_cr,
    validate = validate_cr_eq
  )
}
