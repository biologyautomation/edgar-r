# Split-plot design.
#
# Native R port of the upstream Python `split_plot.py`. Main treatments
# are randomised to whole plots within each block, and sub-treatments
# are randomised within each whole plot.

validate_split_plot <- function(block_count = 2L,
                                 main_treatment_count = 2L,
                                 sub_treatment_count = 2L, ...) {
  edgar_validate_split_plot(block_count, main_treatment_count, sub_treatment_count)
  list(valid = TRUE, warnings = character())
}

#' Generate a `split_plot` design.
#' @param experiment_name Optional experiment name.
#' @param block_factor Block column header (default "Block").
#' @param block_count Number of blocks (1..100).
#' @param main_unit_label Whole-plot column header (default "Main plot").
#' @param main_treatment_factor Main treatment header (default "MainTreat").
#' @param main_treatment_count Number of main treatments (2..100).
#' @param sub_unit_label Sub-plot column header (default "Sub-plot").
#' @param sub_treatment_factor Sub-treatment header (default "SubTreat").
#' @param sub_treatment_count Number of sub-treatments (2..100).
#' @param main_treatment_names Optional character vector of main treatment names.
#' @param sub_treatment_names Optional character vector of sub-treatment names.
#' @param seed Integer seed.
#' @return An `edgar_design` S3 object.
#' @export
design_split_plot <- function(experiment_name = "",
                              block_factor = "Block",
                              block_count = 2L,
                              main_unit_label = "Main plot",
                              main_treatment_factor = "MainTreat",
                              main_treatment_count = 2L,
                              sub_unit_label = "Sub-plot",
                              sub_treatment_factor = "SubTreat",
                              sub_treatment_count = 2L,
                              main_treatment_names = NULL,
                              sub_treatment_names = NULL,
                              seed = 0L) {
  edgar_validate_split_plot(block_count, main_treatment_count, sub_treatment_count)
  main_names <- validate_treatment_names(main_treatment_names, main_treatment_count)
  sub_names <- validate_treatment_names(sub_treatment_names, sub_treatment_count)

  rng <- make_rng(seed)
  total <- block_count * main_treatment_count * sub_treatment_count
  rows <- vector("list", total)
  unit <- 1L
  for (block in seq_len(block_count)) {
    block_main_treatments <- seeded_shuffle(rng, main_names)
    for (main_plot in seq_along(block_main_treatments)) {
      main_treat <- block_main_treatments[main_plot]
      sub_treatments <- seeded_shuffle(rng, sub_names)
      for (sub_plot in seq_along(sub_treatments)) {
        sub_treat <- sub_treatments[sub_plot]
        rows[[unit]] <- stats::setNames(
          list(unit, block, main_plot, main_treat, sub_plot, sub_treat),
          c("Unit", block_factor, main_unit_label, main_treatment_factor,
            sub_unit_label, sub_treatment_factor)
        )
        unit <- unit + 1L
      }
    }
  }

  # Layout: one section per block. Header = [Sub-plot, Main plot 1, ...].
  # Second row = [ "", main_treat_at_main_plot_1, ... ]. Then one row per sub-plot.
  sections <- vector("list", block_count)
  headers <- vector("list", block_count)
  labels <- character(block_count)
  blocks_data <- split(rows, vapply(rows, function(r) r[[block_factor]], integer(1)))
  for (b in seq_len(block_count)) {
    blk_rows <- blocks_data[[b]] %||% list()
    header <- c(sub_unit_label, paste(main_unit_label, seq_len(main_treatment_count)))
    section <- list(header)

    # Sub-header: main treatment assignment per main plot
    main_treats <- character(main_treatment_count)
    for (row in blk_rows) {
      mp <- row[[main_unit_label]]
      if (is.na(main_treats[mp]) || main_treats[mp] == "") {
        main_treats[mp] <- row[[main_treatment_factor]]
      }
    }
    sub_header <- c("", main_treats)
    section <- c(section, list(sub_header))

    for (sp in seq_len(sub_treatment_count)) {
      sp_row <- as.character(sp)
      for (mp in seq_len(main_treatment_count)) {
        found <- ""
        for (r in blk_rows) {
          if (identical(r[[main_unit_label]], mp) && identical(r[[sub_unit_label]], sp)) {
            found <- r[[sub_treatment_factor]]
            break
          }
        }
        sp_row <- c(sp_row, found)
      }
      section <- c(section, list(sp_row))
    }

    sections[[b]] <- section
    headers[[b]] <- header
    labels[b] <- paste(block_factor, b)
  }

  new_edgar_design(
    design_name = "Split plot design with randomised complete blocks",
    parameters = list(
      experiment_name = experiment_name,
      block_factor = block_factor,
      block_count = block_count,
      main_unit_label = main_unit_label,
      main_treatment_factor = main_treatment_factor,
      main_treatment_count = main_treatment_count,
      sub_unit_label = sub_unit_label,
      sub_treatment_factor = sub_treatment_factor,
      sub_treatment_count = sub_treatment_count
    ),
    seed = seed,
    rows = rows,
    layout = sections,
    layout_headers = headers,
    layout_section_labels = labels,
    warnings = character()
  )
}

edgar_register_split_plot <- function() {
  register_design(
    key = "split_plot",
    name = "Split Plot",
    description = paste0(
      "A split-plot design where main treatments are randomised to ",
      "whole plots within each block and sub-treatments are randomised ",
      "within each whole plot."
    ),
    has_layout = TRUE,
    has_blocks = TRUE,
    default_params = list(
      block_count = 2L,
      main_treatment_count = 2L,
      sub_treatment_count = 2L,
      block_factor = "Block",
      main_unit_label = "Main plot",
      main_treatment_factor = "MainTreat",
      sub_unit_label = "Sub-plot",
      sub_treatment_factor = "SubTreat",
      experiment_name = ""
    ),
    param_specs = list(
      list(name = "block_count", type = "integer", min = 1, max = 100),
      list(name = "main_treatment_count", type = "integer", min = 2, max = 100),
      list(name = "sub_treatment_count", type = "integer", min = 2, max = 100),
      list(name = "block_factor", type = "character"),
      list(name = "main_unit_label", type = "character"),
      list(name = "main_treatment_factor", type = "character"),
      list(name = "sub_unit_label", type = "character"),
      list(name = "sub_treatment_factor", type = "character"),
      list(name = "main_treatment_names", type = "character", nullable = TRUE),
      list(name = "sub_treatment_names", type = "character", nullable = TRUE),
      list(name = "experiment_name", type = "character"),
      list(name = "seed", type = "integer", default = 0L)
    ),
    func = design_split_plot,
    validate = validate_split_plot
  )
}
