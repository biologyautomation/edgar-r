# Alpha design (resolvable incomplete block design).
#
# Native R port of the upstream Python `designs/alpha.py`. Uses the
# exact Patterson & Williams (1976) generating arrays captured in
# `ROTATION_TABLES` from the decrypted `Alpha.xls` workbook. The
# rotation tables, the Rep 1 position-major allocation, the cyclic
# interchanging for Rep >= 2, and the reversed-controls insertion are
# all preserved verbatim.
#
# Citation:
#   Patterson, H.D. & Williams, E.R. (1976). A new class of resolvable
#   incomplete block designs. Biometrika, 63(1), 83-92.
#   doi:10.1093/biomet/63.1.83

# Patterson-Williams generating arrays. Format:
#   ROTATION_TABLES[[as.character(s)]][[rep_index]][[position_index]]
#   where rep_index = 1..4 (Rep 1 = all zeros, Reps 2..4 = rotations)
#   and position_index = 1..k (Position 1 always carries rotation 0).
# s = number of blocks per replicate.
.EDGAR_ROTATION_TABLES <- list(
  "5" = list(
    c(0L, 0L, 0L, 0L, 0L),
    c(0L, 1L, 2L, 3L, 4L),
    c(0L, 4L, 3L, 2L, 1L),
    c(0L, 2L, 4L, 1L, 3L)
  ),
  "6" = list(
    c(0L, 0L, 0L, 0L, 0L, 0L),
    c(0L, 1L, 3L, 2L, 4L, 5L),
    c(0L, 5L, 2L, 3L, 1L, 1L),
    c(0L, 4L, 5L, 1L, 2L, 3L)
  ),
  "7" = list(
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L),
    c(0L, 1L, 2L, 4L, 3L, 5L, 6L),
    c(0L, 3L, 6L, 5L, 2L, 1L, 4L),
    c(0L, 2L, 4L, 1L, 6L, 3L, 5L)
  ),
  "8" = list(
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    c(0L, 1L, 3L, 5L, 2L, 4L, 6L, 7L),
    c(0L, 2L, 7L, 3L, 5L, 1L, 0L, 6L),
    c(0L, 6L, 1L, 4L, 3L, 6L, 2L, 5L)
  ),
  "9" = list(
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    c(0L, 1L, 3L, 7L, 2L, 4L, 5L, 6L, 8L),
    c(0L, 8L, 6L, 2L, 3L, 1L, 7L, 5L, 4L),
    c(0L, 7L, 4L, 3L, 5L, 6L, 2L, 1L, 7L)
  ),
  "10" = list(
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    c(0L, 1L, 3L, 5L, 4L, 6L, 7L, 8L, 9L, 2L),
    c(0L, 9L, 6L, 7L, 5L, 3L, 2L, 4L, 8L, 6L),
    c(0L, 5L, 9L, 2L, 6L, 1L, 4L, 7L, 2L, 3L)
  ),
  "11" = list(
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    c(0L, 1L, 4L, 9L, 2L, 5L, 6L, 3L, 7L),
    c(0L, 9L, 6L, 7L, 5L, 3L, 2L, 4L, 8L),
    c(0L, 5L, 9L, 2L, 6L, 1L, 4L, 7L, 2L)
  ),
  "12" = list(
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    c(0L, 1L, 7L, 9L, 4L, 11L, 10L, 5L),
    c(0L, 2L, 5L, 6L, 11L, 3L, 4L, 1L),
    c(0L, 3L, 1L, 4L, 8L, 10L, 7L, 6L)
  ),
  "13" = list(
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L),
    c(0L, 1L, 3L, 9L, 12L, 8L, 6L),
    c(0L, 4L, 8L, 2L, 10L, 5L, 7L),
    c(0L, 10L, 11L, 1L, 6L, 12L, 8L)
  ),
  "14" = list(
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L),
    c(0L, 1L, 9L, 11L, 2L, 5L, 3L),
    c(0L, 8L, 10L, 13L, 6L, 11L, 1L),
    c(0L, 10L, 7L, 2L, 1L, 12L, 11L)
  ),
  "15" = list(
    c(0L, 0L, 0L, 0L, 0L, 0L),
    c(0L, 1L, 3L, 7L, 10L, 14L),
    c(0L, 8L, 12L, 2L, 13L, 3L),
    c(0L, 7L, 14L, 5L, 11L, 8L)
  )
)

# Maximum plots-per-block k for each s, from the rotation table.
.EDGAR_MAX_K <- c(
  "5" = 5L, "6" = 6L, "7" = 7L, "8" = 8L, "9" = 9L, "10" = 10L,
  "11" = 9L, "12" = 8L, "13" = 7L, "14" = 7L, "15" = 6L
)

# Implementation in `alpha.py` that uses `max_v = s * MAX_K[s]`.
edgar_alpha_get_valid_s <- function(treatment_count) {
  valid <- integer()
  for (s in 5:15) {
    min_v <- 4 * s
    max_v <- s * .EDGAR_MAX_K[[as.character(s)]]
    if (min_v <= treatment_count && treatment_count <= max_v) {
      valid <- c(valid, as.integer(s))
    }
  }
  valid
}

# Generate the alpha layout. Returns a list with `reps` (nested list of
# per-replicate, per-block treatment vectors) and `warnings`.
edgar_generate_alpha_layout <- function(rng, treatment_names, s, k, reps,
                                        repeated_controls, control_names) {
  warnings <- character()
  v <- length(treatment_names)

  # Step 1: shuffle the treatment-to-position assignment
  shuffled_names <- seeded_shuffle(rng, treatment_names)

  # Step 2: build Rep 1 layout: layout_1[pos][block] = treatment name
  # position is 1..k, block is 1..s, 1-based in R
  layout_1 <- vector("list", k)
  for (pos in seq_len(k)) {
    row <- character(s)
    for (blk in seq_len(s)) {
      treat_idx <- (pos - 1L) * s + (blk - 1L)  # 0-indexed in upstream
      if (treat_idx < v) {
        row[blk] <- shuffled_names[treat_idx + 1L]
      } else {
        row[blk] <- ""
      }
    }
    layout_1[[pos]] <- row
  }

  # Get rotation table
  if (!(as.character(s) %in% names(.EDGAR_ROTATION_TABLES))) {
    warnings <- c(warnings, sprintf(
      "No rotation table available for s=%d. Using cyclic rotation as a fallback. The design may not be optimal.",
      s
    ))
    rotation_table <- lapply(seq_len(reps), function(r) rep(0L, s))
  } else {
    rotation_table <- .EDGAR_ROTATION_TABLES[[as.character(s)]]
  }

  # Build all replicates
  all_reps <- vector("list", reps)
  for (rep in seq_len(reps)) {
    rep_layout <- vector("list", k)

    if (rep == 1L) {
      # Rep 1: copy layout_1
      for (pos in seq_len(k)) {
        rep_layout[[pos]] <- layout_1[[pos]]
      }
    } else {
      # Rep >= 2: position 1 copied unchanged; positions 2..k rotated
      rep_layout[[1L]] <- layout_1[[1L]]
      rot_row <- if (rep <= length(rotation_table)) {
        rotation_table[[rep]]
      } else {
        rotation_table[[length(rotation_table)]]
      }
      for (pos in 2:k) {
        if (pos <= length(rot_row)) {
          rotation <- rot_row[pos]
        } else {
          rotation <- 0L
          if (pos > length(rot_row)) {
            warnings <- c(warnings, sprintf(
              "Rotation value missing for rep %d, position %d. Using 0 (no rotation).",
              rep, pos
            ))
          }
        }
        rotated_row <- character(s)
        for (blk in seq_len(s)) {
          # source_block = ((blk - 1) + rotation) mod s + 1  (1-based)
          source_block <- (((blk - 1L) + rotation) %% s) + 1L
          rotated_row[blk] <- layout_1[[pos]][source_block]
        }
        rep_layout[[pos]] <- rotated_row
      }
    }

    # Transpose layout to per-block vectors, skipping empty slots
    rep_blocks <- vector("list", s)
    for (blk in seq_len(s)) {
      block_treatments <- character()
      for (pos in seq_len(k)) {
        val <- rep_layout[[pos]][blk]
        if (val != "") {
          block_treatments <- c(block_treatments, val)
        }
      }
      rep_blocks[[blk]] <- block_treatments
    }

    # Insert repeated controls at the top of every block, in reversed
    # order. The net pre-shuffle order matches the upstream Python
    # behaviour: [c1, c2, ..., cn, ...treatments].
    if (repeated_controls > 0L && length(control_names) > 0L) {
      for (blk in seq_along(rep_blocks)) {
        for (ctrl in rev(control_names)) {
          rep_blocks[[blk]] <- c(ctrl, rep_blocks[[blk]])
        }
      }
    }

    # Shuffle within each block
    for (blk in seq_along(rep_blocks)) {
      rep_blocks[[blk]] <- seeded_shuffle(rng, rep_blocks[[blk]])
    }

    all_reps[[rep]] <- rep_blocks
  }

  list(reps = all_reps, warnings = warnings)
}

# Build list rows from the alpha layout.
edgar_alpha_build_list_rows <- function(all_reps, replicate_factor, block_factor,
                                       unit_label, treatment_factor) {
  rows <- list()
  unit <- 1L
  for (rep_idx in seq_along(all_reps)) {
    rep_blocks <- all_reps[[rep_idx]]
    for (block_idx in seq_along(rep_blocks)) {
      block_treatments <- rep_blocks[[block_idx]]
      for (plot in seq_along(block_treatments)) {
        rows <- c(rows, list(stats::setNames(
          list(unit, rep_idx, block_idx, plot, block_treatments[plot]),
          c("Unit", replicate_factor, block_factor, unit_label, treatment_factor)
        )))
        unit <- unit + 1L
      }
    }
  }
  rows
}

# Build the layout view: one section per replicate.
edgar_alpha_build_layout <- function(all_reps, s, k, replicate_factor,
                                    block_factor, unit_label,
                                    treatment_factor, repeated_controls) {
  sections <- vector("list", length(all_reps))
  headers <- vector("list", length(all_reps))
  labels <- character(length(all_reps))
  effective_k <- k + repeated_controls

  for (rep_idx in seq_along(all_reps)) {
    rep_blocks <- all_reps[[rep_idx]]
    header <- c(unit_label, paste(block_factor, seq_along(rep_blocks)))
    section <- list(header)
    for (p in seq_len(effective_k)) {
      row_data <- as.character(p)
      for (block_treatments in rep_blocks) {
        if (p <= length(block_treatments)) {
          row_data <- c(row_data, block_treatments[p])
        } else {
          row_data <- c(row_data, "")
        }
      }
      section <- c(section, list(row_data))
    }
    sections[[rep_idx]] <- section
    headers[[rep_idx]] <- header
    labels[rep_idx] <- paste(replicate_factor, rep_idx)
  }

  list(layout = sections, headers = headers, labels = labels)
}

#' Generate an `alpha` design.
#' @param experiment_name Optional experiment name.
#' @param treatment_factor Treatment column header (default "Variety").
#' @param repeated_controls Number of repeated controls per block (0..6).
#' @param treatment_count Number of treatments (20..100).
#' @param replicate_factor Replicate header (default "Rep").
#' @param reps Number of replicates (2..4).
#' @param block_factor Block header (default "Block").
#' @param blocks_per_replicate Number of blocks per replicate (5..15).
#' @param unit_label Unit column header (default "Plot").
#' @param treatment_names Optional character vector of treatment names.
#' @param control_names Optional character vector of control names. If
#'   `repeated_controls > 0` and this is NULL, defaults to
#'   `paste0("C", 1:repeated_controls)`.
#' @param seed Integer seed.
#' @return An `edgar_design` S3 object.
#' @references Patterson, H.D. & Williams, E.R. (1976). A new class of
#'   resolvable incomplete block designs. Biometrika, 63(1), 83-92.
#'   doi:10.1093/biomet/63.1.83
#' @examples
#' res <- design_alpha(treatment_count = 24, reps = 2,
#'                     blocks_per_replicate = 6, seed = 100)
#' print(res)
#' @export
design_alpha <- function(experiment_name = "",
                         treatment_factor = "Variety",
                         repeated_controls = 0L,
                         treatment_count = 20L,
                         replicate_factor = "Rep",
                         reps = 4L,
                         block_factor = "Block",
                         blocks_per_replicate = 5L,
                         unit_label = "Plot",
                         treatment_names = NULL,
                         control_names = NULL,
                         seed = 0L) {
  s <- as.integer(blocks_per_replicate)
  validation_warnings <- edgar_validate_alpha_design(
    treatment_count, repeated_controls, reps, s
  )
  names_ <- validate_treatment_names(treatment_names, treatment_count)

  k <- edgar_ceil_div(treatment_count, s)
  total_per_rep <- s * k
  total <- total_per_rep * reps + repeated_controls * s * reps
  validate_total_units(total, max_val = 5000L + repeated_controls * s * reps)

  # Resolve control names
  if (repeated_controls > 0L) {
    if (is.null(control_names)) {
      control_names <- paste0("C", seq_len(repeated_controls))
    } else {
      control_names <- as.character(control_names[seq_len(min(length(control_names), repeated_controls))])
      while (length(control_names) < repeated_controls) {
        control_names <- c(control_names, paste0("C", length(control_names) + 1L))
      }
    }
  } else {
    control_names <- character()
  }

  rng <- make_rng(seed)
  gen <- edgar_generate_alpha_layout(rng, names_, s, k, reps,
                                     repeated_controls, control_names)
  all_reps <- gen$reps
  all_warnings <- c(validation_warnings, gen$warnings)

  rows <- edgar_alpha_build_list_rows(all_reps, replicate_factor,
                                      block_factor, unit_label, treatment_factor)
  layout <- edgar_alpha_build_layout(all_reps, s, k, replicate_factor,
                                     block_factor, unit_label,
                                     treatment_factor, repeated_controls)

  new_edgar_design(
    design_name = "Alpha design",
    parameters = list(
      experiment_name = experiment_name,
      treatment_factor = treatment_factor,
      repeated_controls = repeated_controls,
      treatment_count = treatment_count,
      replicate_factor = replicate_factor,
      reps = reps,
      block_factor = block_factor,
      blocks_per_replicate = s,
      plots_per_block = k,
      unit_label = unit_label
    ),
    seed = seed,
    rows = rows,
    layout = layout$layout,
    layout_headers = layout$headers,
    layout_section_labels = layout$labels,
    warnings = all_warnings
  )
}

#' Propose viable alpha design structures for a given treatment count.
#' Mirrors the upstream Python `choose_design(treatment_count)` helper.
#'
#' Returns a data frame with columns `s`, `k`, `blocks_per_replicate`,
#' `plots_per_block`, `min_treatments`, `max_treatments` for every
#' feasible (s, k) combination, where:
#'   - `s` ranges from 5 to 15 (number of blocks per replicate)
#'   - `k` = `ceil(v / s)` (actual plots per block)
#'   - `min_treatments` = `4 * s`
#'   - `max_treatments` = `s * MAX_K[s]` from the rotation table
#'
#' @param treatment_count Number of treatments (must be >= 20 for alpha).
#' @return A data frame. Empty if no feasible structures exist.
#' @references Patterson, H.D. & Williams, E.R. (1976). Biometrika,
#'   63(1), 83-92. doi:10.1093/biomet/63.1.83
#' @export
propose_alpha_structures <- function(treatment_count) {
  options <- list()
  for (s in 5:15) {
    k <- edgar_ceil_div(treatment_count, s)
    min_v <- 4 * s
    max_v <- s * .EDGAR_MAX_K[[as.character(s)]]
    if (min_v <= treatment_count && treatment_count <= max_v) {
      options <- c(options, list(list(
        s = as.integer(s),
        k = as.integer(k),
        blocks_per_replicate = as.integer(s),
        plots_per_block = as.integer(k),
        min_treatments = as.integer(min_v),
        max_treatments = as.integer(max_v)
      )))
    }
  }
  if (length(options) == 0L) {
    return(data.frame(
      s = integer(), k = integer(),
      blocks_per_replicate = integer(),
      plots_per_block = integer(),
      min_treatments = integer(),
      max_treatments = integer()
    ))
  }
  do.call(rbind, lapply(options, function(o) {
    data.frame(
      s = o$s, k = o$k,
      blocks_per_replicate = o$blocks_per_replicate,
      plots_per_block = o$plots_per_block,
      min_treatments = o$min_treatments,
      max_treatments = o$max_treatments,
      stringsAsFactors = FALSE, row.names = NULL
    )
  }))
}

#' Alias for the upstream Python name `choose_design`.
#' @rdname propose_alpha_structures
#' @export
choose_design <- propose_alpha_structures

edgar_register_alpha <- function() {
  register_design(
    key = "alpha",
    name = "Alpha Design",
    description = paste0(
      "An alpha design, a resolvable incomplete block design ",
      "introduced by Patterson and Williams (1976). The implementation ",
      "uses the generating arrays extracted from the historical ",
      "Alpha.xls workbook and supports 5 to 15 blocks per replicate, ",
      "2 to 4 replicates, and up to 6 repeated controls."
    ),
    has_layout = TRUE,
    has_blocks = TRUE,
    default_params = list(
      treatment_count = 20L,
      reps = 4L,
      blocks_per_replicate = 5L,
      repeated_controls = 0L,
      treatment_factor = "Variety",
      replicate_factor = "Rep",
      block_factor = "Block",
      unit_label = "Plot",
      experiment_name = ""
    ),
    param_specs = list(
      list(name = "treatment_count", type = "integer", min = 20, max = 100),
      list(name = "reps", type = "integer", min = 2, max = 4),
      list(name = "blocks_per_replicate", type = "integer", min = 5, max = 15),
      list(name = "repeated_controls", type = "integer", min = 0, max = 6),
      list(name = "treatment_factor", type = "character"),
      list(name = "replicate_factor", type = "character"),
      list(name = "block_factor", type = "character"),
      list(name = "unit_label", type = "character"),
      list(name = "treatment_names", type = "character", nullable = TRUE),
      list(name = "control_names", type = "character", nullable = TRUE),
      list(name = "experiment_name", type = "character"),
      list(name = "seed", type = "integer", default = 0L)
    ),
    func = design_alpha,
    validate = function(treatment_count = 20L, repeated_controls = 0L,
                        reps = 4L, blocks_per_replicate = 5L, ...) {
      warnings <- edgar_validate_alpha_design(
        treatment_count, repeated_controls, reps, blocks_per_replicate
      )
      list(valid = TRUE, warnings = warnings)
    }
  )
}
