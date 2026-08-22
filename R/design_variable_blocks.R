# Variable block sizes design.
#
# Native R port of the upstream Python `variable_blocks.py`. Includes
# the bipartite connectedness check and the retry loop (max 100
# attempts, sharing the same RNG instance so retry count itself
# advances the RNG stream).

validate_variable_blocks <- function(treatment_count = 2L, replicates = 2L,
                                     block_count = 2L,
                                     block_sizes = NULL,
                                     control_flags = NULL, ...) {
  validate_treatment_count(treatment_count)
  if (replicates < 1L || replicates > 100L) {
    edgar_stop(sprintf("Replicates must be between 1 and 100. Got %d.", replicates))
  }
  if (block_count < 1L || block_count > 100L) {
    edgar_stop(sprintf("Block count must be between 1 and 100. Got %d.", block_count))
  }
  # Soft warnings from validate_variable_blocks (R-version names avoids
  # clash with the public function name in validation.R)
  warnings <- edgar_validate_variable_blocks(treatment_count, replicates,
                                             block_sizes, control_flags)
  list(valid = TRUE, warnings = warnings)
}

# Check that the treatment-block graph is connected. Mirrors the BFS
# over a bipartite graph from upstream. Returns TRUE if all treatments
# and all blocks lie in a single connected component.
edgar_check_connectedness <- function(rows, treatment_factor, block_factor) {
  treat_blocks <- list()
  block_treats <- list()
  for (row in rows) {
    t <- row[[treatment_factor]]
    b <- row[[block_factor]]
    if (is.null(treat_blocks[[t]])) treat_blocks[[t]] <- integer()
    if (is.null(block_treats[[as.character(b)]])) block_treats[[as.character(b)]] <- character()
    treat_blocks[[t]] <- c(treat_blocks[[t]], b)
    block_treats[[as.character(b)]] <- c(block_treats[[as.character(b)]], t)
  }
  if (length(treat_blocks) == 0L) return(TRUE)

  all_treatments <- names(treat_blocks)
  all_blocks <- names(block_treats)
  visited_treatments <- character()
  visited_blocks <- character()
  queue <- list(list(type = "t", value = all_treatments[1L]))

  while (length(queue) > 0L) {
    item <- queue[[1L]]
    queue <- queue[-1L]
    if (item$type == "t") {
      node <- item$value
      if (!(node %in% visited_treatments)) {
        visited_treatments <- c(visited_treatments, node)
        for (blk in treat_blocks[[node]]) {
          blk_ch <- as.character(blk)
          if (!(blk_ch %in% visited_blocks)) {
            queue <- c(queue, list(list(type = "b", value = blk_ch)))
          }
        }
      }
    } else {
      node <- item$value
      if (!(node %in% visited_blocks)) {
        visited_blocks <- c(visited_blocks, node)
        for (treat in block_treats[[node]]) {
          if (!(treat %in% visited_treatments)) {
            queue <- c(queue, list(list(type = "t", value = treat)))
          }
        }
      }
    }
  }

  setequal(visited_treatments, all_treatments) &&
    setequal(visited_blocks, all_blocks)
}

# Allocate treatments to blocks. Mirrors `_allocate_treatments`.
edgar_allocate_treatments <- function(rng, treatment_names, control_flags,
                                      replicates, block_count, block_sizes,
                                      treatment_factor, block_factor,
                                      unit_label, max_retries = 100L) {
  warnings <- character()
  controls <- treatment_names[control_flags]
  non_controls <- treatment_names[!control_flags]

  for (attempt in seq_len(max_retries)) {
    remaining <- stats::setNames(as.list(rep(replicates, length(non_controls))),
                                 non_controls)
    block_allocations <- vector("list", block_count)

    for (b in seq_len(block_count)) {
      alloc <- controls
      slots_left <- block_sizes[b] - length(controls)
      available <- names(remaining)[vapply(remaining, function(x) x > 0L, logical(1))]

      if (slots_left > 0L && length(available) > 0L) {
        shuffled <- seeded_shuffle(rng, available)
        for (t in shuffled) {
          if (slots_left <= 0L) break
          if (remaining[[t]] > 0L) {
            alloc <- c(alloc, t)
            remaining[[t]] <- remaining[[t]] - 1L
            slots_left <- slots_left - 1L
          }
        }
      }
      block_allocations[[b]] <- alloc
    }

    # Distribute any remaining allocations across blocks
    for (t in names(remaining)) {
      for (i in seq_len(remaining[[t]])) {
        placed <- FALSE
        block_order <- seeded_shuffle(rng, seq_len(block_count))
        for (b in block_order) {
          if (length(block_allocations[[b]]) < block_sizes[b] &&
              !(t %in% block_allocations[[b]])) {
            block_allocations[[b]] <- c(block_allocations[[b]], t)
            placed <- TRUE
            break
          }
        }
        if (!placed) {
          # Try blocks that already have this treatment
          for (b in block_order) {
            if (length(block_allocations[[b]]) < block_sizes[b]) {
              block_allocations[[b]] <- c(block_allocations[[b]], t)
              placed <- TRUE
              break
            }
          }
        }
      }
    }

    # Randomise within each block and build rows
    rows <- list()
    unit <- 1L
    for (b in seq_len(block_count)) {
      alloc <- seeded_shuffle(rng, block_allocations[[b]])
      for (plot in seq_along(alloc)) {
        rows <- c(rows, list(stats::setNames(
          list(unit, b, plot, alloc[plot]),
          c("Unit", block_factor, unit_label, treatment_factor)
        )))
        unit <- unit + 1L
      }
    }

    # Check connectedness
    if (edgar_check_connectedness(rows, treatment_factor, block_factor)) {
      return(list(rows = rows, warnings = warnings))
    }
  }

  warnings <- c(warnings, paste0(
    "Could not generate a connected design after multiple attempts. ",
    "The design may not be analysable. Consider adjusting block sizes ",
    "or treatment counts."
  ))
  list(rows = rows, warnings = warnings)
}

#' Generate a `variable_blocks` design.
#' @param experiment_name Optional experiment name.
#' @param treatment_factor Treatment column header (default "Variety").
#' @param treatment_count Number of treatments (2..500).
#' @param replicates Number of replicates per treatment (1..100).
#' @param block_factor Block column header (default "Block").
#' @param block_count Number of blocks (1..100).
#' @param unit_label Unit column header (default "Plot").
#' @param treatment_names Optional character vector of treatment names.
#' @param control_flags Optional logical vector of length
#'   `treatment_count`. `TRUE` marks a control treatment, which is
#'   placed in every block.
#' @param block_sizes Optional integer vector of block sizes. Defaults
#'   to evenly distributing `treatment_count * replicates` across
#'   `block_count`, with any remainder distributed to the first blocks.
#' @param seed Integer seed.
#' @return An `edgar_design` S3 object.
#' @export
design_variable_blocks <- function(experiment_name = "",
                                    treatment_factor = "Variety",
                                    treatment_count = 2L,
                                    replicates = 2L,
                                    block_factor = "Block",
                                    block_count = 2L,
                                    unit_label = "Plot",
                                    treatment_names = NULL,
                                    control_flags = NULL,
                                    block_sizes = NULL,
                                    seed = 0L) {
  validate_treatment_count(treatment_count)
  if (replicates < 1L || replicates > 100L) {
    edgar_stop(sprintf("Replicates must be between 1 and 100. Got %d.", replicates))
  }
  if (block_count < 1L || block_count > 100L) {
    edgar_stop(sprintf("Block count must be between 1 and 100. Got %d.", block_count))
  }
  if (is.null(control_flags)) {
    control_flags <- rep(FALSE, treatment_count)
  }
  if (is.null(block_sizes)) {
    base_size <- (treatment_count * replicates) %/% block_count
    block_sizes <- rep(base_size, block_count)
    remainder <- (treatment_count * replicates) - sum(block_sizes)
    if (remainder > 0L) {
      for (i in seq_len(remainder)) {
        block_sizes[i] <- block_sizes[i] + 1L
      }
    }
  }
  block_sizes <- as.integer(block_sizes)
  total <- sum(block_sizes)
  validate_total_units(total)
  names_ <- validate_treatment_names(treatment_names, treatment_count)

  warnings <- edgar_validate_variable_blocks(treatment_count, replicates,
                                             block_sizes, control_flags)

  rng <- make_rng(seed)
  alloc_result <- edgar_allocate_treatments(
    rng, names_, control_flags, replicates, block_count, block_sizes,
    treatment_factor, block_factor, unit_label
  )
  rows <- alloc_result$rows
  warnings <- c(warnings, alloc_result$warnings)

  # Layout: single section, header = [Plot, Block 1, ..., Block n].
  # Rows = max(block_sizes).
  max_block_size <- max(block_sizes)
  header <- c(unit_label, paste(block_factor, seq_len(block_count)))
  section <- list(header)
  # Group rows by block
  blocks_data <- vector("list", block_count)
  for (row in rows) {
    b <- row[[block_factor]]
    if (is.null(blocks_data[[b]])) blocks_data[[b]] <- list()
    blocks_data[[b]] <- c(blocks_data[[b]], list(row))
  }
  for (p in seq_len(max_block_size)) {
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

  new_edgar_design(
    design_name = "Randomised blocks of different sizes",
    parameters = list(
      experiment_name = experiment_name,
      treatment_factor = treatment_factor,
      treatment_count = treatment_count,
      replicates = replicates,
      block_factor = block_factor,
      block_count = block_count,
      unit_label = unit_label,
      block_sizes = block_sizes,
      control_flags = control_flags
    ),
    seed = seed,
    rows = rows,
    layout = list(section),
    layout_headers = list(header),
    layout_section_labels = "Design layout",
    warnings = warnings
  )
}

edgar_register_variable_blocks <- function() {
  register_design(
    key = "variable_blocks",
    name = "Variable Block Sizes",
    description = paste0(
      "A design with variable block sizes. Controls (if any) are ",
      "placed in every block; non-control treatments are allocated to ",
      "respect replication targets and block size limits, with a ",
      "connectedness check."
    ),
    has_layout = TRUE,
    has_blocks = TRUE,
    default_params = list(
      treatment_count = 2L,
      replicates = 2L,
      block_count = 2L,
      treatment_factor = "Variety",
      block_factor = "Block",
      unit_label = "Plot",
      experiment_name = ""
    ),
    param_specs = list(
      list(name = "treatment_count", type = "integer", min = 2, max = 500),
      list(name = "replicates", type = "integer", min = 1, max = 100),
      list(name = "block_count", type = "integer", min = 1, max = 100),
      list(name = "block_sizes", type = "integer", nullable = TRUE),
      list(name = "control_flags", type = "logical", nullable = TRUE),
      list(name = "treatment_factor", type = "character"),
      list(name = "block_factor", type = "character"),
      list(name = "unit_label", type = "character"),
      list(name = "treatment_names", type = "character", nullable = TRUE),
      list(name = "experiment_name", type = "character"),
      list(name = "seed", type = "integer", default = 0L)
    ),
    func = design_variable_blocks,
    validate = validate_variable_blocks
  )
}
