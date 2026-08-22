# Latin Square design.
#
# Native R port of the upstream Python `latin_square.py`. Supports
# multiple squares when rows and columns are larger than the treatment
# count (so the design becomes several replicate Latin squares).

# Generate a single randomised Latin square of size n x n.
# The four-step algorithm matches the upstream VBA Latin.xls:
# (1) build a cyclic Latin square,
# (2) randomise the treatment-to-symbol mapping,
# (3) randomise row order,
# (4) randomise column order.
# Returns a character matrix of shape n x n.
edgar_generate_latin_square <- function(rng, treatment_names, size) {
  n <- size
  # Cyclic square: square[r, c] = (c + r) mod n
  square <- matrix(0L, nrow = n, ncol = n)
  for (r in seq_len(n)) {
    for (c in seq_len(n)) {
      square[r, c] <- ((c - 1L) + (r - 1L)) %% n
    }
  }

  # Randomise treatment-to-number mapping
  perm <- seeded_shuffle(rng, seq_len(n) - 1L)  # 0-indexed like Python
  for (r in seq_len(n)) {
    for (c in seq_len(n)) {
      # 1-based access: perm[square[r, c] + 1L] because perm is 1-based
      # in R but square stores 0-based indices.
      square[r, c] <- perm[square[r, c] + 1L]
    }
  }

  # Randomise row order
  row_order <- seeded_shuffle(rng, seq_len(n) - 1L)
  square <- square[row_order + 1L, , drop = FALSE]

  # Randomise column order
  col_order <- seeded_shuffle(rng, seq_len(n) - 1L)
  square <- square[, col_order + 1L, drop = FALSE]

  # Map indices to treatment names. square holds 0-based indices into
  # treatment_names.
  name_square <- matrix("", nrow = n, ncol = n)
  for (r in seq_len(n)) {
    for (c in seq_len(n)) {
      name_square[r, c] <- treatment_names[square[r, c] + 1L]
    }
  }
  name_square
}

validate_latin <- function(treatment_count = 3L, row_count = 3L,
                           column_count = 3L, ...) {
  validate_treatment_count(treatment_count)
  validate_latin_dimensions(treatment_count, row_count, column_count)
  total <- row_count * column_count
  validate_total_units(total)
  list(valid = TRUE, warnings = character())
}

#' Generate a `latin` design.
#' @param experiment_name Optional experiment name.
#' @param treatment_factor Treatment column header (default "Variety").
#' @param treatment_count Number of treatments (>= 2).
#' @param row_factor Row header (default "Row").
#' @param row_count Number of rows. Must be a multiple of `treatment_count`.
#' @param column_factor Column header (default "Column").
#' @param column_count Number of columns. Must be a multiple of `treatment_count`.
#' @param replicate_factor Square header (default "Square").
#' @param treatment_names Optional character vector of treatment names.
#' @param seed Integer seed.
#' @return An `edgar_design` S3 object.
#' @export
design_latin <- function(experiment_name = "",
                         treatment_factor = "Variety",
                         treatment_count = 3L,
                         row_factor = "Row",
                         row_count = 3L,
                         column_factor = "Column",
                         column_count = 3L,
                         replicate_factor = "Square",
                         treatment_names = NULL,
                         seed = 0L) {
  validate_treatment_count(treatment_count)
  validate_latin_dimensions(treatment_count, row_count, column_count)
  names_ <- validate_treatment_names(treatment_names, treatment_count)
  n_squares <- as.integer((row_count * column_count) /
                          (treatment_count * treatment_count))
  total <- row_count * column_count
  validate_total_units(total)

  rng <- make_rng(seed)
  squares <- vector("list", n_squares)
  for (i in seq_len(n_squares)) {
    squares[[i]] <- edgar_generate_latin_square(rng, names_, treatment_count)
  }

  rows <- vector("list", total)
  unit <- 1L
  for (sq_idx in seq_len(n_squares)) {
    sq <- squares[[sq_idx]]
    for (r in seq_len(treatment_count)) {
      for (c in seq_len(treatment_count)) {
        rows[[unit]] <- stats::setNames(
          list(unit, sq_idx, c, r, sq[r, c]),
          c("Unit", replicate_factor, column_factor, row_factor, treatment_factor)
        )
        unit <- unit + 1L
      }
    }
  }

  # Layout: one section per square, label "Square {sq}".
  sections <- vector("list", n_squares)
  headers <- vector("list", n_squares)
  labels <- character(n_squares)
  for (sq_idx in seq_len(n_squares)) {
    sq <- squares[[sq_idx]]
    n <- treatment_count
    header <- c(row_factor, paste(column_factor, seq_len(n)))
    section <- list(header)
    for (r in seq_len(n)) {
      row_data <- as.character(r)
      for (c in seq_len(n)) {
        row_data <- c(row_data, sq[r, c])
      }
      section <- c(section, list(row_data))
    }
    sections[[sq_idx]] <- section
    headers[[sq_idx]] <- header
    labels[sq_idx] <- paste(replicate_factor, sq_idx)
  }

  new_edgar_design(
    design_name = "Latin squares",
    parameters = list(
      experiment_name = experiment_name,
      treatment_factor = treatment_factor,
      treatment_count = treatment_count,
      row_factor = row_factor,
      row_count = row_count,
      column_factor = column_factor,
      column_count = column_count,
      replicate_factor = replicate_factor,
      n_squares = n_squares
    ),
    seed = seed,
    rows = rows,
    layout = sections,
    layout_headers = headers,
    layout_section_labels = labels,
    warnings = character()
  )
}

edgar_register_latin <- function() {
  register_design(
    key = "latin",
    name = "Latin Square",
    description = paste0(
      "A Latin square design where each treatment appears once in ",
      "each row and once in each column. Multiple squares are ",
      "generated when rows and columns are larger multiples of the ",
      "treatment count."
    ),
    has_layout = TRUE,
    has_blocks = FALSE,
    default_params = list(
      treatment_count = 3L,
      row_count = 3L,
      column_count = 3L,
      row_factor = "Row",
      column_factor = "Column",
      replicate_factor = "Square",
      treatment_factor = "Variety",
      experiment_name = ""
    ),
    param_specs = list(
      list(name = "treatment_count", type = "integer", min = 2),
      list(name = "row_count", type = "integer"),
      list(name = "column_count", type = "integer"),
      list(name = "row_factor", type = "character"),
      list(name = "column_factor", type = "character"),
      list(name = "replicate_factor", type = "character"),
      list(name = "treatment_factor", type = "character"),
      list(name = "treatment_names", type = "character", nullable = TRUE),
      list(name = "experiment_name", type = "character"),
      list(name = "seed", type = "integer", default = 0L)
    ),
    func = design_latin,
    validate = validate_latin
  )
}
