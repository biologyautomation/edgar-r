# The S3 result class returned by every design generator.
#
# `edgar_design` instances are lightweight S3 objects wrapping a list
# with the same shape as the upstream Python `DesignResult` model:
# `design_name`, `parameters`, `seed`, `rows` (a base R `data.frame`),
# `layout`, `layout_headers`, `layout_section_labels`, `warnings`, and
# `generated_at`. A `print` method and an `as.data.frame` method are
# provided; users can always obtain an ordinary base R data frame.

#' Constructor: build an `edgar_design` S3 object.
#'
#' @param design_name Character design name (e.g. "Randomised complete block design").
#' @param parameters Named list of input parameters.
#' @param seed Integer seed used.
#' @param rows A data frame, or a list of named lists coercible by
#'   `edgar_rows_to_dataframe()`.
#' @param layout Nested layout view (sections x rows x cells), or NULL.
#' @param layout_headers Per-section column headers, or NULL.
#' @param layout_section_labels Section labels, or NULL.
#' @param warnings Character vector of soft warnings.
#' @param generated_at POSIXct timestamp; defaults to `Sys.time()`.
#' @return An `edgar_design` S3 object.
new_edgar_design <- function(design_name, parameters, seed, rows,
                             layout = NULL, layout_headers = NULL,
                             layout_section_labels = NULL,
                             warnings = character(),
                             generated_at = Sys.time()) {
  if (is.data.frame(rows)) {
    rows_df <- rows
  } else {
    rows_df <- edgar_rows_to_dataframe(rows)
  }
  structure(
    list(
      design_name = design_name,
      parameters = parameters,
      seed = as.integer(seed),
      rows = rows_df,
      layout = layout,
      layout_headers = layout_headers,
      layout_section_labels = layout_section_labels,
      warnings = warnings,
      generated_at = generated_at
    ),
    class = "edgar_design"
  )
}

#' `print` method for `edgar_design`. Mirrors the upstream CLI summary.
#' @param x An `edgar_design` object.
#' @param ... Unused.
#' @return Invisible x.
#' @export
print.edgar_design <- function(x, ...) {
  cat("<edgar_design>\n")
  cat(sprintf("  design_name : %s\n", x$design_name))
  cat(sprintf("  total_units : %d\n", nrow(x$rows)))
  cat(sprintf("  seed        : %d\n", x$seed))
  if (length(x$warnings) > 0L) {
    cat("  warnings    :\n")
    for (w in x$warnings) cat("    - ", w, "\n", sep = "")
  } else {
    cat("  warnings    : (none)\n")
  }
  if (!is.null(x$layout)) cat("  layout      : present\n") else cat("  layout      : absent\n")
  cat("  rows        :\n")
  print(x$rows, row.names = FALSE, right = FALSE, ...)
  invisible(x)
}

#' `as.data.frame` method: returns the underlying `rows` data frame.
#' @param x An `edgar_design` object.
#' @param row.names See [base::as.data.frame]. Unused.
#' @param optional See [base::as.data.frame]. Unused.
#' @param ... Passed on to other methods.
#' @return The `rows` data frame.
#' @exportS3Method base::as.data.frame
as.data.frame.edgar_design <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$rows
}

#' Total number of experimental units.
#' @param x An `edgar_design` object (or any object with a `total_units` method).
#' @return An integer.
#' @export
total_units <- function(x) {
  UseMethod("total_units")
}
#' @export
total_units.edgar_design <- function(x) {
  nrow(x$rows)
}

#' Whether the design carries a layout view.
#' @param x An `edgar_design` object (or any object with a `has_layout` method).
#' @return Logical.
#' @export
has_layout <- function(x) {
  UseMethod("has_layout")
}
#' @export
has_layout.edgar_design <- function(x) {
  !is.null(x$layout) && length(x$layout) > 0L
}

#' Access the layout view as a list of data frames (one per section),
#' with section names from `layout_section_labels`. Returns NULL if the
#' design has no layout.
#' @param x An `edgar_design` object (or any object with an `as_layout_frames` method).
#' @return A list of data frames, or NULL.
#' @export
as_layout_frames <- function(x) {
  UseMethod("as_layout_frames")
}
#' @export
as_layout_frames.edgar_design <- function(x) {
  if (is.null(x$layout)) return(NULL)
  if (length(x$layout) == 0L) return(NULL)
  frames <- vector("list", length(x$layout))
  for (i in seq_along(x$layout)) {
    section <- x$layout[[i]]
    if (length(section) == 0L) {
      frames[[i]] <- data.frame()
    } else {
      # Each section is a list of rows (each a vector). The first row is
      # the header; subsequent rows are data. Coerce to data frame with
      # the header used as column names.
      header <- as.character(section[[1L]])
      data_rows <- if (length(section) > 1L) {
        do.call(rbind, lapply(section[-1L], as.character))
      } else {
        matrix(nrow = 0L, ncol = length(header))
      }
      df <- as.data.frame(data_rows, stringsAsFactors = FALSE)
      if (ncol(df) == length(header)) names(df) <- header
      frames[[i]] <- df
    }
  }
  names(frames) <- x$layout_section_labels %||% paste0("Section ", seq_along(x$layout))
  frames
}
