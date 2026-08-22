# Exporters.
#
# Mirror the upstream Python `exports/csv_export.py`, `json_export.py`,
# and `xlsx_export.py`. CSV export has no extra dependency. JSON export
# uses `jsonlite` (in Suggests). XLSX export uses `openxlsx` (in
# Suggests). When the optional dependency is not installed, the
# exporter raises a clear error rather than failing silently.

# Internal: build the metadata header rows shared by CSV and XLSX.
edgar_export_header <- function(result) {
  rows <- list(
    c("Edgar II", result$design_name)
  )
  exp_name <- result$parameters$experiment_name
  if (!is.null(exp_name) && nzchar(exp_name)) {
    rows <- c(rows, list(c("Experiment:", exp_name)))
  }
  rows <- c(rows, list(
    c("Designed:", edgar_format_timestamp(result$generated_at)),
    c("Seed:", as.character(result$seed))
  ))
  rows
}

#' Write a design to a CSV file.
#'
#' The CSV format matches the upstream Python exporter: a metadata
#' header block (`Edgar II`, `Experiment:`, `Designed:`, `Seed:`), a
#' blank row, the column header row, and the data rows. Uses `\r\n`
#' line terminators and `QUOTE_MINIMAL` quoting to match.
#'
#' @param result An `edgar_design` object.
#' @param file Path to write to. Use `""` or `NULL` to return the CSV
#'   as a character scalar.
#' @param include_header Whether to include the metadata header block.
#' @return The CSV string (invisibly if `file` is given).
#' @export
write_edgar_csv <- function(result, file = "", include_header = TRUE) {
  if (!inherits(result, "edgar_design")) {
    stop("result must be an edgar_design object", call. = FALSE)
  }
  lines <- character()
  if (isTRUE(include_header)) {
    header_rows <- edgar_export_header(result)
    for (r in header_rows) {
      lines <- c(lines, paste(
        vapply(r, function(x) {
          x <- as.character(x)
          # Quote if it contains comma, quote, or newline.
          if (grepl('[",\n\r]', x)) {
            x <- gsub('"', '""', x, fixed = TRUE)
            x <- paste0('"', x, '"')
          }
          x
        }, character(1)),
        collapse = ","
      ))
    }
    lines <- c(lines, "")
  }
  if (nrow(result$rows) > 0L) {
    headers <- names(result$rows)
    lines <- c(lines, paste(
      vapply(headers, function(x) {
        x <- as.character(x)
        if (grepl('[",\n\r]', x)) {
          x <- gsub('"', '""', x, fixed = TRUE)
          x <- paste0('"', x, '"')
        }
        x
      }, character(1)),
      collapse = ","
    ))
    for (i in seq_len(nrow(result$rows))) {
      row_vals <- vapply(result$rows[i, , drop = TRUE], function(x) {
        x <- if (is.null(x) || is.na(x)) "" else as.character(x)
        if (grepl('[",\n\r]', x)) {
          x <- gsub('"', '""', x, fixed = TRUE)
          x <- paste0('"', x, '"')
        }
        x
      }, character(1))
      lines <- c(lines, paste(row_vals, collapse = ","))
    }
  }
  csv <- paste(lines, collapse = "\r\n")
  csv <- paste0(csv, "\r\n")
  if (is.null(file) || !nzchar(file)) {
    return(csv)
  }
  writeLines(csv, file, useBytes = TRUE)
  invisible(csv)
}

#' Write a design to a JSON file (or return as a string).
#'
#' The JSON structure mirrors the upstream Python exporter:
#' top-level fields `design_name`, `parameters`, `seed`,
#' `generated_at` (ISO 8601), `total_units`, `rows`, `warnings`,
#' and (if the design has a layout) `layout`, `layout_headers`,
#' `layout_section_labels`.
#'
#' Requires the `jsonlite` package (in Suggests).
#' @param result An `edgar_design` object.
#' @param file Path to write to. Use `""` or `NULL` to return the JSON
#'   string.
#' @param pretty Pretty-print with 2-space indent.
#' @return The JSON string (invisibly if `file` is given).
#' @export
write_edgar_json <- function(result, file = "", pretty = TRUE) {
  if (!inherits(result, "edgar_design")) {
    stop("result must be an edgar_design object", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package is required for JSON export. Install with install.packages('jsonlite').",
         call. = FALSE)
  }
  data <- list(
    design_name = result$design_name,
    parameters = result$parameters,
    seed = result$seed,
    generated_at = format(result$generated_at, "%Y-%m-%dT%H:%M:%S"),
    total_units = nrow(result$rows),
    rows = unname(lapply(seq_len(nrow(result$rows)), function(i) {
      as.list(result$rows[i, , drop = TRUE])
    })),
    warnings = as.list(result$warnings)
  )
  if (has_layout(result)) {
    data$layout <- result$layout
    data$layout_headers <- result$layout_headers
    data$layout_section_labels <- result$layout_section_labels
  }
  json <- jsonlite::toJSON(data, auto_unbox = TRUE,
                           pretty = pretty, na = "null")
  if (is.null(file) || !nzchar(file)) {
    return(json)
  }
  writeLines(json, file, useBytes = TRUE)
  invisible(json)
}

#' Write a design to an XLSX file.
#'
#' Requires the `openxlsx` package (in Suggests). The workbook contains
#' a `Design, list` sheet with metadata and the row view, optionally a
#' `Design, layout` sheet with the layout sections, and a `Parameters`
#' sheet with the design parameters.
#' @param result An `edgar_design` object.
#' @param file Path to write to.
#' @return Invisibly `file`.
#' @export
write_edgar_xlsx <- function(result, file) {
  if (!inherits(result, "edgar_design")) {
    stop("result must be an edgar_design object", call. = FALSE)
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("openxlsx package is required for XLSX export. Install with install.packages('openxlsx').",
         call. = FALSE)
  }
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Design, list")
  # Header rows
  header_rows <- edgar_export_header(result)
  for (i in seq_along(header_rows)) {
    openxlsx::writeData(wb, "Design, list", t(as.data.frame(header_rows[[i]])),
                       startRow = i, colNames = FALSE)
  }
  # Row data (header + rows)
  if (nrow(result$rows) > 0L) {
    openxlsx::writeData(wb, "Design, list", result$rows,
                       startRow = length(header_rows) + 2L, colNames = TRUE)
  }
  if (has_layout(result)) {
    openxlsx::addWorksheet(wb, "Design, layout")
    row_cursor <- 1L
    for (i in seq_along(result$layout)) {
      # Section label
      label <- if (!is.null(result$layout_section_labels) &&
                   i <= length(result$layout_section_labels)) {
        result$layout_section_labels[i]
      } else {
        sprintf("Section %d", i)
      }
      openxlsx::writeData(wb, "Design, layout",
                         as.data.frame(matrix(label, nrow = 1)),
                         startRow = row_cursor, colNames = FALSE)
      row_cursor <- row_cursor + 1L
      # Section data (header + rows)
      section <- result$layout[[i]]
      section_df <- do.call(rbind, lapply(section, function(r) {
        as.character(unlist(r))
      }))
      openxlsx::writeData(wb, "Design, layout",
                         as.data.frame(section_df, stringsAsFactors = FALSE),
                         startRow = row_cursor, colNames = FALSE)
      row_cursor <- row_cursor + nrow(section_df) + 1L
    }
  }
  openxlsx::addWorksheet(wb, "Parameters")
  params <- result$parameters
  params_df <- data.frame(
    Parameter = names(params),
    Value = vapply(params, function(v) {
      if (is.null(v)) "" else paste(as.character(v), collapse = ",")
    }, character(1)),
    stringsAsFactors = FALSE
  )
  params_df <- rbind(params_df, data.frame(
    Parameter = c("Seed", "Generated"),
    Value = c(as.character(result$seed),
              edgar_format_timestamp(result$generated_at)),
    stringsAsFactors = FALSE
  ))
  openxlsx::writeData(wb, "Parameters", params_df, colNames = TRUE)
  openxlsx::setColWidths(wb, "Parameters", cols = 1:2, widths = c(25, 40))
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}
