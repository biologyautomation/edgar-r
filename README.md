# ExperimentalDesignGeneratorandRandomiser

Native R implementation of EDGAR, the Experimental Design Generator and Randomiser. EDGAR produces deterministic, reproducible randomisations for nine standard agricultural and biological experimental designs. The package does not require Python, reticulate, or any external service at runtime.

# Lineage

EDGAR was originally developed as a suite of Excel workbooks by the Biometrics team at Rothamsted Research. The original project is at edgarweb.org.uk.

The same algorithms were re-implemented in the open-source Python project `rotsl/edgar`, distributed as `edgar-design` on PyPI. The Python implementation preserves the historical algorithms while adding deterministic reproducibility, web access, modern export capabilities, and comprehensive test coverage.

This R package is a native R port of that Python implementation. It is not a reticulate wrapper, not a Python subprocess wrapper, and not a REST client. The algorithms are reimplemented in R.

```text
Original EDGAR Excel workbooks
    v
Biometrics team at Rothamsted Research
    v
Modern Python implementation: rotsl/edgar / edgar-design
    v
Native R port: biologyautomation/edgar-r
```

# Supported designs

The package implements the nine designs supported by the upstream Python implementation:

Key               Design
`cr_eq`           Completely randomised, equal replication
`cr_uneq`         Completely randomised, unequal replication
`rcb`             Randomised complete block
`rcb_uneq`        Randomised complete block, unequal replication
`two_factor_rcb`  Two-factor randomised complete block
`latin`           Latin square (single or multiple squares)
`split_plot`      Split plot
`variable_blocks` Variable block sizes
`alpha`           Alpha design (resolvable incomplete block, Patterson and Williams 1976)

# Installation

Once the package is available on CRAN, install it with:

```r
install.packages("ExperimentalDesignGeneratorandRandomiser")
```

For development, install from a local checkout using `R CMD INSTALL edgar-r` or `devtools::install_local("edgar-r")`.

# Quick start

```r
library(ExperimentalDesignGeneratorandRandomiser)

# List all available designs
list_designs()

# Generate a randomised complete block design
res <- generate_design("rcb", treatment_count = 8, block_count = 4, seed = 42)
print(res)
as.data.frame(res)

# Generate an alpha design (resolvable incomplete block)
res <- design_alpha(
  treatment_count = 24,
  reps = 2,
  blocks_per_replicate = 6,
  seed = 100
)

# Propose viable alpha structures for a given treatment count
propose_alpha_structures(24)
```

# Reproducibility

The package ports CPython's Mersenne Twister seeding algorithm and Fisher-Yates shuffle to native R. The same integer seed produces the same design in R and in the upstream Python `edgar-design` package. This was verified by direct comparison of generated designs against the Python reference implementation for all nine design types.

Generating a design does not modify the global `.Random.seed`, so unrelated user code that calls `sample()` or `runif()` is not affected. This is verified by an automated test in the package test suite.

# Output handling

Every design returns an `edgar_design` S3 object with the following accessible fields:

- `design_name`: character design name
- `parameters`: named list of input parameters
- `seed`: integer seed used
- `rows`: an ordinary base R `data.frame` (always available)
- `layout`: nested layout view, or NULL
- `layout_headers`: per-section column headers, or NULL
- `layout_section_labels`: section labels, or NULL
- `warnings`: character vector of soft warnings
- `generated_at`: POSIXct timestamp

Use `as.data.frame(res)` to obtain an ordinary `data.frame`. No tidyverse package is required to represent results.

# Export

CSV export has no extra dependencies and matches the upstream Python format (metadata header block, then column headers and data):

```r
write_edgar_csv(res, file = "design.csv")
```

JSON export requires `jsonlite` (in Suggests):

```r
write_edgar_json(res, file = "design.json")
```

XLSX export requires `openxlsx` (in Suggests):

```r
write_edgar_xlsx(res, file = "design.xlsx")
```

# Provenance and credits

The original EDGAR was developed by the Biometrics team at Rothamsted Research as Excel workbooks, hosted at http://www.edgarweb.org.uk/.

The modern Python implementation is `rotsl/edgar`, documented at https://rotsl.github.io/edgar/ and distributed as the `edgar-design` package on PyPI at https://pypi.org/project/edgar-design/.

This repository is a native R port of that Python implementation. It is not the historical Rothamsted implementation, not the Python implementation, and not affiliated beyond the algorithmic lineage above.

# Alpha design citation

Alpha designs follow the methodology described by:

Patterson, H.D. & Williams, E.R. (1976). A new class of resolvable incomplete block designs. *Biometrika*, 63(1), 83-92. doi:10.1093/biomet/63.1.83

Patterson and Williams are credited for the statistical design methodology. They did not write this software.

# License

MIT. Copyright (c) 2026 Rohan R. See `LICENSE` and `LICENSE.note` for details.

# Source and issues

Source: https://github.com/biologyautomation/edgar-r
Issues: https://github.com/biologyautomation/edgar-r/issues
