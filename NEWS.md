# ExperimentalDesignGeneratorandRandomiser 0.1.0

Initial native R port of EDGAR.

- Added nine experimental designs implemented natively in R: `cr_eq`, `cr_uneq`, `rcb`, `rcb_uneq`, `two_factor_rcb`, `latin`, `split_plot`, `variable_blocks`, `alpha`.
- Added public API: `list_designs()`, `generate_design(type, ..., seed = 0L)`, `validate_design(type, ...)`, plus design-specific convenience functions (`design_cr()`, `design_rcb()`, `design_alpha()`, and so on).
- Added S3 `edgar_design` class with `print`, `as.data.frame`, `total_units`, `has_layout`, and `as_layout_frames` methods.
- Added exporters `write_edgar_csv()` (no extra dependencies), `write_edgar_json()` (requires `jsonlite`), `write_edgar_xlsx()` (requires `openxlsx`).
- Added `propose_alpha_structures()` helper for alpha design feasibility checks. `choose_design()` is provided as an alias matching the upstream Python API.
- Added pure-R reimplementation of CPython's `random.Random` Mersenne Twister seeding and Fisher-Yates shuffle, giving byte-identical cross-language reproducibility for the same integer seed.
- Added isolated RNG that does not modify the global `.Random.seed`.
- Added `testthat` test suite covering structural invariants, validation failures, determinism, and byte-identical parity against the upstream Python `edgar-design` package.
- Added two vignettes: "Introduction to EDGAR" and "Alpha designs and structure proposal".

EDGAR was originally developed by the Biometrics team at Rothamsted Research as Excel workbooks (http://www.edgarweb.org.uk/). The algorithms were subsequently re-implemented in the Python project `rotsl/edgar`, documented at https://rotsl.github.io/edgar/ and distributed as `edgar-design` on PyPI. This R package is a native R port of that Python implementation. Alpha designs follow Patterson and Williams (1976), doi:10.1093/biomet/63.1.83.
