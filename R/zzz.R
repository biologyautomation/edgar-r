#' Package-level documentation.
#'
#' EDGAR - Experimental Design Generator and Randomiser.
#'
#' A native R implementation of EDGAR. EDGAR originated as Excel
#' workbooks developed by the Biometrics team at Rothamsted Research
#' (http://www.edgarweb.org.uk/). The algorithms were subsequently
#' re-implemented in the open-source Python project `rotsl/edgar`
#' (`rotsl/edgar`; https://rotsl.github.io/edgar/), distributed as the `edgar-design`
#' package on PyPI (https://pypi.org/project/edgar-design/). This R
#' package is a native R port of that Python implementation: it does
#' not require Python, reticulate, or any external service at runtime.
#'
#' The package provides deterministic, reproducible randomisation for
#' nine experimental designs:
#' `cr_eq`, `cr_uneq`, `rcb`, `rcb_uneq`, `two_factor_rcb`,
#' `latin`, `split_plot`, `variable_blocks`, and `alpha`.
#'
#' The Mersenne Twister seeding and Fisher-Yates shuffle are ported
#' from CPython so the same integer seed produces the same design in R
#' as in the upstream Python implementation.
#'
#' Alpha designs follow the methodology described by Patterson and
#' Williams (1976).
#'
#' @references
#' Patterson, H.D. & Williams, E.R. (1976). A new class of resolvable
#' incomplete block designs. Biometrika, 63(1), 83-92.
#' doi:10.1093/biomet/63.1.83
#' @references
#' Biometrics team at Rothamsted Research, EDGAR Excel workbooks,
#' http://www.edgarweb.org.uk/
#' @references
#' rotsl/edgar Python implementation, https://rotsl.github.io/edgar/
#' @references
#' edgar-design on PyPI, https://pypi.org/project/edgar-design/
"_PACKAGE"

# The designs self-register on first use of list_designs(), get_design(),
# or generate_design(), so no .onLoad() hook is required.
