# Contributing

Thanks for your interest in improving EDGAR for R. This document explains how to set up a development environment and what the review process looks like.

# Development setup

The package is developed against R 3.6 or later. To work on it locally:

```bash
git clone https://github.com/biologyautomation/edgar-r.git
cd edgar-r
R CMD INSTALL .
```

For testthat tests:

```r
install.packages(c("testthat", "roxygen2", "jsonlite", "openxlsx", "knitr", "rmarkdown"))
testthat::test_dir("tests/testthat")
```

To regenerate documentation after editing roxygen comments:

```r
roxygen2::roxygenize(".")
```

To run a full check before opening a pull request:

```bash
R CMD build .
R CMD check --as-cran ExperimentalDesignGeneratorandRandomiser_0.1.0.tar.gz
```

# Writing rules

Before writing or modifying any Markdown file in this repository, read `SKILL_writing.md` and follow it. The skill is the source of truth for what natural prose looks like. In particular:

- No em dashes or en dashes. Use periods, commas, colons, or parentheses.
- No curly quotes. Use straight quotes.
- Sentence case in headings.
- No rule-of-three padding.
- No `-ing` tacking phrases to add fake depth.
- No signposting ("let's dive in", "here's what you need to know").
- No sycophantic tone.
- No knowledge-cutoff disclaimers or speculative gap-filling.

# Porting rules

The R package must remain a native R port. Do not add `reticulate` to Imports, Depends, or Suggests. Do not introduce a Python runtime dependency. Do not shell out to `python` or `python3` at runtime. Python may be used during development only, as a reference implementation for behavioural comparison.

# Pull request process

1. Open an issue describing the change you intend to make (unless the change is trivial).
2. Fork the repository and create a feature branch from `main`. A name like `feat/<short-description>` or `fix/<short-description>` is fine.
3. Make your changes. Add or update tests. Update documentation.
4. Run `R CMD check --as-cran` locally and resolve all ERRORs and WARNINGs. Address NOTEs where reasonable.
5. Open a pull request against `main`. Reference the issue in the description.
6. Wait for CI to pass. Respond to review comments.

# Code of conduct

By participating in this project you agree to abide by its code of conduct, which is based on the Contributor Covenant 2.1.
