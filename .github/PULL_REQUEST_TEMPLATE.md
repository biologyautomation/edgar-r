# Pull request summary

A short paragraph describing what this PR changes and why. Reference any issues ("Closes #123").

# Checklist

- [ ] `R CMD build .` succeeds.
- [ ] `R CMD check --as-cran` has no ERRORs or WARNINGs.
- [ ] `testthat::test_dir("tests/testthat")` passes.
- [ ] Roxygen documentation regenerated if any `@` tags were changed.
- [ ] `NEWS.md` updated if user-visible behaviour changed.
- [ ] No `reticulate`, no Python runtime dependency, no shell-out to `python`.
- [ ] Markdown files follow `SKILL_writing.md`.
- [ ] Attribution preserved (Rothamsted, rotsl/edgar, Patterson and Williams 1976).

# Cross-language parity (if relevant)

If this PR changes the RNG or design algorithms, paste the equivalent Python `edgar-design` output for the same seed and parameters and confirm the R output matches byte-identically:

```bash
edgar-design generate ... --seed ...
```
