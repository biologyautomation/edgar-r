# Test helpers shared across the test suite.
#
# Tests verify the same integer seed produces byte-identical output to
# the upstream Python implementation for representative parameters, in
# addition to structural invariants (column counts, replication
# counts, block structure, resolvability).

library(ExperimentalDesignGeneratorandRandomiser)

# Tolerance for float comparisons: 0 means exact match required for
# byte-identical cross-language parity. Use a small tolerance for
# metadata-only comparisons (e.g. timestamps) where the test cannot be
# byte-exact.
.edgar_tolerance <- 0
