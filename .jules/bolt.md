## 2026-01-22 - Hoisting and Safe Cleanup in R loops
**Learning:** Hoisting partnership-level logic out of locality-level loops in R significantly improves performance (e.g., avoiding redundant `get_stadiamap()` calls). When hoisting, it is critical to use safe cleanup patterns like `rm(list = intersect(c(...), ls()))` to avoid "object not found" errors if some objects are not created (e.g., due to failed API calls or conditional logic).
**Action:** Always identify partnership-level vs locality-level responsibilities in nested reporting loops. Use `intersect()` with `ls()` for robust environment cleanup after hoisted scripts.

**Learning:** Installing R packages via `apt-get` (e.g., `r-cran-tidyverse`) is much faster and more reliable in this environment than `install.packages()` within R, which often times out for large packages.
**Action:** Prefer system package managers for common R libraries in CI/CD or sandbox environments.
