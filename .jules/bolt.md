## 2025-05-15 - [Hoisting Optimization for General Health Chapter]
**Learning:** Redundant I/O and data processing inside nested loops (Locality inside HSCP) is a primary performance bottleneck in this R-based reporting pipeline. Refactoring monolithic chapter scripts into tiered 'Global/HSCP/Locality' scripts (hoisting) significantly reduces execution time by loading and processing invariant data once.
**Action:** Always look for `source()` calls inside loops that read files or perform aggregations that are constant for the parent loop (HSCP) or the entire session. Apply the tiered loading/manipulation/output pattern.

## 2025-05-15 - [Explicit Namespacing in R]
**Learning:** Using explicit namespace prefixes (e.g., `dplyr::filter()`, `arrow::read_parquet()`) in R scripts prevents conflicts in shared environments and makes the code more robust, though it may trigger 'uninstalled package' warnings in linters if the environment is not fully populated.
**Action:** Maintain explicit namespacing but ensure syntax is verified with `parse()` to distinguish between missing environment dependencies and actual code errors.
