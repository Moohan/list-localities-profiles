## 2025-05-15 - Performance Hoisting in R Report Pipelines

**Learning:** Significant performance gains can be achieved by 'hoisting' redundant logic (data loading, map rendering, partnership-level filtering) out of inner locality loops in R-based report generation. However, this architectural change introduces complex memory management requirements. Explicitly cleaning up partnership-level objects at the end of an outer loop is necessary to prevent memory accumulation, but over-aggressive cleanup can accidentally delete globally-scoped datasets, causing functional regressions in multi-iteration runs.

**Action:** When refactoring R loops for performance, clearly define three tiers of logic: Global (run once), Partnership/HSCP (run once per group), and Locality (run once per item). Implement scoped cleanup at the end of each loop tier that only targets objects created within that specific scope, while preserving shared objects for subsequent iterations.
