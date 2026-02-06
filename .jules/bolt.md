## 2024-05-14 - [Hoisting Services Chapter Calculations]
**Learning:** Significant performance gains in R reporting pipelines can be achieved by "hoisting" HSCP-level calculations (data loading, map rendering) out of the locality-level loop. This reduces redundant work and disk I/O.
**Action:** When refactoring, split scripts into HSCP-level (sourced once per partnership) and Locality-level (sourced once per locality) logic. Ensure partnership-level objects are preserved for the inner loop by creating them before `loop_env` is defined, and clean them up at the end of the outer loop.
