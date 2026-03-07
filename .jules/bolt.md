## 2026-03-07 - [Hoisting Services chapter logic]
**Learning:** Hoisting data loading and partnership-level manipulation out of locality-level loops significantly improves performance by reducing redundant I/O and expensive computations (like map rendering).
**Action:** Always identify operations that are constant across a grouping variable (e.g., HSCP) and move them to an outer loop or global level. Use a tiered script structure (global -> group -> item) to maintain clean separation of concerns.
