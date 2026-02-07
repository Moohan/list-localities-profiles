## 2026-02-07 - [Hoisting Services Chapter]
**Learning:** Refactoring a large script into HSCP-level logic (sourced once per partnership in an outer loop) and locality-level logic (sourced once per locality in an inner loop) significantly reduces redundant computation and file I/O.
**Action:** Apply this pattern ('hoisting') to any chapter script that mixes partnership and locality-level processing.
## 2026-02-07 - [False Positives in Code Review]
**Learning:** Code reviews can sometimes identify functional regressions or redundant code that are actually correct based on a deeper understanding of the codebase (e.g., misidentifying used variables or missing existing loop cleanups).
**Action:** Always double-check the code and search the repo to verify review feedback before blindly applying changes or reverting optimizations.
