## 2025-02-18 - [Hoisting Services map and data manipulation]
**Learning:** Hoisting expensive operations (like map rendering, which takes 10-20s) from an inner locality loop to an outer HSCP loop provides a massive performance boost (minutes saved per HSCP). Correct environment management (sourcing before `loop_env` snapshot) is critical.

**Action:** Always look for static or partnership-level assets generated in locality loops. Source preparation scripts before `loop_env` is captured to ensure they persist. Explicitly clean up these objects after the inner loop finishes to avoid memory leaks.
