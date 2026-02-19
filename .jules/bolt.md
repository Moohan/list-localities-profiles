# Bolt's Performance Journal

## 2025-05-15 - [Hoisting Services Chapter]
**Learning:** Map rendering in `Services/3. Service HSCP map.R` is a significant bottleneck due to external API calls and expensive plot rendering. Hoisting this and the associated data preparation (`2a`) to the outer HSCP loop provides a massive performance win (saving ~10-20 seconds per locality iteration).
**Action:** Always check if a script sourced in a loop is actually producing partnership-level or global assets, and hoist them if so.

## 2025-05-15 - [Correcting Clackmannanshire string regression]
**Learning:** HSCP names often contain special characters like `&`. When refactoring, pay close attention to string comparisons (e.g., `Clackmannanshire & Stirling`).
**Action:** Verify exact strings in the source data or original code when refactoring conditional logic.
