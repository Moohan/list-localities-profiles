## 2025-05-14 - Hoisting Services Chapter Logic
**Learning:** Hoisting expensive operations like map rendering and data manipulation from an inner locality loop to an outer HSCP loop significantly improves performance (estimated 10-20s per locality). Care must be taken with variable persistence and global object overwrites (like 'lookup') when moving scripts across loop scopes.
**Action:** Always check for global variable name collisions and use safe cleanup with intersect() when hoisting logic in R.
