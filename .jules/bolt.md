# Bolt's Journal - Services Hoisting Optimization

## 2025-02-10 - Hoisting Services Map and Data Manipulation
**Learning:** Identifying HSCP-level constants in a nested locality loop significantly reduces redundant I/O and expensive spatial rendering. Map rendering with ggmap/ggplot2 is a major bottleneck when repeated unnecessarily.
**Action:** Always check loop structures for operations that are constant relative to the inner loop index and move them to the outer scope (hoisting).

## 2025-02-10 - Safe Cleanup of Hoisted Objects
**Learning:** Hoisted objects that are constant at the partnership level but needed across localities must be explicitly cleaned up after the inner loop finishes to prevent memory accumulation in long-running builds. Using `intersect(c(...), ls())` with `rm()` is a safe pattern for this.
**Action:** When hoisting, also hoist the corresponding cleanup logic to the end of the outer loop.
