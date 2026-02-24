## 2025-01-24 - [Hoisting Services scripts]
**Learning:** Moving expensive operations (like map rendering and data loading) from an inner locality loop to an outer HSCP loop provides a significant performance gain. It requires careful environment management (using 'loop_env') and adjusting cleanup logic to ensure objects persist across locality iterations but are cleaned up between partnerships.
**Action:** Always look for nested loop structures in R reporting pipelines where data or assets are constant for the outer grouping variable.
