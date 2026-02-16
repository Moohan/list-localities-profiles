## 2025-01-24 - Hoisting Services Chapter Logic
**Learning:** In R report generation pipelines using nested loops (HSCP > Locality), moving expensive operations like map rendering (via `ggmap`) and large RDS data loading to the outer loop provides a significant performance gain (~10-20 seconds per locality iteration). However, care must be taken to:
1. Split scripts into HSCP-level (data prep) and Locality-level (table/report logic).
2. Ensure the HSCP-level script is sourced BEFORE the `loop_env` is captured for inner-loop cleanup.
3. Remove aggressive `rm()` calls from the hoisted scripts that would delete objects needed in the inner loop.
4. Add partnership-level housekeeping at the end of the outer loop to prevent memory accumulation across HSCPs.
**Action:** Always check if a script sourced in a loop generates a static asset or loads partnership-wide data that doesn't change per locality. If so, hoist it.
