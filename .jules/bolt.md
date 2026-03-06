## 2024-12-19 - Hoisting R logic in nested loops
**Learning:** In report generation pipelines with nested loops (e.g., HSCP > Locality), monolithic scripts sourced in the inner loop are major bottlenecks. Sourcing involves file I/O, and if the script loads large RDS files or renders complex plots (like 'ggmap'), the overhead scales linearly with the total number of localities. Tiered refactoring (Global > HSCP > Locality) is essential.

**Action:** Always check if 'chapter scripts' can be split into data-loading, partnership-level manipulation, and locality-level output stages. Hoist everything possible to the outermost loop.
