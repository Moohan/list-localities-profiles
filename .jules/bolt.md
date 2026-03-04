## 2024-05-23 - [Hoisting Redundant Data Loading]
**Learning:** In report generation pipelines with nested loops (Partnership > Locality), placing data-heavy script sourcing inside the inner loop is a major performance bottleneck. Hoisting data manipulation and static asset generation (like maps) to the outer loop significantly reduces redundant I/O and computation.
**Action:** Always identify which data objects are constant across a partnership/group and refactor scripts to load and process them once at the highest possible level in the loop hierarchy.
