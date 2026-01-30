## 2026-01-30 - [Hoisting HSCP-level operations out of locality loops]
**Learning:** In nested loops where the outer loop iterates over larger geographical areas (HSCPs) and the inner loop over smaller ones (Localities), many scripts mix data loading/manipulation constant at the HSCP level with locality-specific outputs. Hoisting the HSCP-level logic and map generation out of the inner loop significantly reduces disk I/O and expensive render operations.

**Action:** Identify and split scripts into HSCP-level (data prep/maps) and Locality-level (tables/text) components. Ensure shared variables are preserved for the inner loop by correctly positioning environment captures and cleanups.
