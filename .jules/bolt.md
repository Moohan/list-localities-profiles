## 2025-05-15 - [R Report Pipeline Hoisting]
**Learning:** In R-based reporting pipelines, 'hoisting' computationally expensive operations (like RDS data loading and map rendering with ggmap/get_stadiamap) from the innermost loop (locality) to outer loops (HSCP/global) provides a massive performance boost. Redundant network calls for map tiles and disk I/O for identical data across localities are major bottlenecks.
**Action:** Always identify operations that generate partnership-level or global-level assets and move them to the highest appropriate scope. Ensure variable definition order (e.g., base paths) is strictly maintained during refactoring to avoid 'object not found' errors.

## 2025-05-15 - [Safe Cleanup in R Loops]
**Learning:** When moving logic out of inner loops, standard cleanup blocks (rm()) inside the inner loop can break functionality by deleting partnership-level objects needed for subsequent locality iterations.
**Action:** Use `rm(list = intersect(c("obj1", "obj2"), ls()))` for targeted, safe cleanup of locality-specific objects, and perform partnership-level cleanup only after the inner loop completes.
