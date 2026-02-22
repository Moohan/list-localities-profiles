## 2025-02-22 - Hoisting Expensive Map Rendering
**Learning:** In the Locality Profiles reporting pipeline, the Services map is a significant bottleneck (~10-20 seconds per partnership). Because the map is HSCP-level, generating it inside the locality loop is redundant. Refactoring monolithic scripts into HSCP-level (hoisted) and locality-level components provides a massive performance win (e.g., saving ~2 minutes for a partnership with 10 localities).
**Action:** Always check if assets (maps, totals, data lookups) generated in loops are actually invariant for the loop's current iteration level and hoist them to the outer loop if they are. Ensure proper cleanup of hoisted objects to avoid memory bloat.

## 2025-02-22 - Avoiding Global Variable Overwrites in Loops
**Learning:** Sourced scripts often define or overwrite variables with common names like `lookup`. When hoisting these scripts to an outer loop, these overwrites can persist and cause subtle bugs in subsequent iterations or other scripts that rely on the original state of the global variable.
**Action:** Be extremely careful when sourcing scripts that modify global state. Rename or comment out redundant assignments that might overwrite core orchestration variables.
