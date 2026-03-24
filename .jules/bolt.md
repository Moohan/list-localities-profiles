## 2024-05-23 - R Script Performance Hoisting
**Learning:** In long-running R report pipelines (like the Locality Profiles), a significant performance bottleneck is redundant I/O and data transformation inside nested loops. Large datasets (populations, SIMD, service locations) were being re-read and filtered for every locality.
**Action:** Implement "hoisting" by splitting monolithic scripts into hierarchical stages:
1. Global loading (run once per session)
2. Partnership/HSCP-level manipulation (run once per outer loop)
3. Locality-level table/output generation (run once per inner loop).
This architectural pattern drastically reduces the total execution time for full HSCP builds.

## 2024-05-23 - R Environment Verification
**Learning:** In restricted environments (like the current sandbox) where network mounts (/conf) and data are missing, full script execution is impossible.
**Action:** Use `Rscript -e 'parse(file = "path/to/script.R")'` as a proxy for verification to ensure syntax correctness, even when dependencies or data are unavailable. This ensures the code is at least syntactically valid before submission.
