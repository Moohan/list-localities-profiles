## 2025-05-15 - [Hoisting in R report pipelines]
**Learning:** For R report pipelines that source scripts inside nested loops (HSCP -> Locality), a monolithic script per chapter causes redundant I/O and computation. Hoisting data loading to the global session level and partnership-wide calculations to the HSCP level significantly reduces execution time.
**Action:** Always structure chapter scripts into tiered files: `1a` (global session), `1b` (HSCP level), and `1c` (locality level). Ensure the main entry points (Build Profiles, excel_output) source these at the appropriate loop levels.
