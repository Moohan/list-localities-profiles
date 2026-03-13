# Bolt's Performance Journal ⚡

## 2025-01-24 - Hoisting Redundant Loop Operations
**Learning:** A powerful performance optimization in R report pipelines is 'hoisting': refactoring a large script into global-level logic (run once per session), HSCP-level logic (run once per partnership), and locality-level logic (run once per locality). Map rendering and large data loads are primary candidates for hoisting.
**Action:** When optimizing R loops, analyze script dependencies to see what can be moved to outer scopes. Use tiered script naming (e.g., `2a`, `2b`, `2c`) to make these levels explicit.

## 2025-01-24 - R Loop Housekeeping and Hoisting
**Learning:** In `Build Profiles.R`, the `loop_env` pattern captures the environment *before* the inner locality loop. If partnership-level scripts (sourced in the outer loop) are moved outside this assignment, their resulting objects will be wiped by the `rm(list = setdiff(ls(), loop_env))` call inside the inner loop.
**Action:** Ensure partnership-level variables (like `service_map` or `markers_*`) are created or preserved *after* any environment reset points that precede the locality loop.

## 2025-01-24 - R Verification without Environment
**Learning:** Syntax checking with `Rscript -e 'parse(file = "...")'` is an effective verification tool when full execution is impossible due to missing data or network mounts (like `/conf`).
**Action:** Use `parse()` for rapid verification of script structural integrity in restricted environments.
