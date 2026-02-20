# Bolt's Performance Journal

## 2024-05-23 - Hoisting Services Chapter
**Learning:** Map rendering in `Services/3. Service HSCP map.R` and data manipulation in `Services/2. Services data manipulation & table.R` are partnership-level operations. Sourcing them in a locality-level loop is highly redundant and expensive (estimated 10-20s per locality).
**Action:** Split manipulation into 2a (HSCP) and 2b (Locality), and hoist 2a and 3 to the outer partnership loop in `Build Profiles.R` and `excel_output.R`. Ensure they are created BEFORE `loop_env` is captured.

## 2024-05-23 - Reviewer False Positive
**Learning:** A code review incorrectly flagged `excel_output.R` as broken for not sourcing `2b. Services table.R`. However, `excel_output.R` only needs raw data (`markers_*` from `2a`) and does not use the summary table (`services_tibble` from `2b`).
**Action:** Trust codebase-wide grep and code inspection over external reviews if they contradict the actual code usage.
