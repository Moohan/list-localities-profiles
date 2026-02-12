## 2025-02-12 - [Hoisting Services Chapter]
**Learning:** Hoisting expensive data processing and asset generation (like maps) from a locality-level loop to an HSCP-level loop significantly improves performance in report generation pipelines. The pattern involves splitting a script into partnership-level (2a) and locality-level (2b) components.
**Action:** Always check nested loops for operations that are constant at the outer loop's level. For R reports, ensure hoisted objects are included in the 'loop_env' to prevent premature cleanup by the inner loop's housekeeping.
