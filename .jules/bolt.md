## 2025-05-15 - [Hoisting expensive operations in R loops]
**Learning:** Hoisting expensive I/O and visualization operations (like `ggmap::get_stadiamap` and `readRDS` of large datasets) from an inner locality loop to an outer partnership loop or global session level provides a significant performance boost. In this codebase, map generation alone takes 10-20 seconds per partnership; running it per locality was a major bottleneck.
**Action:** Always check if scripts sourced in loops can be split into session-level loading, partnership-level manipulation, and locality-level output.
