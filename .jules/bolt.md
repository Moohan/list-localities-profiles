## 2025-05-14 - [Hoisting Services Chapter Logic]
**Learning:** In nested reporting loops (HSCP -> Locality), static assets like maps and partnership-wide service data were being re-computed for every locality. Since map rendering in `ggmap` is a major bottleneck (10-20s per call), moving this to the outer loop provides a substantial performance boost.
**Action:** Always decompose 'chapter scripts' into global (run once), partnership (run once per group), and locality (run in inner loop) tiers to minimize redundant computation and I/O.
