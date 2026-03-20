## 2025-03-20 - Hoisting Services Chapter Logic
**Learning:** Performance in iterative R report generation can be significantly improved by 'hoisting' static or partnership-level logic (data loading, map generation) out of the innermost locality loops. This reduces redundant I/O and expensive computations like geospatial mapping.
**Action:** Always check if a script being sourced inside a loop can be split into global, outer-loop, and inner-loop components.
