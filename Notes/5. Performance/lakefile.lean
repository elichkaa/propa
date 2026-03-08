import Lake
open Lake DSL

package Functional

lean_lib Testing

@[default_target]
lean_lib Functional where
  precompileModules := true

-- ===============================================
-- Benchmarking Framework for Accumulator Transformations
-- ===============================================

lean_exe "benchmark-baseline" where
  root := `Functional.PerformanceBaseline

lean_exe "benchmark-accumulators" where
  root := `Functional.PerformanceAccumulators
