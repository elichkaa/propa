

inductive TimeUnit where
  | ms
  | μs
  | ns

def TimeUnit.fromNano (nanoTime : Float) (unit : TimeUnit) : Float :=
  match unit with
  | TimeUnit.ms => nanoTime / 1_000_000.0
  | TimeUnit.μs => nanoTime / 1_000.0
  | TimeUnit.ns => nanoTime

instance : ToString TimeUnit where
  toString
    | TimeUnit.ms => "ms"
    | TimeUnit.μs => "μs"
    | TimeUnit.ns => "ns"

-- ===============================================
-- Forcing Functions for Different Result Types
-- ===============================================

def forceList {α : Type} (xs : List α) : IO Nat := do
  let mut count := 0
  for _ in xs do
    count := count + 1
  return count

-- ===============================================
-- Core Benchmarking Functions
-- ===============================================

def benchmarkListToList {α β : Type} (f : List α → List β) (input : List α) (iterations : Nat := 50) : IO Float := do
  let mut total : UInt64 := 0
  for _ in [0:iterations] do
    let start ← IO.monoNanosNow
    let result := f input
    let _ ← forceList result
    let stop ← IO.monoNanosNow
    total := total + (stop - start).toUInt64
  return total.toFloat / iterations.toFloat

-- ===============================================
-- Size Generation and Test Configurations
-- ===============================================

structure Params where
  start : Nat
  stop : Nat
  step : Nat
  iterations : Nat

def makeSizes (start stop step : Nat) : List Nat :=
  if step == 0 then [start]
  else
    let count := (stop - start) / step + 1
    (List.range count).map (fun i => start + i * step)

def standardInputGen : Nat → List String := List.map ToString.toString ∘ List.range

def Params.parse (args : List String) : Params :=
  match args with
  | [startStr, stopStr, stepStr, iterStr] =>
    { start := String.toNat! startStr
      stop := String.toNat! stopStr
      step := String.toNat! stepStr
      iterations := String.toNat! iterStr }
  | [startStr, stopStr, stepStr] =>
    { start := String.toNat! startStr
      stop := String.toNat! stopStr
      step := String.toNat! stepStr
      iterations := 3 }
  | _ => Params.mk 0 5000 500 3

-- ===============================================
-- CSV Export Functions
-- ===============================================

def formatCSV (name : String) (unit : TimeUnit) (size : Nat) (baseTime : Float) (optTimes : List Float) : String :=
  let optTimesString := String.join (optTimes.map (fun optTime =>
      let ratio : Float := if optTime > 0 then baseTime / optTime else 0.0
      s!", {unit.fromNano optTime}, {ratio}"))
  s!"{name}, {size}, {unit}, {unit.fromNano baseTime} {optTimesString}\n"

def exportCSV (name : String) (results : List (Nat × Float × List Float)) : String :=
  let header := "Function,Size,Base_Time,Opt_Time,Unit,Ratio\n"
  let allRows := results.foldl (fun acc (size, baseTime, optTimes) =>
    let unit := TimeUnit.μs
    let funcRows := formatCSV name unit size baseTime optTimes
    funcRows :: acc
  ) []
  let csvLines := List.append [header] allRows.reverse
  String.join csvLines

def saveCSV (name : String) (results : List (Nat × Float × List Float)) (path : System.FilePath) (print : Bool) : IO Unit := do
  let csvContent := exportCSV name results
  if print then
    IO.println csvContent
  IO.FS.writeFile path csvContent

-- ===============================================
-- Output Formatting
-- ===============================================

def printAndSave (content : String) (path : System.FilePath) : IO Unit := do
  IO.println content
  IO.FS.writeFile path content

def benchmark (params : Params) (fBaseline : List String → List β) (fsOptimized : List (List String → List β)) : IO (List (Nat × Float × List Float)) := do
  let sizes := makeSizes params.start params.stop params.step
  let mut results : List (Nat × Float × List Float) := []
  for size in sizes do
    let input := standardInputGen size
    let baseTime ← benchmarkListToList fBaseline input params.iterations
    let optimizedTimes ← fsOptimized.mapM (fun fOpt => benchmarkListToList fOpt input params.iterations)
    results := (size, baseTime, optimizedTimes) :: results
  return results.reverse

def benchmarkAndStore (dir name : String) (params : Params) (fBaseline : List String → List β) (fsOptimized : List (List String → List β)) : IO Unit := do
  IO.println s!"Running {name} benchmark..."
  let result ← benchmark params fBaseline fsOptimized

  IO.FS.createDirAll dir
  let dirPath ← IO.FS.realPath dir
  IO.println s!"Storing results in directory: {dirPath}"
  saveCSV name result (dirPath / s!"{name}_benchmark.csv") true


-- ===============================================
-- Individual Test Functions
-- ===============================================

-- def testReverse (iterations : Nat := 50) : IO (String × List (Nat × Nat × Nat)) := do
--   let sizes := makeSizes 500 9000 500
--   let mut results : List (Nat × Nat × Nat) := []
--   for size in sizes do
--     let input := standardInputGen size
--     let baseTime ← benchmarkListToList List.rev input iterations
--     let optTime ← benchmarkListToList List.revTR input iterations
--     results := (size, baseTime, optTime) :: results
--   return ("reverse", results.reverse)

-- def testLength (iterations : Nat := 50) : IO (String × List (Nat × Nat × Nat)) := do
--   let sizes := makeSizes 5000 100000 5000
--   let mut results : List (Nat × Nat × Nat) := []
--   for size in sizes do
--     let input := standardInputGen size
--     let baseTime ← benchmarkListToNat List.len input iterations
--     let optTime ← benchmarkListToNat List.lenTR input iterations
--     results := (size, baseTime, optTime) :: results
--   return ("length", results.reverse)

-- def testAppend (iterations : Nat := 50) : IO (String × List (Nat × Nat × Nat)) := do
--   let sizes := makeSizes 1000 100000 4000
--   let fixedList : List Nat := [1]
--   let appendBase (ys : List Nat) : List Nat → List Nat := fun xs => xs.myAppend ys
--   let appendOpt (ys : List Nat) : List Nat → List Nat := fun xs => xs.myAppendTR ys
--   let mut results : List (Nat × Nat × Nat) := []
--   for size in sizes do
--     let input := standardInputGen size
--     let baseTime ← benchmarkListToList (appendBase fixedList) input iterations
--     let optTime ← benchmarkListToList (appendOpt fixedList) input iterations
--     results := (size, baseTime, optTime) :: results
--   return ("append", results.reverse)

-- ===============================================
-- Individual Benchmark Runners
-- ===============================================

-- def runReverseOnly (iterations : Nat := 50) : IO Unit := do
--   IO.println "Running reverse benchmark..."
--   let result ← testReverse iterations

--   let textFormatted := formatSingleResult result.1 result.2
--   printAndSave textFormatted "Functional/Accumulators/Results/reverse_benchmark.txt"

--   saveCSV [result] "Functional/Accumulators/Results/reverse_benchmark.csv"

--   IO.println "Reverse benchmark complete - saved .txt and .csv files"

-- def runLengthOnly (iterations : Nat := 50) : IO Unit := do
--   IO.println "Running length benchmark..."
--   let result ← testLength iterations

--   let textFormatted := formatSingleResult result.1 result.2
--   printAndSave textFormatted "Functional/Accumulators/Results/length_benchmark.txt"

--   saveCSV [result] "Functional/Accumulators/Results/length_benchmark.csv"

--   IO.println "Length benchmark complete - saved .txt and .csv files"

-- def runAppendOnly (iterations : Nat := 50) : IO Unit := do
--   IO.println "Running append benchmark..."
--   let result ← testAppend iterations

--   let textFormatted := formatSingleResult result.1 result.2
--   printAndSave textFormatted "Functional/Accumulators/Results/append_benchmark.txt"

--   saveCSV [result] "Functional/Accumulators/Results/append_benchmark.csv"

--   IO.println "Append benchmark complete - saved .txt and .csv files"

-- -- ===============================================
-- -- Usage Examples
-- -- ===============================================

-- -- #eval Functional.Accumulators.Benchmark.runReverseOnly

-- -- #eval runLengthOnly

-- -- #eval Functional.Accumulators.Benchmark.runAppendOnly
