import Testing
/-
  time complexity = O(n) with n = length of list
  the function makes a single pass over the list, performing a constant amount of work
  for each element (work = 1)
  -> O(1 * n)
-/
def List.len (xs : List α) : Nat :=
  match xs with
  | [] => 0
  | _ :: xs => 1 + len xs

/- ## Length with accumulators -/

def List.lenTRAux (xs : List α) (acc : Nat) : Nat :=
  match xs with
  | [] => acc
  | _ :: xs => xs.lenTRAux (acc + 1)

def List.lenTR (xs : List α) : Nat := xs.lenTRAux 0

/-
  time complexity = O(n) but here we have k = 2 because one work unit for
  the pattern match, 1 work unit for the cons operator
  -> O(2 * n) = O(n)
-/
def List.app (xs ys : List α) : List α :=
  match xs with
  | [] => ys
  | x :: xs' => x :: app xs' ys

def List.appTRAux (xs acc : List α) : List α :=
  match xs with
    | [] => acc
    | x :: xs' => xs'.appTRAux (x :: acc)

def List.appTR (xs ys : List α) : List α := appTRAux xs.reverse ys

/-
  structural recursion = O(n * k) but k (the work for each element)
  depends on append which has complexity = O(n)
  so in total we have = O(n^2)
-/
def List.rev (xs : List α) : List α :=
  match xs with
  | [] => []
  | x :: xs => (rev xs).app [x]

def List.revTRAux (xs : List α) (acc : List α): List α :=
  match xs with
    | [] => acc
    | x :: xs' => xs'.revTRAux (x :: acc)

def List.revTR (xs : List α) : List α := xs.revTRAux []

/- ## Benchmarking -/
def main (args : List String) : IO Unit := do
  let dir := "benchmark-results"
  let params := Params.parse (args.drop 1)
  match args.head? with
  | some "length" => benchmarkAndStore dir "baseline-length" params (List.singleton ∘ List.len) [List.singleton ∘ List.length]
  | some "append" => benchmarkAndStore dir "baseline-append" params (fun x => List.app x x) [fun x => List.append x x]
  | some "reverse" => benchmarkAndStore dir "baseline-reverse" params List.rev [List.reverse]
  | some "length_acc" => benchmarkAndStore dir "accumulator-length" params (List.singleton ∘ List.lenTR) [List.singleton ∘ List.length]
  | some "append_acc" => benchmarkAndStore dir "accumulator-append" params (fun x => List.appTR x x) [fun x => List.append x x]
  | some "reverse_acc" => benchmarkAndStore dir "accumulator-reverse" params List.revTR [List.reverse]
  | _ => IO.println "Unknown benchmark."
