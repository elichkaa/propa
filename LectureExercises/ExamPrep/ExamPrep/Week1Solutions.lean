import Testing

/-
### Exercise 1
Use `#eval` to evaluate the following (one `#eval` per item):
  a) The result of integer division: 17 divided by 4
  b) Whether the string "Lean" is equal to "lean" (use `==`)
  c) The list of natural numbers from 5 to 14 (inclusive), using `List.range'`
  d) The boolean: `(not false) && (3 > 2)`
  e) The result of appending [10, 20] to [1, 2, 3] and then summing the result
-/
#eval 17 / 4
#eval "Lean" == "lean"
#eval List.range' 5 10
#eval (not false) && (3 > 2)
#eval List.sum ([10, 20] ++ [1, 2, 3])

/-
### Exercise 2
Define a function `clamp` that takes three integers `lo`, `hi`, and `x`.
It returns `lo` if `x < lo`, `hi` if `x > hi`, and `x` otherwise.
Write at least four `#assert` tests.
-/
def clamp (lo hi x: Int): Int :=
  if x < lo then lo else if x > hi then hi else x

/-
### Exercise 3
Define a function `bmi` that takes a `Float` weight (kg) and `Float` height (m)
and returns a `String` category:
  - "Underweight" if BMI < 18.5
  - "Normal" if BMI < 25.0
  - "Overweight" if BMI < 30.0
  - "Obese" otherwise
BMI = weight / (height * height).
Use a `let` binding for the BMI value.
-/
def bmi (weight height: Float32): String :=
  let bmi := weight / (height * height)
  if bmi < 18.5 then "Underweight"
  else if bmi < 25 then "Normal"
  else if bmi < 30 then "Overweight"
  else "Obese"

#eval bmi 63.4 1.7

/-
### Exercise 4
Define a function `fizzBuzz` that takes a `Nat` and returns a `String`:
  - "FizzBuzz" if divisible by both 3 and 5
  - "Fizz" if divisible by 3 only
  - "Buzz" if divisible by 5 only
  - The decimal string representation of the number otherwise (use `toString`)
-/
def fizzBuzz (a: Nat): String :=
  if (a % 3 == 0) && (a % 5 == 0) then "FizzBuzz"
  else if (a % 3 == 0) then "Fizz"
  else if (a % 5 == 0) then "Buzz"
  else toString a

/-
### Exercise 5
Define a function `describeList` that takes a `List Int` and returns a `String`:
  - "empty" if the list has no elements
  - "singleton" if it has exactly one element
  - "short" if it has 2–4 elements
  - "long" otherwise
Use `List.length`.
-/
-- def describeList (xs: List Int): String :=
--   match xs with
--     | [] => "empty"
--     | [x] => "singleton"
--     | [x, y] | [x, y, z] | [x, y, z, f] => "short"
--     | _ => "long"

def describeList (xs: List Int): String :=
  let len := xs.length
  if len == 0 then "empty"
  else if len == 1 then "singleton"
  else if len <= 4 then "short"
  else "long"

/-
### Exercise 6
Using only `List.map`, `List.filter`, `List.sum`, and the pipe operator,
define a function `sumOfDoubledEvens` that takes a `List Int` and returns
the sum of all even numbers doubled.
Do NOT use explicit recursion or `let` bindings — write it as a single pipeline.
-/
def sumOfDoubledEvens (xs: List Int): Int :=
  xs |>.filter (fun x => x % 2 == 0)
     |>.map (fun x => x * 2)
     |>.sum

/-
### Exercise 7
Define a function `normalize` that takes a `List Float` and returns a new list
where every element is divided by the maximum element in the list.
If the list is empty, return an empty list.
Use `List.map` and `List.foldl` (to find the maximum).

Hint: `List.foldl (fun acc x => if x > acc then x else acc) 0.0 xs`
gives the maximum of `xs`.
-/
def normalize (xs: List Float): List Float :=
  let max := xs.foldl (fun acc hd => if hd > acc then hd else acc) 0.0
  xs.map (fun x => x / max)

#eval normalize [1, 2, 3]

/-
### Exercise 8
Define a function `countWhere` that takes a predicate `p : Int → Bool`
and a `List Int`, and returns the number of elements satisfying `p`.
Then use it to define `countPositive` and `countNegative` (no recursion).

Hint: `List.filter` + `List.length`.
-/
def countWhere (p: Int -> Bool) (lst: List Int): Nat :=
  let lst' := lst.foldl (fun acc hd => if p hd then p hd :: acc else acc) []
  lst'.length

def countWhere' (p: Int -> Bool) (lst: List Int): Nat :=
  (lst.filter p).length

def countPositive := countWhere (fun x => x > 0)
def countNegative := countWhere (fun x => x < 0)

#eval countPositive [1, 2, -1, 3, -5]

/-
### Exercise 9
Define a function `applyTwice` that takes a function `f : Nat → Nat`
and a `Nat` value `x`, and returns `f (f x)`.
Then define a value `result` by calling `applyTwice` with a lambda that
adds 3, applied to 10. Verify with `#assert` that the result is 16.
-/
def applyTwice (f: Nat -> Nat) (x: Nat): Nat :=
  f (f x)

#eval applyTwice (fun x => x + 3) 3

/-
### Exercise 10
Define an inductive type `Season` with constructors `Spring`, `Summer`,
`Autumn`, `Winter`. Derive `Repr` and `BEq`.

Then define a function `nextSeason` that returns the following season.
Write four `#assert` tests covering all transitions.
-/
inductive Season where
  | Spring
  | Summer
  | Autumn
  | Winter
  deriving Repr, BEq

open Season
def nextSeason (s: Season): Season :=
  match s with
    | Spring => Summer
    | Summer => Autumn
    | Autumn => Winter
    | Winter => Spring

#assert (nextSeason Spring) == Summer
#assert (nextSeason Summer) == Autumn
#assert (nextSeason Autumn) == Winter
#assert (nextSeason Winter) == Spring

/-
### Exercise 11
Define an inductive type `Shape` with constructors:
  - `Circle` carrying a `Float` radius
  - `Rectangle` carrying `Float` width and `Float` height
  - `Triangle` carrying `Float` base and `Float` height
Derive `Repr` and `BEq`.

Define a function `area` that computes the area of a `Shape` using pattern matching.
Formulas: circle = π * r², rectangle = w * h, triangle = 0.5 * b * h.
Use `let pi := 3.14159` inside the function.
-/
inductive Shape where
  | Circle (r: Float)
  | Rectangle (width height: Float)
  | Triangle (base height: Float)
  deriving Repr, BEq

open Shape
def area (s: Shape): Float :=
  let pi := 3.14159
  match s with
    | Circle r => pi * r * r
    | Rectangle width height => width * height
    | Triangle base height => 0.5 * base * height

/-
### Exercise 12
Define a structure `Point3D` with `Float` fields `x`, `y`, `z`.
Define a function `distance3D` that computes the Euclidean distance between two `Point3D` values.
Use `let` bindings for the differences, and `Float.sqrt` for the square root.

Formula: sqrt((x2-x1)² + (y2-y1)² + (z2-z1)²)
-/
structure Point3D where
  x: Float
  y: Float
  z: Float

def distance3D (a b: Point3D): Float :=
  let xDiff := a.x - b.x
  let yDiff := a.y - b.y
  let zDiff := a.z - b.z
  Float.sqrt (xDiff^2 + yDiff^2 + zDiff^2)

/-
### Exercise 13
Define a structure `Student` with fields:
  - `name : String`
  - `grade : Nat`  (0–100)
  - `passed : Bool`

Create three example students (at least one passed, one failed).
Define a function `honorRoll` that takes a `List Student` and returns
the names of students who passed AND have a grade of 90 or above.
Use `List.filter` and `List.map`.
-/
structure Student where
  name: String
  grade: Nat
  passed: Bool

def Mike: Student := {name := "Mike", grade := 90, passed := true}
def Fiona: Student := {name := "Fiona", grade := 87, passed := false}
def Karl: Student := {name := "Karl", grade := 100, passed := true}

open Student
def honorRoll (lst: List Student): List String :=
  lst |>.filter (fun s => s.passed && s.grade >= 90)
      |>.map (fun s => s.name)

#eval honorRoll [Mike, Fiona, Karl]

/-
### Exercise 14
Define an inductive type `Expr` with constructors:
  - `Lit` carrying an `Int`
  - `Add` carrying two `Expr`
  - `Mul` carrying two `Expr`
  - `Neg` carrying one `Expr`
Derive `Repr`.

Define a function `eval` that evaluates an `Expr` to an `Int` using pattern matching.

Verify with `#assert` that:
  - `eval (Add (Lit 3) (Lit 4))` == 7
  - `eval (Mul (Lit 2) (Add (Lit 3) (Lit 1)))` == 8
  - `eval (Neg (Lit 5))` == -5
-/
inductive Expr where
  | Lit (value: Int)
  | Add (exp1 exp2: Expr)
  | Mul (exp1 exp2: Expr)
  | Neg (exp: Expr)

open Expr
def eval (exp: Expr): Int :=
  match exp with
    | .Lit value => value
    | .Add exp1 exp2 => (eval exp1) + (eval exp2)
    | .Mul exp1 exp2 => (eval exp1) * (eval exp2)
    | .Neg exp => - (eval exp)

#assert (eval (Add (Lit 3) (Lit 4))) == 7
#assert (eval (Mul (Lit 2) (Add (Lit 3) (Lit 1)))) == 8
#assert (eval (Neg (Lit 5))) == -5

/-
### Exercise 15
Define a function `runLengthEncode` that takes a `List Nat` and returns a `List (Nat × Nat)`,
where each pair `(value, count)` represents a run of consecutive equal values.

Example: `[1, 1, 2, 3, 3, 3]` → `[(1, 2), (2, 1), (3, 3)]`

Use `List.foldl`. Start with an empty accumulator `[]` and build the result list.

Hint: The accumulator type is `List (Nat × Nat)`. In each step, check if the current
element equals the value in the last pair; if so, increment its count, otherwise append
a new pair. You can use `List.getLast?` and pattern match on the result, or keep the
accumulator in reverse and reverse at the end.
-/
def runLengthEncode (lst: List Nat): List (Nat × Nat) :=
  (lst.foldl (fun acc hd =>
    match acc with
    | [] => [(hd, 1)]
    | (x, count) :: rest =>
      if x == hd then (x, count + 1) :: rest
      else (hd, 1) :: acc
  ) []).reverse

#eval runLengthEncode [1, 1, 2, 3, 3, 3]

/-
### Exercise 16
Define a structure `BankAccount` with fields:
  - `owner : String`
  - `balance : Float`
Derive `Repr` and `BEq`.

Define an inductive type `Transaction` with constructors:
  - `Deposit (amount : Float)`
  - `Withdraw (amount : Float)`

Define a function `applyTransaction` that takes a `BankAccount` and a `Transaction`
and returns an updated `BankAccount`. Withdrawals that would make the balance negative
are rejected (return the account unchanged).

Define a function `applyAll` that takes a `BankAccount` and a `List Transaction`
and applies all transactions in order using `List.foldl`.

Test with at least three `#assert` statements.
-/
structure BankAccount where
  owner: String
  balance: Float
  deriving Repr, BEq

inductive Transaction where
  | Deposit (amount: Float)
  | Withdraw (amount: Float)

open BankAccount Transaction
def applyTransaction (b: BankAccount) (t: Transaction): BankAccount :=
  match t with
    | Deposit amount => {b with balance := amount + b.balance}
    | Withdraw amount => if amount < b.balance then b else {b with balance := b.balance - amount}

def applyAll (b: BankAccount) (lst: List Transaction): BankAccount :=
  lst.foldl (fun acc hd => applyTransaction acc hd) b

/-
### Exercise 17
Using the pipe operator exclusively (no intermediate `def`s, just one expression),
compute the following from `List.range 50` (numbers 0 to 49):
  1. Keep only numbers divisible by 3
  2. Map each to its square
  3. Keep only squares greater than 100
  4. Sum the result

Assign the final value to `pipelineResult` and verify it with `#assert`.
-/
def lst := List.range 50
def pipelineResult :=
  lst |>.filter (fun x => x % 3 == 0)
      |>.map (fun x => x * x)
      |>.filter (fun x => x > 100)
      |>.sum
#eval pipelineResult

/-
### Exercise 18
Translate the following pseudocode into Lean:
```
function ticketPrice(age, isStudent, isWeekend):
  base = if isWeekend then 15 else 10
  discount =
    if age < 6 then base        -- free
    else if age < 18 then base * 0.5
    else if age >= 65 then base * 0.3
    else if isStudent then base * 0.2
    else 0
  return base - discount
```

Return type is `Float`. Use `let` bindings for `base` and `discount`.
Write five `#assert` tests covering distinct cases.
-/
def ticketPrice (age: Nat) (isStudent isWeekend: Bool) : Float :=
  let base := if isWeekend then 15 else 10
  let discount := if age < 6 then base
  else if age < 18 then base * 0.5
  else if age >= 65 then base * 0.3
  else if isStudent then base * 0.2
  else 0
  base - discount

/-
### Exercise 19
Define a function `groupBySign` that takes a `List Int` and returns a tuple
`(List Int × List Int × List Int)` containing:
  - first: all negative numbers (preserving order)
  - second: all zeros
  - third: all positive numbers (preserving order)

Use `List.filter` three times.
Verify with one `#assert` that covers all three components.

Hint: tuple syntax is `(a, b, c)` and you can `#assert` the whole tuple at once.
-/
def groupBySign (lst: List Int): (List Int × List Int × List Int) :=
  let first := lst.filter (fun x => x < 0)
  let snd := lst.filter (fun x => x == 0)
  let third := lst.filter (fun x => x > 0)
  (first, snd, third)

/-
### Exercise 20
Define an inductive type `TrafficLight` with constructors `Red`, `Yellow`, `Green`.
Derive `Repr` and `BEq`.

Define:
  - `next : TrafficLight → TrafficLight` (Red→Green→Yellow→Red cycle)
  - `canGo : TrafficLight → Bool` (only Green allows going)
  - `simulate : Nat → TrafficLight → List TrafficLight` that returns
    the list of states after each of `n` transitions, starting from the given light.
    (The initial state is NOT included; use `List.range` and `List.foldl` or build it
    with a helper approach using the pipe operator.)

Verify:
  - `next Red == Green`
  - `canGo Yellow == false`
  - `simulate 4 Red == [Green, Yellow, Red, Green]`
-/
inductive TrafficLight where
  | Red
  | Yellow
  | Green
  deriving Repr, BEq

open TrafficLight

def next (t: TrafficLight): TrafficLight :=
  match t with
    | Red => Green
    | Yellow => Red
    | Green => Yellow

def canGo (t: TrafficLight): Bool := if t == Green then true else False

def simulate (n: Nat) (starting: TrafficLight): List TrafficLight :=
  let (_, result) := List.range n |>.foldl
    (fun (acc : TrafficLight × List TrafficLight) _ =>
      let nextLight := next acc.1
      (nextLight, acc.2 ++ [nextLight]))
    (starting, [])
  result
