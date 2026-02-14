/- # Week1: Recursive Data Types and Functions -/

import Testing

namespace Practice

/- ## Data Type Definitions -/

/- Task 1: Define a recursive List data type with nil and cons constructors -/
inductive List (α: Type) where
  | nil
  | cons (head: α) (tail: List α)
  deriving Repr, BEq


open List
/- Task 2: Define convenient notation for :: and [] -/
scoped infixr:68 "::" => cons
scoped notation "[]" => nil


/- ## Basic Pattern Matching -/

/- Task 3: Check if a list is empty -/
def List.isEmpty {α} (xs: List α): Bool :=
  match xs with
  | [] => true -- can be written as nil => true
  | _ :: _ => false -- can be written as cons _ _


/- Task 4: Get the first element of a list (return Option) -/
def List.head? {α} (xs: List α): Option α :=
  match xs with
    | [] => none
    | x :: _ => x


/- Task 5: Get the tail of a list (return Option) -/
def List.tail? {α} (xs: List α): Option (List α) :=
  match xs with
    | [] => none
    | _ :: xs => xs

def numbers : List Nat := 1 :: 2 :: 3 :: []
#assert numbers.head? == some 1
#assert numbers.tail? == some (2 :: 3 :: [])


/- Task 6: Get the second element of a list -/
def List.second? {α} (xs: List α): Option α :=
  match xs with
    | [] => none
    | _ :: xs =>
      match xs with
      | [] => none
      | x :: _ => some x

def List.second2? {α} (xs: List α): Option α :=
  match xs with
  | _ :: x :: _ => some x
  | _ => none

#assert numbers.second? == some 2


/- Task 7: Get the third element of a list -/
def List.third? {α} (xs: List α): Option α :=
  match xs with
   | _ :: _ :: x :: _ => x
   | _ => none

#assert numbers.third? == some 3


/- Task 8: Get the first three elements as a tuple -/
def List.firstThree? {α} (xs: List α): Option (α × α × α) :=
  match xs with
    | f :: s :: t :: _ => some (f, s, t)
    | _ => none

#assert numbers.firstThree? == some (1, 2, 3)

/- Task 9: Implement XOR for two booleans using pattern matching -/
def Bool.xor (f s: Bool): Bool :=
  match f, s with
    | true, false => true
    | false, true => true
    | _, _ => false


/- Task 10: Check if heads of two lists are equal -/
def List.headsEqual? {α} [BEq α] (xs: List α) (ys: List α): Bool :=
  match xs, ys with
    | [], [] => true
    | x :: _, y :: _ => x == y
    | _, _ => false


/- ## Structural Recursion on Lists -/

/- Task 11: Calculate the length of a list -/
def List.length {α} (xs: List α): Nat :=
  match xs with
    | [] => 0
    | _ :: xs => 1 + List.length xs

#assert numbers.length == 3


/- Task 12: Append two lists together -/
-- IMPORTANT
def List.append {α} (xs ys: List α): List α :=
  match xs with
    | [] => ys -- left list is empty so we just append a pointer to the right list
    | x :: xs => x :: (xs.append ys) -- we create a copy of xs and then at the end apped ys

instance : Append (List α) where
  append := List.append

/- Task 13: Sum all elements in a list of natural numbers -/
def List.sum (xs: List Int): Int :=
  match xs with
    | [] => 0
    | x :: xs => x + xs.sum

def numbersInt : List Int := 1 :: 2 :: 3 :: []
def numbersIn2t : List Int := 5 :: 6 :: 7 :: []
#assert numbersInt.sum == 6
#assert (numbersInt.append numbersIn2t) == (1 :: 2 :: 3 :: 5 :: 6 :: 7 :: [])


/- Task 14: Zip two lists into a list of pairs -/
def List.zip {α β} (xs: List α) (ys: List β): List (α × β) :=
  match xs, ys with
    | [], _ => []
    | _, [] => []
    | x :: xs, y :: ys => (x, y) :: (List.zip xs ys)

#eval List.zip (1 :: 2 :: 3 :: []) ("a" :: "b" :: "c" :: "d" :: [])

/- On strings -/
-- Convert a list to a string representation with infix `::`
def List.toString {α} [ToString α] (l : List α) : String :=
  match l with
  | [] => "[]"
  | hd :: tl => ToString.toString hd ++ " :: " ++ tl.toString

-- to override the default repr with our custom implementation
instance [ToString α] : Repr (List α) where
  reprPrec l _ := l.toString

#eval numbersInt.toString


/- ## Natural Numbers -/

/- Task 15: Define a recursive Nat data type with zero and succ -/
namespace MyNat

-- TODO: Define inductive Nat type
inductive Nat where
  | zero
  | succ (pred: Nat)
  deriving Repr, BEq

open Nat
def two: Nat := succ (succ zero)
def three: Nat := succ (two)

/- Task 16: Add two natural numbers -/
def Nat.add (m n: Nat): Nat :=
  match n with
    | zero => m
    -- (n + 1) + m = 1 + (n + m)
    | succ n => succ (m.add n)

#eval two.add three

/- Task 17: Multiply two natural numbers -/
def Nat.mul (m n: Nat): Nat :=
  match n with
    | zero => zero
    -- (n + 1) * m = (n * m) + m
    | succ n => (m.mul n).add m

#eval two.mul three


/- Task 18: Raise m to the power of n -/
def Nat.pow (m n: Nat): Nat :=
  match n with
    | zero => succ zero
    -- m ^ (n + 1) = m ^ n * m
    | succ n => (m.pow n).mul m

#eval two.pow three


/- Task 19: Convert custom Nat to built-in Nat -/
def Nat.toNat (n : Nat) : _root_.Nat :=
  match n with
  | zero => 0
  | succ m => 1 + m.toNat


/- Task 20: Convert built-in Nat to custom Nat -/
def fromNat (n: _root_.Nat) : Nat :=
  match n with
    | 0 => zero
    | n + 1 => succ (fromNat n)

#eval (Nat.pow (fromNat 2) (fromNat 8)).toNat == 256
#eval (Nat.pow (fromNat 3) (fromNat 5)).toNat == 243
#eval (Nat.pow (fromNat 5) (fromNat 3)).toNat == 125

end MyNat


/- ## Simultaneous Recursion -/

/- Task 21: Take the first n elements from a list -/
def List.take {α} (n: Nat) (xs: List α): List α :=
  match n, xs with
    | 0, _ => []
    | _, [] => []
    | n + 1, x :: xs => x :: (List.take n xs)

#assert (List.take 2 (1 :: 2 :: 3 :: 4 :: [])) == (1 :: 2 :: [])


/- ## Complex Recursive Data Types -/

/- Task 22: Define a Route data type with destination, unreachable, turn, sequence, and alternative -/
inductive Route where
  | destination (name : String)              -- Base case 1: destination reached
  | unreachable                              -- Base case 2: destination is unreachable
  | turn (direction : String) (rest : Route) -- Recursive case 1: a turn followed by the rest of the route
  | sequence (r1 : Route) (r2 : Route)       -- Recursive case 2: sequence of two routes
  | alternative (r1 : Route) (r2 : Route)    -- Recursive case 3: two alternative routes
  deriving Repr, BEq


/- Task 23: Count the number of turns in a route -/
def Route.turnCount (r : Route) : Nat :=
  match r with
  | destination _ => 0                                      -- Base case 1
  | unreachable => 0                                        -- Base case 2
  | turn _ rest => rest.turnCount + 1                       -- Recursive case 1
  | sequence r1 r2 => r1.turnCount + r2.turnCount           -- Recursive case 2
  | alternative r1 r2 => Nat.min r1.turnCount r2.turnCount  -- Recursive case 3

end Practice

/- ## Generative Recursion -/
-- the recursive calls aren't executed on the structural elements but on newly generated data

/- Task 24: Implement quicksort (mark as partial) -/
private partial def List.quicksort [Ord α] (list : List α) : List α :=
  match list with
  | nil => nil  -- Base case: an empty list is already sorted
  | pivot :: tail =>
    let (less, greaterEq) := tail.partition (fun x => (Ord.compare x pivot == Ordering.lt))
    less.quicksort ++ (pivot :: greaterEq.quicksort)  -- Combine the sorted sublists with the pivot

#assert ([].quicksort : List Nat) == []
#assert [3, -5, 9, 2, 5].quicksort == [-5, 2, 3, 5, 9]
#assert ([3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5].quicksort) == [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9]

/- Task 25: Implement the Collatz function (mark as partial) -/
/-
The Collatz conjecture states that for any positive integer n, the sequence defined by:
    - If n is even, divide it by 2
    - If n is odd, multiply it by 3 and add 1
  will eventually reach 1.
  The Collatz function applies these rules recursively until it reaches 1. Whether the conjecture
  is true for all positive integers is still an open question in mathematics. So clearly, we
  cannot expect Lean to prove termination of this function for all inputs.
-/
partial def collatz (n: Nat): Nat :=
  match n with
    | 0 => 0
    | 1 => 1
    | n + 1 => if n % 2 == 0 then collatz (n / 2) else collatz (n * 3 + 1)

/- Task 26: Implement breadth-first search on a graph (mark as partial) -/
/-
  As a final example of generative recursion, here is a breadth-first search (BFS) algorithm
  for traversing a graph. The graph is represented as a hash map from node IDs to lists of
  neighbor IDs. The BFS function takes a starting node and a set of visited nodes to avoid cycles.
  It recursively explores each unvisited neighbor, accumulating the visited nodes in a list.
-/

open Std

partial def bfs (graph : HashMap Nat (List Nat)) (queue : List Nat) (visited : HashSet Nat) (result : List Nat) : List Nat :=
  match queue with
    | [] => result
    | node :: rest =>
      if visited.contains node then
        bfs graph rest visited result
      else
        let neighbours := graph.getD node []
        let newVisited := visited.insert node
        let newQueue := rest ++ neighbours
        bfs graph newQueue newVisited (result ++ [node])

def graph : Std.HashMap Nat (List Nat) :=
  Std.HashMap.ofList [(1, [5, 3]), (2, [4]), (3, [1, 4, 5]), (4, []), (5, [6]), (6, [1, 2])]

#eval bfs graph [1] (Std.HashSet.ofList []) []
