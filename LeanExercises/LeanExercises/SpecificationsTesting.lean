import Plausible

namespace Lists

def reverse (xs : List α) : List α :=
  match xs with
  | [] => []
  | x :: xs => reverse xs ++ [x]

-- for any list xs, length (reverse xs) = length xs
abbrev reverse_same_length (xs : List α) : Prop :=
  (reverse xs).length = xs.length

-- for any list xs, reverse (reverse xs) = xs
abbrev reverse_involution (xs : List α) : Prop :=
  reverse (reverse xs) = xs

-- for any value x, reverse [x] = [x]
abbrev reverse_singleton_id (x : α) : Prop :=
  reverse [x] = [x]

-- for any lists xs and ys, reverse (xs ++ ys) = reverse ys ++ reverse xs
abbrev reverse_append (xs ys : List α) : Prop :=
  reverse (xs ++ ys) = reverse ys ++ reverse xs

#eval reverse_same_length [1,2,3]
#eval reverse_same_length [4,5,6]

#eval reverse_involution ([] : List Int)
#eval reverse_involution [1,2,3]

#eval reverse_singleton_id 42
#eval reverse_singleton_id "hello"

#eval reverse_append [1,2,3] []
#eval reverse_append [] [1,2,3]

-- set_option trace.plausible.success true

-- example (xs: List α): reverse_same_length xs :=
--   by plausible

-- example (xs: List α): reverse_involution xs :=
--   by plausible

-- example (x: α): reverse_singleton_id x :=
--   by plausible

-- example (xs ys: List α): reverse_append xs ys :=
--   by plausible (config := {numInst := 1000, maxSize := 1000})

def reverseTRAux (ys acc : List α) : List α :=
  match ys with
  | [] => acc
  | y :: ys' => reverseTRAux ys' (y :: acc)

def reverseTR (xs : List α) : List α :=
  reverseTRAux xs []

-- example (xs: List α): (reverseTR xs).length = xs.length :=
--   by plausible

-- example (xs : List α) : reverseTR xs = reverse xs := by
--   plausible

namespace Theorems

theorem reverse_same_length : (reverse xs).length = xs.length := by
  induction xs with
  | nil => simp [reverse]
  | cons x xs ih => simp [reverse]; apply ih

theorem reverse_involution_aux : reverse (xs ++ [x]) = x :: reverse xs := by
  induction xs with
  | nil => simp [reverse]
  | cons y ys ih => simp [reverse]; rw [ih]; trivial

theorem reverse_involution : reverse (reverse xs) = xs := by
  induction xs with
  | nil => simp [reverse]
  | cons x xs ih => simp [reverse]; rw [reverse_involution_aux, ih]

theorem reverse_singleton_id : reverse [x] = [x] := by
  simp [reverse]

theorem reverse_append : reverse (xs ++ ys) = reverse ys ++ reverse xs := by
  induction xs with
  | nil => simp [reverse]
  | cons x xs ih => simp [reverse]; rw [ih]; simp

end Theorems

end Lists

partial def List.quicksort [Ord α] (list : List α) : List α :=
  match list with
  | .nil => .nil  -- Base case: an empty list is already sorted
  | pivot :: tail =>
    let left := tail.filter (λ x => Ord.compare x pivot |>.isLE)      -- Elements less than pivot
    let right := tail.filter (λ x => Ord.compare x pivot |>.isGT)     -- Elements greater than pivot
    (quicksort left) ++ (pivot :: (quicksort right))  -- Combine the sorted sublists with the pivot

#eval List.quicksort [3, 1, 4, 2, 5]
#eval List.quicksort [5, 4, 3, 2, 1]
#eval List.quicksort [1]

abbrev List.isSorted [Ord α] [Inhabited α] (l : List α) (i j : Nat) : Prop :=
  i < j ∧ j < l.length → (Ord.compare l[i]? l[j]? |>.isLE)

example (xs : List Int) (i j : Nat) : xs.quicksort.isSorted i j := by
  plausible

abbrev List.isPermutation [Ord α] (l1 l2: List α) (x: α): Prop :=
  l1.countP (fun y => Ord.compare x y |>.isEq) = l2.countP (fun y => Ord.compare x y |>.isEq)

example (xs : List Int) (x : Int) : xs.quicksort.isPermutation xs x := by
  plausible

structure Bounded (lower upper : Nat) where
  num : Nat
deriving Repr

open Plausible

instance : Arbitrary (Bounded lower upper) where
  arbitrary := do
    let size ← Gen.getSize -- the target size
    if h: size == 0 || (upper - lower) <= 0
    then Except.error (GenError.genError "impossible to generate value")
    else Bounded.mk <$> Gen.chooseNatLt lower upper (by grind)

instance : Shrinkable (Bounded lower upper) where

set_option trace.plausible.success true
example (l : List Int) (i : Bounded 0 (l.length - 1)) (j : Bounded (i.num + 1) l.length) :
  l.quicksort.isSorted i.num j.num := by
  plausible

namespace Treesort

inductive BinaryTree (α : Type) where
| empty
| node (elem : α) (left : BinaryTree α) (right : BinaryTree α)

def BinaryTree.toString [ToString α] : BinaryTree α → String
  | empty => "∅"
  | node elem left right =>
    "[" ++ ToString.toString elem ++ ": " ++ left.toString ++ " | " ++ right.toString ++ "]"

instance [ToString α] : Repr (BinaryTree α) where
  reprPrec tree _ := tree.toString

def BinaryTree.toList : BinaryTree α → List α → List α
  | empty, acc => acc
  | node elem left right, acc =>
    left.toList (elem :: right.toList acc)

def BinaryTree.insert [Ord α] (x : α) : BinaryTree α → BinaryTree α
  | empty => node x empty empty
  | node elem left right =>
    match Ord.compare x elem with
    | .lt => node elem (insert x left) right
    | _ => node elem left (insert x right)

def BinaryTree.fromList [Ord α] (lst : List α) : BinaryTree α :=
  lst.foldl (fun tree x => tree.insert x) empty

def treesort [Ord α] (lst : List α) : List α :=
  BinaryTree.fromList lst |>.toList []

example (xs : List Int) (i j : Nat) : treesort xs |>.isSorted i j := by
  plausible

example (xs : List Int) (x : Int) : treesort xs |>.isPermutation xs x := by
  plausible


-- uncomment the following lines to see the test results
-- example (t : BinaryTree Int) (a : Int) : (t.insert a |>.sum) = t.sum + a := by
--   plausible

/-
  Plausible tells us "Failed to create a `testable` instance" to indicate that it does
  not know how to generate random `BinaryTree` values. To fix this, we need to define
  two instances for `BinaryTree`:  `Arbitrary` and `Shrinkable`.
-/

open Plausible
open Plausible.Arbitrary

def BinaryTree.sum : BinaryTree Int → Int
  | empty => 0
  | node elem left right => elem + (sum left) + (sum right)

/- Enumerate randomly generated binary trees -/
instance [Arbitrary α] : Arbitrary (BinaryTree α) where
  /- We need to provide a Gen (BinaryTree α). `Gen.sized` uses
     the size parameter to control the size of generated values. -/
  arbitrary := Gen.sized go
  where
    /- Given a target size, we generate random binary trees. -/
    go : Nat → Gen (BinaryTree α)
    | 0 => return BinaryTree.empty /- Size = 0 => empty tree-/
    | n+1 =>
        /- Otherwise, generate instances based on weight -/
        Gen.frequency (default := return BinaryTree.empty)
          [ (1, return BinaryTree.empty)
          , (3, do
              let elem ← arbitrary     /- choose an arbitrary element -/
              let left ← go (n / 2)    /- generate left subtree of size n/2 -/
              let right ← go (n / 2)   /- generate right subtree of size n/2 -/
              return BinaryTree.node elem left right)
          ]

/- When a counter example is found, it is often overly complicated. The `shrink`
   function suggests ways to reduce the example. The framework then tests the
   shrunk examples, or reports the original one. -/
instance : Shrinkable (BinaryTree α) where
  shrink
  | .empty => []
  | .node _ left right => [left, right]

-- example (t : BinaryTree Int) (a : Int) : (t.insert a |>.sum) = t.sum + a := by
--   plausible
