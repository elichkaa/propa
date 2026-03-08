import Testing

namespace AccumulatorExercises

-- ============================================================
-- ### Exercise 1: tail-recursive sum
-- The naive version below is not tail-recursive.
-- Write a tail-recursive version using an accumulator.
-- ============================================================

def sum (xs : List Nat) : Nat :=
  match xs with
  | [] => 0
  | x :: xs => x + sum xs

def sumAcc (xs : List Nat) : Nat :=
  go xs 0
  where go xs acc :=
    match xs with
      | [] => acc
      | x :: rest => go rest (x + acc)

#assert (sum []) == sumAcc []
#assert (sum [1, 2, 3, 4, 5]) == sumAcc [1, 2, 3, 4, 5]
#assert (sum [10, 20, 30]) == sumAcc [10, 20, 30]


-- ============================================================
-- ### Exercise 2: tail-recursive product
-- ============================================================

def product (xs : List Nat) : Nat :=
  match xs with
  | [] => 1
  | x :: xs => x * product xs

def productAcc (xs : List Nat) : Nat :=
  go xs 1
  where go xs acc :=
    match xs with
      | [] => acc
      | x :: rest => go rest (x * acc)

#assert (product []) == productAcc []
#assert (product [1, 2, 3, 4, 5]) == productAcc [1, 2, 3, 4, 5]
#assert (product [10, 2, 5]) == productAcc [10, 2, 5]


-- ============================================================
-- ### Exercise 3: tail-recursive factorial
-- ============================================================

def factorial (n : Nat) : Nat :=
  match n with
  | 0 => 1
  | n + 1 => (n + 1) * factorial n

def factorialAcc (n : Nat) : Nat :=
  go n 1
  where go n acc :=
    match n with
      | 0 => acc
      | n + 1 => go n ((n + 1) * acc)

#assert (factorial 0) == factorialAcc 0
#assert (factorial 1) == factorialAcc 1
#assert (factorial 5) == factorialAcc 5
#assert (factorial 10) == factorialAcc 10


-- ============================================================
-- ### Exercise 4: tail-recursive fibonacci
-- The naive version is exponential. Write an efficient
-- tail-recursive version using two accumulators,
-- analogous to the iterative version:
--   a = 0, b = 1
--   for i in range(n): a, b = b, a + b
--   return a
-- ============================================================

def fib (n : Nat) : Nat :=
  match n with
  | 0 => 0
  | 1 => 1
  | n + 1 => fib n + fib (n - 1)

def fibAcc (n : Nat) : Nat :=
  go n 0 1
  where go n acc last :=
    match n with
      | 0 => acc
      | 1 => last
      | n + 1 => go n last (acc + last)

#assert (fib 0) == fibAcc 0
#assert (fib 1) == fibAcc 1
#assert (fib 2) == fibAcc 2
#assert (fib 7) == fibAcc 7
#assert (fib 10) == fibAcc 10


-- ============================================================
-- ### Exercise 5: tail-recursive map
-- ============================================================

def myMap (f : α → β) (xs : List α) : List β :=
  match xs with
  | [] => []
  | x :: xs => f x :: myMap f xs

def myMapAcc (f : α → β) (xs : List α) : List β :=
  go f xs []
  where go f xs acc :=
    match xs with
      | [] => acc
      | x :: rest => (f x) :: go f rest acc

#assert (myMap (· * 2) [1, 2, 3]) == myMapAcc (· * 2) [1, 2, 3]
#assert (myMap (· + 1) []) == myMapAcc (· + 1) []
#assert (myMap toString [1, 2, 3]) == myMapAcc toString [1, 2, 3]


-- ============================================================
-- ### Exercise 6: tail-recursive filter
-- ============================================================

def myFilter (p : α → Bool) (xs : List α) : List α :=
  match xs with
  | [] => []
  | x :: xs => if p x then x :: myFilter p xs else myFilter p xs

def myFilterAcc (p : α → Bool) (xs : List α) : List α :=
  go p xs []
  where go p xs acc :=
    match xs with
      | [] => acc
      | x :: rest => if p x then x :: go p rest acc else go p rest acc

#assert (myFilter (· > 2) [1, 2, 3, 4]) == myFilterAcc (· > 2) [1, 2, 3, 4]
#assert (myFilter (· > 2) []) == myFilterAcc (· > 2) []
#assert (myFilter (fun _ => false) [1, 2, 3]) == myFilterAcc (fun _ => false) [1, 2, 3]


-- ============================================================
-- ### Exercise 7: tail-recursive reverse
-- The naive version is O(n²). Write an O(n) version.
-- ============================================================

def myReverse (xs : List α) : List α :=
  match xs with
  | [] => []
  | x :: xs => myReverse xs ++ [x]

def myReverseAcc (xs : List α) : List α :=
  go xs []
  where go xs acc :=
    match xs with
      | [] => acc
      | x :: rest => go rest acc ++ (x :: acc)

#assert (myReverse ([] : List Nat)) == myReverseAcc []
#assert (myReverse [1, 2, 3]) == myReverseAcc [1, 2, 3]
#assert (myReverse [1]) == myReverseAcc [1]


-- ============================================================
-- ### Exercise 8: tail-recursive flatten
-- ============================================================

def myFlatten (xs : List (List α)) : List α :=
  match xs with
  | [] => []
  | x :: xs => x ++ myFlatten xs

def myFlattenAcc (xs : List (List α)) : List α :=
  go xs []
  where go xs acc :=
    match xs with
      | [] => acc
      | x :: rest => x ++ go rest acc

#assert (myFlatten [[1, 2], [3], [4, 5]]) == myFlattenAcc [[1, 2], [3], [4, 5]]
#assert (myFlatten ([] : List (List Nat))) == myFlattenAcc []
#assert (myFlatten [[], [1], []]) == myFlattenAcc [[], [1], []]


-- ============================================================
-- ### Exercise 9: analyze tail recursion
-- For each function below, state whether it is tail-recursive.
-- Then rewrite the non-tail-recursive ones with an accumulator.
-- ============================================================

-- Is this tail-recursive?
def countDown (n : Nat) : List Nat :=
  match n with
  | 0 => [0]
  | n + 1 => (n + 1) :: countDown n

#eval countDown 4

def countDownAcc (n : Nat) : List Nat :=
  go n []
  where go n acc :=
    match n with
    | 0 => acc ++ [0]
    | n + 1 => go n (acc ++ [n + 1])

#assert (countDown 5) == countDownAcc 5
#assert (countDown 0) == countDownAcc 0

-- Is this tail-recursive?
def doubled (xs : List Nat) : Nat :=
  match xs with
  | [] => 0
  | x :: xs => doubled xs + x * 2

def doubledAcc (xs : List Nat) : Nat :=
  go xs 0
  where go xs acc :=
    match xs with
      | [] => acc
      | x :: rest => go rest (x * 2 + acc)

#assert (doubled [1, 2, 3]) == doubledAcc [1, 2, 3]
#assert (doubled []) == doubledAcc []


-- ============================================================
-- ### Exercise 10: BinaryTree flatten with accumulator
-- The naive version uses ++ which is O(n) per call.
-- Write an O(n) version using an accumulator.
-- ============================================================

inductive BinaryTree (α : Type) where
  | empty
  | leaf (v : α)
  | node (l : BinaryTree α) (r : BinaryTree α)
  deriving Repr, BEq

open BinaryTree

def BinaryTree.flatten (t : BinaryTree α) : List α :=
  match t with
  | empty => []
  | leaf v => [v]
  | node l r => flatten l ++ flatten r

partial def BinaryTree.flattenAcc (t : BinaryTree α) : List α :=
  go [t] []
  where go stack acc :=
    match stack with
    | [] => acc
    | empty :: rest => go rest acc
    | leaf v :: rest => go rest (acc ++ [v])
    | node l r :: rest => go (l :: r :: rest) acc

def exTree := node (node (leaf 1) (leaf 2)) (node (leaf 3) empty)

#assert exTree.flatten == exTree.flattenAcc
#assert (empty : BinaryTree Nat).flatten == (empty : BinaryTree Nat).flattenAcc
#assert (leaf 42 : BinaryTree Nat).flatten == (leaf 42 : BinaryTree Nat).flattenAcc


-- ============================================================
-- ### Exercise 11: BinaryTree contains with explicit stack
-- The naive version is not tail-recursive because it recurses
-- on two subtrees. Write a tail-recursive version using an
-- explicit stack (List of remaining trees to visit).
-- ============================================================

def BinaryTree.contains [BEq α] (t : BinaryTree α) (x : α) : Bool :=
  match t with
  | empty => false
  | leaf v => v == x
  | node l r => l.contains x || r.contains x

partial def BinaryTree.containsTR [BEq α] (t : BinaryTree α) (x : α) : Bool :=
  go [t]
  where go stack :=
    match stack with
    | [] => false
    | empty :: rest => go rest
    | leaf v :: rest => v == x || go rest
    | node l r :: rest => go (l :: r :: rest)

#assert (exTree.contains 1) == exTree.containsTR 1
#assert (exTree.contains 3) == exTree.containsTR 3
#assert (exTree.contains 9) == exTree.containsTR 9
#assert ((empty : BinaryTree Nat).containsTR 1) == false


-- ============================================================
-- ### Exercise 12: BinaryTree sum with accumulator
-- ============================================================

def BinaryTree.sum (t : BinaryTree Int) : Int :=
  match t with
  | empty => 0
  | leaf v => v
  | node l r => sum l + sum r

partial def BinaryTree.sumAcc (t : BinaryTree Int) : Int :=
  go [t] 0
  where go stack acc :=
    match stack with
      | [] => acc
      | empty :: rest => go rest acc
      | leaf v :: rest => go rest (acc + v)
      | node l r :: rest => go (l :: r :: rest) acc

def exTreeInt := node (node (leaf (1 : Int)) (leaf 2)) (node (leaf 3) empty)

#assert exTreeInt.sum == exTreeInt.sumAcc


-- ============================================================
-- ### Exercise 13: RoseTree
-- A RoseTree is a tree where each node has a value and an
-- arbitrary number of children.
-- Implement sum and flatten with accumulators.
-- ============================================================

structure RoseTree (α : Type) where
  val : α
  children : List (RoseTree α)

partial def RoseTree.sum (t : RoseTree Nat) : Nat :=
  t.val + t.children.foldl (fun acc c => acc + c.sum) 0

partial def RoseTree.sumAcc (t : RoseTree Nat) : Nat :=
  go t.children t.val
  where go stack acc :=
    match stack with
    | [] => acc
    | t :: rest => go (t.children ++ rest) (acc + t.val)

partial def RoseTree.flatten (t : RoseTree α) : List α :=
  t.val :: t.children.foldl (fun acc c => acc ++ c.flatten) []

partial def RoseTree.flattenAcc (t : RoseTree α) : List α :=
  go t.children [t.val]
    where go stack acc :=
      match stack with
      | [] => acc
      | t :: rest => go (t.children ++ rest) (acc ++ [t.val])

def roseTree : RoseTree Nat := {
  val := 1,
  children := [
    { val := 2, children := [{ val := 4, children := [] }] },
    { val := 3, children := [] }
  ]
}

#assert roseTree.sum == roseTree.sumAcc
#assert roseTree.flatten == roseTree.flattenAcc


-- ============================================================
-- ### Exercise 14: power with accumulator
-- Write a tail-recursive version of exponentiation.
-- ============================================================

def power (base exp : Nat) : Nat :=
  match exp with
  | 0 => 1
  | n + 1 => base * power base n

def powerAcc (base exp : Nat) : Nat :=
  go base exp 1
  where go b e acc :=
    match e with
      | 0 => acc
      | e + 1 => go b e (b * acc)

#assert (power 2 0) == powerAcc 2 0
#assert (power 2 8) == powerAcc 2 8
#assert (power 3 5) == powerAcc 3 5
#assert (power 5 0) == powerAcc 5 0


-- ============================================================
-- ### Exercise 15: count elements with accumulator
-- ============================================================

def countWhere (p : α → Bool) (xs : List α) : Nat :=
  match xs with
  | [] => 0
  | x :: xs => (if p x then 1 else 0) + countWhere p xs

def countWhereAcc (p : α → Bool) (xs : List α) : Nat :=
  go p xs 0
  where go p xs acc :=
    match xs with
      | [] => acc
      | x :: rest => if p x then go p rest (acc + 1) else go p rest acc

#assert (countWhere (· > 2) [1, 2, 3, 4]) == countWhereAcc (· > 2) [1, 2, 3, 4]
#assert (countWhere (· > 2) []) == countWhereAcc (· > 2) []
#assert (countWhere (fun _ => true) [1, 2, 3]) == countWhereAcc (fun _ => true) [1, 2, 3]


end AccumulatorExercises
