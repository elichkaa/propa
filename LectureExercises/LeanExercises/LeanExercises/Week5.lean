import Testing

/-
Complete the functions ending with `Acc` with an accumulator version of their original.
The functions ending with `Acc` don't have to be tail-recursive.
-/

def factorial (n: Nat) : Nat :=
  match n with
  | 0 => 1
  | n + 1 => (n + 1) * factorial n

def factorialAcc (n: Nat) : Nat :=
  aux n 1
  where aux n acc :=
      match n with
        | 0 => acc
        | n + 1 => (factorial n) * (acc + n)

#assert (factorial 0) == factorialAcc 0
#assert (factorial 1) == factorialAcc 1
#assert (factorial 2) == factorialAcc 2
#assert (factorial 3) == factorialAcc 3
#assert (factorial 5) == factorialAcc 5
#assert (factorial 10) == factorialAcc 10

def fib (n: Nat) : Nat :=
  match n with
  | 0 => 0
  | 1 => 1
  | n + 1 => fib n + fib (n - 1)

/-
This is how you would write the fibonacci function efficiently using iteration:

```python
def fib(n):
  a = 0
  b = 1
  for x in range(0, n):
    tmp = a + b
    a = b
    b = tmp
  return a
```

Write an accumulator version analogous to the iterative version above.
-/
def fibAcc (n: Nat) : Nat :=
  aux n 0
  where aux n acc :=
    match n with
    | 0 => acc
    | 1 => acc + 1
    | n + 1 => aux n acc + aux (n - 1) (acc - n)

#assert (fib 0) == fibAcc 0
#assert (fib 1) == fibAcc 1
#assert (fib 2) == fibAcc 2
#assert (fib 7) == fibAcc 7
#assert (fib 10) == fibAcc 10
#assert (fib 20) == fibAcc 20


def map (f: α -> β) (list: List α) : List β :=
  match list with
  | [] => []
  | x :: xs => f x :: map f xs

def mapAcc (f: α -> β) (list: List α) : List β :=
  aux f list.reverse []
  where aux f list acc :=
    match list with
      | [] => acc
      | x :: xs => let mapped := f x
        aux f xs (mapped :: acc)

#assert (map (fun x => x % 2 == 0) [0, 1, 2, 3]) == mapAcc (fun x => x % 2 == 0) [0, 1, 2, 3]
#assert (map (fun x => 2 * x) [0, 1, 2, 3]) == mapAcc (fun x => 2 * x) [0, 1, 2, 3]

inductive BinaryTree (α : Type) where
  | empty
  | leaf (elem : α)
  | node (left : BinaryTree α) (right : BinaryTree α)

-- Collect all values in left-to-right order.
def BinaryTree.flatten (tree: BinaryTree α) : List α :=
  match tree with
  | empty => []
  | leaf elem => [elem]
  | node left right => flatten left ++ flatten right

def BinaryTree.flattenAcc (tree: BinaryTree α) : List α :=
  go tree []
where
  go tree acc :=
    match tree with
    | empty => acc
    | leaf elem => elem :: acc
    | node left right => go left (go right acc)

def emptyTree : BinaryTree Nat := .empty
def singleLeaf : BinaryTree Nat := .leaf 42
def binaryTree : BinaryTree Nat :=
  .node
    (.node (.leaf 1) (.leaf 2))
    (.node .empty (.leaf 3))

#assert emptyTree.flatten == emptyTree.flattenAcc
#assert singleLeaf.flatten == singleLeaf.flattenAcc
#assert binaryTree.flatten == binaryTree.flattenAcc

-- Collect all values stisfying the perdicate `p` in left-to-right order.
def BinaryTree.filter (tree: BinaryTree α) (p: α → Bool) : List α :=
  match tree with
  | empty => []
  | leaf val => if p val then [val] else []
  | node left right => filter left p ++ filter right p

def BinaryTree.filterAcc (tree: BinaryTree α) (p: α → Bool) : List α :=
  go tree p []
  where go tree p acc :=
    match tree with
      | empty => acc
      | leaf val => if p val then val :: acc else acc
      | node left right => go left p (go right p acc)

#assert (binaryTree.filter (fun x => x % 2 == 1)) == (binaryTree.filterAcc (fun x => x % 2 == 1))
#assert (singleLeaf.filter (fun x => x > 0)) == (singleLeaf.filterAcc (fun x => x > 0))

structure RoseTree (α : Type) where
  val: α
  children: List (RoseTree α)

-- Sum all values in the tree.
def RoseTree.sum (tree: RoseTree Nat) : Nat :=
  match tree with
  | { val, children } => val + sumChildren children
where
  sumChildren children :=
    match children with
    | [] => 0
    | tree' :: rest => tree'.sum + sumChildren rest

-- def RoseTree.sumAcc (tree: RoseTree Nat) : Nat :=
--   go tree.children tree.val
--   where go tree acc :=
--     match tree with
--       | [] => acc
--       | tree :: rest => go (tree.children ++ rest) (acc + tree.val)

def RoseTree.sumAcc (tree: RoseTree Nat) : Nat :=
  go tree 0
where
  go tree acc :=
    match tree with
    | { val, children } => sumChildren children (val + acc)
  sumChildren children acc :=
    match children with
    | [] => acc
    | tree :: rest => go tree (sumChildren rest acc)

def singleRose : RoseTree Nat := { val := 42, children := [] }
def roseTree : RoseTree Nat :=
  { val := 10, children := [
    { val := 5, children := [
        { val := 1, children := []},
        { val := 2, children := []}
      ] },
    { val := 8, children := [] }
  ] }

#assert singleRose.sum == singleRose.sumAcc
#assert roseTree.sum == roseTree.sumAcc

-- Collect all values in depth-first order.
def RoseTree.flatten (tree: RoseTree α) : List α :=
  match tree with
  | { val, children } => val :: flattenChildren children
where
  flattenChildren children :=
    match children with
    | [] => []
    | tree :: rest => tree.flatten ++ flattenChildren rest

-- def RoseTree.flattenAcc (tree: RoseTree α) : List α :=
--   go tree.children [tree.val]
--   where go tree acc :=
--     match tree with
--       | [] => acc
--       | x :: xs => go (x.children ++ xs) (acc ++ [x.val])

def RoseTree.flattenAcc (tree: RoseTree α) : List α :=
  -- SOLUTION
  (go tree []).reverse
where
  go tree acc : List α :=
    match tree with
    | { val, children } => flattenChildren children (val :: acc)
  flattenChildren children acc :=
    match children with
    | [] => acc
    | tree :: rest => flattenChildren rest (go tree acc) -- **NOT** tail-recursive
  -- END

#assert singleRose.flatten == singleRose.flattenAcc
#assert roseTree.flatten == roseTree.flattenAcc

-- Check if the `target` value exists anywhere in the tree.
def BinaryTree.contains (target : α) [BEq α] (tree: BinaryTree α) : Bool :=
  match tree with
  | empty => false
  | leaf val => val == target
  | node left right => left.contains target || right.contains target

/-
Implement a **tail-recursive** version of `BinaryTree.contains`.
Use an explicit stack as a list of trees to keep track of remaining (sub-)trees. Initialize it with
the initial `tree`, so your helper function should be called with `[tree]` as the only argument.

Since the explicit stack is allocated on the heap, it avoids overflowing the implicit stack which
can happen if `BinaryTree.contains` is applied on a huge tree.
-/
partial def BinaryTree.containsTR (target : α) [BEq α] (tree: BinaryTree α) : Bool :=
  go target [tree]
  where go target trees :=
    match trees with
      | [] => false
      | empty :: rest => go target rest
      | leaf val :: rest => if val == target then true else go target rest
      | node left right :: rest  => go target (left :: rest) || go target (right :: rest)

#assert (binaryTree.contains 3) == (binaryTree.containsTR 3)
#assert (binaryTree.contains 9) == (binaryTree.containsTR 9)
