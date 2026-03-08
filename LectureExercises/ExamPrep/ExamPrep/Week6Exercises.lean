import Testing
import Plausible

open Plausible

namespace SpecificationExercises

-- ============================================================
-- ### Exercise 1: specify and test reverse
-- Our implementation of reverse below has a bug.
-- Write Boolean properties to specify reverse, use plausible
-- to find the bug, then fix it.
-- ============================================================

def myReverse (xs : List α) : List α :=
  match xs with
  | [] => []
  | x :: xs => [x] ++ myReverse xs  -- buggy

-- Property 1: reversing preserves length
def reverseSameLength (xs : List Nat) : Bool :=
  (myReverse xs).length == xs.length

-- Property 2: reversing twice gives back the original
def reverseInvolution (xs : List Nat) : Bool :=
  sorry

-- Property 3: reverse distributes over append (with flipped sides)
def reverseAppend (xs ys : List Nat) : Bool :=
  sorry

-- Property 4: reverse of a singleton is itself
def reverseSingleton (x : Nat) : Bool :=
  sorry

-- use plausible to find which properties fail:
example (xs : List Nat) : reverseSameLength xs := by plausible
-- add more examples here...

-- fix myReverse here:
def myReverseFixed (xs : List α) : List α := sorry

-- verify the fixed version agrees with the standard library:
example (xs : List Nat) : myReverseFixed xs = xs.reverse := by plausible


-- ============================================================
-- ### Exercise 2: specify and test quicksort
-- The quicksort below drops duplicate elements.
-- Write properties to catch the bug, then fix it.
-- ============================================================

partial def myQuickSort [Ord α] (xs : List α) : List α :=
  match xs with
  | [] => []
  | pivot :: rest =>
    let smaller := rest.filter (fun x => Ord.compare x pivot |>.isLT)
    let greater := rest.filter (fun x => Ord.compare x pivot |>.isGT)
    myQuickSort smaller ++ [pivot] ++ myQuickSort greater

-- Property: output is sorted (adjacent elements are ordered)
def isSorted (xs : List Int) : Bool :=
  sorry

def sortedOutput (xs : List Int) : Bool :=
  isSorted (myQuickSort xs)

-- Property: output is a permutation (same count of each element)
def isPermutation (xs ys : List Int) : Bool :=
  xs.length == ys.length &&
  xs.all (fun x => xs.countP (· == x) == ys.countP (· == x))

def permutationProperty (xs : List Int) : Bool :=
  isPermutation (myQuickSort xs) xs

-- test with plausible:
example (xs : List Int) : sortedOutput xs := by plausible
example (xs : List Int) : permutationProperty xs := by plausible

-- fix quicksort:
partial def myQuickSortFixed [Ord α] (xs : List α) : List α := sorry

example (xs : List Int) : isPermutation (myQuickSortFixed xs) xs := by plausible
example (xs : List Int) : isSorted (myQuickSortFixed xs) := by plausible


-- ============================================================
-- ### Exercise 3: specify insertion sort and verify it agrees
-- with the standard library mergeSort.
-- ============================================================

def insertSorted (x : Nat) (xs : List Nat) : List Nat :=
  match xs with
  | [] => [x]
  | y :: ys => if x <= y then x :: y :: ys else y :: insertSorted x ys

def insertionSort (xs : List Nat) : List Nat :=
  xs.foldl (fun acc x => insertSorted x acc) []

-- specify: output is sorted
def insertionSortSorted (xs : List Nat) : Bool := sorry

-- specify: output is permutation of input
def insertionSortPermutation (xs : List Nat) : Bool := sorry

-- specify: agrees with mergeSort reference implementation
def insertionSortAgreesWithMerge (xs : List Nat) : Bool := sorry

example (xs : List Nat) : insertionSortSorted xs := by plausible
example (xs : List Nat) : insertionSortPermutation xs := by plausible
example (xs : List Nat) : insertionSortAgreesWithMerge xs := by plausible


-- ============================================================
-- ### Exercise 4: BST well-formedness
-- Define a Boolean property WellFormed for BSTs.
-- Then test that insert preserves well-formedness.
-- ============================================================

inductive BST (α : Type) where
  | empty
  | node (v : α) (l r : BST α)
  deriving Repr, BEq

def BST.toList : BST α → List α → List α
  | empty, acc => acc
  | node v l r, acc => l.toList (v :: r.toList acc)

def BST.insert [Ord α] (t : BST α) (x : α) : BST α :=
  match t with
  | empty => node x empty empty
  | node v l r =>
    match Ord.compare x v with
    | .lt => node v (l.insert x) r
    | .gt => node v l (r.insert x)
    | .eq => node v l r

def BST.fromList [Ord α] (xs : List α) : BST α :=
  xs.foldl BST.insert empty

-- define well-formedness: every value in left subtree < node value
-- and every value in right subtree > node value, recursively
def BST.WellFormed [Ord α] (t : BST α) : Bool := sorry

-- test: fromList always produces a well-formed BST
example (xs : List Nat) : (BST.fromList xs).WellFormed := by plausible

-- test: insert preserves well-formedness (use fromList to get valid input)
example (xs : List Nat) (x : Nat) :
  (BST.fromList xs).insert x |>.WellFormed := by plausible

-- test: toList of a well-formed BST is sorted
example (xs : List Nat) :
  isSorted ((BST.fromList xs).toList []).map Int.ofNat := by plausible


-- ============================================================
-- ### Exercise 5: specify BST membership
-- After inserting x into a BST, x should be findable.
-- After inserting x, all previously present elements should
-- still be present.
-- ============================================================

def BST.contains [Ord α] (t : BST α) (x : α) : Bool :=
  match t with
  | empty => false
  | node v l r =>
    match Ord.compare x v with
    | .eq => true
    | .lt => l.contains x
    | .gt => r.contains x

-- property: inserted element is found
def insertContains (xs : List Nat) (x : Nat) : Bool :=
  sorry

-- property: existing elements still present after insert
def insertPreservesExisting (xs : List Nat) (x y : Nat) : Bool :=
  sorry

example (xs : List Nat) (x : Nat) : insertContains xs x := by plausible
example (xs : List Nat) (x y : Nat) : insertPreservesExisting xs x y := by plausible


-- ============================================================
-- ### Exercise 6: specify and test flatten for a Tree
-- ============================================================

inductive Tree (α : Type) where
  | leaf (v : α)
  | node (l r : Tree α)
  deriving Repr, BEq

instance [Arbitrary α] : Arbitrary (Tree α) where
  arbitrary := Gen.sized go
  where go : Nat → Gen (Tree α)
    | 0 => return Tree.leaf default
    | n + 1 => Gen.frequency (default := return Tree.leaf default)
        [ (2, do return Tree.leaf (← arbitrary))
        , (3, do return Tree.node (← go (n/2)) (← go (n/2))) ]

instance : Shrinkable (Tree α) where
  shrink | .leaf _ => [] | .node l r => [l, r]

def Tree.flatten : Tree α → List α
  | leaf v => [v]
  | node l r => l.flatten ++ r.flatten

def Tree.size : Tree α → Nat
  | leaf _ => 1
  | node l r => l.size + r.size

-- property: flatten length equals tree size
def flattenLengthEqualsSize (t : Tree Nat) : Bool := sorry

-- property: flatten of node = flatten of left ++ flatten of right
def flattenNodeProperty (l r : Tree Nat) : Bool := sorry

-- property: flatten of leaf is singleton
def flattenLeafProperty (x : Nat) : Bool := sorry

example (t : Tree Nat) : flattenLengthEqualsSize t := by plausible
example (l r : Tree Nat) : flattenNodeProperty l r := by plausible
example (x : Nat) : flattenLeafProperty x := by plausible


-- ============================================================
-- ### Exercise 7: specify map properties
-- ============================================================

def myMap (f : α → β) : List α → List β
  | [] => []
  | x :: xs => f x :: myMap f xs

-- property: map preserves length
def mapPreservesLength (xs : List Nat) : Bool := sorry

-- property: map with identity is identity
def mapIdentity (xs : List Nat) : Bool := sorry

-- property: map distributes over append
def mapAppend (xs ys : List Nat) : Bool := sorry

-- property: map composes (map (f ∘ g) = map f ∘ map g)
def mapCompose (xs : List Nat) : Bool :=
  let f := (· * 2)
  let g := (· + 1)
  myMap (f ∘ g) xs == (myMap f (myMap g xs))

example (xs : List Nat) : mapPreservesLength xs := by plausible
example (xs : List Nat) : mapIdentity xs := by plausible
example (xs ys : List Nat) : mapAppend xs ys := by plausible
example (xs : List Nat) : mapCompose xs := by plausible


-- ============================================================
-- ### Exercise 8: find bugs with plausible
-- The function below is supposed to remove duplicates from a
-- sorted list. It has a bug. Write a property to catch it
-- and fix the function.
-- ============================================================

def dedupSorted (xs : List Nat) : List Nat :=
  match xs with
  | [] => []
  | [x] => [x]
  | x :: y :: rest =>
    if x == y then x :: dedupSorted rest  -- bug: should skip x not keep it
    else x :: dedupSorted (y :: rest)

-- property: no duplicates in output (all adjacent elements differ)
def noDuplicates (xs : List Nat) : Bool := sorry

-- property: dedup of a sorted list is sorted
def dedupPreservesSorted (xs : List Nat) : Bool := sorry

-- property: every element in output was in input
def dedupSubset (xs : List Nat) : Bool := sorry

example (xs : List Nat) :
  noDuplicates (dedupSorted xs.mergeSort) := by plausible

def dedupSortedFixed (xs : List Nat) : List Nat := sorry

example (xs : List Nat) :
  noDuplicates (dedupSortedFixed xs.mergeSort) := by plausible


-- ============================================================
-- ### Exercise 9: specify filter properties
-- ============================================================

def myFilter (p : Nat → Bool) (xs : List Nat) : List Nat :=
  match xs with
  | [] => []
  | x :: xs => if p x then x :: myFilter p xs else myFilter p xs

-- property: all elements in output satisfy p
def filterSatisfies (xs : List Nat) : Bool :=
  let p := (· > 2)
  (myFilter p xs).all p

-- property: filter result is a sublist (length ≤ original)
def filterShorter (xs : List Nat) : Bool := sorry

-- property: filter then filter = filter with combined predicate
def filterIdempotent (xs : List Nat) : Bool :=
  let p := (· > 2)
  myFilter p (myFilter p xs) == myFilter p xs

-- property: filter distributes over append
def filterAppend (xs ys : List Nat) : Bool := sorry

example (xs : List Nat) : filterSatisfies xs := by plausible
example (xs : List Nat) : filterShorter xs := by plausible
example (xs : List Nat) : filterIdempotent xs := by plausible
example (xs ys : List Nat) : filterAppend xs ys := by plausible


-- ============================================================
-- ### Exercise 10: encode-decode roundtrip
-- A simple run-length encoder that compresses consecutive
-- duplicates. Specify and test the roundtrip property.
-- ============================================================

def encode (xs : List Nat) : List (Nat × Nat) :=
  xs.foldl (fun acc x =>
    match acc with
    | [] => [(x, 1)]
    | (v, c) :: rest =>
      if v == x then (v, c + 1) :: rest
      else (x, 1) :: acc
  ) [] |>.reverse

def decode (xs : List (Nat × Nat)) : List Nat :=
  xs.flatMap (fun (v, c) => List.replicate c v)

-- property: decode (encode xs) = xs (roundtrip)
def encodeDecodeRoundtrip (xs : List Nat) : Bool := sorry

-- property: encode never produces pairs with count 0
def encodeNonZeroCounts (xs : List Nat) : Bool := sorry

-- property: total count in encoded equals original length
def encodeLengthPreserved (xs : List Nat) : Bool := sorry

example (xs : List Nat) : encodeDecodeRoundtrip xs := by plausible
example (xs : List Nat) : encodeNonZeroCounts xs := by plausible
example (xs : List Nat) : encodeLengthPreserved xs := by plausible


end SpecificationExercises
