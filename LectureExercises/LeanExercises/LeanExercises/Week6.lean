import Testing
import Plausible
import LeanExercises.SpecificationsTesting

open Plausible

/-
In the lecture, you have seen one way of specifying whether a list is sorted:

abbrev List.isSorted [Ord α] [Inhabited α] (l : List α) (i j : Nat) : Prop :=
  i < j ∧ j < l.length → (Ord.compare l[i]? l[j]? |>.isLE)

However, this definition is not optimal for property-based testing.
Since `i` and `j` are generated randomly, they sometimes fail to satisfy the precondition
`i < j ∧ j < l.length`.
When the precondition is false, the implication becomes vacuously true.
This means the test will pass even if l is not actually sorted. For example:
-/

#eval List.isSorted [3, 2, 3] 1 0

/-
Thus, in this exercise, let's define a better sorted property for list,
which only takes a list as input to eliminate the problem introduced by `i` and `j`.
From what you have learned so far, it is not possible to define such property with the type `Prop`.
Instead, you can define it as a **recursive** function that returns a value of type `Bool`.
-/

def List.isSorted' [Ord α] (l : List α) : Bool :=
  match l with
  | [] => true
  | [_] => true
  | x :: y :: l' =>
    match Ord.compare x y with
    | Ordering.gt => false
    | _ => isSorted' (y :: l')

-- As the following example shows, `plausible` still works when the property is a boolean.
example (l : List Nat) :
  l.quicksort.isSorted' := by plausible


/-
Maybe you start to wonder what the difference is between `Prop` and `Bool`.
You can understand it as a property of type `Bool` is *computable* (or terminates),
but a `Prop` property can be non-terminating. This means `Prop` is more expressive than `Bool`.
Nevertheless, we recommend you to define properties as `Bool` instead of `Prop`,
which allows you to define more interesting properties like `List.isSorted'`.
In the rest of the exercises, all properties can be written as a boolean function.
-/

open Treesort.BinaryTree
namespace Treesort
/-
In the lecture, you have seen the binary search tree (BST) data structure.
In fact, not all binary trees are BSTs.
For example, the following tree is not a BST, because the left child is greater than the parent.
-/
def illFormedBST :=
  node 1 (node 2 empty empty) (node 3 empty empty)


/-
#### Exercise 1
Define a property `WellFormed` which checks if a binary tree is a binary search tree.
And try to define several examples to check if you implement this property correctly.
-/
def BinaryTree.compare [Ord α] (tree : BinaryTree α) (elem: α) (p: α -> α -> Bool): Bool :=
  match tree with
    | empty => true
    | node elem' l r =>
      match (p elem elem') with
        | true => false
        | _ => l.compare elem p && r.compare elem p

def BinaryTree.WellFormed [Ord α] (tree : BinaryTree α) : Bool :=
  match tree with
    | empty => true
    | node _ empty empty => true
    | node elem left right =>
      left.compare elem (fun x y => (Ord.compare x y).isLT) &&
      right.compare  elem (fun x y => (Ord.compare x y).isGT) &&
      left.WellFormed && right.WellFormed

-- You can test that a tree built from `BinaryTree.fromList` is well-formed.
example (l : List Nat) :
  (BinaryTree.fromList l).WellFormed := by plausible

-- Also, a well-formed tree should be sorted when converting back to a list.
example (tree : BinaryTree Nat) :
  WellFormed tree → (tree.toList []).isSorted' := by plausible


/-
#### Exercise 2
A correct implementation of `insert` requires that, if a binary tree is a binary search tree,
after an insertion, the elements in the binary search tree are still sorted.
Define this property using `List.isSorted'` and `WellFormed` separately.
-/

-- With `List.isSorted'`
def insertPreservesSortedness₁ [Ord α] (tree : BinaryTree α) (a : α) : Bool :=
  WellFormed tree -> List.isSorted' ((tree.insert a).toList [])

-- With `BinaryTree.WellFormed`
def insertPreservesSortedness₂ [Ord α] (tree : BinaryTree α) (a : α) : Bool :=
  WellFormed tree -> WellFormed (tree.insert a)

/-
It is not straightforward to check whether this property holds with property-based testing.
You may test it this way:
-/
example (tree : BinaryTree Nat) (a : Nat) :
  WellFormed tree → WellFormed (tree.insert a) := by plausible

/-
It works, but it does not mean that `insert` is correct. For example, if the random generator
produces an invalid tree (e.g. `node 5 (node 10 empty empty) empty`), the precondition
`WellFormed tree` evaluates to `false`. Therefore, the test passes "vacuously" (trivially) simply
because the input was bad, without ever actually testing if the `insert` logic is correct.

To fix this, we can rely on `BinaryTree.fromList`, which is known to always return a well-formed
tree. Instead of generating a random tree directly, we ask the generator for a random `List`,
convert it to a tree, and then run the test. This ensures the precondition holds, forcing the test
to meaningfully verify the `insert` logic.
-/

example (lst : List Nat) (a : Nat) :
  let tree := BinaryTree.fromList lst
  insertPreservesSortedness₁ tree a := by plausible

example (lst : List Nat) (a : Nat) :
  let tree := BinaryTree.fromList lst
  insertPreservesSortedness₂ tree a := by plausible

/-
#### Exercise 3
Maybe you've realized that, a correct `insert` for binary trees not only requires preserving the
sorted property. Indeed, we need to check whether that new element is actually in the tree after
the insertion. Define this property and test whether `BinaryTree.insert` satisfies this property
using the technique we showed above.
-/

def insertIsCorrect (t : BinaryTree Nat) (x : Nat) : Bool :=
  let tree_result := (t.insert x).toList []
  let list_result := (t.toList [] ++ [x]).mergeSort
  tree_result == list_result

example (l : List Nat) (x : Nat) :
  insertIsCorrect (BinaryTree.fromList l) x := by plausible

/-
#### Exercise 4
While inserting into a BST is straightforward,
deletion is complex because it requires
re-structuring the tree to preserve the ordering invariant.

Below is an attempt at implementing delete.

Your task is to:

1. Formalize the correctness properties (Is the result still a BST? Is the element actually gone?).

2. Use these properties to test the function and repair any bugs.
-/

def BinaryTree.splitMin [Ord α] (tree : BinaryTree α) : Option (α × BinaryTree α) :=
  match tree with
  | empty => none
  | node k l r =>
    match l.splitMin with
    | none => some (k, r)
    | some (min, newL) => some (min, node k newL r)

def BinaryTree.delete [Ord α] (tree : BinaryTree α) (x : α) : BinaryTree α :=
  match tree with
  | empty => empty
  | node elem l r =>
    match Ord.compare elem x with
    | Ordering.lt => node elem l (r.delete x)
    | Ordering.gt => node elem (l.delete x) r
    | Ordering.eq =>
      match l, r with
      | _, empty => l
      | empty, _ => r
      | _, _ =>
        match r.splitMin with
        | none => l
        | some (elem', newR) => node elem' l newR


-- Please write your specifications and perform
-- property-based testing at here:
example (l : List Nat) (a : Nat) :
  let tree := BinaryTree.fromList l
  WellFormed tree → WellFormed (tree.delete a) := by plausible

def deleteIsCorrect (tree : BinaryTree Nat) (x : Nat) : Bool :=
  let tree_result := (tree.delete x).toList []
  let list_result := (tree.toList []).erase x
  tree_result == list_result

example (l : List Nat) (x : Nat) :
  deleteIsCorrect (BinaryTree.fromList l) x := by plausible

end Treesort


/-
#### Exercise 5: Merge Intervals
This exercise is about the following Leetcode problem:
https://leetcode.com/problems/merge-intervals/description/

> Given an array of intervals where intervals[i] =
[starti, endi], merge all overlapping intervals, and
return an array of the non-overlapping intervals that
cover all the intervals in the input.


> Example 1:
>
> Input: intervals = [[1,3],[2,6],[8,10],[15,18]]
> Output: [[1,6],[8,10],[15,18]]
> Explanation: Since intervals [1,3] and [2,6] overlap, merge them into [1,6].

> Example 2:
>
> Input: intervals = [[1,4],[4,5]]
> Output: [[1,5]]
> Explanation: Intervals [1,4] and [4,5] are considered overlapping.

> Example 3:
>
> Input: intervals = [[4,7],[1,4]]
> Output: [[1,7]]
> Explanation: Intervals [1,4] and [4,7] are considered overlapping.

The function `mergeIntervals` is a buggy solution
of this problem,
identify properties of this problem to find its
bugs and fix the solution.

-/

-- This structure defines the interval `[s,e]`.
structure Interval  where
  s : Nat -- Start point
  e : Nat -- End point
deriving BEq, Repr

-- We provide the generators for intervals so you can directly use them in property-based testing.
instance : Arbitrary Interval where
  arbitrary := do
    let n ← Gen.getSize
    let l ← Gen.chooseNatLt 0 (n + 1) (by grind)
    let r ← Gen.chooseNatLt l (n + 1) (by grind)
    pure ⟨l, r⟩

instance : Shrinkable Interval where

#sample Interval

-- This is the buggy solution
def mergeIntervals (intervals : List Interval) : List Interval :=
  let sorted := intervals.mergeSort (fun a b => a.s < b.s)
  match sorted with
  | [] => []
  | head :: tail =>
    tail.foldl (fun acc curr =>
      match acc with
      | [] => [curr]
      | last :: rest =>
        if last.e >= curr.s then
          let newEnd := max last.s curr.e
          ⟨last.e, newEnd⟩ :: rest
        else
          last :: curr :: rest
    ) [head] |>.reverse

-- Please write your specifications and perform
-- property-based testing at here:

def lessOrEqualLength (intervals: List Interval): Bool :=
  let merged := mergeIntervals intervals
  merged.length <= intervals.length

example (intervals : List Interval): lessOrEqualLength intervals :=
  by plausible

def allSLessThanE (intervals: List Interval): Bool :=
  let merged := mergeIntervals intervals
  merged |>.filter (fun i => i.s > i.e)|>.length == 0

example (intervals : List Interval): allSLessThanE intervals :=
  by plausible

/-

#### Exercise 6 Longest Substring Without Repeating Characters
This exercise is about the following Leetcode problem:

https://leetcode.com/problems/longest-substring-without-repeating-characters/?envType=problem-list-v2&envId=string

> Given a string s, find the length of the longest
substring without duplicate characters.

> Example 1:
>
> Input: s = "abcabcbb"
> Output: 3
> Explanation: The answer is "abc", with the length of 3. Note that "bca" and "cab" are also correct answers.

> Example 2:
>
> Input: s = "bbbbb"
> Output: 1
> Explanation: The answer is "b", with the length of 1.

> Example 3:
>
> Input: s = "pwwkew"
> Output: 3
> Explanation: The answer is "wke", with the length of 3.
> Notice that the answer must be a substring, "pwke" is a subsequence and not a substring.

Still, we provide you an efficient but buggy solution `lengthOfLongestSubstring`.

Similar to exercise 6, your task is to identify the
properties to test this solution and fix bugs.
-/

-- This is the buggy solution
def lengthOfLongestSubstring (s : String) : Nat :=
  go s.toList [] 0
  where go (rem : List Char) (curr : List Char) (maxLen : Nat) : Nat :=
    match rem with
    | [] => curr.length
    | c :: cs =>
      if curr.elem c then
        let newMax := max maxLen curr.length
        let suffix := curr.dropWhile (· != c)
        go cs (suffix ++ [c]) newMax
      else
        go cs (curr ++ [c]) maxLen

-- Please write your specifications and perform
-- property-based testing at here:
