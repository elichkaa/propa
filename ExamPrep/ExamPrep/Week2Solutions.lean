import Testing

namespace RecursionExercises

-- ============================================================
-- ### Exercise 1: sum of digits
-- Given a natural number, return the sum of its digits.
-- ============================================================

def sumDigits (n : Nat) : Nat :=
  if n < 10 then n
  else (n % 10) + sumDigits (n / 10)
termination_by n

#assert (sumDigits 0) == 0
#assert (sumDigits 5) == 5
#assert (sumDigits 12) == 3
#assert (sumDigits 99) == 18
#assert (sumDigits 123) == 6
#assert (sumDigits 9999) == 36


-- ============================================================
-- ### Exercise 2: replicate
-- Given a natural number `n` and a value `x`, return a list
-- containing `n` copies of `x`.
-- ============================================================

def replicate (n : Nat) (x : α) : List α :=
  match n with
    | 0 => []
    | n + 1 => x :: replicate n x

#assert (replicate 0 'a') == []
#assert (replicate 1 true) == [true]
#assert (replicate 3 0) == [0, 0, 0]
#assert (replicate 4 "hi") == ["hi", "hi", "hi", "hi"]


-- ============================================================
-- ### Exercise 3: zipWith
-- Given a function `f` and two lists, return a list where each
-- element is the result of applying `f` to the corresponding
-- elements. Stops at the shorter list.
-- ============================================================

def zipWith (f : α → β → γ) (xs : List α) (ys : List β) : List γ :=
  match xs, ys with
    | _, [] => []
    | [], _ => []
    | x :: xs, y :: ys => (f x y) :: zipWith f xs ys

#assert (zipWith (· + ·) [1, 2, 3] [4, 5, 6]) == [5, 7, 9]
#assert (zipWith (· + ·) [1, 2] [4, 5, 6]) == [5, 7]
#assert (zipWith (· && ·) [true, false] [true, true]) == [true, false]
#assert (zipWith (fun a b => (a, b)) [1, 2] ["a", "b"]) == [(1, "a"), (2, "b")]
#assert (zipWith (· * ·) ([] : List Int) [1, 2]) == []


-- ============================================================
-- ### Exercise 4: take and drop
-- ============================================================

def take (n : Nat) (xs : List α) : List α :=
  match n, xs with
    | 0, _ => []
    | _, [] => []
    | n + 1, x :: xs => x :: (take n xs)

def drop (n : Nat) (xs : List α) : List α :=
  match n, xs with
    | 0, _ => xs
    | _, [] => []
    | n + 1, _ :: xs => drop n xs

#assert (take 2 [1, 2, 3, 4]) == [1, 2]
#assert (take 0 [1, 2, 3]) == []
#assert (take 5 [1, 2]) == [1, 2]
#assert (drop 2 [1, 2, 3, 4]) == [3, 4]
#assert (drop 0 [1, 2, 3]) == [1, 2, 3]
#assert (drop 5 [1, 2]) == []


-- ============================================================
-- ### Exercise 5: last element
-- ============================================================

def last? (xs : List α) : Option α :=
  match xs with
    | [] => none
    | [x] => some x
    | _ :: xs => last? xs

#assert (last? ([] : List Nat)) == none
#assert (last? [1]) == some 1
#assert (last? [1, 2, 3]) == some 3
#assert (last? ["a", "b", "c"]) == some "c"


-- ============================================================
-- ### Exercise 6: interleave
-- Merge two lists by alternating elements starting with the
-- first. Once one is exhausted, append the remainder.
-- ============================================================

def interleave (xs ys : List α) : List α :=
  match xs, ys with
    | [], ys => ys
    | xs, [] => xs
    | x :: xs, y :: ys => x :: y :: interleave xs ys

#assert (interleave [1, 3, 5] [2, 4, 6]) == [1, 2, 3, 4, 5, 6]
#assert (interleave [1, 2] [3, 4, 5, 6]) == [1, 3, 2, 4, 5, 6]
#assert (interleave ([] : List Nat) [1, 2]) == [1, 2]
#assert (interleave [1, 2] []) == [1, 2]


-- ============================================================
-- ### Exercise 7: chunk
-- Split a list into consecutive sublists of size `n`.
-- The last chunk may be smaller if the list doesn't divide evenly.
-- ============================================================

def chunk (n : Nat) (xs : List α) : List (List α) :=
  if n = 0 then []
  else match xs with
    | [] => []
    | xs => List.take n xs :: chunk n (List.drop n xs)
termination_by xs.length

#assert (chunk 2 [1, 2, 3, 4]) == [[1, 2], [3, 4]]
#assert (chunk 2 [1, 2, 3]) == [[1, 2], [3]]
#assert (chunk 3 [1, 2, 3, 4, 5, 6]) == [[1, 2, 3], [4, 5, 6]]
#assert (chunk 10 [1, 2]) == [[1, 2]]
#assert (chunk 1 [1, 2, 3]) == [[1], [2], [3]]


-- ============================================================
-- ### Exercise 8: isPalindrome
-- ============================================================

def isPalindrome [BEq α] (xs : List α) : Bool :=
  match xs with
    | [] => true
    | [_] => true
    | x :: xs => if x == xs.getLast? then isPalindrome xs.dropLast else false
termination_by xs.length

#assert (isPalindrome ([] : List Nat)) == true
#assert (isPalindrome [1]) == true
#assert (isPalindrome [1, 2, 1]) == true
#assert (isPalindrome [1, 2, 3]) == false
#assert (isPalindrome ['a', 'b', 'b', 'a']) == true


-- ============================================================
-- ### Exercise 9: rotate
-- Rotate a list left by `n` positions.
-- rotate 2 [1,2,3,4,5] => [3,4,5,1,2]
-- ============================================================

def rotate (n : Nat) (xs : List α) : List α :=
  match n, xs with
    | 0, _ => xs
    | _, [] => []
    | n + 1, x :: xs => rotate n (xs ++ [x])

#assert (rotate 0 [1, 2, 3]) == [1, 2, 3]
#assert (rotate 1 [1, 2, 3]) == [2, 3, 1]
#assert (rotate 2 [1, 2, 3, 4, 5]) == [3, 4, 5, 1, 2]
#assert (rotate 3 [1, 2, 3]) == [1, 2, 3]
#assert (rotate 0 ([] : List Nat)) == []


-- ============================================================
-- ### Exercise 10: sliding windows
-- Return all contiguous sublists of length `n`.
-- ============================================================

def windows (n : Nat) (xs : List α) : List (List α) :=
  match xs with
    | [] => []
    | xs => if xs.length >= n
              then take n xs :: windows n (drop 1 xs)
              else []
termination_by xs.length

#assert (windows 2 [1, 2, 3, 4]) == [[1, 2], [2, 3], [3, 4]]
#assert (windows 3 [1, 2, 3, 4]) == [[1, 2, 3], [2, 3, 4]]
#assert (windows 1 [1, 2, 3]) == [[1], [2], [3]]
#assert (windows 4 [1, 2, 3]) == []
#assert (windows 2 ([] : List Nat)) == []


-- ============================================================
-- ### Exercise 11: flatten
-- Concatenate a list of lists into a single list.
-- ============================================================

def flatten (xs : List (List α)) : List α :=
  match xs with
    | [] => []
    | x :: xs => x ++ flatten xs

#assert (flatten ([] : List (List Nat))) == []
#assert (flatten [[1, 2], [3, 4]]) == [1, 2, 3, 4]
#assert (flatten [[1], [], [2, 3]]) == [1, 2, 3]
#assert (flatten [[], []]) == ([] : List Nat)


-- ============================================================
-- ### Exercise 12: unique
-- Remove duplicate elements, keeping the first occurrence.
-- ============================================================

def unique [BEq α] (xs : List α) : List α :=
  go xs []
  where go xs acc :=
    match xs with
      | [] => acc
      | x :: xs => if !acc.contains x then go xs (acc ++ [x]) else go xs acc

#assert (unique ([] : List Nat)) == []
#assert (unique [1, 2, 3]) == [1, 2, 3]
#assert (unique [1, 1, 2, 3, 2]) == [1, 2, 3]
#assert (unique [1, 1, 1, 1]) == [1]
#assert (unique [3, 1, 2, 1, 3]) == [3, 1, 2]


-- ============================================================
-- ### Exercise 13: maximum and minimum with Option
-- ============================================================

def maximum? (xs : List Nat) : Option Nat :=
  go xs none
  where go xs max :=
      match xs, max with
        | [], _ => max
        | x :: xs, none => go xs (some x)
        | x :: xs, some m => if x > m then go xs (some x) else go xs (some m)

def minimum? (xs : List Nat) : Option Nat :=
  go xs none
  where go xs min :=
      match xs, min with
        | [], _ => min
        | x :: xs, none => go xs (some x)
        | x :: xs, some m => if x < m then go xs (some x) else go xs (some m)


#assert (maximum? []) == none
#assert (maximum? [3]) == some 3
#assert (maximum? [1, 5, 3, 2]) == some 5
#assert (minimum? []) == none
#assert (minimum? [3]) == some 3
#assert (minimum? [4, 1, 3, 2]) == some 1


-- ============================================================
-- ### Exercise 14: partition
-- Split a list into two lists: elements satisfying the
-- predicate and elements that do not.
-- ============================================================

def partition (p : α → Bool) (xs : List α) : List α × List α :=
  go p xs ([], [])
  where go p xs acc :=
    match xs with
      | [] => acc
      | x :: rest => if p x then go p rest (acc.fst ++ [x], acc.snd) else go p rest (acc.fst, acc.snd ++ [x])

#assert (partition (· > 3) [1, 4, 2, 5, 3]) == ([4, 5], [1, 2, 3])
#assert (partition (· % 2 == 0) [1, 2, 3, 4, 5, 6]) == ([2, 4, 6], [1, 3, 5])
#assert (partition (· > 10) [1, 2, 3]) == ([], [1, 2, 3])
#assert (partition (fun _ => true) [1, 2, 3]) == ([1, 2, 3], [])


-- ============================================================
-- ### Exercise 15: insertion sort
-- Sort a list using insertion sort.
-- Define a helper `insert` that inserts a single element into
-- an already sorted list at the correct position.
-- ============================================================

def insertSorted (x : Nat) (xs : List Nat) : List Nat :=
  match xs with
    | [] => [x]
    | x' :: xs => if x < x' then x :: x' :: xs else x' :: insertSorted x xs

def insertionSort (xs : List Nat) : List Nat :=
  go xs [] where
  go xs acc :=
    match xs with
      | [] => acc
      | x :: xs => go xs (insertSorted x acc)

#assert (insertSorted 3 [1, 2, 4, 5]) == [1, 2, 3, 4, 5]
#assert (insertSorted 0 [1, 2, 3]) == [0, 1, 2, 3]
#assert (insertSorted 5 [1, 2, 3]) == [1, 2, 3, 5]
#assert (insertionSort []) == []
#assert (insertionSort [3, 1, 4, 1, 5, 9, 2, 6]) == [1, 1, 2, 3, 4, 5, 6, 9]
#assert (insertionSort [5, 4, 3, 2, 1]) == [1, 2, 3, 4, 5]
#assert (insertionSort [1]) == [1]


-- ============================================================
-- ### Exercise 16: merge sort
-- Sort a list using merge sort.
-- Define a helper `merge` that merges two already sorted lists.
-- Use `take` and `drop` to split the list in half.
-- Mark as `partial`.
-- ============================================================

def merge (xs ys : List Nat) : List Nat :=
  match xs, ys with
  | [], ys => ys
  | xs, [] => xs
  | x :: xs, y :: ys =>
    if x <= y then x :: merge xs (y :: ys)
    else y :: merge (x :: xs) ys

partial def mergeSort (xs : List Nat) : List Nat :=
  match xs with
  | [] => []
  | [x] => [x]
  | _ =>
    let mid := xs.length / 2
    let left := mergeSort (xs.take mid)
    let right := mergeSort (xs.drop mid)
    merge left right

#assert (merge [1, 3, 5] [2, 4, 6]) == [1, 2, 3, 4, 5, 6]
#assert (merge [1, 2, 3] []) == [1, 2, 3]
#assert (merge [] [1, 2, 3]) == [1, 2, 3]
#assert (mergeSort []) == []
#assert (mergeSort [1]) == [1]
#assert (mergeSort [3, 1, 4, 1, 5, 9, 2, 6]) == [1, 1, 2, 3, 4, 5, 6, 9]
#assert (mergeSort [5, 4, 3, 2, 1]) == [1, 2, 3, 4, 5]


-- ============================================================
-- ### Exercise 17: quicksort
-- Sort a list using quicksort. Use the head as pivot.
-- Mark as `partial`.
-- ============================================================

partial def quickSort (xs : List Nat) : List Nat :=
  match xs with
    | [] => []
    | pivot :: rest => let smaller := rest.filter (fun x => x < pivot)
                       let geq := rest.filter (fun x => x >= pivot)
                       (quickSort smaller) ++ [pivot] ++ (quickSort geq)

#assert (quickSort []) == []
#assert (quickSort [1]) == [1]
#assert (quickSort [3, 1, 4, 1, 5, 9, 2, 6]) == [1, 1, 2, 3, 4, 5, 6, 9]
#assert (quickSort [5, 4, 3, 2, 1]) == [1, 2, 3, 4, 5]
#assert (quickSort [1, 2, 3, 4, 5]) == [1, 2, 3, 4, 5]


-- ============================================================
-- ### Exercise 18: selection sort
-- Sort a list using selection sort.
-- Repeatedly find the minimum element and move it to the front.
-- Use your `minimum?` or define a helper.
-- Mark as `partial`.
-- ============================================================

def removeFirst (x : Nat) (xs : List Nat) : List Nat :=
  match xs with
  | [] => []
  | y :: ys => if x == y then ys else y :: removeFirst x ys

partial def selectionSort (xs : List Nat) : List Nat :=
  match minimum? xs with
  | none => []
  | some m => m :: selectionSort (removeFirst m xs)


#assert (selectionSort []) == []
#assert (selectionSort [1]) == [1]
#assert (selectionSort [3, 1, 4, 1, 5, 9, 2, 6]) == [1, 1, 2, 3, 4, 5, 6, 9]
#assert (selectionSort [5, 4, 3, 2, 1]) == [1, 2, 3, 4, 5]
#assert (selectionSort [1, 2, 3, 4, 5]) == [1, 2, 3, 4, 5]


-- ============================================================
-- ### Exercise 19: bubble sort
-- Sort a list using bubble sort.
-- Define a helper `bubble` that performs one pass through the
-- list, swapping adjacent elements that are out of order.
-- Call it recursively `n` times where `n` is the list length.
-- ============================================================

def bubble (xs : List Nat) : List Nat :=
  match xs with
    | [] => []
    | [x] => [x]
    | x :: y :: rest => if x > y then y :: bubble (x :: rest) else x :: bubble (y ::rest)

def bubbleSort (xs : List Nat) : List Nat :=
  go xs xs.length
  where go xs n :=
    match n with
      | 0 => xs
      | n + 1 => go (bubble xs) n

#assert (bubble [3, 1, 2]) == [1, 2, 3]
#assert (bubble [1, 2, 3]) == [1, 2, 3]
#assert (bubbleSort []) == []
#assert (bubbleSort [1]) == [1]
#assert (bubbleSort [3, 1, 4, 1, 5, 9, 2, 6]) == [1, 1, 2, 3, 4, 5, 6, 9]
#assert (bubbleSort [5, 4, 3, 2, 1]) == [1, 2, 3, 4, 5]


-- ============================================================
-- ### Exercise 20: tree definition and basic operations
-- ============================================================

inductive Tree (α : Type) where
  | Leaf
  | Node (v : α) (l r : Tree α)
  deriving Repr, BEq

open Tree

def sumTree (t : Tree Int) : Int :=
  match t with
    | Leaf => 0
    | Node v l r => v + sumTree l + sumTree r

def mirror (t : Tree α) : Tree α :=
  match t with
    | Leaf => Leaf
    | Node v l r => Node v r l

def contains [BEq α] (t : Tree α) (x : α) : Bool :=
  match t with
    | Leaf => false
    | Node v l r => v == x || (contains l x) || (contains r x)

#assert (sumTree Leaf) == 0
#assert (sumTree (Node 1 Leaf Leaf)) == 1
#assert (sumTree (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))) == 6

#assert (mirror (Leaf : Tree Nat)) == Leaf
#assert (mirror (Node 1 (Node 2 Leaf Leaf) Leaf)) == Node 1 Leaf (Node 2 Leaf Leaf)
#assert (mirror (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))) == Node 1 (Node 3 Leaf Leaf) (Node 2 Leaf Leaf)

#assert (contains (Leaf : Tree Nat) 1) == false
#assert (contains (Node 1 Leaf Leaf) 1) == true
#assert (contains (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)) 3) == true
#assert (contains (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)) 5) == false


-- ============================================================
-- ### Exercise 21: tree traversals
-- ============================================================

def preOrder (t : Tree α) : List α :=
  go t []
  where go t acc :=
    match t with
      | Leaf => acc
      | Node v l r => v :: (go l acc) ++ (go r acc)

def inOrder (t : Tree α) : List α :=
  go t []
  where go t acc :=
    match t with
      | Leaf => acc
      | Node v l r => (go l acc) ++ [v] ++ (go r acc)

def postOrder (t : Tree α) : List α :=
  go t []
  where go t acc :=
    match t with
      | Leaf => acc
      | Node v l r => (go l acc) ++ (go r acc) ++ [v]

#assert (preOrder (Leaf : Tree Nat)) == []
#assert (preOrder (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))) == [1, 2, 3]
#assert (preOrder (Node 1 (Node 2 (Node 4 Leaf Leaf) Leaf) (Node 3 Leaf Leaf))) == [1, 2, 4, 3]

#assert (inOrder (Leaf : Tree Nat)) == []
#assert (inOrder (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))) == [2, 1, 3]
#assert (inOrder (Node 1 (Node 2 (Node 4 Leaf Leaf) Leaf) (Node 3 Leaf Leaf))) == [4, 2, 1, 3]

#assert (postOrder (Leaf : Tree Nat)) == []
#assert (postOrder (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))) == [2, 3, 1]
#assert (postOrder (Node 1 (Node 2 (Node 4 Leaf Leaf) Leaf) (Node 3 Leaf Leaf))) == [4, 2, 3, 1]


-- ============================================================
-- ### Exercise 22: mapTree and foldTree
-- Apply a function to every value in the tree.
-- foldTree combines all node values using a binary function
-- and a base value for leaves.
-- ============================================================

def mapTree (f : α → β) (t : Tree α) : Tree β :=
  match t with
    | Leaf => Leaf
    | Node v l r => Node (f v) (mapTree f l) (mapTree f r)

def foldTree (base : β) (f : α → β → β → β) (t : Tree α) : β :=
  match t with
    | Leaf => base
    | Node v l r => f v (foldTree base f l) (foldTree base f r)

#assert (mapTree (· * 2) (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))) ==
        Node 2 (Node 4 Leaf Leaf) (Node 6 Leaf Leaf)
#assert (mapTree toString (Node 1 (Node 2 Leaf Leaf) Leaf)) ==
        Node "1" (Node "2" Leaf Leaf) Leaf

-- foldTree can express sumTree: combine with (+), base 0
#assert (foldTree 0 (fun v l r => v + l + r) (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))) == 6
-- foldTree can express size: ignore value, count nodes
#assert (foldTree 0 (fun _ l r => 1 + l + r) (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))) == 3


-- ============================================================
-- ## b  # Exercise 23: BST insert and search
-- A BST satisfies: left subtree values < node value,
-- right subtree values > node value.
-- ============================================================

def bstInsert (t : Tree Nat) (x : Nat) : Tree Nat :=
  match t with
    | Leaf => Node x Leaf Leaf
    | Node v l r =>
      if x < v then Node v (bstInsert l x) r
      else if x > v then Node v l (bstInsert r x)
      else t

def bstSearch (t : Tree Nat) (x : Nat) : Bool :=
  match t with
    | Leaf => false
    | Node v l r => if v == x then true
                    else if x < v then bstSearch l x
                    else bstSearch r x

def exampleBST := bstInsert (bstInsert (bstInsert Leaf 5) 3) 7

#assert (bstSearch exampleBST 5) == true
#assert (bstSearch exampleBST 3) == true
#assert (bstSearch exampleBST 7) == true
#assert (bstSearch exampleBST 1) == false
#assert (bstSearch exampleBST 6) == false
#assert (bstSearch Leaf 1) == false
-- inserting duplicates should not create new nodes
#assert (bstSearch (bstInsert exampleBST 3) 3) == true


-- ============================================================
-- ### Exercise 24: BST to sorted list
-- Extract all values from a BST into a sorted list using
-- in-order traversal. Then verify: sorting a list by
-- inserting into a BST and extracting gives a sorted result.
-- ============================================================

def bstToList (t : Tree Nat) : List Nat :=
  match t with
    | Leaf => []
    | Node v l r => bstToList l ++ [v] ++ bstToList r

#assert (bstToList Leaf) == []
#assert (bstToList exampleBST) == [3, 5, 7]

-- inserting [3,1,4,1,5,9,2,6] into a BST and extracting should yield a sorted list
-- (duplicates may be dropped depending on your insert — that is fine)
def sortViaBST (xs : List Nat) : List Nat :=
  bstToList (xs.foldl bstInsert Leaf)

#assert (sortViaBST [3, 1, 4, 5, 9, 2, 6]) == [1, 2, 3, 4, 5, 6, 9]


-- ============================================================
-- ### Exercise 25: Collatz sequence and longest in a range
-- Return the full Collatz sequence as a list starting from n,
-- including both n and the final 1.
-- Then find the starting number in [1, limit] that produces
-- the longest Collatz sequence.
-- Both require generative recursion, mark as `partial`.
-- ============================================================

partial def collatzSeq (n : Nat) : List Nat :=
  match n with
  | 0 => []
  | 1 => [1]
  | n => if n % 2 == 0 then n :: collatzSeq (n / 2)
         else n :: collatzSeq (3 * n + 1)

partial def longestCollatz (limit : Nat) : Nat :=
  let nums := List.range limit |>.map (· + 1)
  nums.foldl (fun best n =>
    if (collatzSeq n).length > (collatzSeq best).length then n else best
  ) 1

#assert (collatzSeq 1) == [1]
#assert (collatzSeq 2) == [2, 1]
#assert (collatzSeq 6) == [6, 3, 10, 5, 16, 8, 4, 2, 1]
#assert (longestCollatz 10) == 9
#assert (longestCollatz 20) == 18


end RecursionExercises
