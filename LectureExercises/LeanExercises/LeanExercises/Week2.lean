/-
Welcome to Week 2's lab.

This lab focuses on structural recursion.
The problems are split into two difficulty levels: Basic and Advanced.
Completing all basic exercises is required, while the advanced exercises are an optional challenge.
For each exercise, please replace the `sorry` placeholder with your solution."
-/

import Testing

namespace MyList
namespace List

/-
Please ignore the macro below. It implements syntactic sugar for our own List,
allowing you to write `[1, 2, 3]` instead of the more verbose `1 :: 2 :: 3 :: nil`.
-/
open Lean in
macro_rules
  | `([ $elems,* ]) => do
    let rec expandListLit (i : Nat) (skip : Bool) (result : TSyntax `term) : MacroM Syntax := do
      match i, skip with
      | 0,   _     => pure result
      | i+1, true  => expandListLit i false result
      | i+1, false => expandListLit i true  (← ``(List.cons $(⟨elems.elemsAndSeps.get!Internal i⟩) $result))
    let size := elems.elemsAndSeps.size
    if size < 64 then
      expandListLit size (size % 2 == 0) (← ``(List.nil))
    else
      `(%[ $elems,* | List.nil ])

#check ([1, 2, 3] : List Int)

section ListExercises
variable {α : Type} [BEq α]

-- ===============================================
-- ### Exercise 1: reverse
-- Return the reverse of a list.
-- ===============================================

def reverse (l : List α) : List α :=
  match l with
    | [] => []
    | x :: xs => reverse xs ++ [x]

#assert (reverse ([] : List Int)) == []
#assert (reverse [1]) == [1]
#assert (reverse [1, 2, 3]) == [3, 2, 1]


-- ================================================
-- ### Exercise 2: drop
-- Takes a natural number `n` and a list `l`, and returns a list that drops the first `n` elements in `l`.
-- ================================================
def drop (n : Nat) (l : List α) : List α :=
  match n, l with
    | 0, _ => l
    | _, [] => []
    | n + 1, _ :: ls => drop n ls


#assert (drop 0 ([] : List Int)) == []
#assert (drop 0 [1]) == [1]
#assert (drop 1 [1]) == []
#assert (drop 1 [1, 2]) == [2]
#assert (drop 2 [1, 2, 3]) == [3]
#assert (drop 3 [1, 2, 3]) == []


-- ================================================
-- ### Exercise 3: getD
-- Returns the element at the provided index, counting from 0. Returns fallback if the index is out of bounds.
-- ================================================
def getD (l : List α) (idx : Nat) (fallback : α) : α :=
  match l with
    | [] => fallback
    | l :: ls => if idx == 0 then l else getD ls (idx - 1) fallback

#assert (getD [] 0 0) == 0
#assert (getD [1] 0 0) == 1
#assert (getD [1] 1 0) == 0
#assert (getD [1, 2, 3] 0 0) == 1
#assert (getD [1, 2, 3] 1 0) == 2
#assert (getD [1, 2, 3] 2 0) == 3
#assert (getD [1, 2, 3] 3 0) == 0
#assert (getD [1, 2, 3] 4 0) == 0

-- ================================================
-- ### Exercise 4: flatten
-- Concatenates a list of lists into a single list, preserving the order of the elements.
-- ================================================

def flatten (l : List (List α)) : List α :=
  match l with
    | [] => []
    | [[]] => []
    | l :: ls => l ++ (flatten ls)

#assert (flatten ([] : List (List Int))) == []
#assert (flatten ([[]] : List (List Int))) == []
#assert (flatten [[1]]) == [1]
#assert (flatten [[1], [2, 3]]) == [1, 2, 3]
#assert (flatten [[1, 2], [3, 4]]) == [1, 2, 3, 4]
#assert (flatten [[1, 2], [3, 4], [5, 6]]) == [1, 2, 3, 4, 5, 6]
#assert (flatten ([[], [], []]) : List (List Int)) == []

-- ================================================
-- ### Exercise 5: idxOf
-- Returns the index of the first element equal to `a`, or `none` if no element is equal to `a`.
-- ================================================

/-
this implementation is problematic because original length changes every time we call the function
def idxOf? (l : List α) (a : α) : Option Nat :=
  let originalLength := l.length
match l with
    | [] => none
    | l' :: ls => if l' == a then some (originalLength - ls.length - 1) else idxOf? ls a
-/
def idxOf? (l : List α) (a : α) : Option Nat :=
  let originalLength := l.length
  let rec helper (remaining : List α) : Option Nat :=
    match remaining with
    | [] => none
    | l' :: ls => if l' == a then some (originalLength - ls.length - 1) else helper ls
  helper l

#assert (idxOf? ([] : List Int) 0) == none
#assert (idxOf? [1] 1) == some 0
#assert (idxOf? [1] 2) == none
#assert (idxOf? [1, 2, 3] 1) == some 0
#assert (idxOf? [1, 2, 3] 2) == some 1
#assert (idxOf? [1, 2, 3] 3) == some 2
#assert (idxOf? [1, 2, 3] 4) == none


end ListExercises



section TreeExercises

-- ================================================
-- ### Exercise 1 : tree definition
-- Define an inductive data type `Tree` for binary trees.
-- Requirements:
-- 1. The `Tree` data type should contain two variants: `Leaf` and `Node`;
-- 2. The `Leaf` variant should have no fields;
-- 3. The `Node` variant should have three fields: value, left child and right child.
-- ================================================
inductive Tree (α : Type) where
  | Leaf
  | Node (v : α) (l r : Tree α)
  deriving Repr, BEq


open Tree
-- ================================================
-- ### Exercise 2 : size
-- Return the size of a binary tree, which is the number of nodes.
-- ================================================
def size {α} (t : Tree α) : Nat :=
  match t with
    | Leaf => 0
    | Node _ l r => 1 + size l + size r

#assert (size (Leaf : Tree Nat)) == 0
#assert (size (Node 1 Leaf Leaf)) == 1
#assert (size (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))) == 3
#assert (size (Node 1 (Node 2 (Node 4 Leaf Leaf) Leaf) (Node 3 Leaf Leaf))) == 4
#assert (size (Node 1 (Node 2 (Node 4 Leaf Leaf) Leaf) (Node 3 (Node 5 Leaf Leaf) Leaf))) == 5


-- ================================================
-- ### Exercise 3 : height
-- Return the height of a binary tree, which is the length of the longest path from the root to a leaf.
-- ================================================
def height (t : Tree α) : Nat :=
  match t with
    | Leaf => 0
    | Node _ l r => 1 + (height l).max (height r)

#assert (height (Leaf : Tree Nat)) == 0
#assert (height (Node 1 Leaf Leaf)) == 1
#assert (height (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))) == 2
#assert (height (Node 1 (Node 2 (Node 4 Leaf Leaf) Leaf) (Node 3 Leaf Leaf))) == 3
#assert (height (Node 1 (Node 2 (Node 4 Leaf Leaf) Leaf) (Node 3 (Node 5 Leaf Leaf) Leaf))) == 3

-- ================================================
-- ### Exercise 4 : inOrder
-- Return the in-order traversal of a binary tree.
-- Think:
--  1. What is the time complexity of your implementation?
--  2. Is it possible to optimize your implementation?
-- ================================================

/-
Slow version with complexity O(n^2) because ++ must walk through the entire left list to attach the right list
def inOrder (t : Tree α) : List α :=
match t with
    | Leaf => []
    | Node v l r => inOrder l ++ [v] ++ inOrder r
-/

/-
here the complexity is O(n) because :: just adds to the front without traversing the left list
-/
def inOrder (t : Tree α) : List α :=
  let rec helper (t : Tree α) (acc: List α) : List α :=
    match t with
      | Leaf => acc
      | Node v l r => helper l (v :: helper r acc)
  helper t []


#assert (inOrder (Leaf : Tree Nat)) == []
#assert (inOrder (Node 1 Leaf Leaf)) == [1]
#assert (inOrder (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))) == [2, 1, 3]
#assert (inOrder (Node 1 (Node 2 (Node 4 Leaf Leaf) Leaf) (Node 3 Leaf Leaf))) == [4, 2, 1, 3]
#assert (inOrder (Node 1 (Node 2 (Node 4 Leaf Leaf) Leaf) (Node 3 (Node 5 Leaf Leaf) Leaf))) == [4, 2, 1, 5, 3]
#assert (inOrder (Node 1 (Node 2 (Node 4 Leaf Leaf) Leaf) (Node 3 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf)))) == [4, 2, 1, 5, 3, 6]



-- ================================================
-- ### Exercise 5 : level
-- Return the elements of a tree on a given level from left to right
-- ================================================
def level (t : Tree α) (n : Nat) : List α :=
  match t, n with
    | Leaf, _ => []
    | Node v _ _, 0 => [v]
    | Node _ l r, n + 1 => (level l n) ++ (level r n)

#assert (level (Leaf : Tree Nat) 0) == []
#assert (level (Node 1 Leaf Leaf) 0) == [1]
#assert (level (Node 1 Leaf Leaf) 1) == []
#assert (level (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)) 0) == [1]
#assert (level (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)) 1) == [2, 3]
#assert (level (Node 1 (Node 2 (Node 4 Leaf Leaf) Leaf) (Node 3 Leaf Leaf)) 2) == [4]
#assert (level (Node 1 (Node 2 (Node 4 Leaf Leaf) Leaf) (Node 3 Leaf Leaf)) 1) == [2, 3]

-- ================================================
-- ### Exercise 6 : complete tree
-- Return whether the given tree is complete.
-- A complete tree is a tree in which every level is completely filled (or all leaves are in the same level).
-- ================================================
def complete (t : Tree α) : Bool :=
  match t with
    | Leaf => true
    | Node _ l r => if (complete l) && (complete r) && (height l == height r) then true else false

#assert (complete (Leaf : Tree Nat)) == true
#assert (complete (Node 1 Leaf Leaf)) == true
#assert (complete (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))) == true
#assert (complete (Node 1 (Node 2 (Node 4 Leaf Leaf) Leaf) (Node 3 Leaf Leaf))) == false
#assert (complete (Node 1 (Node 2 (Node 4 Leaf Leaf) Leaf) (Node 3 (Node 5 Leaf Leaf) Leaf))) == false
#assert (complete (Node 1 (Node 2 (Node 3 Leaf Leaf) Leaf) (Node 4 (Node 5 Leaf Leaf) Leaf))) == false
end TreeExercises


section AdvancedExercises

/-
This set of exercise uses lists as a data structure for representing and manipulating univariate polynomials
(polynomials with one variable, which we will simply call polynomials in this context).

A univariate polynomial is a finite sum of monomials, each consisting of a coefficient multiplied by a monomial,
where each monomial is a power of a variable. By default, we denote the variable as x.


For example, `f(x) = 3x^3 + 2x^2 + 1` is a polynomial.

We can represent `f(x)` as a list of coefficients `[1, 0, 2, 3]`.
-/

def Polynomial := List Int deriving BEq, Repr

/-
Hint: When working on the following exercises, you may find the List functions introduced in class
and used in the previous exercises helpful.
-/

-- ================================================
-- ### Exercise 1: degree
-- Return the degree of a polynomial (the highest power of the variable that has a non-zero coefficient).
-- ================================================
def degree (p : Polynomial) : Nat :=
  let rec helper (p: Polynomial) (currentIndex: Nat) (maxIndex: Nat) : Nat :=
    match p with
    | [] => maxIndex
    | coeff :: ps =>
        if coeff != 0 then
          helper ps (currentIndex + 1) currentIndex
        else
          helper ps (currentIndex + 1) maxIndex
  helper p 0 0

#assert (degree []) == 0               -- zero polynomial
#assert (degree [1, 0, 2, 3]) == 3     -- 3*x^3 + 2*x^2 + 0*x + 1
#assert (degree [0, 0, 0, 1]) == 3     -- x^3
#assert (degree [0, 0, 5]) == 2        -- 5*x^2
#assert (degree [0, 0, 7, 0, 0]) == 2  -- 7*x^2
#assert (degree [0, 0, 0]) == 0        -- zero polynomial
#assert (degree [4]) == 0              -- constant polynomial



-- ================================================
-- ### Exercise 2 : eval
-- Evaluate a polynomial at a given point.
-- ================================================
def eval (p : Polynomial) (v : Int) : Int :=
  let rec helper (p: Polynomial) (currentIndex: Nat) (v: Int) : Int :=
    match p with
    | [] => 0
    | coeff :: ps =>
        if coeff != 0 then
          (coeff * v ^ currentIndex) + (helper ps (currentIndex + 1) v)
        else
          helper ps (currentIndex + 1) v
  helper p 0 v

#assert (eval [-3, 0, 2] 4) == 29    -- 2*x² - 3
#assert (eval [-4, 3, -2, 1] 2) == 2 -- x³ - 2*x² + 3*x - 4
#assert (eval [1, 7, 0, 0] 3) == 22  -- 7*x + 1
#assert (eval [2, 0, 0, 0, 4] 5) == 2502 -- 4*x⁴ + 2
#assert (eval [0, 0, 0, 0, 1] 10) == 10000  -- x⁴
#assert (eval [-3, 3, -3, 3] 1) == 0 -- 3*x³ - 3*x² + 3*x - 3
#assert (eval [-1, 0, 0, 9] 2) == 71 -- 9*x³ - 1
#assert (eval [5, 0, 0, 0] 8) == 5  -- constant 5
#assert (eval [0, 1, 2] 6) == 78 -- 2*x² + x
#assert (eval [1, 1, 1, 1, 1] 2) == 31 -- x⁴ + x³ + x² + x + 1
#assert (eval [1, -2, 3] (-2)) == 17 -- 3*x² - 2*x + 1
#assert (eval [2, -6, 4, -1] (-3)) == 83 -- -x³ + 4*x² - 6*x + 2
#assert (eval [-2, 0, -1, 5] (-1)) == -8 -- 5*x³ - x² - 2
#assert (eval [7, 0, 0, -3] 3) == -74 -- -3*x³ + 7
#assert (eval [-1, 1, -1, 1, -1, 1] (-2)) == -63 -- alternating signs, degree 5


-- ================================================
-- ### Exercise 3 : add
-- Return the addition of two polynomials.
-- ================================================
def add (p q : Polynomial) : Polynomial :=
  match p, q with
    | [], [] => []
    | [], _ => q
    | _, [] => p
    | p :: ps, q :: qs => (p + q) :: (add ps qs)

#assert (add [] []) == []
#assert (add [1, 0, 2, 3] []) == [1, 0, 2, 3]
#assert (add [] [1, 0, 2, 3]) == [1, 0, 2, 3]
#assert (add [1, 0, 2, 3] [1, 0, 2, 3]) == [2, 0, 4, 6]
#assert (add [1, 0] [1, 0, 2, 5]) == [2, 0, 2, 5]
#assert (add [0, 0, 5] [0, 3]) == [0, 3, 5]
#assert (add [2, -1] [3, 1]) == [5, 0]
#assert (add [4] [-4]) == [0]

-- ================================================
-- ### Exercise 4 : sub
-- Return the subtraction of two polynomials.
-- ================================================
def sub (p q : Polynomial) : Polynomial :=
  match p, q with
    | [], [] => []
    | [], _ => neg q
    | _, [] => p
    | p :: ps, q :: qs => (p - q) :: (sub ps qs)
    where neg (l: Polynomial) : Polynomial :=
      match l with
        | [] => []
        | x :: xs => (-x) :: neg xs

#assert (sub [] []) == []
#assert (sub [1, 0, 2, 3] []) == [1, 0, 2, 3]
#assert (sub [] [1, 0, 2, 3]) == [-1, 0, -2, -3]
#assert (sub [1, 0, 2, 3] [1, 0, 2, 3]) == [0, 0, 0, 0]
#assert (sub [1, 0] [1, 0, 2, 5]) == [0, 0, -2, -5]
#assert (sub [0, 0, 5] [0, 3]) == [0, -3, 5]
#assert (sub [2, -1] [3, 1]) == [-1, -2]
#assert (sub [4] [-4]) == [8]
#assert (sub [1, 2, 3] [0, 0, 0, 4]) == [1, 2, 3, -4]
#assert (sub [0, -1, 2] [0, 1, -2]) == [0, -2, 4]
#assert (sub [5] [0, 0, 3]) == [5, 0, -3]
#assert (sub [1, 2, 3] [4, 5]) == [-3, -3, 3]



-- ================================================
-- ### Exercise 5 : mul
-- Return the multiplication of two polynomials
-- ================================================


def mul (p q : Polynomial) : Polynomial :=
  let rec scale (coeff : Int) (poly : Polynomial) : Polynomial :=
    match poly with
    | [] => []
    | x :: xs => (coeff * x) :: scale coeff xs

  let rec shift (n : Nat) (poly : Polynomial) : Polynomial :=
    match n with
    | 0 => poly
    | n + 1 => 0 :: shift n poly

  let rec helper (p : Polynomial) (index : Nat) : Polynomial :=
    match p with
    | [] => []
    | coeff :: ps => add (shift index (scale coeff q)) (helper ps (index + 1))

  helper p 0

#assert (mul [] []) == []
#assert (mul [1] []) == []
#assert (mul [] [1, 2, 3]) == []
#assert (mul [1] [1, 2, 3]) == [1, 2, 3]
#assert (mul [1, 0] [1, 0, 2, 3]) == [1, 0, 2, 3, 0]
#assert (mul [1] [1, 2, 3]) == [1, 2, 3]
#assert (mul [1, 1] [1, 1]) == [1, 2, 1]
#assert (mul [2, 0, 3] [1, 2]) == [2, 4, 3, 6]
#assert (mul [1, 2, 3] [0, 1]) == [0, 1, 2, 3]
#assert (mul [1, -1] [1, 1]) == [1, 0, -1]
#assert (mul [0, 5] [2, 3]) == [0, 10, 15]


-- ===============================================
-- ### Exercise 6 : power
-- Return the power of a polynomial.
-- ===============================================
def power (p : Polynomial) (n : Nat) : Polynomial :=
  match n with
    | 0 => [1]
    | 1 => p
    | n + 1 => mul p (power p n)

#assert (power [1, 2, 3] 0) == [1]
#assert (power [5] 0) == [1]
#assert (power [] 0) == [1]  -- 0^0) == 1
#assert (power [1, 2, 3] 1) == [1, 2, 3]
#assert (power [4] 1) == [4]
#assert (power [] 1) == []
#assert (power [] 2) == []
#assert (power [] 5) == []
#assert (power [5] 2) == [25]
#assert (power [2] 3) == [8]
#assert (power [-3] 2) == [9]
#assert (power [-3] 3) == [-27]
#assert (power [0, 1] 2) == [0, 0, 1]
#assert (power [0, 1] 3) == [0, 0, 0, 1]
#assert (power [1, 1] 2) == [1, 2, 1]
#assert (power [1, 1] 3) == [1, 3, 3, 1]
#assert (power [2, 1] 2) == [4, 4, 1]
#assert (power [0, 3] 2) == [0, 0, 9]
#assert (power [0, 0, 2] 3) == [0, 0, 0, 0, 0, 0, 8]
#assert (power [1, 0, -1] 2) == [1, 0, -2, 0, 1]

-- ================================================
-- ### Exercise 7 : diff
-- Return the differentiation of a polynomial.
-- ================================================


-- def diff (poly : Polynomial) : Polynomial :=
--   sorry

-- #assert (diff []) == []
-- #assert (diff [5]) == []
-- #assert (diff [1, 2]) == [2]
-- #assert (diff [1, 0, 2, 3]) == [0, 4, 9]
-- #assert (diff [0, 0, 5]) == [0, 10]
-- #assert (diff [2, -1]) == [-1]
-- #assert (diff [1, 1, 1]) == [1, 2]
-- #assert (diff [0, 3, 0, 4]) == [3, 0, 12]

-- -- ================================================
-- -- ### Exercise 8 : compose
-- -- Return the composition of two polynomials `f(g(x))`.
-- -- ===========================================
-- def compose (p q : Polynomial) : Polynomial :=
--   sorry


-- #assert (compose [] []) == []
-- #assert (compose [1] []) == [1]
-- #assert (compose [] [1, 2]) == []
-- #assert (compose [1, 0] [1, 2]) == [1, 0]
-- #assert (compose [0, 1] [1, 2]) == [1, 2]
-- #assert (compose [1, 1] [1, 2]) == [2, 2]
-- #assert (compose [1, 0, 1] [1, 1]) == [2, 2, 1]
-- #assert (compose [2, 0, 1] [0, 1]) == [2, 0, 1]
-- #assert (compose [1, 0, 1, 2] [1, 1, 1]) == [4, 8, 15, 16, 13, 6, 2]
-- #assert (compose [0, 1, 2, 3] [1, 2, 1]) == [6, 28, 58, 68, 47, 18, 3]
-- #assert (compose [1, 2, 1, 0] [0, 1, 1, 1]) == [1, 2, 3, 4, 3, 2, 1, 0, 0, 0]
-- #assert (compose [2, 0, 1, 1] [1, 1, 2, 0]) == [4, 5, 14, 17, 22, 12, 8, 0, 0, 0]
-- #assert (compose [1, 1, 1, 1] [1, 0, 1, 1]) == [4, 0, 6, 6, 4, 8, 5, 3, 3, 1]

end AdvancedExercises
end List
end MyList
