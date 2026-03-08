import Testing

namespace HOFExercises

-- ============================================================
-- ### Exercise 1: implement map, filter, zipWith from scratch
-- Do not use the built-in versions. Use structural recursion.
-- ============================================================

def myMap (f : α → β) (xs : List α) : List β := sorry
def myFilter (p : α → Bool) (xs : List α) : List α := sorry
def myZipWith (f : α → β → γ) (xs : List α) (ys : List β) : List γ := sorry

#assert (myMap (· * 2) [1, 2, 3]) == [2, 4, 6]
#assert (myMap toString [1, 2, 3]) == ["1", "2", "3"]
#assert (myMap (· * 2) ([] : List Int)) == []

#assert (myFilter (· > 2) [1, 2, 3, 4]) == [3, 4]
#assert (myFilter (fun _ => false) [1, 2, 3]) == []
#assert (myFilter (fun _ => true) [1, 2, 3]) == [1, 2, 3]

#assert (myZipWith (· + ·) [1, 2, 3] [4, 5, 6]) == [5, 7, 9]
#assert (myZipWith (· + ·) [1, 2] [4, 5, 6]) == [5, 7]
#assert (myZipWith (fun a b => (a, b)) [1, 2] ["a", "b"]) == [(1, "a"), (2, "b")]


-- ============================================================
-- ### Exercise 2: implement fold and express other functions with it
-- Define right-fold from scratch, then use it to define
-- sum, product, length, reverse, and append.
-- ============================================================

def myFold (init : β) (combine : α → β → β) (xs : List α) : β := sorry

-- now define these using only myFold, no recursion, no other list functions
def mySum (xs : List Int) : Int := sorry
def myProduct (xs : List Nat) : Nat := sorry
def myLength (xs : List α) : Nat := sorry
def myReverse (xs : List α) : List α := sorry
def myAppend (xs ys : List α) : List α := sorry

#assert (mySum [1, 2, 3, 4]) == 10
#assert (mySum []) == 0
#assert (myProduct [1, 2, 3, 4]) == 24
#assert (myProduct []) == 1
#assert (myLength [1, 2, 3]) == 3
#assert (myLength ([] : List Nat)) == 0
#assert (myReverse [1, 2, 3]) == [3, 2, 1]
#assert (myReverse ([] : List Nat)) == []
#assert (myAppend [1, 2] [3, 4]) == [1, 2, 3, 4]
#assert (myAppend ([] : List Nat) [1]) == [1]


-- ============================================================
-- ### Exercise 3: left fold
-- Implement left fold (processes left to right with accumulator).
-- Then use it to define flatten and countWhere.
-- ============================================================

def myFoldl (combine : β → α → β) (init : β) (xs : List α) : β := sorry
def myFlatten (xs : List (List α)) : List α := sorry
def myCountWhere (p : α → Bool) (xs : List α) : Nat := sorry

#assert (myFoldl (fun acc x => acc - x) 10 [1, 2, 3]) == 4
#assert (myFoldl (fun acc x => x :: acc) [] [1, 2, 3]) == [3, 2, 1]
#assert (myFlatten [[1, 2], [3], [4, 5]]) == [1, 2, 3, 4, 5]
#assert (myFlatten ([] : List (List Nat))) == []
#assert (myCountWhere (· > 2) [1, 2, 3, 4]) == 2
#assert (myCountWhere (· > 2) []) == 0


-- ============================================================
-- ### Exercise 4: takeWhile and dropWhile
-- ============================================================

def takeWhile (p : α → Bool) (xs : List α) : List α := sorry
def dropWhile (p : α → Bool) (xs : List α) : List α := sorry

#assert (takeWhile (· < 4) [1, 2, 3, 4, 5]) == [1, 2, 3]
#assert (takeWhile (· < 4) [5, 6, 7]) == []
#assert (takeWhile (· < 4) ([] : List Nat)) == []
#assert (dropWhile (· < 4) [1, 2, 3, 4, 5]) == [4, 5]
#assert (dropWhile (· < 4) [5, 6, 7]) == [5, 6, 7]
#assert (dropWhile (· < 4) ([] : List Nat)) == []


-- ============================================================
-- ### Exercise 5: function composition and applyN
-- Define compose and applyN (apply f n times to x).
-- Then use them to build pipelines.
-- ============================================================

def compose (f : β → γ) (g : α → β) : α → γ := sorry
def applyN (f : α → α) (n : Nat) (x : α) : α := sorry

#assert (compose (· * 2) (· + 1) 3) == 8
#assert (compose toString (· * 2) 3) == "6"
#assert (applyN (· + 1) 5 0) == 5
#assert (applyN (· * 2) 3 1) == 8
#assert (applyN (· + 1) 0 42) == 42


-- ============================================================
-- ### Exercise 6: summation using higher-order functions
-- Implement summation ∑_{i=0}^{n-1} f(i).
-- Then use it to implement doubleSummation ∑_{i=0}^{m-1} ∑_{j=0}^{i-1} f(j).
-- ============================================================

def summation (f : Nat → Nat) (n : Nat) : Nat := sorry
def doubleSummation (f : Nat → Nat) (m : Nat) : Nat := sorry

#assert (summation (fun x => x + 1) 0) == 0
#assert (summation (fun x => x + 1) 1) == 1
#assert (summation (fun x => x * x) 4) == 14
#assert (doubleSummation (fun _ => 1) 4) == 6
#assert (doubleSummation (fun x => x) 4) == 4


-- ============================================================
-- ### Exercise 7: binary tree with map, filter, fold
-- Define the tree type, then implement map, filter, fold.
-- Use fold to reimplement size, height, and sum.
-- ============================================================

inductive Tree (α : Type) where
  | empty
  | leaf (v : α)
  | node (l : Tree α) (r : Tree α)
  deriving Repr, BEq

open Tree

def Tree.map (f : α → β) (t : Tree α) : Tree β := sorry
def Tree.filter (p : α → Bool) (t : Tree α) : Tree α := sorry
def Tree.fold (fempty : β) (fleaf : α → β) (fnode : β → β → β) (t : Tree α) : β := sorry

-- implement these three using only fold
def Tree.size (t : Tree α) : Nat := sorry
def Tree.height (t : Tree α) : Nat := sorry
def Tree.sum (t : Tree Int) : Int := sorry

def exTree := node (node (leaf 1) (leaf 2)) (node (leaf 3) empty)

#assert (exTree.map (· * 2)) == node (node (leaf 2) (leaf 4)) (node (leaf 6) empty)
#assert (exTree.filter (· > 1)) == node (node empty (leaf 2)) (node (leaf 3) empty)
#assert (exTree.size) == 3
#assert (exTree.height) == 2
#assert (exTree.sum) == 6
#assert ((empty : Tree Int).sum) == 0


-- ============================================================
-- ### Exercise 8: ap
-- Apply every function in the first list to every value in the
-- second list. Concatenate results in function order.
-- [f, g] [x, y] => [f x, f y, g x, g y]
-- ============================================================

def ap (fs : List (α → β)) (xs : List α) : List β := sorry

#assert (ap [(· + 1), (· * 2)] [1, 2, 3]) == [2, 3, 4, 2, 4, 6]
#assert (ap ([] : List (Nat → Nat)) [1, 2]) == []
#assert (ap [(· + 1)] ([] : List Nat)) == []
#assert (ap [toString, fun n => toString (n * 10)] [1, 2]) == ["1", "2", "10", "20"]


-- ============================================================
-- ### Exercise 9: groupBy
-- Group consecutive elements that satisfy the same predicate
-- into sublists. Elements satisfying p go into one group,
-- elements not satisfying p go into another, alternating.
-- ============================================================

def groupBy (p : α → Bool) (xs : List α) : List (List α) := sorry

#assert (groupBy (· % 2 == 0) [2, 4, 1, 3, 6]) == [[2, 4], [1, 3], [6]]
#assert (groupBy (· > 0) [1, 2, -1, -2, 3]) == [[1, 2], [-1, -2], [3]]
#assert (groupBy (· > 0) ([] : List Int)) == []
#assert (groupBy (· > 0) [1, 2, 3]) == [[1, 2, 3]]


-- ============================================================
-- ### Exercise 10: Church Booleans
-- Define Church-encoded booleans and basic operations using
-- only lambda abstractions (no match, no if-then-else).
-- ============================================================

def CBool := ∀ {β}, β → β → β

def cTrue : CBool := sorry
def cFalse : CBool := sorry
def cNot (b : CBool) : CBool := sorry
def cAnd (b1 b2 : CBool) : CBool := sorry
def cOr (b1 b2 : CBool) : CBool := sorry

-- test by passing "T" and "F" as the two branches
#assert (cTrue "T" "F") == "T"
#assert (cFalse "T" "F") == "F"
#assert (cNot cTrue "T" "F") == "F"
#assert (cNot cFalse "T" "F") == "T"
#assert (cAnd cTrue cTrue "T" "F") == "T"
#assert (cAnd cTrue cFalse "T" "F") == "F"
#assert (cOr cFalse cTrue "T" "F") == "T"
#assert (cOr cFalse cFalse "T" "F") == "F"


-- ============================================================
-- ### Exercise 11: Church Naturals
-- Define Church-encoded naturals, toNat, and arithmetic.
-- ============================================================

def CNat := ∀ {β}, β → (β → β) → β

def cZero : CNat := sorry
def cSucc (n : CNat) : CNat := sorry
def cToNat (n : CNat) : Nat := sorry
def cAdd (m n : CNat) : CNat := sorry
def cMul (m n : CNat) : CNat := sorry

def cOne := cSucc cZero
def cTwo := cSucc cOne
def cThree := cSucc cTwo

#assert (cToNat cZero) == 0
#assert (cToNat cOne) == 1
#assert (cToNat cTwo) == 2
#assert (cToNat (cAdd cTwo cThree)) == 5
#assert (cToNat (cMul cTwo cThree)) == 6
#assert (cToNat (cMul cZero cThree)) == 0


-- ============================================================
-- ### Exercise 12: inSort with comparator
-- Implement insertion sort parameterized by a comparison
-- function, using fold.
-- ============================================================

def insertSorted (f : α → α → Bool) (x : α) (xs : List α) : List α := sorry
def inSort (f : α → α → Bool) (xs : List α) : List α := sorry

#assert (inSort (· ≤ ·) [3, 1, 4, 2]) == [1, 2, 3, 4]
#assert (inSort (· ≥ ·) [3, 1, 4, 2]) == [4, 3, 2, 1]
#assert (inSort (· ≤ ·) ([] : List Nat)) == []
#assert (inSort (· ≤ ·) ["banana", "apple", "cherry"]) == ["apple", "banana", "cherry"]
#assert (inSort (fun a b => a.length ≤ b.length) ["banana", "hi", "cherry", "ok"]) == ["hi", "ok", "banana", "cherry"]


-- ============================================================
-- ### Exercise 13: Utilities structure with lambdas
-- Define a structure holding functions as fields.
-- Construct an instance by passing configuration at creation time.
-- ============================================================

structure StringProcessor where
  encode : String → String
  decode : String → String
  validate : String → Bool

-- Create a StringProcessor where:
-- encode: reverses the string and appends the key
-- decode: drops key.length chars from the end then reverses
-- validate: checks the string ends with the key (use String.endsWith)
def makeProcessor (key : String) : StringProcessor := sorry

def proc := makeProcessor "abc"

#assert (proc.encode "hello") == "ollehoabc"
-- wait that's wrong, let's be precise:
-- reverse "hello" = "olleh", append "abc" = "ollehabc"
#assert (proc.encode "hello") == "ollehabc"
#assert (proc.decode (proc.encode "hello")) == "hello"
#assert (proc.validate (proc.encode "hello")) == true
#assert (proc.validate "hello") == false


-- ============================================================
-- ### Exercise 14: find and all/any via fold on Tree
-- Use Tree.fold to implement find?, all, and any on Trees.
-- ============================================================

def Tree.find? (p : α → Bool) (t : Tree α) : Option α := sorry
def Tree.all (p : α → Bool) (t : Tree α) : Bool := sorry
def Tree.any (p : α → Bool) (t : Tree α) : Bool := sorry

#assert (exTree.find? (· > 2)) == some 3
#assert (exTree.find? (· > 10)) == none
#assert (exTree.all (· > 0) exTree) == true
#assert (exTree.all (· > 1) exTree) == false
#assert (exTree.any (· > 2) exTree) == true
#assert (exTree.any (· > 10) exTree) == false


end HOFExercises
