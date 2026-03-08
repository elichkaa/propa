import Testing

namespace HOFExercises

-- ============================================================
-- ### Exercise 1: implement map, filter, zipWith from scratch
-- Do not use the built-in versions. Use structural recursion.
-- ============================================================

def myMap (f : α → β) (xs : List α) : List β :=
  match xs with
    | [] => []
    | x :: xs => (f x) :: myMap f xs

def myFilter (p : α → Bool) (xs : List α) : List α :=
  match xs with
    | [] => []
    | x :: xs => if p x then x :: myFilter p xs else myFilter p xs

def myZipWith (f : α → β → γ) (xs : List α) (ys : List β) : List γ :=
  match xs, ys with
    | [], _ => []
    | _, [] => []
    | x :: xs, y :: ys => (f x y) :: myZipWith f xs ys

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

-- element first then accumulated rest (produces inorder result) (not tail-recursive)
def myFoldr (init : β) (combine : α → β → β) (xs : List α) : β :=
  match xs with
    | [] => init
    | x :: xs => combine x (myFoldr init combine xs)

-- first accumulator then element (produces reversed result) (tail-recursive)
def myFoldl (combine : β → α → β) (init : β) (xs : List α) : β :=
  match xs with
  | [] => init
  | x :: xs => myFoldl combine (combine init x) xs

-- now define these using only myFold, no recursion, no other list functions
def mySumr (xs : List Int) : Int := myFoldr 0 (fun hd acc => hd + acc) xs
def mySuml (xs : List Int) : Int := myFoldl (fun acc hd => acc + hd) 0 xs

#assert (mySumr [1, 2, 3, 4]) == 10
#assert (mySumr []) == 0
#assert (mySuml [1, 2, 3, 4]) == 10
#assert (mySuml []) == 0

def myProductr (xs : List Nat) : Nat := myFoldr 1 (fun hd acc => hd * acc) xs
def myProductl (xs : List Nat) : Nat := myFoldl (fun acc hd => acc * hd) 1 xs

#assert (myProductr [1, 2, 3, 4]) == 24
#assert (myProductr []) == 1
#assert (myProductl [1, 2, 3, 4]) == 24
#assert (myProductl []) == 1

def myLength (xs : List α) : Nat := myFoldr 0 (fun _ acc => 1 + acc) xs

#assert (myLength [1, 2, 3]) == 3
#assert (myLength ([] : List Nat)) == 0


def myReverser (xs : List α) : List α := myFoldr [] (fun hd acc => acc ++ [hd]) xs
def myReversel (xs : List α) : List α := myFoldl (fun acc hd => hd :: acc) [] xs

#assert (myReverser [1, 2, 3]) == [3, 2, 1]
#assert (myReverser ([] : List Nat)) == []
#assert (myReversel [1, 2, 3]) == [3, 2, 1]
#assert (myReversel ([] : List Nat)) == []

def myAppendr (xs ys : List α) : List α := myFoldr ys (fun hd acc => hd :: acc) xs
def myAppendl (xs ys : List α) : List α := myFoldl (fun acc hd => acc ++ [hd]) xs ys

#assert (myAppendr [1, 2] [3, 4]) == [1, 2, 3, 4]
#assert (myAppendr ([] : List Nat) [1]) == [1]
#assert (myAppendl [1, 2] [3, 4]) == [1, 2, 3, 4]
#assert (myAppendl ([] : List Nat) [1]) == [1]


-- ============================================================
-- ### Exercise 3: left fold
-- Implement left fold (processes left to right with accumulator).
-- Then use it to define flatten and countWhere.
-- ============================================================

def myFlatten (xs : List (List α)) : List α := myFoldl (fun acc hd => acc ++ hd) [] xs
def myCountWhere (p : α → Bool) (xs : List α) : Nat := myFoldl (fun acc hd => if p hd then 1 + acc else acc) 0 xs

#assert (myFoldl (fun acc x => acc - x) 10 [1, 2, 3]) == 4
#assert (myFoldl (fun acc x => x :: acc) [] [1, 2, 3]) == [3, 2, 1]
#assert (myFlatten [[1, 2], [3], [4, 5]]) == [1, 2, 3, 4, 5]
#assert (myFlatten ([] : List (List Nat))) == []
#assert (myCountWhere (· > 2) [1, 2, 3, 4]) == 2
#assert (myCountWhere (· > 2) []) == 0


-- ============================================================
-- ### Exercise 4: takeWhile and dropWhile
-- ============================================================

def takeWhile (p : α → Bool) (xs : List α) : List α :=
  match xs with
  | [] => []
  | x :: xs => if p x then x :: takeWhile p xs else []

def dropWhile (p : α → Bool) (xs : List α) : List α :=
  match xs with
  | [] => []
  | x :: xs => if p x then dropWhile p xs else x :: xs

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

def compose (f : β → γ) (g : α → β) : α → γ := λ x => f (g x)

def applyN (f : α → α) (n : Nat) (x : α) : α :=
  match n with
    | 0 => x
    | n + 1 => applyN f n (f x)

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

def summation (f : Nat → Nat) (n : Nat) : Nat :=
  match n with
    | 0 => 0
    | n + 1 => f n + summation f n

def doubleSummation (f : Nat → Nat) (m : Nat) : Nat :=
  summation (fun i => summation f i) m

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

def Tree.map (f : α → β) (t : Tree α) : Tree β :=
  match t with
    | empty => empty
    | leaf v => leaf (f v)
    | node l r => node (l.map f) (r.map f)

def Tree.filter (p : α → Bool) (t : Tree α) : Tree α :=
  match t with
    | empty => empty
    | leaf v => if p v then leaf v else empty
    | node l r => node (l.filter p) (r.filter p)

def Tree.fold (fempty : β) (fleaf : α → β) (fnode : β → β → β) (t : Tree α) : β :=
  match t with
    | empty => fempty
    | leaf v => fleaf v
    | node l r => fnode (l.fold fempty fleaf fnode) (r.fold fempty fleaf fnode)

-- implement these three using only fold
def Tree.size (t : Tree α) : Nat := t.fold 0 (fun _ => 1) (fun l r => l + r)
def Tree.height (t : Tree α) : Nat := t.fold 0 (fun _ => 0) (fun l r => 1 + max l r)
def Tree.sum (t : Tree Int) : Int := t.fold 0 (fun v => v) (fun l r => l + r)

def exTree := node (node (leaf 1) (leaf 2)) (node (leaf 3) empty)

#assert (exTree.map (· * 2)) == node (node (leaf 2) (leaf 4)) (node (leaf 6) empty)
#assert (exTree.filter (· > 1)) == node (node empty (leaf 2)) (node (leaf 3) empty)
#assert (exTree.size) == 3
#assert (exTree.height) == 2
def exTree' := node (node (leaf (1 : Int)) (leaf 2)) (node (leaf 3) empty)
#assert (exTree'.sum) == 6
#assert ((empty : Tree Int).sum) == 0


-- ============================================================
-- ### Exercise 8: ap
-- Apply every function in the first list to every value in the
-- second list. Concatenate results in function order.
-- [f, g] [x, y] => [f x, f y, g x, g y]
-- ============================================================

def ap (fs : List (α → β)) (xs : List α) : List β :=
  fs.foldl (fun acc hd => acc ++ List.map hd xs) []

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

def groupBy (p : α → Bool) (xs : List α) : List (List α) :=
  go xs [] [] true
where go xs acc grp grpTrue :=
  match xs with
  | [] => if grp.isEmpty then acc else acc ++ [grp]
  | x :: rest =>
    if p x == grpTrue then
      go rest acc (grp ++ [x]) grpTrue
    else
      go rest (acc ++ [grp]) [x] (p x)

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

def cTrue : CBool := λ ftrue _ => ftrue
def cFalse : CBool := λ _ ffalse => ffalse
def cNot (b : CBool) : CBool := λ ftrue ffalse => b ffalse ftrue
def cAnd (b1 b2 : CBool) : CBool := λ ftrue ffalse => b1 (b2 ftrue ffalse) ffalse
def cOr (b1 b2 : CBool) : CBool := λ ftrue ffalse => b1 ftrue (b2 ftrue ffalse)

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

abbrev CNat := ∀ {β}, β → (β → β) → β

def cZero : CNat := λ fzero _ => fzero
def cSucc (n : CNat) : CNat := λ fzero fsucc => fsucc (n fzero fsucc) -- (n fzero fsucc) is n
def cToNat (n : CNat) : Nat := n 0 (· + 1)
def cAdd (m n : CNat) : CNat := λ fzero fsucc => m (n fzero fsucc) fsucc
def cMul (m n : CNat) : CNat := λ fzero fsucc => m fzero (fun x => n x fsucc)

def cOne : CNat := cSucc cZero
def cTwo : CNat := cSucc cOne
def cThree: CNat := cSucc cTwo

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

def insertSorted (f : α → α → Bool) (x : α) (xs : List α) : List α :=
  match xs with
    | [] => [x]
    | x' :: rest => if f x x' then x :: x' :: rest else x' :: insertSorted f x rest

def inSort (f : α → α → Bool) (xs : List α) : List α :=
  xs.foldr (fun hd acc => insertSorted f hd acc) []

#assert (inSort (· ≤ ·) [3, 1, 4, 2]) == [1, 2, 3, 4]
#assert (inSort (· ≥ ·) [3, 1, 4, 2]) == [4, 3, 2, 1]
#assert (inSort (· ≤ ·) ([] : List Nat)) == []
#assert (inSort (· ≤ ·) ["banana", "apple", "cherry"]) == ["apple", "banana", "cherry"]
#assert (inSort (fun a b => a.length ≤ b.length) ["banana", "hi", "cherry", "ok"]) == ["hi", "ok", "banana", "cherry"]


-- ============================================================
-- ### Exercise 13: find and all/any via fold on Tree
-- Use Tree.fold to implement find?, all, and any on Trees.
-- ============================================================

def Tree.find? (p : α → Bool) (t : Tree α) : Option α :=
  t.fold none (fun v => if p v then some v else none) (fun l r => l <|> r)

def Tree.all (p : α → Bool) (t : Tree α) : Bool :=
  t.fold true (fun v => p v) (fun l r => l && r)

def Tree.any (p : α → Bool) (t : Tree α) : Bool :=
  t.fold false (fun v => p v) (fun l r => l || r)

#assert (exTree.find? (· > 2)) == some 3
#assert (exTree.find? (· > 10)) == none
#assert (exTree.all (· > 0)) == true
#assert (exTree.all (· > 1)) == false
#assert (exTree.any (· > 2)) == true
#assert (exTree.any (· > 10)) == false


end HOFExercises
