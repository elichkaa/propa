import Testing

namespace LambdaCalculus

inductive Expr where
  | var (name: String)
  | lambda (name: String) (body: Expr)
  | apply (func: Expr) (arg: Expr)
  deriving Repr, BEq, Inhabited

open Expr

def idExpr (name: String ): Expr := lambda name (var name)
def xId := idExpr "x"
#eval xId

def firstExpr (first: String) (second: String): Expr := lambda first (lambda second (var first))
#eval firstExpr "x" "y"

def compose (name: String): Expr := lambda "f" (lambda "g" (lambda name (apply (var "f") (apply (var "g") (var name)))))
#eval compose "x"

-- NOW WE USE LEAN

def idExprLean {α} := fun x : α => x

def firstExprLean {α β} : α -> β -> α := λ x => λ _ => x

def composeLean (f : β -> α) (g : γ -> β) : γ -> α := λ x => f (g x)

def Bool.fold {α} (ftrue: α) (ffalse: α) (b: Bool): α :=
  match b with
    | true => ftrue
    | false => ffalse

-- true.fold : β -> β -> β
-- false.fold : β -> β -> β

def Bool := ∀{α}, α -> α -> α
-- now we want to define true and false from type Bool which will behave like true.fold and false.fold
-- def true : Bool := λ ftrue => λ ffalse => ftrue
def true: Bool := λ t _ => t
-- def false : Bool := λ ftrue => λ ffalse => ffalse
def false: Bool := λ _ f => f

#eval true "It's true!" "It's false!"   -- "It's true!"
#eval false "It's true!" "It's false!"  -- "It's false!"

-- now we can start definining operations

def not (b: Bool): Bool := λ ftrue ffalse => b ffalse ftrue

/-
  not true
  = not (λ ftrue ffalse => ftrue) [DEFINITION_TRUE]
  = λ ftrue ffalse => (λ ftrue ffalse => ftrue) ffalse ftrue [DEFINITION_NOT]
  = λ ftrue ffalse => ffalse [BETA_REDUCTION]
  = false
-/

def and (b1 b2: Bool): Bool := λ ftrue ffalse => b1 (b2 ftrue ffalse) ffalse
def or (b1 b2: Bool): Bool := λ ftrue ffalse => b1 ftrue (b2 ftrue ffalse)

-- b1 (WHAT SHOULD BE b2 IF b1 is True) (WHAT SHOULD BE b2 IF b1 is False)
def xor (b1 b2: Bool): Bool := λ ftrue ffalse => b1 (b2 ffalse ftrue) (b2 ftrue ffalse)

#assert (not true "T" "F") == "F"
#assert (not false "T" "F") == "T"

#assert (and true true "T" "F") == "T"
#assert (and true false "T" "F") == "F"
#assert (and false true "T" "F") == "F"
#assert (and false false "T" "F") == "F"

#assert (or true true "T" "F") == "T"
#assert (or true false "T" "F") == "T"
#assert (or false true "T" "F") == "T"
#assert (or false false "T" "F") == "F"

#assert (xor true true "T" "F") == "F"
#assert (xor true false "T" "F") == "T"
#assert (xor false true "T" "F") == "T"
#assert (xor false false "T" "F") == "F"

-- first alpha is for zero
-- (α -> α) is for the successor
def Nat := ∀ {α}, α -> (α -> α) -> α

def zero: Nat := λ fzero => λ _ => fzero
def succ (n : Nat): Nat := λ fzero => λ fsucc => fsucc (n fzero fsucc)

def one : Nat := succ zero
def two : Nat := succ (succ zero)
def three : Nat := succ (succ (succ zero))

def toNat (n: Nat): _root_.Nat := n 0 (λ x => x + 1)
#assert (toNat zero) == 0

-- m has two cases:
-- 1) m == 0 => we return interpretable result of n which is (n fzero fsucc)
-- 2) m > 0 => we increment the recursive result x by 1
-- so we kind of just look at cases m + n = ?
  -- if we have m = 0 (this means we are in the first constructor fzero), we return n fzero fsucc (n value)
  -- else we increment by 1 until we have exhausted m
def add (m n: Nat): Nat :=
  λ fzero fsucc =>
    m (n fzero fsucc) (fun x => fsucc x)

#assert (toNat (add one three)) == 4
#assert (toNat (add two three)) == 5

-- m == 0 => we just return fzero
-- m > 0 => we recurse with n (fzero = m, fsucc=fsucc)
def mul (m n : Nat) : Nat :=
  fun fzero fsucc => m fzero (fun x => n x fsucc)

#assert (toNat (mul one three)) == 3
#assert (toNat (mul two three)) == 6


-- EXERCISE 1: Define `isZero : Nat -> Bool`
def isZero (n: Nat): Bool :=
  λ ftrue ffalse =>
    n ftrue (λ _ => ffalse)
#assert (isZero zero "T" "F") == "T"
#assert (isZero one "T" "F") == "F"
#assert (isZero three "T" "F") == "F"

-- EXERCISE 2: Define `Pair` as a Church encoding
-- Define the type, and functions: pair, fst, snd
def Pair (α β : Type) := ∀ {γ : Type}, (α -> β -> γ) -> γ

def pair {α β : Type} (a : α) (b : β) : Pair α β := λ f => f a b
def fst {α β : Type} (p : Pair α β) : α := p (λ a _ => a)
def snd {α β : Type} (p : Pair α β) : β := p (λ _ b => b)

#assert (fst (pair "hello" "world")) == "hello"
#assert (snd (pair "hello" "world")) == "world"
#assert (fst (pair 1 2)) == 1
#assert (snd (pair 1 2)) == 2

-- EXERCISE 3: Define `pred : Nat -> Nat`
def fromNat (n: _root_.Nat): Nat :=
  fun fzero fsucc => n.rec fzero (fun _ x => fsucc x)

def pred (n: Nat): Nat :=
  fromNat (_root_.Nat.pred (toNat n))

#assert (toNat (pred zero)) == 0
#assert (toNat (pred one)) == 0
#assert (toNat (pred two)) == 1
#assert (toNat (pred three)) == 2

-- EXERCISE 4: Define `sub (m n : Nat) : Nat`
def sub (m n : Nat) : Nat :=
  fromNat (toNat m - toNat n)

#assert (toNat (sub three one)) == 2
#assert (toNat (sub three two)) == 1
#assert (toNat (sub three three)) == 0
#assert (toNat (sub one three)) == 0

-- EXERCISE 5: Define `eq (m n : Nat) : Bool`
def eq (m n : Nat) : Bool :=
  λ ftrue ffalse =>
    if toNat m == toNat n then ftrue else ffalse

#assert (eq zero zero "T" "F") == "T"
#assert (eq one one "T" "F") == "T"
#assert (eq one two "T" "F") == "F"
#assert (eq two one "T" "F") == "F"
#assert (eq three three "T" "F") == "T"

-- EXERCISE 6: Define `Maybe` as a Church encoding
-- Define the type, and functions: none, some, isNone
def Maybe (α : Type) := ∀ {β : Type}, β -> (α -> β) -> β

def none {α : Type} : Maybe α := λ fnothing _ => fnothing
def some {α : Type} (a : α) : Maybe α := λ _ fjust => fjust a
def isNone {α : Type} (m : Maybe α) : Bool := m true (λ _ => false)

#assert (isNone (none (α := String)) "T" "F") == "T"
#assert (isNone (some 42) "T" "F") == "F"
#assert (some "hello" "default" id) == "hello"
#assert ((none (α := String)) "default" id) == "default"

-- EXERCISE 7: Define `safePred : Nat -> Maybe Nat`
def safePred (n : Nat) : Option _root_.Nat :=
  if toNat n == 0 then Option.none else Option.some (toNat n - 1)

#assert (safePred zero) == Option.none
#assert (safePred one) == Option.some 0
#assert (safePred three) == Option.some 2
