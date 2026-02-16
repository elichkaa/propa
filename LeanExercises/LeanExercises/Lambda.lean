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
def true : Bool := λ ftrue => λ ffalse => ftrue
def false : Bool := λ ftrue => λ ffalse => ffalse

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

def zero: Nat := λ fzero => λ fsucc => fzero
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
