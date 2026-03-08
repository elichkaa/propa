import Testing

namespace ChurchEncoding

-- BOOLS

def Bool := ∀{α}, α -> α -> α
def true : Bool := λ ftrue _ => ftrue
def false : Bool := λ _ ffalse => ffalse

def test {α β γ} := λ (l : α → β → γ) (m : α) (n : β) => l m n
#eval test true 1 2 -- 1
#eval test false 1 2 -- 2

-- b1 ∧ b2 => if b1 then b2 else false
def and (b1 b2: Bool): Bool := b1 b2 false
-- "T" and "F" are the ftrue/ffalse arguments of the resulting Bool
#assert (and true false "T" "F") == "F"
#assert (and true true "T" "T") == "T"
#assert (and true false 1 0) == 0

-- with this implementation we are explicitly building a new bool (defining the branches)
-- def not (b: Bool): Bool := λ ftrue ffalse => b ffalse ftrue

-- with this one we are reusing the existing methods
def not (b: Bool): Bool := b false true
#assert (not true "T" "F") == "F"
#assert (not false "T" "F") == "T"
#assert (not false 1 0) == 1

def or (b1 b2: Bool): Bool := b1 true b2
#assert (or true false 1 0) == 1
#assert (or true true 1 0) == 1
#assert (or false true 1 0) == 1
#assert (or false false 1 0) == 0

-- def xor (b1 b2: Bool): Bool := b1 (not b2) b2
def xor (b1 b2: Bool): Bool := λ ftrue ffalse => b1 (b2 ffalse ftrue) (b2 ftrue ffalse)
#assert (xor true false 1 0) == 1
#assert (xor true true 1 0) == 0
#assert (xor false true 1 0) == 1
#assert (xor false false 1 0) == 0

-- PAIRS

def Pair (α β: Type) := ∀{γ : Type}, (α -> β -> γ) -> γ
def pair {α β : Type} (a : α) (b : β) : Pair α β := λ f => f a b
-- pair 1 2 = λ f => f 1 2
def fst {α β} (p: Pair α β) := p (λ a _ => a)
/-
fst (pair 1 2) = (λ f => f 1 2) (λ a _ => a)
               = (λ a _ => a) 1 2
               = 1
-/
def snd {α β} (p: Pair α β) := p (λ _ b => b)

#assert (fst (pair 1 2)) == 1
#assert (snd (pair 1 2)) == 2
#assert ((pair 1 2) (λ a b => a + b)) == 3

-- NUMERALS

def c_0 {α} (z : α) (_ : α -> α) : α := z
def c_1 {α} (z : α) (s : α -> α) : α := s z
def c_2 {α} (z : α) (s : α -> α) : α := s (s z)
def c_3 {α} (z : α) (s : α -> α) : α := s (s (s z))

def Nat := ∀{α}, α -> (α -> α) -> α
def zero : Nat := λ fzero _ => fzero
def succ (n : Nat) : Nat := λ fzero fsucc => fsucc (n fzero fsucc)

-- yields Nat (e.g. a function that accepts arguments s z, applies s iterated n times to z) and
-- then applies s iterated m more times to the result
def plus (m n: Nat): Nat := λ fzero fsucc =>
  m (n fzero fsucc) (fun x => fsucc x)

def mul (m n : Nat) : Nat := λ fzero fsucc =>
  m fzero (fun x => n x fsucc)

def isZero (n: Nat): Bool :=
  λ ftrue ffalse =>
    n ftrue (λ _ => ffalse)

def pow (m n : Nat) : Nat := λ fzero fsucc =>
  n fsucc (fun g x => m x g) fzero

def toNat (n: Nat): _root_.Nat := n 0 (λ x => x + 1)

def one : Nat := succ zero
def two : Nat := succ one
def three : Nat := succ two

#assert (toNat (mul two three)) == 6
#assert (toNat (mul zero three)) == 0
#assert (toNat (mul one three)) == 3
#assert (toNat (mul two two)) == 4

#assert (toNat (pow two zero)) == 1
#assert (toNat (pow two one)) == 2
#assert (toNat (pow two three)) == 8
#assert (toNat (pow three two)) == 9
#assert (toNat (pow zero three)) == 0
