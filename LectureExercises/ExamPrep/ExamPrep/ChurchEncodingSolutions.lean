import Testing

namespace Church

-- BOOLS
def Bool := ∀{α}, α -> α -> α

-- Exercise 1: true picks the first argument
def true: Bool := λ ftrue _ => ftrue

-- Exercise 2: false picks the second argument
def false: Bool := λ _ ffalse => ffalse

-- Exercise 3: flip the two arguments
def not (b : Bool) : Bool := λ ftrue ffalse => b ffalse ftrue

-- Exercise 4: if a then b else false
def and (a b : Bool) : Bool := λ ftrue ffalse => a (b ftrue ffalse) ffalse

-- Exercise 5: if a then true else b
def or (a b : Bool) : Bool := λ ftrue ffalse => a ftrue (b ftrue ffalse)

-- NATs
def Nat := ∀{α}, α -> (α -> α) -> α
def zero : Nat := λ fzero _ => fzero
def succ (n : Nat) : Nat := λ fzero fsucc => fsucc (n fzero fsucc)
def one : Nat := succ zero
def two : Nat := succ (succ zero)
def three : Nat := succ (succ (succ zero))

def plus (a b: Nat): Nat := λ fzero fsucc =>
  a (b fzero fsucc) (fun x => fsucc x)

def mul (a b: Nat): Nat := λ fzero fsucc =>
  a fzero (fun x => b x fsucc)

def pow (a b: Nat): Nat := λ fzero fsucc =>
  b fsucc (fun g x => a x g) fzero

-- Exercise 6: zero → true, anything else → false
-- Hint: base = true, step should ignore input and return false
def isZero (n : Nat) : Bool := λ ftrue ffalse =>
  n ftrue (fun _ => ffalse)

-- Exercise 7: 0→true, 1→false, 2→true, 3→false...
-- Hint: start with true, flip each step
def isEven (n : Nat) : Bool := λ ftrue ffalse =>
  n (fun t _ => t) (fun x t f => x f t) ftrue ffalse

-- PAIRS
def Pair (A B : Type) := ∀{α}, (A → B → α) → α

-- Exercise 8: store a and b, wait for a destructor function
def mkPair {A B} (a : A) (b : B) : Pair A B := λ f => f a b

-- Exercise 9: give the pair a function that takes first, ignores second
def fst {A B} (p : Pair A B) : A := p (fun a _ => a)

-- Exercise 10: give the pair a function that ignores first, takes second
def snd {A B} (p : Pair A B) : B := p (fun _ b => b)

-- Exercise 11: make a new pair with elements swapped
def swap {A B} (p : Pair A B) : Pair B A := p (fun a b => mkPair b a)


-- Exercise 12: predecessor (pred 0 = 0, pred 3 = 2)
-- Hint: iterate n times on a pair:
--   start: (0, 0)
--   step:  (k, prev) → (k+1, k)
--   after n steps: (n, n-1), take snd
def pred (n : Nat) : Nat :=
  snd (n (mkPair zero zero) (fun p => mkPair (succ (fst p)) (fst p)))

-- Exercise 13: subtraction (monus: sub 2 5 = 0)
-- Hint: apply pred n times to m
def sub (m n : Nat) : Nat := sorry

-- Exercise 14: fibonacci (fib 0=0, fib 1=1, fib 2=1, fib 3=2, fib 4=3)
-- Hint: iterate n times on a pair:
--   start: (0, 1) = (fib 0, fib 1)
--   step:  (a, b) → (b, a+b)
--   take fst
def fib (n : Nat) : Nat := sorry

-- Exercise 15: factorial (fact 0=1, fact 3=6, fact 4=24)
-- Hint: iterate n times on a pair:
--   start: (1, 1) = (counter, accumulator)
--   step:  (i, acc) → (i+1, i*acc)
--   take snd
def factorial (n : Nat) : Nat := sorry

-- MAYBE
def Maybe (A : Type) := ∀{α}, α → (A → α) → α

-- Exercise 16: nothing — ignore the function, return base
def nothing : Maybe A := sorry

-- Exercise 17: just — apply the function to the value
def just (a : A) : Maybe A := sorry

-- Exercise 18: unwrap with a default
def fromMaybe (default : A) (m : Maybe A) : A := sorry

-- Exercise 19: apply f inside the Maybe
-- Hint: if nothing, return nothing; if just a, return just (f a)
def mapMaybe (f : A → B) (m : Maybe A) : Maybe B := sorry


-- LISTS
def List (A : Type) := ∀{α}, α → (A → α → α) → α

-- Exercise 20: empty list
def nil : List A := sorry

-- Exercise 21: prepend an element
-- Hint: like succ for Nat, but step also receives the element
def cons (x : A) (xs : List A) : List A := sorry

-- Exercise 22: count elements (return a Nat)
-- Hint: ignore each element, just succ the accumulator
def length (xs : List A) : Nat := sorry

-- Exercise 23: apply f to each element
def mapList (f : A → B) (xs : List A) : List B := sorry

-- Exercise 24: concatenate two lists
-- Hint: replace the base of xs with ys
def append (xs ys : List A) : List A := sorry

-- Exercise 25: sum a list of Nats
-- Hint: base = zero, step = add element to accumulator
def sum (xs : List Nat) : Nat := sorry

-- Exercise 26: first element as Maybe
-- Hint: step function ignores accumulator, returns just x
def head (xs : List A) : Maybe A := sorry

-- Exercise 27: reverse a list
-- Hint: the accumulator is a FUNCTION (same trick as pow!)
--   base: identity function
--   step: fun x cont acc => cont (f x acc)
--   apply result to base
def reverse (xs : List A) : List A := sorry

-- Exercise 28: keep only elements where predicate is true
-- Hint: use Bool to choose between (f x acc) and acc
def filter (p : A → Bool) (xs : List A) : List A := sorry
