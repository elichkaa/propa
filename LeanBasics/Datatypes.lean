/- Datatypes
- allows choices -> sum types
- can include instances of itself -> recursive types
- recursive + sum -> inductive types (mathematical induction can be used to prove statements about them)
-/

/- Bool is inductive
the lines starting with | describe constructors (containers for other data, not a place for initialization and validation)
inductive types may have multiple constructors (unlike structures)
here there are two constructors (true and false) and neither take arguments
-/
inductive Bool2 where
  | false : Bool2
  | true : Bool2

inductive Nat2 where
  | zero : Nat2 -- represents 0
  | succ (n : Nat2) : Nat2 -- represents the successor of some other number

-- pattern matching
def isZero (n: Nat) : Bool := -- returns true if n is zero else false
  match n with
    | Nat.zero => true
    | Nat.succ _ => false -- k makes the argument to Nat.succ visible (with the provided name k)

def pred (n: Nat) : Nat :=
  match n with
    | Nat.zero => Nat.zero
    | Nat.succ k => k

#eval pred (Nat.succ 4)
#eval pred 5

structure Point3D where
  x: Float
  y: Float
  z: Float

-- extracts the third dimension of a point 3D
def depth (p: Point3D): Float :=
  match p with
    | { x := _, y := _, z := d} => d

#eval isZero 5

-- recursive functions
def even (n : Nat) : Bool :=
  match n with
    | Nat.zero => true
    | Nat.succ k => not (even k)

/-
Lean must make sure the recursive function reaches a base case
-> this rules out infinite loops
- a version of even which attempts to invoke itself recursively is not accepted

def evenLoops (n : Nat) : Bool :=
  match n with
  | Nat.zero => true
  | Nat.succ k => not (evenLoops n)
-/

def plus (n: Nat) (k: Nat) : Nat :=
  match k with
    | Nat.zero => n
    | Nat.succ k' => Nat.succ (plus n k')

#eval plus 3 2

def minus (n: Nat) (k: Nat) : Nat :=
  match k with
    | Nat.zero => n
    | Nat.succ k' => pred (minus n k')

#eval minus 7 3

def times (n: Nat) (k: Nat) : Nat :=
  match k with
    | Nat.zero => Nat.zero
    | Nat.succ k' => plus n (times n k')
