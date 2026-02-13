-- polymorphism - datatypes / definitions can take types as arguments

structure PPoint (α : Type) where
  x : α
  y : α

def natOrigin: PPoint Nat :=
  { x := Nat.zero, y := Nat.zero}

def replaceX (α: Type) (point: PPoint α) (newX: α): PPoint α :=
  { point with x := newX }
#check replaceX -- replaceX (α : Type) (point : PPoint α) (newX : α) : PPoint α
#check replaceX Nat -- replaceX Nat : PPoint Nat → Nat → PPoint Nat
#check replaceX Nat natOrigin 5

inductive Sign where
  | pos
  | neg

def posOrNegThree (s: Sign) :
  match s with
    | Sign.pos => Nat
    | Sign.neg => Int :=
      match s with
      | Sign.pos => (3: Nat)
      | Sign.neg => (-3: Int)

#eval posOrNegThree Sign.neg -- -3
#eval posOrNegThree Sign.pos -- 3

def primesUnder10 : List Nat := [2, 3, 5, 7]
-- definition of List behind the scenes
inductive List2 (α : Type) where
  | nil : List2 α -- nil represents empty lists
  | cons: α -> List2 α -> List2 α -- cons represents non-empty lists; the first argument to cons is the head of the list and the second is the tail; a list with n entries contains n cons constructors the last of which has nil as its tail

def explicitPrimesUnder10 : List Nat :=
  List.cons 2 (List.cons 3 (List.cons 5 (List.cons 7 List.nil)))

/- length is:
- polimorphic because it takes the list entry type as an argument
- recursive because it refers to itself
-/
def length (α : Type) (xs : List α) : Nat :=
  match xs with
  | List.nil => Nat.zero
  | List.cons _ ys => Nat.succ (length α ys)

def length2 (α : Type) (xs : List α) : Nat :=
  match xs with
  | [] => 0
  | _ :: ys => Nat.succ (length α ys)

#eval length String ["Sourdough", "bread"]


-- implicit arguments (by wrapping them in curly braces)
def replaceX_implicit {α : Type} (point: PPoint α) (newX : α) : PPoint α :=
  {point with x := newX }
#eval replaceX_implicit natOrigin 5
-- vs. explicit
def replace_explicit (α: Type) (point: PPoint α) (newX: α): PPoint α :=
  { point with x := newX }
#eval replace_explicit Nat natOrigin 5

def length_implicit {α : Type} (xs: List α) : Nat :=
  match xs with
    | [] => 0
    | _ :: ys => Nat.succ (length_implicit ys)
#eval length_implicit primesUnder10
#eval length Nat primesUnder10
#eval primesUnder10.length

-- Option - datatype that equips some other type with an indicator for missing values
-- ex. nullable Int is represented by Option Int; nullable list of strings = Option (List String)
-- with option the type system ensures that checks for null cannot be forgotten
-- two constructors: none (null version of type) and some (non-null version of type)
inductive OptionDef (α: Type): Type where
  | none: OptionDef α
  | some (val: α): OptionDef α

def List.head?_Def {α: Type} (xs: List α): Option α :=
  match xs with
    | [] => none
    | y :: _ => some y

#eval primesUnder10.head?
-- #eval [].head? returns errors because Lean can't determine the expression's type
-- (cant find neither implicit nor explicit type argument to List.nil)

-- #eval [].head? -- error
#eval [].head? (α := Int) -- none
#eval ([] : List Int).head? -- none


-- Prod - structure which multiplies two values together
-- PPoint Nat = Prod Nat Nat
-- Prod α β = α x β
structure ProdDef (α β : Type) : Type where
  fst : α
  snd : β

def fives: String × Int := {fst := "five", snd := 5}
def fives_v2: String × Int := ("five", 5)
def sevens: String × Int × Nat := ("VII", 7, (4 + 3))

def swap {α β : Type} (pair : α × β) : β × α :=
  (pair.snd, pair.fst)
def swap2 {α β : Type} (pair : Prod α β) : Prod β α :=
  (pair.snd, pair.fst)

#eval swap ("hello", 42)  -- (42, "hello")
#eval swap2 ("hello", 42)  -- (42, "hello")

-- Sum - structure which sums two values together
-- Sum α β = α ⊕ β
-- Sum α β represents a value that is either type α or type β (but not both)
-- the constructors tag which side it is
  -- inl wraps a value of type α
  -- inr wraps a value of type β
structure SumDef (α β: Type) : Type where
  inl : α -> Sum α β -- left injection
  inr : β -> Sum α β -- right injection

def PetName: Type := String ⊕ String

def animals: List PetName :=
  [Sum.inl "Spot", Sum.inr "Tiger", Sum.inl "Fifi",
  Sum.inl "Rex", Sum.inr "Floof"]
#eval animals
def howManyDogs (pets : List PetName) : Nat :=
  match pets with
  | [] => 0
  | Sum.inl _ :: morePets => howManyDogs morePets + 1
  | Sum.inr _ :: morePets => howManyDogs morePets
#eval howManyDogs animals

-- Unit - type with just one argumentless constructor, called unit
-- similar to void
-- it can be used as a placeholder for data that is missing
-- describes only a single value, which consists of the constructor applied to no arguments whatsoever
inductive UnitDef: Type where
  | unit: UnitDef

-- () is shorthand for Unit.unit
def doNothing : Unit := ()

inductive ArithExpr (ann : Type) : Type where
  | int : ann → Int → ArithExpr ann
  | plus : ann → ArithExpr ann → ArithExpr ann → ArithExpr ann
  | minus : ann → ArithExpr ann → ArithExpr ann → ArithExpr ann
  | times : ann → ArithExpr ann → ArithExpr ann → ArithExpr ann
-- each constructor is annotated (ann)
-- expressions coming from a parser might be annotated with source locations, so a return type of ArithExpr SourcePos ensures that the parser put a SourcePos at each subexpression
-- expressions that don't come from the parser, however, will not have source locations so their type can be ArithExpr Unit

-- Empty - type without constructors
-- indicates unreachable code because no series of calls can ever terminate with a value at type Empty

def lastListEntry {α: Type} (xs: List α): Option α :=
  match xs with
    | [] => none
    | [x] => some x
    | _ :: xs => lastListEntry xs

#eval lastListEntry primesUnder10
#eval primesUnder10.getLast?

def List.findFirst? {α : Type} (xs: List α) (predicate: α -> Bool) : Option α :=
  match xs with
    | [] => none
    | x :: xs => if predicate x then some x else List.findFirst? xs predicate

#eval primesUnder10.findFirst? (. > 5)
-- (· > 5) is shorthand for (fun x => x > 5)

def zip {α β: Type} (xs: List α) (ys: List β) : List (α × β) :=
  match xs, ys with
    | x :: xs, y :: ys => (x, y) :: zip xs ys
    | _, _ => []

#eval zip [1, 2, 3] [4, 5, 6]

def take {α: Type} (xs: List α) (n: Nat) : List α :=
  match xs, n with
    | _, 0 => []
    | [], _ => []
    | x :: xs, n => x :: take xs (n - 1)

#eval take primesUnder10 2

def distribute {α β γ : Type} : α × (β ⊕ γ) -> (α × β) ⊕ (α × γ)
  | (a, Sum.inl b) => Sum.inl (a, b)
  | (a, Sum.inr c) => Sum.inr (a, c)

#check distribute

def duplicate {α : Type} (xs : List α) : List α :=
  match xs with
    | [] => []
    | x :: _ => x :: xs  -- use the original parameter xs

#eval duplicate [1,2,3]
