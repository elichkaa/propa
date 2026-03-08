/- # Higher-Order Functions - Implementation Exercises -/

import Testing

section Lists

/-
Compute hash codes for a list of strings.
- Empty list returns empty list
- For non-empty list, compute hash of head and recursively process tail
-/
def List.hashString (xs: List String): List UInt64 :=
  match xs with
    | [] => []
    | x :: xs => x.hash :: xs.hashString

/-
Negate a list of booleans.
- Empty list returns empty list
- For non-empty list, negate head and recursively process tail
-/
def List.negate (xs: List Bool): List Bool :=
  match xs with
    | [] => []
    | x :: xs => (!x) :: xs.negate

/-
Round a list of floats.
- Empty list returns empty list
- For non-empty list, round head and recursively process tail
-/
def List.round (xs: List Float): List Float :=
  match xs with
    | [] => []
    | x :: xs => x.round :: xs.round

/-
Generic map function that applies a function to each element.
- Takes function f : α → β and list of α
- Empty list returns empty list
- For non-empty list, apply f to head and recursively map tail
-/
def List.mymap (f: α -> β) (l: List α): List β :=
  match l with
    | [] => []
    | l :: ls => f l :: ls.mymap f

/-
Redefine the functions using map.
-/
def List.hashString' := List.mymap String.hash
def List.negate' := List.mymap (fun x => !x)
def List.round' := List.mymap Float.round

#assert (List.hashString ["hello", "world", "!"])
     == (List.hashString' ["hello", "world", "!"])
#assert (List.round [1.2, 3.5, 4.7])
     == (List.round' [1.2, 3.5, 4.7])
#assert (List.negate [true, false, true])
     == (List.negate' [true, false, true])

/-
Filter even numbers from a list of integers.
- Empty list returns empty list
- For non-empty list: if head is even (% 2 == 0), include it; otherwise skip it
- Recursively process tail
-/
def List.evenNumbers (xs: List Int): List Int :=
  match xs with
    | [] => []
    | x :: xs => if x % 2 == 0 then x :: xs.evenNumbers else xs.evenNumbers

/-
Filter non-empty strings from a list.
- Empty list returns empty list
- For non-empty list: if head is not empty string, include it; otherwise skip it
- Recursively process tail
-/
def List.nonEmptyStrings (xs: List String): List String :=
  match xs with
    | [] => []
    | x :: xs => if x != "" then x :: xs.nonEmptyStrings else xs.nonEmptyStrings

/-
Generic filter function that keeps elements satisfying a predicate.
- Takes predicate p : α → Bool and list of α
- Empty list returns empty list
- For non-empty list: if p holds for head, include it; otherwise skip it
- Recursively filter tail
-/
def List.myfilter {α} (f: α -> Bool) (l: List α): List α :=
  match l with
    | [] => []
    | l :: ls => if f l then l :: ls.myfilter f else ls.myfilter f

def List.evenNumbers' := List.myfilter (fun x : Int => x % 2 == 0)
def List.nonEmptyStrings' := List.myfilter (fun x => x != "")

#assert (List.evenNumbers [1, 2, 3, 4, 5, 6])
     == (List.evenNumbers' [1, 2, 3, 4, 5, 6])
#assert (List.nonEmptyStrings ["hello", "", "world", ""])
     == (List.nonEmptyStrings' ["hello", "", "world", ""])

/-
Add corresponding elements from an Int list and Float list.
- If either list is empty, return empty list
- Convert Int to Float, add to corresponding Float, cons to recursive result
- Stops when either list runs out
-/
def List.addAll (lst1: List Int) (lst2: List Float): List Float :=
  match lst1, lst2 with
    | [], [] => []
    | _, [] => []
    | [], _ => []
    | l1 :: lst1, l2 :: lst2 => (Float.ofInt l1 + l2) :: List.addAll lst1 lst2

/-
Generic zipWith that combines two lists element-wise using a function.
- Takes function f : α → β → γ and two lists
- If either list is empty, return empty list
- Apply f to both heads, cons to recursive result on tails
-/
def List.zipWith' {α β γ} (f: α -> β -> γ) (lst1: List α) (lst2: List β): List γ :=
  match lst1, lst2 with
    | [], _ => []
    | _, [] => []
    | l1 :: lst1, l2 :: lst2 => f l1 l2 :: List.zipWith' f lst1 lst2

/-
Sum all integers in a list.
- Empty list returns 0
- For non-empty list, add head to recursive sum of tail
-/
def List.mysum (l: List Int): Int :=
  match l with
    | [] => 0
    | l :: ls => l + ls.mysum

/-
Compute a single hash for a list of hashable elements.
- Empty list returns 12345678
- For non-empty list, use mixHash to combine hash of head with recursive hash of tail
-/
def List.hash {α} [Hashable α] (l: List α): UInt64 :=
  match l with
    | [] => 12345678
    | l :: ls => mixHash (Hashable.hash l) ls.hash

/-
Partition a list based on a predicate.
- Returns pair (satisfying elements, non-satisfying elements)
- Empty list returns ([], [])
- For non-empty list: recursively partition tail to get (ts, fs)
  - If p holds for head, return (head :: ts, fs)
  - Otherwise return (ts, head :: fs)
-/
def List.mypartition {α} (p: α -> Bool) (l: List α): List α × List α :=
  match l with
    | [] => ([], [])
    | l :: ls =>
      let (matchingPartition, others) := ls.mypartition p
      if p l then (l :: matchingPartition, others) else (matchingPartition, l :: others)

/-
Generic fold (right fold) for lists.
- Takes init value of type β, combine function (α → β → β), and list
- Empty list returns init
- For non-empty list, apply combine to head and recursive fold of tail
-/
def List.fold {α β} (init : β) (combine : α → β → β) (l: List α): β :=
  match l with
    | [] => init
    | l :: ls => combine l (ls.fold init combine)


/-
Reimplement sum using fold.
- Use 0 as init and Int.add as combine function
-/
def List.mysum' := List.fold 0 Int.add
#assert (List.mysum [1, 2, 3, 4, 5])
     == (List.mysum' [1, 2, 3, 4, 5])

/-
Reimplement hash using fold with Hashable constraint.
- Use 12345678 as init
- Use lambda that takes element x and accumulator acc, returns mixHash of hash(x) and acc
-/
def List.hash' {α} [Hashable α] := List.fold 12345678 (fun (x : α) acc => mixHash (Hashable.hash x) acc)
#assert (List.hash ["a", "b", "c"])
     == (List.hash' ["a", "b", "c"])

/-
Reimplement partition using fold.
- Use ([], []) as init
- Use lambda that takes element x and pair (ts, fs)
  - If p(x) return (x :: ts, fs)
  - Otherwise return (ts, x :: fs)
-/


def List.mypartition' (p : α → Bool) := List.fold ([], []) (fun x (ts, fs) => if p x then (x :: ts, fs) else (ts, x::fs))
#assert (List.mypartition (fun x => x % 2 == 0) [1, 2, 3, 4, 5])
     == (List.mypartition' (fun x => x % 2 == 0) [1, 2, 3, 4, 5])


end Lists

section Trees

/-
Define an inductive type BinaryTree with type parameter α.
-/
inductive BinaryTree (α: Type) where
  | empty
  | leaf (v: α)
  | node (left: BinaryTree α) (right: BinaryTree α)

open BinaryTree

/-
Define toString for BinaryTree (requires ToString α constraint).
- empty displays as "∅"
- leaf displays the element using ToString.toString
- node displays as "[left.toString, right.toString]"
-/
def BinaryTree.toString {α} [ToString α] (bt: BinaryTree α): String :=
  match bt with
    | empty => "∅"
    | leaf v => ToString.toString v
    | node left right =>  "[" ++ left.toString ++ ", " ++ right.toString ++ "]"

/-
Define Repr instance for BinaryTree (requires ToString α).
- Use toString method, ignore precedence parameter
-/
instance [ToString α] : Repr (BinaryTree α) where
  reprPrec tree _ := tree.toString

/- Map function for binary trees. -/
def BinaryTree.map {α β} (f: α -> β) (bt: BinaryTree α): BinaryTree β :=
  match bt with
    | empty => empty
    | leaf v => leaf (f v)
    | node left right => node (left.map f) (right.map f)

/-
Filter function for binary trees.
-/
def BinaryTree.filter {α} (p: α -> Bool) (bt: BinaryTree α) : BinaryTree α :=
  match bt with
    | empty => empty
    | leaf v => if p v then leaf v else empty
    | node left right => node (left.filter p) (right.filter p)

/-
ZipWith function for binary trees.
-/
def BinaryTree.zipWith {α β γ} (f: α -> β -> γ) (bt1: BinaryTree α) (bt2: BinaryTree β): BinaryTree γ :=
  match bt1, bt2 with
    | _, empty => empty
    | empty, _ => empty
    | leaf v1, leaf v2 => leaf (f v1 v2)
    | node l1 r1, node l2 r2 => node (l1.zipWith f l2) (r1.zipWith f r2)
    | _, _ => empty

/-
Fold function for binary trees.
- Takes three parameters for each constructor:
  - fempty : β for empty case
  - fleaf : α → β for leaf case
  - fnode : β → β → β for node case
-/
def BinaryTree.fold {α β} (fempty: β) (fleaf: α -> β) (fnode: β -> β -> β) (bt: BinaryTree α): β :=
  match bt with
    | empty => fempty
    | leaf v => fleaf v
    | node left right => fnode (left.fold fempty fleaf fnode) (right.fold fempty fleaf fnode)

/-
Compute height of tree using fold.
- empty has height 0
- leaf has height 1
- node has height 1 + max(left height, right height)
-/
def BinaryTree.height {α} (bt: BinaryTree α): Nat :=
  bt.fold 0 (fun _ => 1) (fun l r => 1 + Nat.max l r)

/-
Create scaffold of tree (same structure, all leaves replaced with empty).
- empty stays empty
- leaf becomes empty
- node structure preserved
-/
def BinaryTree.scaffold {α} (bt: BinaryTree α): BinaryTree α :=
  bt.fold empty (fun _ => empty) (fun l r => node l r)

end Trees

section Routes

/-
Define inductive type Route' with following constructors:
- destination: takes a String (name), represents arrival at destination
- unreachable: represents an unreachable destination
- turn: takes String (direction) and Route' (rest), represents a turn followed by more route
- sequence: takes two Route's (r1, r2), represents sequence of routes
- alternative: takes two Route's (r1, r2), represents choice between routes
Derive Repr and BEq.
-/
inductive Route' where
  | destination (name: String)
  | unreachable
  | turn (direction: String) (rest: Route')
  | sequence (r1: Route') (r2: Route')
  | alternative (r1: Route') (r2: Route')

/-
Fold function for Route'.
- Takes five parameters, one for each constructor:
  - fdestination : String → β
  - funreachable : β
  - fturn : String → β → β
  - fsequence : β → β → β
  - falternative : β → β → β
- Match each constructor and apply corresponding f-function
- Recursively fold substructures
-/
open Route'
def Route'.fold {β} (fdestination: String -> β) (funreachable: β) (fturn: String -> β -> β) (fsequence: β -> β -> β) (falternative: β -> β -> β) (r: Route'): β :=
  match r with
    | destination name => fdestination name
    | unreachable => funreachable
    | turn direction rest => fturn direction (rest.fold fdestination funreachable fturn fsequence falternative)
    | sequence r1 r2 => fsequence (r1.fold fdestination funreachable fturn fsequence falternative) (r2.fold fdestination funreachable fturn fsequence falternative)
    | alternative r1 r2 => falternative (r1.fold fdestination funreachable fturn fsequence falternative) (r2.fold fdestination funreachable fturn fsequence falternative)

/-
Count number of turns in a route using fold.
- destination: 0 turns
- unreachable: 0 turns
- turn: 1 + (turns in rest)
- sequence: (turns in r1) + (turns in r2)
- alternative: minimum of (turns in r1) and (turns in r2)
-/
def Route'.countTurns (r: Route'): Nat :=
  r.fold (fun _ => 0) 0 (fun _ r => 1 + r) (fun r1 r2 => r1 + r2) (fun r1 r2 => Nat.min r1 r2)

end Routes

section Lambda

/-
Add three numbers x, y, z using a local lambda.
-/

-- NO LAMBDA
def plus (x y: Int) : Int := x + y
def plus3 (x y z: Int): Int := plus z (plus x y)

-- WITH LAMBDA
def plus3' (x y z: Int): Int :=
  let f := fun x y => x + y
  f (f x y) z

/-
Read two values from memory array and add them.
- Memory array is of UInt32
- Takes base address, two addresses x and y, and wordSize
- Define local lambda 'read' that takes address a and returns memory[base + wordSize * a]
- Return read(x) + read(y)
-/
def addFromMemory (memory : Array UInt32) (base x y : Nat) (wordSize : Nat) : UInt32 :=
  let read := fun a => memory[base + wordSize * a]!
  read x + read y

/-
Helper function that returns hash function based on config string.
- If config is "simple", return function that hashes string length
- Otherwise, return function that uses String.hash
-/
def hashFunction (config : String) : String → UInt64 :=
  if config == "simple" then
    fun s => s.length.toUInt64
  else
    fun s => s.hash

/-
Helper function that returns encryption function.
- Takes a key string
- Returns function that takes string s, appends key, and reverses as List Char
-/
def encryptFunction (key : String) : String → List Char :=
  fun s => (s ++ key).toList.reverse

/-
Helper function that returns decryption function.
- Takes a key string
- Returns function that takes List Char, drops key length, reverses, and converts to String
-/
def decryptFunction (key : String) : List Char → String :=
  fun x => x.drop (key.length) |>.reverse.toString

/-
Define structure Utilities with three fields:
- hashFunc : String → UInt64
- encryptFunc : String → List Char
- decryptFunc : List Char → String
-/
structure Utilities where
  hashFunc : String → UInt64
  encryptFunc : String → List Char
  decryptFunc : List Char → String

/-
Create Utilities instance given a private key.
- Use hashFunction with "complex" config
- Use encryptFunction with the private key
- Use decryptFunction with the private key
-/
def myUtilities (privateKey : String) : Utilities :=
  {
    hashFunc := hashFunction "complex",
    encryptFunc := encryptFunction privateKey,
    decryptFunc := decryptFunction privateKey
  }

end Lambda
