import Std.Data.HashSet
import Testing

-- general building
inductive BuildingA
| office (campus : String) (name : String) (address : String) (numFloors : Nat) (numOffices : Nat) (parkingSpaces : Nat)
| lecture (campus : String) (name : String) (address : String) (numFloors : Nat) (numLectureHalls : Nat) (hasCafeteria : Bool)

def BuildingA.campus (building : BuildingA) : String :=
  match building with
  | BuildingA.office campus _ _ _ _ _ => campus
  | BuildingA.lecture campus _ _ _ _ _ => campus

-- but what if we want to add another building type now?
-- we need to add it first to BuildingA (our datatype) and then add the case to all implementations which use it
-- this doesn't work when we have a lot of things depending on our datatype

-- ## Interfaces
-- class is the keyword for an interface, signifiees type classes
class Building (b: Type) where
  campus: b → String
  name: b → String
  address: b → String
  numFloors: b → Nat

-- this doesn't work because we first need an instance of the class
-- #check Building.campus "Mensa"

structure OfficeBuilding where
  campus : String
  name : String
  visitorAddress : String
  postAddress : String
  numFloors : Nat
  numOffices : Nat
  parkingSpaces : Nat

instance : Building OfficeBuilding where
  campus b := b.campus
  name b := b.name
  address b := b.visitorAddress
  numFloors b := b.numFloors

structure LectureBuilding where
  campus : String
  name : String
  address : String
  numFloors : Nat
  numLectureHalls : Nat
  hasCafeteria : Bool

instance : Building LectureBuilding where
  campus b := b.campus
  name b := s! "{b.campus}:{b.name}"
  address b := b.address
  numFloors b := b.numFloors

-- now OfficeBuilding and LectureBuilding are instances of Building class
-- we can now define functions that work with Building, without knowing the concrete definition


def printBuildingInfo [Building build] (b : build) : String :=
  let campus := Building.campus b
  let name := Building.name b
  let address := Building.address b
  let floors := Building.numFloors b
  s! "Building Info:\nCampus: {campus}\nName: {name}\nAddress: {address}\nNumber of Floors: {floors}"

#eval OfficeBuilding.mk "North" "Admin Building" "123 Admin St." "PO Box 456" 5 100 50
      |> printBuildingInfo

#eval LectureBuilding.mk "South" "Science Hall" "456 Science Rd." 4 10 true
      |> printBuildingInfo


-- extension by new building type without problems
structure LaboratoryBuilding where
  campus : String
  name : String
  address : String
  numFloors : Nat
  numLabs : Nat
  safetyLevel : String
  hasFumeHoods : Bool

instance : Building LaboratoryBuilding where
  campus _ := "Redacted"
  name b := b.name
  address _ := "Redacted"
  numFloors _ := 0

#eval LaboratoryBuilding.mk "North" "Chemistry Lab" "789 Lab Ave." 3 15 "High" true
    |> printBuildingInfo


/-
  interface Serialize which defines two operations: `serialize`, which converts a value of type `α`
  into a `ByteArray`, and `deserialize`, which converts a `ByteArray` back into a value
  of type `α`, if possible.
-/
class Serializable (α : Type) where
  serialize : α → ByteArray
  deserialize : ByteArray → Option α

/-  We can implement this interface for different types.
    For example, we can implement `Serializable` for `Nat` and `String`:-/

instance : Serializable Nat where
  serialize n := ByteArray.mk (Array.singleton n.toUInt8)
  deserialize ar :=
    if ar.size != 1 then none
    else some (ar[0]!.toNat)

instance : Serializable String where
  serialize s := s.toUTF8
  deserialize ar := String.fromUTF8? ar

#eval Serializable.serialize (42 : Nat)
#eval (Serializable.deserialize (ByteArray.mk #[42]) : Option Nat)
#eval (Serializable.deserialize (ByteArray.mk #[42, 44]) : Option Nat)

#eval Serializable.serialize "Hello, Lean!"
#eval Serializable.serialize "Hello, Lean!▵"
#eval (Serializable.deserialize (Serializable.serialize "Hello, Lean" ) : Option String)
#eval (Serializable.deserialize (ByteArray.mk #[72, 101, 108, 108, 111, 10]) : Option String)
#eval (Serializable.deserialize (ByteArray.mk #[72, 101, 108, 108, 111, 254]) : Option String)


-- we can now write a small library that requires this interface

namespace Network

/- All subsequent definitions in this namespace have an implicit `Serializable α` constraint. -/
variable {α} [Serializable α]

def send (data : α) : String :=
  let ar := Serializable.serialize data
  let _ := ar -- simulate sending byte array over network
  "ok"

def receive [Inhabited α] : α :=
  let ar := sorry -- simulate receiving byte array from network
  match Serializable.deserialize ar with
  | none => panic! "Received corrupted data"
  | some data => data

end Network

-- how to write efficient data structures in functional programming
-- BFS requires a queue, DFS requires a stack

-- we can create a queue interface and then use the BFS with any queue implementation
class Queue (q : Type → Type) where
  empty : q α
  isEmpty : q α → Bool
  enqueue : q α → α → q α
  -- we return the dequeued element and the rest of the queue
  dequeue? : q α → Option (α × q α)

def Queue.singleton [Queue q] (x : α) : q α :=
  Queue.enqueue Queue.empty x

partial def bfsWithQueue (q : Type → Type) (α : Type) [Queue q] [BEq α] [Hashable α] (start : α) (neighbors : α → List α) : List α :=
  let rec loop (queue : q α) (visited : Std.HashSet α) (result : List α) :=
    if Queue.isEmpty queue then
      result.reverse
    else
      match Queue.dequeue? queue with
      | none => result.reverse
      | some (node, q') =>
        if visited.contains node then
          loop q' visited result
        else
          let newVisited := visited.insert node
          let newNeighbors := neighbors node
          let newQueue := newNeighbors.foldl Queue.enqueue q'
          loop newQueue newVisited (node :: result)
  loop (Queue.singleton start) (Std.HashSet.emptyWithCapacity 16) []

-- list based queue

instance List.Queue : Queue List where
  empty := []
  isEmpty q := q.isEmpty
  enqueue q x := q ++ [x]
  dequeue? q :=
    match q with
    | [] => none
    | x :: xs => some (x, xs)

partial def bfs_list [BEq α] [Hashable α] := bfsWithQueue List α

-- however the enqueue operation is inefficient because we need to traverse the whole list to append an element at the end
-- banker's queue is an O(1) implementation for both enqueue and dequeue
  -- we maintain two lists - one for the front of the queue and one for the back
  -- when we dequeue an elemen, we take it from the front list
  -- when we enqueue an elemen, we add it to the back lis
  -- if the front list is empty when we try to dequeue, we reverse the back list and use it as the new front list

structure BankersQueue (α : Type) where
  front : List α
  back : List α

instance BankersQueue.Queue : Queue BankersQueue where
  empty := { front := [], back := [] }
  isEmpty q := q.front.isEmpty && q.back.isEmpty
  enqueue q x := { q with back := x :: q.back }
  dequeue? q :=
    match q.front with
    | x :: xs => some (x, { q with front := xs })
    | [] =>
      match q.back.reverse with
      | [] => none
      | y :: ys => some (y, { front := ys, back := [] })

partial def bfs_banker [BEq α] [Hashable α] := bfsWithQueue BankersQueue α



-- ## Module Systems
-- implement encapsulation (information hiding)

/- Invariant: denominator ≠ 0 ∧ coprime num den  -/
structure Rational where
  /- Hide the internal constructor to prevent violations of invariant -/
  private mk' ::
    num : Int
    den : Nat
deriving BEq

/-
  Smart constructor that ensures the denominator is not zero.
  This is the only constructor accessible from outside this module.
-/
def Rational.mk (n : Int) (d : Int) : Rational :=
  if d = 0 then
    { num := 0, den := 1 }  -- Default to 0/1 if denominator is zero
  else
    let gcd := Int.gcd n d
    Rational.mk' (n / gcd * d.sign) (d.natAbs / gcd)

infixl:72 " /. " => Rational.mk

instance : Repr Rational where
  reprPrec r _ := s!"{r.num} /. {r.den}"

#eval 3 /. 4
#eval 3 /. -4
#assert (3 /. -4) == (-3 /. 4)
#assert (-3 /. -4) == (3 /. 4)

#assert (5 /. 0) == (0 /. 1)  -- Denominator zero case
#assert (-12 /. -96) == (1 /. 8)

instance : Zero Rational where
  zero := Rational.mk 0 1

instance : Add Rational where
  add r1 r2 :=
    Rational.mk
      (r1.num * Int.ofNat r2.den + r2.num * Int.ofNat r1.den)
      (Int.ofNat (r1.den * r2.den))

instance : One Rational where
  one := Rational.mk 1 1

instance : Mul Rational where
  mul r1 r2 :=
    Rational.mk
      (r1.num * r2.num)
      (Int.ofNat (r1.den * r2.den))

#assert (1 /. 2 + 1 /. 3) == (5 /. 6)
#assert (1 /. 4 + 3 /. 4) == (1 /. 1)
#assert (2 /. 3 + (-2) /. 3) == (0 /. 1)

#assert (2 /. 3 * 3 /. 4) == (1 /. 2)
#assert (-2 /. 3 * 3 /. -4) == (1 /. 2)
#assert (2 /. 3 * 0 /. 1) == (0 /. 1)

def Rational.toFloat (r : Rational) : Float := Float.ofInt r.num / Float.ofNat r.den

namespace Test

def r : Rational := 3 /. 4
#assert r.num == 3
#assert r.den == 4

/- We're in the same module, thus we can access the private constructor directly -/
private def bad : Rational := Rational.mk' 1 0
#eval bad.toFloat

/- Check RationalClient.lean to confirm that an external module cannot access the private constructor. -/

end Test

/-
  We're in a different module, thus we cannot access the private constructor directly.
  Uncomment the following lines to see the error.
-/
-- import Functional.Modularity.Rational
-- def bad : Rational := Rational.mk' 1 0
-- #eval bad.toFloat
