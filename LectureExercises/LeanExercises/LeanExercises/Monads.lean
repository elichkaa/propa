import Testing

namespace Monads

class MyMonad (m: Type → Type) where
  pure: α → m a
  bind: m a -> (α → m β) -> m β

def echoTwiceDo : IO Unit := do
  let stdin ← IO.getStdin
  IO.println "First lines:"
  let line1 ← stdin.getLine
  IO.println "Second lines:"
  let line2 ← stdin.getLine
  IO.println (line1 ++ line2)


def echoTwice : IO Unit :=
  IO.getStdin >>= (fun stdin =>
  IO.println "First lines:" >>= (fun _ =>
  stdin.getLine >>= (fun line1 =>
  IO.println "Second lines:" >>= (fun _ =>
  stdin.getLine >>= (fun line2 =>
  IO.println (line1 ++ line2))))))


/-
## Except Monad
-/

inductive Except (ε α: Type) where
  | ok (value: α)
  | error (err: ε)
  deriving BEq, Repr

instance: Monad (Except ε) where
  pure x := Except.ok x
  bind x f := match x with
    | Except.ok v => f v
    | Except.error err => Except.error err

def parseInt (s : String) : Except String Int :=
  match s.toInt? with
  | some n => Except.ok n
  | none => Except.error s!"Invalid integer: {s}"

def parseAndAdd_nomonad (s1 s2 : String) : Except String Int :=
  match parseInt s1 with
  | Except.ok n1 =>
    match parseInt s2 with
    | Except.ok n2 => Except.ok (n1 + n2)
    | Except.error err2 => Except.error err2
  | Except.error err1 => Except.error err1

def parseAndAdd (s1 s2 : String) : Except String Int := do
  let n1 ← parseInt s1
  let n2 ← parseInt s2
  return n1 + n2

#assert (parseAndAdd "10" "20") == Except.ok 30
#assert (parseAndAdd "xyz" "20") == Except.error "Invalid integer: xyz"
#assert (parseAndAdd "10" "abc") == Except.error "Invalid integer: abc"

def throw {ε α: Type} (err: ε) : Except ε α :=
  Except.error err

def handle {ε α: Type} (comp: Except ε α) (handler: ε -> α) : α :=
  match comp with
    | Except.ok v => v
    | Except.error err => handler err

def parseFloat (bits : UInt64) : Except UInt64 Float :=
  let f := Float.ofBits bits
  if f.isNaN || f.isInf then
    throw bits -- throws the invalid bits to be handled later
  else
    return f

def parseAndMultiply (b1 b2 : UInt64) : Float :=
  handle (
    do
      let f1 ← parseFloat b1
      let f2 ← parseFloat b2
      return f1 * f2
  ) (fun invalidBits =>
    Float.ofBits (invalidBits - invalidBits) -- yields positive or negative zero
  )

#eval parseAndMultiply 0x400921FB54442D18 0x4005BF0A8B145769  -- approx. π * 2.718
#eval parseAndMultiply 0x7FF8000000000000 0x4005BF0A8B145769  -- NaN input handled
#eval parseAndMultiply 0x8FF8000000000000 0x4005BF0A8B145769  -- NaN input handled

/-
## Option Monad
-/

instance : Monad Option where
  pure x := some x
  bind x f := match x with
    | some v => f v
    | none => none

def safeDiv (a b : Int) : Option Int :=
  if b = 0 then none else some (a / b)

def compute (x y z : Int) : Option Int := do
  let a ← safeDiv x y
  let b ← safeDiv y z
  return a + b

#eval compute 10 2 5    -- some 7
#eval compute 10 0 5    -- none
#eval compute 10 2 0    -- none

/- A generic helper function for the option monad: -/
def orElse {α : Type} (comp : Option α) (fallback : α) : α :=
  match comp with
  | some v => v
  | none => fallback

def compute' (x y z : Int) : Int :=
  let a := orElse (safeDiv x y) 0
  let b := orElse (safeDiv y z) 0
  a + b

/-
## List Monad
-/
instance : Monad List where
  pure x := [x]
  bind xs f := xs.flatMap f

def pairs (xs : List α) (ys : List β) : List (α × β) := do
  let a ← xs
  let b ← ys
  return (a, b)

#eval pairs [1, 2, 3] ["a", "b"]

/-
  We can also define the `Alternative` instance for lists, which provides
  operations for failure and choice.
-/

instance : Alternative List where
  failure := []
  orElse xs ys := xs ++ ys ()
  seq fs xs := do
    let f ← fs
    let x ← xs ()
    return (f x)

/-
  With this instance, we can use `guard` to filter results based on a predicate.
  This essentially allows us to express list comprehensions using `do` notation:
-/

def coprimeProducts (n : Nat) : List Nat := do
  let x ← List.range' 2 n
  let y ← List.range' x (n - x)
  guard (y > x)
  guard (Int.gcd x y == 1)  -- keep only even numbers
  return x * y

#eval coprimeProducts 10 |>.mergeSort

/-
## Reader Monad
-/

structure Reader (ρ α : Type) where
  run : ρ → α

instance : Monad (Reader ρ) where
  pure x := ⟨fun _ => x⟩
  bind f1 f2 := ⟨fun env =>
    let v := f1.run env
    f2 v |>.run env
  ⟩

def read : Reader ρ ρ :=
  ⟨fun env => env⟩

def withReader {α : Type} (f : ρ → ρ) (comp : Reader ρ α) : Reader ρ α :=
  ⟨fun env => comp.run (f env)⟩

abbrev SessionID := String

def sendRequest (req : String) : Reader SessionID String := do
  let id ← read
  return s!"Sending request '{req}' with session ID '{id}'."

def asAnonymous (f : Reader SessionID α) : Reader SessionID α := do
  withReader (fun _ => "") f

def mixedRequests : Reader SessionID String := do
  let resp1 ← sendRequest "GET /data"
  let resp2 ← asAnonymous (sendRequest "POST /update")
  let resp3 ← sendRequest "DELETE /data"
  return resp1 ++ "\n" ++ resp2 ++ "\n" ++ resp3

#eval IO.println $ mixedRequests.run "session_12345"

/-
## State Monad - can represent changeable state
-/
structure State (σ α : Type) where
  run : σ → (α × σ)

instance : Monad (State σ) where
  -- in pure we dont change the state
  pure x := ⟨fun s => (x, s)⟩
  -- fun s (initial state) =>
  bind m f := ⟨fun s =>
    -- generates a new state s'
    let (v, s') := m.run s
    -- and then we run with the new state s'
    (f v).run s'
  ⟩

def get : State σ σ :=
  ⟨fun s => (s, s)⟩
def set (s : σ) : State σ Unit :=
  ⟨fun _ => ((), s)⟩
def modify (f : σ → σ) : State σ Unit := do
  let s ← get
  set (f s)

/-
  We can use the State monad to simulate a global variable. For example,
  we can define a counter that can be incremented and read.
-/

def tick : State Nat Unit := do
  modify (fun n => n + 1)

def reset : State Nat Unit :=
  set 0

/-
  Each tick increments the counter by one. The reset operation sets the counter back to zero.
  We can define a computation that performs several ticks and reads the counter value.
-/
def ticker : State Nat (Nat × Nat) := do
  tick
  tick
  let n1 ← get
  reset

  [0:5].forM (fun _ => tick)
  let n2 ← get
  reset

  return (n1, n2)

/-
  To run `ticker`, we provide an initial state (e.g., `0`), and it returns the final result
  along with the final state (which is `0` again after the final reset).
-/

#eval ticker.run 0

namespace Laps

/-
  We can use more complex state structures as well. For example, we can define a lap timer
  that tracks the current time, total time, number of laps, best lap time, and average lap time.
  Our state structure `S` holds this information.
-/

structure S where
  current : Nat
  total : Nat
  laps : Nat
  bestLap : Option Nat
  avgLap : Float
deriving Repr

/-
  Convenience instance that allows us to write `{}` or `∅` to create the initial state.
-/
instance : EmptyCollection S where
  emptyCollection := ⟨ 0, 0, 0, none, 0.0 ⟩

/-
  Each tick increments the current time and total time by one.
  The reset operation sets the current time back to zero.
  The `lap` operation records a lap, updating the best lap time and average lap time accordingly.
-/
def tick : State S Unit := do
  modify (fun s => { s with current := s.current + 1, total := s.total + 1 })

def reset : State S Unit :=
  set {}

def lap : State S Unit := do
  let s ← get
  let lapTime := s.current
  let laps := s.laps + 1
  let bestLap := match s.bestLap with
    | some best => some (Nat.min best lapTime)
    | none => some lapTime
  let avgLap := s.total.toFloat / laps.toFloat
  set ⟨ 0, s.total, laps, bestLap, avgLap ⟩

/-
  As an example, consider we run three laps. These laps take 10, 12, and 8 ticks respectively.
-/

def lapTicker := do
  [0:10].forM (fun _ => tick)
  lap

  [0:12].forM (fun _ => tick)
  lap

  [0:8].forM (fun _ => tick)
  lap

  return ()

#eval lapTicker.run {}
#eval (do lapTicker; reset).run {}

end Laps


end Monads
