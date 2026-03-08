-- obeys the mathematical rules of precedence
#eval 1 + 2 * 5
#eval 42 + 19
-- prints out 0
#eval (1 - 2 : Nat)
-- prints out -1
#eval (1 - 2 : Int)
-- to check what the default return type is
#check (1 - 2) -- prints out 1 - 2 : Nat

def add1 (n: Nat): Nat := n + 1
#eval add1 7 -- 8
#check add1 -- add1 (n : Nat) : Nat
#check (add1) -- add1 : Nat → Nat

def maximum (n: Nat) (k: Nat): Nat := if n >= k then n else k
#eval maximum 2 3
#eval maximum (5 + 8) (2 * 7)

def volume (h: Nat) (w: Nat) (d: Nat): Nat := h * w * d
#eval volume 1 2 3

abbrev N : Type := Nat
def thirtyNine : N := 39

def evenOrOdd (n: Int) :=
  if n % 2 == 0
  then "even"
  else "odd"
def posNegOrZero (n: Int) :=
  if n < 0 then "negative"
  else
    if n == 0
    then "zero"
    else "positive"
def both (n: Int) := String.append (evenOrOdd n) (String.append " " (posNegOrZero n))
#eval both (0)

#check 1.2 -- Float
