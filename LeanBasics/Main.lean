import LeanBasics

-- obeys the mathematical rules of precedence
#eval 1 + 2 * 5
#eval 42 + 19
-- prints out 0
#eval (1 - 2 : Nat)
-- prints out -1
#eval (1 - 2 : Int)
-- to check what the default return type is
#check (1 - 2) -- prints out 1 - 2 : Nat

-- f x rather than f(x)
#eval String.append "Hello, " "Lean"
#eval String.append "A" (String.append "B" "C")
#eval String.append (String.append "A" "B") "C"
-- conditionals
#eval String.append "it is " (if 1 > 2 then "yes" else "no")
#eval if 3 == 3 then 5 else 7
#eval if 3 == 4 then "equal" else "not equal"

def hello2 := "Hello"
#check hello2 -- String (simple enough so that the compiler can determine type)
def lean: String := "Lean"
#eval String.append hello2 (String.append " " lean)

def add1 (n: Nat): Nat := n + 1
#eval add1 7
#check add1 -- add1 (n : Nat) : Nat
#check (add1) -- add1 : Nat → Nat

def maximum(n: Nat) (k: Nat): Nat := if n >= k then n else k
#eval maximum 2 3
#eval maximum (5 + 8) (2 * 7)

def joinStringsWith (f: String) (s: String) (t: String): String := String.append s (String.append f t)
#eval joinStringsWith ", " "one" "and another"
#check (joinStringsWith) -- joinStringsWith : String → String → String → String
#check joinStringsWith ": " -- joinStringsWith ": " : String → String → String

def volume (h: Nat) (w: Nat) (d: Nat): Nat := h * w * d
#eval volume 1 2 3

def Str : Type := String
def aStr : Str := "This is a string"
abbrev N : Type := Nat
def thirtyNine : N := 39

def main : IO Unit :=
  IO.println s!"Hello, {hello}!"
