-- f x rather than f(x)
#eval String.append "Hello, " "Lean"
#eval String.append "A" (String.append "B" "C")
#eval String.append (String.append "A" "B") "C"

def joinStringsWith (f: String) (s: String) (t: String): String := String.append s (String.append f t)
#eval joinStringsWith ", " "one" "and another"
#check (joinStringsWith) -- joinStringsWith : String → String → String → String
#check joinStringsWith ": " -- joinStringsWith ": " : String → String → String

def Str : Type := String
def aStr : Str := "This is a string"

def hello2 := "Hello"
#check hello2 -- String (simple enough so that the compiler can determine type)
def lean: String := "Lean"
#eval String.append hello2 (String.append " " lean)
