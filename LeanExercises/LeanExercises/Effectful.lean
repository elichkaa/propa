-- hello world program

-- IO Unit here is a side effect
def greet(name: String) : IO Unit :=
  IO.println s!"Hello {name}"

-- basic greet
-- def main (args: List String) :=
--   greet "seba"

def helloWorld := do
  IO.println "What is your name?"
  let stdin <- IO.getStdin
  let line <- stdin.getLine
  let name := line.trimAscii.toString
  greet name

-- comand line arguments
inductive Argument
| flag (name : String) -- ex. --volatile
| var (name value : String) -- ex. --name=eli
deriving Repr

-- we aim to still keep the base elements as pure as possible
def Argument.isVar (arg: Argument) (name: String) : Bool :=
  match arg with
    | flag _ => false
    | var vn _ => name == vn


inductive Error where
  | argumentFormat (arg : String)
  deriving Repr

def trimDash (arg : String) : Except Error String :=
  if arg.startsWith "--" then
    return arg.drop 2 |>.toString
  else if arg.startsWith "-" then
    return arg.drop 1 |>.toString
  else
    throw (Error.argumentFormat s!"{arg}, expected argument to start with '-' or '--'")

def parseArg (arg: String) : Except Error Argument := do
  -- first trim the leading dashes
  let trimmed ← trimDash arg
  -- only if trimmed succeeded, we continue parsing
  match trimmed.splitOn "=" with
    | [""] => throw (Error.argumentFormat s!"{trimmed}, got emptry argument")
    | [name] => return Argument.flag name
    | [name, value] => return Argument.var name value
    | [] => throw (Error.argumentFormat s!"{trimmed}, got emptry argument")
    | split => throw (Error.argumentFormat s!"{trimmed}, got too many '=': {split}")

def List.mapExcept (f: α → Except ε β) (xs: List α) : Except ε (List β) := do
  match xs with
    | [] => return []
    | x :: xs' =>
      let y ← f x
      let ys ← mapExcept f xs'
      return y :: ys

def parseArgs (args : List String) : Except Error (List Argument) :=
  args.mapExcept parseArg

def main (args : List String) : IO Unit := do
  match parseArgs args with
  | Except.ok config =>
    match config.find? (·.isVar "name") with
    | some (Argument.var _ name) => greet name -- bypass the stdin prompt
    | _ => helloWorld
  | Except.error err =>
    IO.println s!"Error parsing arguments: {reprStr err}"
