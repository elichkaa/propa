import Testing

/-
# Stack Based Language

This week, we want to implement a stack based language.
- First, we will define core instructions for our language.
- Later on, we will extend these with jumps and function calls.
- At the end, we will implement structured control flow inspired by **WASM (WebAssembly)**.

Possible values in our language are booleans and natural numbers:
-/
inductive Value where
  | bool (b: Bool)
  | nat (n: Nat)
deriving Repr, BEq

-- Instructions manipulate the stack of values, implemented as a `List`:
abbrev Stack := List Value

/-
The language's core instructions.
Note that the binary operators take the top value as the left operand
and the second top value as the right operand: [x, y, …] -> [x ∘ y, …]
-/
inductive Core where
  | push (val: Value) -- Push the specified value        […]       -> [val, …]
  | drop              -- Drop the top value              [x, …]    -> […]
  | dup               -- Duplicate the top value         [x, …]    -> [x, x, …]
  | swap              -- Swap the top two values         [x, y, …] -> [y, x, …]
  | over              -- Duplicate the second top value  [x, y, …] -> [y, x, y, …]
  | add               -- Add the top two values          [x, y, …] -> [x + y, …]
  | sub               -- Subtract the top two values     [x, y, …] -> [x - y, …]
  | mul               -- Multiply the top two values     [x, y, …] -> [x * y, …]
  | gt                -- Compare the two top values      [x, y, …] -> [x > y, …]
deriving Repr, BEq

/-
Implement the interpretation of the core instructions as explained above.
Use `panic!` with a proper message if the stack shape is incompatible with the current instruction.
-/
def Core.interpret! (instruction: Core) (stack: Stack) : Stack :=
  match instruction, stack with
    | .push val, _ => val :: stack
    | .drop, _ :: stack => stack
    | .dup, s :: stack => s :: s :: stack
    | .swap, fst :: snd :: stack => snd :: fst :: stack
    | .over, fst :: snd :: stack => snd :: fst :: snd :: stack
    | .add, .nat fst :: .nat snd :: stack => .nat (fst + snd) :: stack
    | .sub, .nat fst :: .nat snd :: stack => .nat (fst - snd) :: stack
    | .mul, .nat fst :: .nat snd :: stack => .nat (fst * snd) :: stack
    | .gt, .nat fst :: .nat snd :: stack => .bool (fst > snd) :: stack
    | _, _ => panic! "Stack shape incompatible with instruction"


#assert ((Core.push (.nat 42)).interpret! []) == [.nat 42]
#assert (Core.drop.interpret! [.nat 42]) == []
#assert (Core.dup.interpret! [.nat 42]) == [.nat 42, .nat 42]
#assert (Core.swap.interpret! [.nat 0, .nat 1]) == [.nat 1, .nat 0]
#assert (Core.over.interpret! [.bool true, .nat 42]) == [.nat 42, .bool true, .nat 42]
#assert (Core.add.interpret! [.nat 1, .nat 2]) == [.nat 3]
#assert (Core.sub.interpret! [.nat 2, .nat 1]) == [.nat 1]
#assert (Core.sub.interpret! [.nat 1, .nat 2]) == [.nat 0]
#assert (Core.mul.interpret! [.nat 2, .nat 3]) == [.nat 6]
#assert (Core.gt.interpret! [.nat 3, .nat 2]) == [.bool true]
#assert (Core.gt.interpret! [.nat 2, .nat 2]) == [.bool false]
#assert (Core.gt.interpret! [.nat 2, .nat 3]) == [.bool false]

-- Use `interpret!` to interpret a program as a list of instructions.
def Core.interpretProgram (instructions: List Core) (stack : Stack) : Stack :=
  match instructions, stack with
    | [], stack => stack
    | i :: is, _ => interpretProgram is (i.interpret! stack)

-- Specify the instructions needed to double an input value: [x] -> [2 * x]
def double: List Core := [
   .push (.nat 2),
  .mul
]

#assert (Core.interpretProgram double [.nat 42]) == [.nat 84]

-- Implement [x, a, c] -> [a * x^2 + c]
def axxPlusC: List Core := [
  .dup,
  .mul,
  .mul,
  .add
]

#assert (Core.interpretProgram axxPlusC [.nat 2, .nat 3, .nat 4]) == [.nat 16]

-- Identify instructions that push a value onto the stack that shouldn't be dropped directly.
def Core.mustUse (instruction: Core) : Bool :=
  match instruction with
  | .push _ | .dup | .over | .add | .sub | .mul | .gt => true
  | .drop | .swap => false

/-
Use `Core.mustUse` to detect whether a "must use" value is directly dropped.
For example, a `push` directly followed by a `drop` doesn't make sense.
This is not a sophisticated analysis. Only check the top two instructions!
-/
def dropOfMustUse (instructions: List Core) : Bool :=
  match instructions with
    | [] => false
    | f :: _ :: rest => f.mustUse || dropOfMustUse rest
    | _ :: rest => dropOfMustUse rest

#assert (dropOfMustUse [.drop]) == false
#assert (dropOfMustUse [.drop, .drop]) == false
#assert (dropOfMustUse [.swap, .drop]) == false
#assert (dropOfMustUse [.push (.bool false), .drop]) == true
#assert (dropOfMustUse [.dup, .dup, .drop]) == true
#assert (dropOfMustUse [.over, .drop]) == true
#assert (dropOfMustUse [.add, .drop]) == true
#assert (dropOfMustUse [.sub, .drop]) == true
#assert (dropOfMustUse [.mul, .drop]) == true
#assert (dropOfMustUse [.gt, .drop]) == true

namespace Unstructured

-- In this namespace, we want to extend our core instructions with jumps and function calls.
inductive Instruction where
  | core (instruction: Core)    -- Core instruction
  | jump (target: Nat)          -- Unconditional jump to the `target` instruction
  | jumpIf (target: Nat)        -- Pop the stack's top value. If it is true, jump to `target`
  | return                      -- Exit the current function
  | call (functionName: String) -- Pass the stack to the called function and then back to the callee
deriving Repr, BEq

/-
A program consists of functions.
Every function has a name and a list of instructions.
The function "main" is the entrypoint of the program.
-/
abbrev Program := Std.HashMap String (List Instruction)

-- Create a program with a "main" function containing the provided `instructions`.
def Program.main (instructions: List Instruction) : Program :=
  Std.HashMap.emptyWithCapacity |>.insert "main" instructions

/-
Interpret a function starting with the instruction at index `pc` in `instructions`.
- `pc` conventionally stands for "program counter". In our case, it is the index of the currently
  interpreted instruction in the currently interpreted function (`instructions`).
- `program` is needed for looking up a function in the case of a `call`.
- Interpret until the end of the function.
- When recursing, increment `pc` or use the target of a jump.
- For `jumpIf`, if the top value is not a boolean, use `panic!` with a proper message.
-/
partial def Program.interpretFun! (program: Program) (instructions: List Instruction) (pc: Nat) (stack: Stack) : Stack :=
  match instructions[pc]? with
    | none => stack
    | some (.core instruction) => program.interpretFun! instructions (pc + 1) (instruction.interpret! stack)
    | some (.jump target) => program.interpretFun! instructions target stack
    | some (.jumpIf target) =>
      match stack with
      | .bool true :: ss => program.interpretFun! instructions target ss
      | .bool false :: ss => program.interpretFun! instructions (pc + 1) ss
      | _ => panic! "Expected a bool on the stack for a jumpIf instruction"
    | some (.return) => stack
    | some (.call functionName) => let function := program.get! functionName
                                   let newStack := program.interpretFun! function 0 stack
                                   program.interpretFun! instructions (pc + 1) newStack

-- Use `Program.interpretFun!` to interpret the function "main" starting with the first instruction.
def Program.interpret (program: Program) (stack: Stack) : Stack :=
  let mainFunction := program.get! "main"
  program.interpretFun! mainFunction 0 stack

-- [x] -> [x + 2]
#assert (Program.main [
  .core (.push (.nat 2)),
  .core .add
] |>.interpret [.nat 3]) == [.nat 5]

-- [x] -> [2 * x + 1]
#assert (Program.interpret (Program.main [
  .call "double",
  .core (.push (.nat 1)),
  .core .add
] |>.insert "double" [
  .core (.push (.nat 2)),
  .core .mul
]) [.nat 3]) == [.nat 7]

-- [x] -> [x!]
def factorial: List Instruction := [
                          -- n
  .core (.push (.nat 1)), -- 1, n
  .core .over,            -- n, 1, n
  .core .gt,              -- IF n <= 1 {
  .jumpIf 7,              --
  .core .drop,            --
  .core (.push (.nat 1)), --   1
  .return,                --   RETURN 1 }
  .core .dup,             -- n, n
  .core (.push (.nat 1)), -- DO { 1, n, n
  .core .swap,            --      n, 1, n
  .core .sub,             --      n-1, n
  .core .swap,            --      n, n-1
  .core .over,            --      n-1, n, n-1
  .core .mul,             --      n(n-1), n-1
  .core .swap,            --      n-1, n(n-1)
  .core (.push (.nat 2)), --      2, n-1, n(n-1)
  .core .over,            --      n-1, 2, n-1, n(n-1)
  .core .gt,              -- } WHILE n-1 > 2
  .jumpIf 8,              --
  .core .drop             -- n(n-1)…2
]

#assert (Program.main factorial |>.interpret [.nat 0]) == [.nat 1]
#assert (Program.main factorial |>.interpret [.nat 1]) == [.nat 1]
#assert (Program.main factorial |>.interpret [.nat 2]) == [.nat 2]
#assert (Program.main factorial |>.interpret [.nat 5]) == [.nat 120]

/-
Implement the factorial function recursively by calling the "main" function.
Understand the non-recursive version above first.
-/
def recursiveFactorial: List Instruction := [
  .core (.push (.nat 1)),     -- 0: [1, n, ...]
  .core .over,                -- 1: [n, 1, n, ...]
  .core .gt,                  -- 2: [n>1, n, ...]
  .jumpIf 7,                  -- 3: if n >1, jump to 7, else fall through
  .core .drop,                -- 4: [...] (base case n <= 1)
  .core (.push (.nat 1)),     -- 5: [1, ...]
  .return,                    -- 6: return 1
  .core .dup,                 -- 7: [n, n, ...] (recursive case)
  .core (.push (.nat 1)),     -- 8: [1, n, n, ...]
  .core .swap,                -- 9: [n, 1, n, ...]
  .core .sub,                 -- 10: [n-1, n, ...]
  .call "main",               -- 11: [(n-1)!, n, ...] (recursive call)
  .core .swap,                -- 12: [n, (n - 1)!, ...]
  .core .mul                  -- 13: [n * (n - 1)!, ...]
]

#assert (Program.main recursiveFactorial |>.interpret [.nat 0]) == [.nat 1]
#assert (Program.main recursiveFactorial |>.interpret [.nat 1]) == [.nat 1]
#assert (Program.main recursiveFactorial |>.interpret [.nat 2]) == [.nat 2]
#assert (Program.main recursiveFactorial |>.interpret [.nat 5]) == [.nat 120]

/-
Return the index of the first `jump` or `jumpIf` instruction with a target bigger than the length
of all instructions.
-/
def outOfBoundsJumpTarget? (instructions: List Instruction) : Option Nat :=
  instructions.findIdx? (
    fun instruction => match instruction with
    | .jump tg | .jumpIf tg => tg >= instructions.length
    | _ =>false
  )

#assert (outOfBoundsJumpTarget? [.jump 1]) == some 0
#assert (outOfBoundsJumpTarget? [.call "f", .jump 42]) == some 1
#assert (outOfBoundsJumpTarget? [.jumpIf 42, .jumpIf 42]) == some 0
#assert (outOfBoundsJumpTarget? factorial) == none
#assert (outOfBoundsJumpTarget? recursiveFactorial) == none

-- Return the targets of all `jump` and `jumpIf` instructions.
def jumpTargets (instructions: List Instruction) : List Nat :=
  instructions |>.filterMap (
    fun i => match i with
      | .jump tg | .jumpIf tg => some tg
      | _ => none
  )

#assert (jumpTargets [.jumpIf 1, .jumpIf 0, .jump 1]) == [1, 0, 1]
#assert (jumpTargets [.call "f", .jumpIf 0, .core .dup, .core (.push (.nat 42)), .core .gt, .jump 1]) == [0, 1]
#assert (jumpTargets factorial) == [7, 8]

/-
Instructions after a `return` or `jump` can't be reached unless they are targeted by some jump.
- Use `jumpTargets` to check if the instruction after a `return` or `jump` is targeted.
- Return the index of the first instruction that isn't targeted in this case.
- It is possible that the instruction after `return` or `jump` is targeted by an unreachable jump.
  To not overcomplicate this exercise, ignore that case.
-/
def deadCode? (instructions: List Instruction) : Option Nat :=
  go instructions 0 (jumpTargets instructions)
  where
    go instructions ind targets :=
      let next_ind := ind + 1
      match instructions with
        | [] => none
        | .jump _ :: is | .return :: is => if is.isEmpty || (targets.contains next_ind) then
                                              go is next_ind targets
                                            else
                                              some next_ind
        | _ :: rest => go rest next_ind targets

#assert (deadCode? [.return, .call "f"]) == some 1
#assert (deadCode? [.jumpIf 2, .return, .call "f"]) == none
#assert (deadCode? [.core .gt, .return, .call "f"]) == some 2
#assert (deadCode? [.jump 2, .call "f", .core .mul]) == some 1
#assert (deadCode? [.jumpIf 2, .return, .call "f", .jump 0]) == none
#assert (deadCode? factorial) == none
#assert (deadCode? recursiveFactorial) == none
#assert (deadCode? [.return, .call "f", .jump 1]) == none -- The jump is unreachable, but we ignore this case.

/-
Check whether some function in `program` has a `call` to a function name that `program` doesn't
contain. In that case, return the name of the function containing the invalid `call` and the index
of that `call` instruction.
-/
def Program.invalidCall? (program: Program) : Option (String × Nat) :=
  go program.toList
  where
    go functions :=
      match functions with
        | (func_name, instructions) :: rest =>
          let ind := instructions.findIdx? (
            fun instruction => match instruction with
            | .call name => !program.contains name
            | _ => false
          )
          match ind with
            | some ind => (func_name, ind)
            | none => go rest
        | [] => none

#assert (Program.main [.call "f"] |>.invalidCall?) == some ("main", 0)
#assert (Program.main [.call "f"] |>.insert "f" [.core .mul] |> Program.invalidCall?) == none
#assert (Program.main [.call "f"] |>.insert "f" [.core .mul, .core .gt, .call "g"] |> Program.invalidCall?) == some ("f", 2)
#assert (Program.main factorial |>.invalidCall?) == none
#assert (Program.main recursiveFactorial |>.invalidCall?) == none

end Unstructured

namespace Structured

/-
In this namespace, we want to extend our core instructions with structured control flow
inspired by **WASM (WebAssembly)**.
Instead of arbitrary jumps, we have unconditional (`br`) and conditional (`brIf`) branching to
the end of a `block` or the beginning of a `loop`.

In the following example, if the top value is `true`, `brIf 0` would jump to the end of the block
wrapping it (nesting `0`):
```
block {
  …
  brIf 0  --
  …        |
}  <--------
```

The following `brIf 1` would jump to the end of the outer block because it targets the nesting `1`:
```
block {
  …
  block {
    …
    brIf 1  --
    …        |
  }          |
  …          |
}  <----------
```

Loops don't behave as expected from other languages. Looping is **not implicit** at the end of the
loop. The following loop executes `mul` and `add` **only once**, then the loop is exited:
```
loop {
  mul
  add
}
```

The upper loop is not really necessary since it doesn't loop. It is meant as a demonstration.
To actually start a new iteration in a loop, branching is required. The following `brIf 0` would
jump back to the beginning of the loop wrapping it:
```
loop {  <---
  …        |
  brIf 0  --
  …
}
```

Similarly, the following `brIf 1` would jump to the beginning of the outer loop (nesting `1`):
```
loop {  <-----
  …          |
  loop {     |
    …        |
    brIf 1  --
    …
  }
  …
}
```

If loops and blocks are nested, the branching behavior depends on whether a loop or a block is
targeted with the nesting. The following `brIf 1` would jump to the end of the block, although it is
wrapped by a loop:
```
block {
  …
  loop {
    …
    brIf 1  --
    …        |
  }          |
  …          |
}  <----------
```

On the other hand, the following `brIf 1` would jump to the beginning of the loop, although it is
wrapped by a block:
```
loop {  <-----
  …          |
  block {    |
    …        |
    brIf 1  --
    …
  }
  …
}
```
-/
inductive Instruction where
  | core (instruction: Core)                -- Core instruction
  | loop (instructions: List Instruction)   -- Loop of instructions
  | block (instructions: List Instruction)  -- Block of instructions
  | br (nesting: Nat)                       -- Unconditional branching
  | brIf (nesting: Nat)                     -- Pop the stack's top value. Branch if it is true

abbrev BrOpt := Option Nat

/-
Interpret an instruction.
- Implement the helper function `interpretList` first to interpret the instructions of a loop or a
  block.
- In the case of a branching, return `some nesting` as `BrOpt`. Otherwise, return `none`.
- If the instructions of a loop return `some 0` as `BrOpt`, interpret the loop instruction again as
  a new iteration.
- If the instructions of a block return `some 0` as `BrOpt`, return `none`. This means that the
  block was exited early.
- If the instructions of a loop or a block return `some n + 1` as `BrOpt`, return `some n` to
  propagate the branching to outer loops and blocks.
- Finally, if the instructions of a loop or a block return `none` as `BrOpt`, return `none`. This
  means that the loop or block was exited normally by reaching its end.
- For `brIf`, if the top value is not a boolean, use `panic!` with a proper message.
-/
partial def Instruction.interpret (instruction: Instruction) (stack: Stack) : Stack × BrOpt :=
  match instruction with
    | .core instruction => (instruction.interpret! stack, none)
    | .loop instructions => let (stack, brOpt) := interpretList instructions stack
                            match brOpt with
                            | some 0 => instruction.interpret stack
                            | some (n + 1) => (stack, some n)
                            | none => (stack, none)
    | .block instructions => let (stack, brOpt) := interpretList instructions stack
                            match brOpt with
                            | some (n + 1) => (stack, some n)
                            | none | some 0 => (stack, none)
    | .br n => (stack, some n)
    | .brIf n => match stack with
                | .bool true :: stack => (stack, some n)
                | .bool false :: stack => (stack, none)
                | _ => panic! "Expected a bool on the stack for a jumpIf sttement"
    where
    interpretList (instructions: List Instruction) (stack: Stack) : Stack × BrOpt :=
      /-
      Interpret a list of instructions using `Instruction.interpret`.
      If an instruction in the list returns `some nesting` as `BrOpt`, then a branching occurred and
      needs to be propagated.
      -/
      match instructions with
        | [] => (stack, none)
        | i :: is => let (stack, brOpt) := i.interpret stack
                    match brOpt with
                    | some n => (stack, some n)
                    | none => interpretList is stack

/-
Use `Instruction.interpret` to interpret the instructions of a program.
If an instruction returns `some nesting` as `BrOpt`, then that is interpreted as an early return/exit
of the program.
-/
def interpretProgram (instructions: List Instruction) (stack: Stack) : Stack :=
  match instructions with
    | [] => stack
    | i :: is => let (stack, n) := i.interpret stack
                match n with
                | some _ => stack
                | none => interpretProgram is stack

#assert (interpretProgram [
  .loop [
    .core (.push (.nat 42))
  ]
] []) == [.nat 42]

#assert (interpretProgram [
  .block [
    .br 0,
    .core (.push (.nat 42))
  ]
] []) == []

#assert (interpretProgram [
  .loop [
    .brIf 0,
    .core (.push (.bool false)),
    .brIf 0
  ]
] [.bool true, .bool false]) == []

#assert (interpretProgram [
  .block [
    .loop [
      .br 1,
      .core (.push (.nat 42))
    ],
    .core (.push (.nat 42))
  ]
] []) == []

#assert (interpretProgram [
  .br 0,
  .core (.push (.nat 42))
] []) == []

#assert (interpretProgram [
  .loop [
    .core (.push (.nat 1)),
    .core .add,
    .core .dup,
    .core (.push (.nat 3)),
    .core .gt,
    .brIf 0
  ]
] [.nat 0]) == [.nat 3]

#assert (interpretProgram [
                            -- n
  .core .dup,               -- n, n
  .loop [                   -- DO {
    .core (.push (.nat 1)), --   1, n, n
    .core .swap,            --   n, 1, n
    .core .sub,             --   n-1, n
    .core .dup,             --   n-1, n-1, n
    .core .dup,             --   n-1, n-1, n-1, n
    .core (.push (.nat 0)), --   0, n-1, n-1, n-1, n
    .core .swap,            --   n-1, 0, n-1, n-1, n
    .core .gt,              -- } WHILE n-1 > 0
    .brIf 0
  ],                        -- 0, 0, 1, …, n-1, n
  .core .drop               -- 0, 1, …, n-1, n
] [.nat 3]) == [.nat 0, .nat 1, .nat 2, .nat 3]

/-
Since we have nested instructions now, an index is not enough for identifying an instruction.
A path as a list of indices could work by prepending a new index when entering a loop or a block:
```
add                [0]
mul                [1]
block {            [2]
  loop {           [0, 2]
    dup            [0, 0, 2]
    push (nat 42)  [1, 0, 2]
    gt             [2, 0, 2]
    brIf 0         [3, 0, 2]
  }
  …                [1, 2]
}
…                  [3]
```

Since the list will always contain at least one element, we will define our own type:
-/
structure Path where
  head: Nat
  tail: List Nat
deriving Repr, BEq

-- A new path starting with the very first instruction.
def Path.new: Path := { head := 0, tail := [] }

-- Step in the same nesting.
def Path.step (path: Path) : Path :=
  { path with head := path.head + 1 }

-- Enter a nesting (loop or block).
def Path.recurse (path: Path) : Path :=
  { head := 0, tail := path.head :: path.tail }

/-
In this language extension, instructions after a `br` (unconditional) are dead code.
Return the path to the first instruction after a `br` in the same nesting.
To not overcomplicate this exercise, do **not** consider nested cases like the following:
```
block {
  block {
    br 1
  }
  … (Actually dead code, but we ignore it)
}
```
-/
def deadCode? (instructions: List Instruction) : Option Path :=
  go instructions Path.new
  where
    go instructions path :=
      match instructions with
        | .br _ :: rest => if !rest.isEmpty then some path.step else go rest path.step
        | .loop instructions :: rest | .block instructions :: rest => go instructions path.recurse <|> go rest path.step
        | _ :: rest => go rest path.step
        | [] => none

#assert (deadCode? [.br 0, .core .gt]) == some { head := 1, tail := [] }
#assert (deadCode? [.loop [.br 0, .core .gt]]) == some { head := 1, tail := [0] }
#assert (deadCode? [.block [.br 0, .core .gt]]) == some { head := 1, tail := [0] }
#assert (deadCode? [.loop [.core .gt, .block [.br 0, .core .gt]]]) == some { head := 1, tail := [1, 0] }
#assert (deadCode? [.br 0]) == none
#assert (deadCode? [.brIf 0, .core .gt]) == none
#assert (deadCode? [.loop [.core .gt, .br 0]]) == none
#assert (deadCode? [.block [.block [.br 1], .core .gt]]) == none -- We ignore nested cases here.

/-
Return whether a loop or a block is targeted by some branching by checking its `instructions`.
A loop or a block is targeted if their instructions contain a branching instruction (`br` or `brIf`)
nested at level `n` with the branching target `n`. Example:
```
block {       -- This block is not targeted
  loop {
    block {
      brIf 0, -- Targets the inner block
      br 1,   -- Targets the loop
    }
  }
}
```

We check one `loop instructions` or `block instructions` by passing these `instructions` to this
function. Do **not** check whether every loop or block in `instructions` is targeted. That is done
in the next function.
-/
def loopOrBlockTargeted (instructions: List Instruction) : Bool :=
  go instructions 0
  where
    go instructions nesting :=
      match instructions with
      | .br n :: rest | .brIf n :: rest => n == nesting || go rest nesting
      | .loop instructions :: rest | .block instructions :: rest => go instructions (nesting + 1) || go rest nesting
      | _ :: rest => go rest nesting
      | [] => false

#assert (loopOrBlockTargeted []) == false
#assert (loopOrBlockTargeted [.br 1]) == false
#assert (loopOrBlockTargeted [.block [.br 2]]) == false
#assert (loopOrBlockTargeted [.br 0]) == true
#assert (loopOrBlockTargeted [.brIf 0]) == true
#assert (loopOrBlockTargeted [.block [.br 1]]) == true

-- Use `loopOrBlockTargeted` to check whether some loop or block in `instructions` is not targeted.
partial def unnecessaryNesting (instructions: List Instruction) : Bool :=
  instructions.any (
    fun instruction => match instruction with
                       | .loop instructions | .block instructions => !loopOrBlockTargeted instructions || unnecessaryNesting instructions
                       | _ => false
  )

#assert (unnecessaryNesting []) == false
#assert (unnecessaryNesting [.br 0]) == false
#assert (unnecessaryNesting [.loop [.br 0]]) == false
#assert (unnecessaryNesting [.block [.br 0]]) == false
#assert (unnecessaryNesting [.loop [.loop [.br 0], .br 0]]) == false
#assert (unnecessaryNesting [.loop [.br 0, .block [.br 0]]]) == false
#assert (unnecessaryNesting [.loop []]) == true
#assert (unnecessaryNesting [.block []]) == true
#assert (unnecessaryNesting [.loop [.br 1]]) == true
#assert (unnecessaryNesting [.loop [.br 0, .loop []]]) == true
#assert (unnecessaryNesting [.loop [.loop [], .br 0]]) == true
#assert (unnecessaryNesting [.loop [.loop [.br 1]]]) == true

/-
Return whether some loop in `instructions` directly wraps a `br 0`.
This can be a possible indicator of an endless loop.
-/
partial def br0Loop (instructions: List Instruction) : Bool :=
  br0 instructions false
  where
    br0 instructions inLoop :=
      instructions.any (
        fun instruction => match instruction with
                           | .loop loopInstructions => br0 loopInstructions true
                           | .block blockInstruction => br0 blockInstruction false
                           | .br 0 => inLoop
                           | _ => false
      )

#assert (br0Loop []) == false
#assert (br0Loop [.loop [.br 0]]) == true
#assert (br0Loop [.loop [.br 1]]) == false
#assert (br0Loop [.br 0]) == false
#assert (br0Loop [.loop [.loop [.br 0]]]) == true
#assert (br0Loop [.loop [.loop [.br 1]]]) == false
#assert (br0Loop [.loop [.loop [.br 1], .br 0]]) == true
#assert (br0Loop [.block [.loop [.br 0]]]) == true

end Structured
