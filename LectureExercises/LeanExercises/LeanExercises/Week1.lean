/-
# Week 1
To solve the exercises, follow the instructions in the comments and replace every
`sorry` with your code.
Make sure that all assertions succeed!
Reading the assertions can help you further understand what you need to do.

If you get stuck at some point, feel free to post a question on the Ilias forum.
-/

import Testing

namespace ExTranslation

/-
Translate the following Java function into a Lean one:
```java
/**
 * Returns a message indicating whether the person may enter the club.
 *
 * @param age            the person's age (non‑negative)
 * @param hasMuttiZettel true if the person has a parental permission slip
 * @return "Welcome" if entry is allowed, otherwise "Not allowed"
 */
public static String enterClub(int age, boolean hasMuttiZettel) {
    if (age >= 18 || (age >= 16 && hasMuttiZettel)) {
        return "Welcome";
    } else {
        return "Not allowed";
    }
}
```
-/
def enterClub (age: Nat) (hasMuttiZettel: Bool): String :=
  if (age >= 18 || (age >= 16 && hasMuttiZettel)) then "Welcome" else "Not allowed"


#assert (enterClub 20 false) == "Welcome"
#assert (enterClub 16 true) == "Welcome"
#assert (enterClub 17 false) == "Not allowed"
#assert (enterClub 15 true) == "Not allowed"

/-
Now, translate this function:
```java
/**
 * Calculates shipping cost.
 *
 * @param weightInKg weight of the package (kg)
 * @param country    destination country
 * @param express    true for express service
 * @return total cost in €, or 0.0 if the weight is <= 0
 */
public static double calcShipping(double weightInKg, String country, boolean express) {
    // Guard clause for invalid weight
    if (weightInKg <= 0.0) {
        return 0.0;
    }

    // Base cost proportional to weight
    double base = weightInKg * 5.0;
    // Country factor: 1.0 for Germany, otherwise 2.0
    double factor = "Germany".equals(country) ? 1.0 : 2.0;
    // Express surcharge
    double surcharge = express ? 10.0 : 0.0;
    // Final cost
    return factor * base + surcharge;
}
```
-/
def calcShipping (weightInKg: Float) (country: String) (express: Bool): Float :=
  if weightInKg <= 0.0 then 0.0
  else
    let base := weightInKg * 5.0
    let factor := if country == "Germany" then 1.0 else 2.0
    let surcharge := if express then 10.0 else 0.0
    factor * base + surcharge

#assert (calcShipping (-1.0) "Germany" false) == 0.0
#assert (calcShipping 0.0 "Germany" false) == 0.0
#assert (calcShipping 1.0 "Germany" false) == 5.0
#assert (calcShipping 0.42 "Germany" false) == 2.1
#assert (calcShipping 0.42 "Germany" true) == 12.1
#assert (calcShipping 0.42 "Spain" false) == 4.2
#assert (calcShipping 0.42 "Spain" true) == 14.2

end ExTranslation

namespace ExIfElse

/-
You are running a tiny shop selling melons with a special offer.
- The price of a melon is 3€.
- If the customer buys more than 10 melons, then the price of every melon becomes 2€.
- For more than 100 melons, the price becomes 1€.
Calculate the price in euros.
-/
def price (numberOfMelons: Nat) : Nat :=
  if numberOfMelons > 100 then 1 * numberOfMelons
  else if numberOfMelons > 10 then 2 * numberOfMelons
  else 3 * numberOfMelons

#assert (price 0) == 0
#assert (price 1) == 3
#assert (price 10) == 30
#assert (price 11) == 22
#assert (price 42) == 84
#assert (price 100) == 200
#assert (price 101) == 101

end ExIfElse

namespace ExLambda

-- This is how a full function definition looks like:
def double (x: Nat) : Nat :=
  2 * x

/-
Lambdas / anonymous functions are normally used as arguments to other functions.
For example, many `List` functions expect a function as an argument.
Define the function `double` above as a lambda to practice the syntax.
-/
def doubleLambda := fun x => 2 * x
def doubleLambda2 := (. * 2)

#assert (double 2) == doubleLambda 2
#assert (double 2) == doubleLambda2 2

-- You can check that both functions have the same signature:
#check double
#check doubleLambda
#check doubleLambda2

def add (x: Nat) (y: Nat) :=
  x + y

/-
Define the function `add` above as a lambda to practice the syntax of
lambdas with multiple arguments.
Since the type can't be inferred in this case, you also need to annotate the types.
Note that (x: T) (y: T) can be joined to (x y: T).
-/
def addLambda := fun (x y: Nat) => x + y

#assert (add 1 2) == addLambda 1 2

/-
This function takes the natural number `x` and two functions `f` and `g`
that map a natural number to another natural number.
Complete the function body to first execute `f` and then `g` on `x`.
Try the pipe operator `|>`
-/
def chain (f g: Nat -> Nat) (x: Nat) : Nat :=
  x |> f |> g

-- Call `chain` with two lambdas that return 42 when applied on 20 as a chain.
def meaningOfLife :=
  chain
    (fun _ => 42)
    (fun x => x)
    20

#assert meaningOfLife == 42

end ExLambda

namespace ExLists

/-
# Lists
This week, you are not supposed to write any recursive function yourself.
For the following exercises, use functions defined on `List` instead.
-/

-- Double each number in the input list using the `map` function.
def double (xs: List Nat) : List Nat :=
  xs |> List.map (fun x => x * 2)

#assert (double [1, 2, 3]) == [2, 4, 6]
#assert (double []) == []

-- Only keep even numbers of the input list using the `filter` function.
def evens (xs: List Nat) : List Nat :=
  xs |> List.filter (fun x => x % 2 == 0)

#assert (evens [1, 2, 3]) == [2]
#assert (evens []) == []

-- Return the sum of the square roots of non-negative floats in the input list.
def sumOfSquareRoots (xs: List Float) : Float :=
  xs |> List.filter (fun x => x >= 0)
     |> List.map (fun x => Float.sqrt x)
     |> List.sum

#assert (sumOfSquareRoots [42.13, 1.0, -7.5]) == 42.13.sqrt + 1.0
#assert (sumOfSquareRoots []) == 0.0

end ExLists

namespace ExCheckedSub

/-
## Checked Subtraction
In Lean, subtraction of natural numbers saturates at 0:
-/
#assert (1 - 42) == 0

/-
This silent saturation could be surprising and undesired.
We want to define our own datatype `CheckedSubNat` containing
some `Nat` in the normal case. Instead of saturating at 0,
our type will apply checked subtraction and return `Underflow`
in case the right term is bigger than the left one.
-/

inductive CheckedSubNat where
  | Some (n: Nat)
  | Underflow
deriving Repr, BEq

open CheckedSubNat

/-
Addition should be applied normally if both terms are `Some`.
Propagate `Underflow` otherwise.
-/
def add (n m: CheckedSubNat) : CheckedSubNat :=
  match m, n with
    | Some m, Some n => Some (m + n)
    | _, _ => Underflow

#assert (add Underflow Underflow) == Underflow
#assert (add (Some 1) Underflow) == Underflow
#assert (add Underflow (Some 2)) == Underflow
#assert (add (Some 1) (Some 2)) == Some 3

/-
Apply checked subtration if both terms are `Some`.
Propagate `Underflow` otherwise.
-/
def sub (n m: CheckedSubNat) : CheckedSubNat :=
  match n, m with
    | Some n, Some m => if n >= m then Some (n - m) else Underflow
    | _, _ => Underflow

#assert (sub Underflow Underflow) == Underflow
#assert (sub (Some 3) Underflow) == Underflow
#assert (sub Underflow (Some 2)) == Underflow
#assert (sub (Some 1) (Some 42)) == Underflow
#assert (sub (Some 3) (Some 2)) == Some 1

end ExCheckedSub

namespace ExTextEditor

/-
We want to implement a text editor, a very primitive one.
You should create the required datatypes as described below.
- The `Editor` should store its `content` as a list of characters (`Char`).
- The editor's tab width (`tabWidth`) should be configurable. When `Tab` is pressed on the keyboard,
whitespaces ' ' are added according to the tab width.
- The editor can receive `Input` as a single `Character`, a `Backspace`, a `Newline` or a `Tab`.
- Add `deriving Repr, BEq` to the end of `Editor` for editor assertions to work.
-/

inductive Input where
    | Character (c: Char)
    | Backspace
    | Newline
    | Tab

structure Editor where
  content : List Char
  tabWidth : Nat
  deriving Repr, BEq

/-
Define a function named `new` for `Editor`. It should take a `tabWidth` and return an editor with
that tab width and empty content.
-/
def Editor.new (tabWidth: Nat): Editor := {content := [], tabWidth := tabWidth : Editor}

#assert (Editor.new 2) == ({ content := [], tabWidth := 2 } : Editor)

/-
Update the editor's content according to the input:
- `Character` prepends the single character.
- `Backspace` removes the last character entered if one exists (not the list's last element).
- `Newline` prepends the character `\n`.
- `Tab` prepends as many whitespaces ' ' as the editor's `tabWidth`.
  To implement `Tab`, use `List.replicate` and list concatenation.

Store the updated content in a variable, then return the editor with the new content.
-/
def Editor.update (editor: Editor) (input: Input) : Editor :=
  let oldContent := editor.content
  match input with
    | Input.Character c => {editor with content := c :: oldContent}
    | Input.Backspace => {editor with content := if oldContent.length >= 1 then oldContent.tail else []}
    | Input.Newline => {editor with content := '\n' :: oldContent}
    | Input.Tab => {editor with content := (List.replicate editor.tabWidth ' ') ++ oldContent}

#assert (Editor.update { content := [], tabWidth := 2 } (Input.Character 'a')).content == ['a']
#assert (Editor.update { content := ['b', 'a'], tabWidth := 2 } Input.Backspace).content == ['a']
#assert (Editor.update { content := ['a'], tabWidth := 2 } Input.Newline).content == ['\n', 'a']
#assert (Editor.update { content := ['a'], tabWidth := 2 } Input.Tab).content == [' ', ' ', 'a']
#assert (Editor.update { content := ['a'], tabWidth := 4 } Input.Tab).content == [' ', ' ', ' ', ' ', 'a']

end ExTextEditor

namespace ExMaze

/-
## Maze
We want to program a maze game!
Here is a sketch of our example maze:

5|++++++
4|G+   +
3| + + +
2|   + +
1|++++ +
0|x    +
  ------
  012345

- The player (represented by x) starts at the lower left corner with the
  coordinates 0,0. It can move up, down, left and right.
- Players can't move beyond the left or lower wall which means that its coordinates
  are always positive.
- There are some obstacles (represented by +) on the way to the
  goal (represented by G). When an obstacle is hit, the move is skipped.
- The player must reach the goal (G). Once the goal is reached, further moves are ignored.
- The player has a maximum number of moves to reach the goal. If the maximum number of moves
  is exceeded, the game is over.

We will start by defining the datatypes representing our game logic.
-/

structure Point where
  x: Nat
  y: Nat
deriving Repr, BEq

def Point.origin: Point := ⟨0,0⟩

structure Maze where
  obstacles: List Point
  goal: Point
  maxMoves: Nat -- The maximum number of moves to solve the maze

inductive Move where
  | Up
  | Down
  | Left
  | Right

open Move

-- Apply a move on the input position and return the new position, ignoring obstacles.
def Move.apply (move: Move) (position: Point) : Point :=
  match move with
    | Up => {x := position.x, y := position.y + 1}
    | Down => {x := position.x, y := position.y - 1}
    | Left => {x := position.x - 1, y := position.y}
    | Right => {x := position.x + 1, y := position.y}

/-
Apply a move on the input position if the goal is not already reached and the new position
is not an obstacle. Otherwise, return the input position unchanged.
-/
def Move.applyChecked (move: Move) (position: Point) (maze: Maze) : Point :=
  if position == maze.goal then position
  else
    let newPos := move.apply position
    if not (maze.obstacles.contains newPos) then newPos
    else position

inductive GameResult where
  | TooManyMoves
  | GoalReached
  | NotAtGoal (position: Point)
deriving Repr, BEq

open GameResult

-- Apply the provided moves on the maze and return the final position.
def Maze.run (maze : Maze) (moves: List Move) : Point :=
  moves.foldl (fun position move => move.applyChecked position maze) Point.origin

/-
Check whether the provided moves solve the maze.
- If the number of moves is higher than the maze's maximum number of moves, return `TooManyMoves`.
- Call `run` and check whether the goal is reached. Otherwise, return the final position in `NotAtGoal`.
-/
def Maze.checkSolution (maze: Maze) (moves: List Move) : GameResult :=
  if moves.length > maze.maxMoves then GameResult.TooManyMoves
  else
    let finalPos := maze.run moves
    if finalPos == maze.goal then GameResult.GoalReached else GameResult.NotAtGoal finalPos

-- The maze sketched above.
def maze: Maze := {
  obstacles := [⟨5,0⟩, ⟨5,1⟩, ⟨5,2⟩, ⟨5,3⟩, ⟨5,4⟩, ⟨5,5⟩, ⟨4,5⟩, ⟨3,5⟩, ⟨2,5⟩, ⟨1,5⟩, ⟨0,5⟩, ⟨0,1⟩,
                ⟨1,1⟩, ⟨2,1⟩, ⟨3,1⟩, ⟨3,2⟩, ⟨3,3⟩, ⟨1,3⟩, ⟨1,4⟩],
  goal := ⟨0,4⟩,
  maxMoves := 16,
}

-- Play the game by entering the required moves to reach the goal in the sketch above.
def solution: List Move := [
    Right, Right, Right, Right, Up, Up, Up, Up, Left, Left, Down, Down, Left, Left, Up, Up
]

#assert (maze.checkSolution solution) == GoalReached
#assert (maze.checkSolution [Up]) == NotAtGoal ⟨0,0⟩
#assert (maze.checkSolution [Left]) == NotAtGoal ⟨0,0⟩
#assert (maze.checkSolution [Down]) == NotAtGoal ⟨0,0⟩
#assert (maze.checkSolution (List.replicate 5 Right)) == NotAtGoal ⟨4,0⟩
#assert (maze.checkSolution (List.replicate 17 Left)) == GameResult.TooManyMoves

end ExMaze
