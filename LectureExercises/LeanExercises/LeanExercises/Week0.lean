/-
# Week 0
The primary goal of this week's lab is to become familiar with basic commands, defining and calling
functions, list operations, conditional expressions, unit testing, and the use of let bindings.
-/

import Testing


/-
### Exercise 1
Use the `#eval` command to evaluate at least five expressions, such as
- boolean,
- arithmetic,
- list,
- string,
- or if-then-else expressions.
-/
#eval 1 == 1
#eval 1 + 2 == 2 - 1
#eval 425243 * 5
#eval [1, 2, 3]
#eval String.append "Lean " "slays"
#eval "Lean ".append "slays"
#eval if 1 == 1 then "one" else "no"

/-
### Exercise 2
Define a function `addThree` that takes three integers (`a`, `b`, and `c`) and returns their sum.
Write several unit tests using `#assert` to verify that your implementation is correct.

(For all subsequent exercises, you are still expected to test your functions, but the requirement
will not be written out.)
-/
def addThree (a b c: Int): Int := a + b + c

#assert (addThree 1 2 3) == 6
#assert (addThree 1 (-2) (-5)) == -6
#assert (addThree 1 2 3) == (addThree 2 1 3)
#assert (addThree 0 0 1) == 1

/-
### Exercise 3
Define a function `makeFullName` that takes two strings, `firstName` and `lastName`, and returns
a single string that combines them with a space in the middle.
-/
def makeFullName (a b: String): String :=
  a.append (" ".append b)

#assert (makeFullName "John" "Doe") == "John Doe"
#eval makeFullName "Henry" "Tom"
#assert (makeFullName "ABC" "DEF") == "ABC DEF"

/-
### Exercise 4
Define a function `findMax` that takes two integers, `x` and `y`, and returns the larger one.
-/
def findMax (x y: Int): Int := if x >= y then x else y

#assert (findMax 1 2) == 2
#assert (findMax (-1) (-3)) == -1
#eval findMax 1 2
#eval findMax 100 1
#assert (findMax 1 2) == 2
#assert (findMax 100 1) == 100

/-
### Exercise 5
Define a function `canVote` that takes a natural number `age` and a boolean `isCitizen`. It should
return `true` if and only if the `age` is 18 or greater and `isCitizen` is true.
-/
def canVote (age: Nat) (isCitizen: Bool): Bool :=
  if age >= 18 && isCitizen then True else False

#assert (canVote 15 False) == False
#assert (canVote 18 False) == False
#assert (canVote 18 True) == True
#assert (canVote 1 true) == false
#assert (canVote 19 false) == false
#assert (canVote 30 true) == true

/-
### Exercise 6
Define a function `diffOfSquares` that takes two integers, `x` and `y`.
It should calculate `(x + y) * (x - y)`.
Use let bindings to store the `x + y` part and the `x - y` part in separate variables before
multiplying them.
-/
def diffOfSquares (x y: Int): Int :=
  let a := x + y
  let b := x - y
  a * b

#assert (diffOfSquares 2 4) == -12
#assert (diffOfSquares 4 2) == 12
#assert (diffOfSquares 1 2) /- (1 + 2) * (1 - 2) -/ == -3
#assert (diffOfSquares 2 2) /- (2 + 2) * (2 - 2) -/ == 0

/-
### Exercise 7
Define a function `getQuadrant` that takes two integers, `x` and `y`, representing a 2D coordinate.
The function should return a string describing which quadrant the point `(x, y)` lies in:
- `"Upper right"`: `x` is positive and `y` is positive.
- `"Upper left"`: `x` is negative and `y` is positive.
- `"Lower left"`: `x` is negative and `y` is negative.
- `"Lower right"`: `x` is positive and `y` is negative.
- You should also handle the cases where `x` or `y` (or both) are zero:
  - If `x` is zero and `y` is not, return `"On Y-axis"`.
  - If `y` is zero and `x` is not, return `"On X-axis"`.
  - If both `x` and `y` are zero, return `"Origin"`.
-/
def getQuadrant (x y: Int): String :=
  if x > 0 && y > 0 then "Upper right"
  else if x < 0 && y > 0 then "Upper left"
  else if x < 0 && y < 0 then "Lower left"
  else if x > 0 && y < 0 then "Lower right"
  else if x == 0 && y != 0 then "On Y-axis"
  else if x != 0 && y == 0 then "On X-axis"
  else "Origin"

#assert (getQuadrant 0 0) == "Origin"
#assert (getQuadrant 2 4) == "Upper right"
#assert (getQuadrant 0 4) == "On Y-axis"
#assert (getQuadrant 1 1) == "Upper right"
#assert (getQuadrant (-1) 1) == "Upper left"
#assert (getQuadrant (-1) (-1)) == "Lower left"
#assert (getQuadrant 1 (-1)) == "Lower right"
#assert (getQuadrant 0 0) == "Origin"
#assert (getQuadrant 0 1) == "On Y-axis"
#assert (getQuadrant 1 0) == "On X-axis"

/-
### Exercise 8
Find out how many numbers in a list are both even and greater than 10.
-/
def evenAndGreaterThan10 (xs: List Int): Nat :=
  match xs with
    | [] => 0
    | x :: xs => if x > 10 && (x % 2 == 0) then 1 + evenAndGreaterThan10 xs else evenAndGreaterThan10 xs

def evenAndGreaterThan10_Pipeline (xs : List Int) : Nat :=
  xs |> List.filter (fun x => x > 10 && (x % 2 == 0))
     |> List.length

#assert (evenAndGreaterThan10_Pipeline [1, 2, 12, 14]) == 2
#assert (evenAndGreaterThan10 []) == 0
#assert (evenAndGreaterThan10 [1, 2, 3, 4, 5]) == 0
#assert (evenAndGreaterThan10 [1, 2, 3, 4, 12]) == 1
#assert (evenAndGreaterThan10 [99, 19]) == 0
#assert (evenAndGreaterThan10 [99, 19, 100]) == 1

/-
### Exercise 9
Calculate a single number by summing the squares of only the odd numbers.
-/
def sumOfSquaresOfOddNumbers (xs: List Int): Int :=
  match xs with
    | [] => 0
    | x :: xs => if (x % 2 == 1) then x^2 + sumOfSquaresOfOddNumbers xs else sumOfSquaresOfOddNumbers xs

def sumOfSquaresOfOddNumbers_Pipeline (xs: List Int): Int :=
  xs |> List.filter (fun x => (x % 2 == 1))
     |> List.map (fun x => x^2)
     |> List.sum

#assert (sumOfSquaresOfOddNumbers_Pipeline [1, 2, 3, 4, 5]) == 35
#assert (sumOfSquaresOfOddNumbers []) == 0
#assert (sumOfSquaresOfOddNumbers [1, 2, 3, 4, 5]) == 35
#assert (sumOfSquaresOfOddNumbers [1, 3, 5]) == 35
#assert (sumOfSquaresOfOddNumbers [2, 4, 6]) == 0
