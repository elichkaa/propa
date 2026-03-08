import Testing

namespace InterpreterExercises

/-
  We use the same Expression and Value types from the lecture.
-/

inductive Expression where
| intLiteral (n : Int)
| boolLiteral (b : Bool)
| add (e1 e2 : Expression)
| multiply (e1 e2 : Expression)
| less_than (e1 e2 : Expression)
| ifThenElse (cond eThen eElse : Expression)
| var (name : String)
| lambda (name : String) (body : Expression)
| apply (func arg : Expression)
deriving Repr, BEq, Inhabited

inductive Value where
| int (i : Int)
| bool (b : Bool)
| closure (name : String) (body : Expression) (env : Std.HashMap.Raw String Value)
deriving Inhabited, Repr

abbrev Environment := Std.HashMap.Raw String Value

def Value.asInt! : Value → Int
  | .int n => n
  | v => panic! s!"Expected int, got {v}"

def Value.asBool! : Value → Bool
  | .bool b => b
  | v => panic! s!"Expected bool, got {v}"

def Value.asClosure! : Value → String × Expression × Environment
  | .closure name body env => (name, body, env)
  | v => panic! s!"Expected closure, got {v}"


-- ============================================================
-- ### Exercise 1: expression size
-- Count the total number of nodes in an expression tree.
-- Literals and variables count as 1.
-- All other constructors count as 1 plus the size of subexpressions.
-- ============================================================

def Expression.size : Expression → Nat := sorry

#assert (Expression.size (.intLiteral 42)) == 1
#assert (Expression.size (.boolLiteral true)) == 1
#assert (Expression.size (.var "x")) == 1
#assert (Expression.size (.add (.intLiteral 1) (.intLiteral 2))) == 3
#assert (Expression.size (.ifThenElse (.boolLiteral true) (.intLiteral 1) (.intLiteral 2))) == 4
#assert (Expression.size (.lambda "x" (.add (.var "x") (.intLiteral 1)))) == 4
#assert (Expression.size (.apply (.lambda "x" (.var "x")) (.intLiteral 42))) == 4


-- ============================================================
-- ### Exercise 2: expression depth
-- Return the maximum nesting depth of an expression.
-- Literals and variables have depth 1.
-- ============================================================

def Expression.depth : Expression → Nat := sorry

#assert (Expression.depth (.intLiteral 42)) == 1
#assert (Expression.depth (.var "x")) == 1
#assert (Expression.depth (.add (.intLiteral 1) (.intLiteral 2))) == 2
#assert (Expression.depth (.add (.add (.intLiteral 1) (.intLiteral 2)) (.intLiteral 3))) == 3
#assert (Expression.depth (.lambda "x" (.add (.var "x") (.intLiteral 1)))) == 3


-- ============================================================
-- ### Exercise 3: contains variable
-- Return true if a given variable name appears anywhere in
-- the expression, including inside lambdas.
-- ============================================================

def Expression.containsVar (name : String) : Expression → Bool := sorry

#assert (Expression.containsVar "x" (.var "x")) == true
#assert (Expression.containsVar "y" (.var "x")) == false
#assert (Expression.containsVar "x" (.add (.var "x") (.intLiteral 1))) == true
#assert (Expression.containsVar "x" (.lambda "y" (.var "x"))) == true
#assert (Expression.containsVar "x" (.lambda "x" (.var "x"))) == true
#assert (Expression.containsVar "z" (.lambda "x" (.var "x"))) == false


-- ============================================================
-- ### Exercise 4: pretty print
-- Convert an expression to a readable string.
-- Use the following format:
--   intLiteral n     => toString n
--   boolLiteral b    => toString b
--   var name         => name
--   add e1 e2        => "(e1 + e2)"
--   multiply e1 e2   => "(e1 * e2)"
--   less_than e1 e2  => "(e1 < e2)"
--   ifThenElse c t e => "(if c then t else e)"
--   lambda x body    => "(λx. body)"
--   apply f arg      => "(f arg)"
-- ============================================================

def Expression.pretty : Expression → String := sorry

#assert (Expression.pretty (.intLiteral 42)) == "42"
#assert (Expression.pretty (.boolLiteral true)) == "true"
#assert (Expression.pretty (.var "x")) == "x"
#assert (Expression.pretty (.add (.intLiteral 1) (.intLiteral 2))) == "(1 + 2)"
#assert (Expression.pretty (.lambda "x" (.var "x"))) == "(λx. x)"
#assert (Expression.pretty (.apply (.lambda "x" (.var "x")) (.intLiteral 42))) == "((λx. x) 42)"
#assert (Expression.pretty (.ifThenElse (.boolLiteral true) (.intLiteral 1) (.intLiteral 2))) == "(if true then 1 else 2)"


-- ============================================================
-- ### Exercise 5: implement the lexically-scoped interpreter
-- Implement the environment-based interpreter with lexical
-- scoping using closures. Lambdas should capture the current
-- environment when created.
-- ============================================================

partial def interp (env : Environment) : Expression → Value := sorry

-- basic arithmetic
#assert (interp ∅ (.add (.intLiteral 1) (.intLiteral 2))) == .int 3
#assert (interp ∅ (.multiply (.intLiteral 3) (.intLiteral 4))) == .int 12

-- conditionals
#assert (interp ∅ (.ifThenElse (.boolLiteral true) (.intLiteral 1) (.intLiteral 2))) == .int 1
#assert (interp ∅ (.ifThenElse (.less_than (.intLiteral 3) (.intLiteral 5)) (.intLiteral 10) (.intLiteral 20))) == .int 10

-- lambda and apply
#assert (interp ∅ (.apply (.lambda "x" (.add (.var "x") (.intLiteral 1))) (.intLiteral 41))) == .int 42

-- lexical scoping: the classic test from the lecture
-- plus = λx. λy. x + y
-- inc  = plus 1
-- prog = λx. inc x   -- x here should NOT interfere with plus's x
def plusExpr : Expression := .lambda "x" (.lambda "y" (.add (.var "x") (.var "y")))
def incExpr : Expression := .apply plusExpr (.intLiteral 1)
def progExpr : Expression := .lambda "x" (.apply incExpr (.var "x"))

#assert (interp ∅ (.apply progExpr (.intLiteral 41))) == .int 42


-- ============================================================
-- ### Exercise 6: extend the interpreter with subtract and negate
-- Add two new constructors to Expression and handle them in
-- the interpreter.
-- ============================================================

inductive Expression2 where
| intLiteral (n : Int)
| boolLiteral (b : Bool)
| add (e1 e2 : Expression2)
| subtract (e1 e2 : Expression2)   -- new
| negate (e : Expression2)         -- new
| multiply (e1 e2 : Expression2)
| less_than (e1 e2 : Expression2)
| ifThenElse (cond eThen eElse : Expression2)
| var (name : String)
| lambda (name : String) (body : Expression2)
| apply (func arg : Expression2)
deriving Repr, BEq, Inhabited

inductive Value2 where
| int (i : Int)
| bool (b : Bool)
| closure (name : String) (body : Expression2) (env : Std.HashMap.Raw String Value2)
deriving Inhabited, Repr

abbrev Environment2 := Std.HashMap.Raw String Value2

def Value2.asInt! : Value2 → Int | .int n => n | v => panic! s!"Expected int, got {v}"
def Value2.asBool! : Value2 → Bool | .bool b => b | v => panic! s!"Expected bool, got {v}"
def Value2.asClosure! : Value2 → String × Expression2 × Environment2
  | .closure n b e => (n, b, e) | v => panic! s!"Expected closure, got {v}"

partial def interp2 (env : Environment2) : Expression2 → Value2 := sorry

#assert (interp2 ∅ (.subtract (.intLiteral 10) (.intLiteral 3))) == .int 7
#assert (interp2 ∅ (.negate (.intLiteral 5))) == .int (-5)
#assert (interp2 ∅ (.negate (.subtract (.intLiteral 3) (.intLiteral 10)))) == .int 7
#assert (interp2 ∅ (.add (.negate (.intLiteral 1)) (.intLiteral 5))) == .int 4


-- ============================================================
-- ### Exercise 7: let binding as syntactic sugar
-- A let binding `let x = e1 in e2` is just syntactic sugar for
-- `(λx. e2) e1`. Define a helper function that constructs this
-- and use it to write a multi-step computation.
-- ============================================================

def letIn (name : String) (value : Expression) (body : Expression) : Expression := sorry

-- (let x = 3 in let y = 4 in x + y) should evaluate to 7
def letExample : Expression :=
  letIn "x" (.intLiteral 3)
    (letIn "y" (.intLiteral 4)
      (.add (.var "x") (.var "y")))

#assert (interp ∅ letExample) == .int 7

-- (let x = 2 in let y = x * 3 in y + 1) should evaluate to 7
def letExample2 : Expression :=
  letIn "x" (.intLiteral 2)
    (letIn "y" (.multiply (.var "x") (.intLiteral 3))
      (.add (.var "y") (.intLiteral 1)))

#assert (interp ∅ letExample2) == .int 7


-- ============================================================
-- ### Exercise 8: count lambdas
-- Count the number of lambda expressions in an expression tree,
-- including nested ones.
-- ============================================================

def Expression.countLambdas : Expression → Nat := sorry

#assert (Expression.countLambdas (.intLiteral 1)) == 0
#assert (Expression.countLambdas (.lambda "x" (.var "x"))) == 1
#assert (Expression.countLambdas (.lambda "x" (.lambda "y" (.var "x")))) == 2
#assert (Expression.countLambdas (.apply (.lambda "x" (.var "x")) (.lambda "y" (.var "y")))) == 2
#assert (Expression.countLambdas (.add (.intLiteral 1) (.intLiteral 2))) == 0


-- ============================================================
-- ### Exercise 9: check if expression is a value
-- An expression is a value if it is a literal or a lambda.
-- Variables, applications, and arithmetic are not values.
-- ============================================================

def Expression.isValue : Expression → Bool := sorry

#assert (Expression.isValue (.intLiteral 42)) == true
#assert (Expression.isValue (.boolLiteral false)) == true
#assert (Expression.isValue (.lambda "x" (.var "x"))) == true
#assert (Expression.isValue (.var "x")) == false
#assert (Expression.isValue (.add (.intLiteral 1) (.intLiteral 2))) == false
#assert (Expression.isValue (.apply (.lambda "x" (.var "x")) (.intLiteral 1))) == false


-- ============================================================
-- ### Exercise 10: fold for Expression
-- Derive a fold function for Expression mechanically from its
-- constructors. Then use it to reimplement size and containsVar.
-- ============================================================

def Expression.fold
  (fint : Int → β)
  (fbool : Bool → β)
  (fadd : β → β → β)
  (fmul : β → β → β)
  (flt : β → β → β)
  (fif : β → β → β → β)
  (fvar : String → β)
  (flambda : String → β → β)
  (fapply : β → β → β)
  (e : Expression) : β := sorry

-- reimplement size using fold
def Expression.size2 (e : Expression) : Nat :=
  e.fold
    sorry sorry sorry sorry sorry sorry sorry sorry sorry

-- reimplement containsVar using fold
def Expression.containsVar2 (name : String) (e : Expression) : Bool :=
  e.fold
    sorry sorry sorry sorry sorry sorry sorry sorry sorry

#assert (Expression.size2 (.add (.intLiteral 1) (.intLiteral 2))) == 3
#assert (Expression.size2 (.lambda "x" (.add (.var "x") (.intLiteral 1)))) == 4
#assert (Expression.containsVar2 "x" (.lambda "y" (.var "x"))) == true
#assert (Expression.containsVar2 "z" (.add (.var "x") (.var "y"))) == false


end InterpreterExercises
