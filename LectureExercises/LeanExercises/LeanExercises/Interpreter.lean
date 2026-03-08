import Testing

-- this is an abstract syntax of a functional programming language that we want to interpret
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

instance : ToString Expression where
  toString := reprStr

-- our goal is to write a function that takes this Expression and returns a result
-- first through substitution

def Expression.freeVariables : Expression → Std.HashSet String
  | .intLiteral _ => {}
  | .boolLiteral _ => {}
  | .add e1 e2 => e1.freeVariables ∪ e2.freeVariables
  | .multiply e1 e2 => e1.freeVariables ∪ e2.freeVariables
  | .less_than e1 e2 => e1.freeVariables ∪ e2.freeVariables
  | .ifThenElse cond eThen eElse => cond.freeVariables ∪ eThen.freeVariables ∪ eElse.freeVariables
  | .var name => {name}
  | .lambda name body => body.freeVariables |>.erase name
  | .apply func arg => func.freeVariables ∪ arg.freeVariables

namespace Interpreter

-- all of the values our language can produce
inductive Value where
| int (i : Int)
| bool (b : Bool)
| lambda (name : String) (body : Expression)
deriving Repr, BEq, Inhabited

instance : ToString Value where
  toString := reprStr

-- helper functions which return the value if its correct and else error if not as expected
def Value.asInt! : Value → Int
  | .int n => n
  | v => panic! s!"Expected int value, but got {v}"
def Value.asBool! : Value → Bool
  | .bool b => b
  | v => panic! s!"Expected bool value, but got {v}"
def Value.asLambda! : Value → String × Expression
  | .lambda name body => (name, body)
  | v => panic! s!"Expected function value, but got {v}"

def Value.asExpression : Value → Expression
  | .int n => .intLiteral n
  | .bool b => .boolLiteral b
  | .lambda name body => Expression.lambda name body

#eval Value.asInt! (.int 42)
#eval Value.asInt! (.bool true)
#eval Value.asBool! (.bool true)
#eval Value.asBool! (.int 0)
#eval Value.asExpression (.int 42)
#eval Value.asExpression (.bool false)

def Value.hasFreeVar (name : String) : Value → Bool
  | .lambda x body => (Expression.lambda name body).freeVariables.contains x
  | _ => false


/- The interpreter requires a substitution function to handle let-bindings.
   The substitution function replaces all free occurrences of a variable
   with a given value in an expression.

   New programming feature: local recursive definitions (let rec)
   We can define local functions inside another function using `let`.
   This is useful to structure code and avoid code duplication. In particular,
   the bound variables of the outer function are in scope in the local function.
   If the local function is recursive, we use `let rec`.
 -/
def substitute (e : Expression) (name : String) (value : Value) : Expression :=
  /- Helper function to recursively substitute in expression `e`. Note that
      `subst` is defined inside `substitute`, so it has access to `name` and `value`. -/
  let rec subst : Expression → Expression
  | .intLiteral n => .intLiteral n
  | .boolLiteral b => .boolLiteral b
  | .add e1 e2 => .add (subst e1) (subst e2)
  | .multiply e1 e2 => .multiply (subst e1) (subst e2)
  | .less_than e1 e2 => .less_than (subst e1) (subst e2)
  | .ifThenElse cond eThen eElse => .ifThenElse (subst cond) (subst eThen) (subst eElse)
  | .var n => if n == name then value.asExpression else .var n
  | .lambda x body =>
    if x == name then
      .lambda x body
    else if value.hasFreeVar x then
      panic! "Not implemented: fresh variable generation in Expression.substitute"
    else
      .lambda x (subst body)
  | .apply func arg => .apply (subst func) (subst arg)

  subst e

/- The substitution function must be careful with variable shadowing:
   In `.lambda x body`, the variable `x` is bound in `body`. Thus, when
   the substituted variable is also `x`, we must not substitute inside `body`.
   Otherwise, if the value we are inserting contains the lambda's variable `x`
   as a free variable, we must rename `x` to avoid any collisions (not implemented
   above, see the labs).

   These situations arise when variables are shadowed. For example, in
   `λ x. (λ x. x + 3)`, the inner `x` shadows the outer `x`. Therefore, the outer
   lambda binding has no effect on the expression `x + 3`.
 -/

-- intepreters are partial because their termination depends on the input (which may not be terminating)
-- they also have ! after the name because they may crash
partial def interp! : Expression → Value
  /- Literals evaluate to their value -/
  | .intLiteral i => .int i
  | .boolLiteral b => .bool b

  /- Arithmetics and comparisons evaluate the operands and apply the operator -/
  | .add e1 e2 =>
    let v1 := interp! e1
    let v2 := interp! e2
    .int (v1.asInt! + v2.asInt!)

  | .multiply e1 e2 =>
    let v1 := interp! e1
    let v2 := interp! e2
    .int (v1.asInt! * v2.asInt!)

  | .less_than e1 e2 =>
    let v1 := interp! e1
    let v2 := interp! e2
    .bool (v1.asInt! < v2.asInt!)

  /- Conditionals evaluate the condition and select the branch -/
  | .ifThenElse c t e =>
    let vc := interp! c
    if (vc.asBool!) then
      interp! t
    else
      interp! e

  /- Lambdas evaluate to a value with the lambda intact. -/
  | .lambda name body => Value.lambda name body

  /- To apply a lambda, we first evaluate the function expression and then
     the argument expression. The function expression must indeed evaluate to
     a lambda value, so that we can use substitution to replace `name` by the
     argument value.
   -/
  | .apply funexp arg =>
    let (name, body) := (interp! funexp).asLambda!
    let argval := interp! arg
    -- NOTE: SUBOPTIMAL PERFORMANCE because substitute traverses the body two times (one with intepreter one without)
    -- even if the variable is not even contained in the body
    interp! (substitute body name argval)

  /- Variables: must be eliminated through substitution. Any left-over variable
     indicates that it was unbound. -/
  | .var name => panic! s!"Unbound variable {name}"

#eval interp! (.ifThenElse (.less_than (.intLiteral 3) (.intLiteral 5))
                (.add (.intLiteral 1) (.intLiteral 2))
                (.multiply (.intLiteral 10) (.intLiteral 20)))
#eval interp! (.lambda "x" (.add (.var "x") (.intLiteral 1)))
#eval interp! (.apply (.lambda "x" (.add (.var "x") (.intLiteral 1))) (.intLiteral 42))
#eval interp! (.var "x")  -- unbound variable

-- in order to solve the issue with the slow substitute we use Environment
-- this is a map of variable_names to variable_values
-- and now we don't need to substitute the variable through traversing but we can lookup the map

abbrev Environment := Std.HashMap String Value

/- The environment-based interpreter that evaluates expression `e` to a value or panics. -/
partial def interpEnv! (env : Environment) : Expression → Value
  /- Literals evaluate to their value -/
  | .intLiteral i => .int i
  | .boolLiteral b => .bool b

  /- Arithmetics and comparisons evaluate the operands and apply the operator -/
  | .add e1 e2 =>
    let v1 := interpEnv! env e1
    let v2 := interpEnv! env e2
    .int (v1.asInt! + v2.asInt!)

  | .multiply e1 e2 =>
    let v1 := interpEnv! env e1
    let v2 := interpEnv! env e2
    .int (v1.asInt! * v2.asInt!)

  | .less_than e1 e2 =>
    let v1 := interpEnv! env e1
    let v2 := interpEnv! env e2
    .bool (v1.asInt! < v2.asInt!)

  /- Conditionals evaluate the condition and select the branch -/
  | .ifThenElse c t e =>
    let vc := interpEnv! env c
    if (vc.asBool!) then
      interpEnv! env t
    else
      interpEnv! env e

  /- Lambdas evaluate to a value with the lambda intact. -/
  | .lambda name body => Value.lambda name body

  /- To apply a lambda, we first evaluate the function expression and then
     the argument expression. The function expression must indeed evaluate to
     a lambda value, so that we can use substitution to replace `name` by the
     argument value.
   -/
  | .apply funexp arg =>
    let (name, body) := (interpEnv! env funexp).asLambda!
    let argval := interpEnv! env arg
    let env' := env.insert name argval
    interpEnv! env' body

  /- Variables: must be eliminated through substitution. Any left-over variable
     indicates that it was unbound. -/
  | .var name =>
    match env.get? name with
    | some v => v
    | none => panic! s!"Unbound variable: {name}, env is {env.toList}"


#eval interpEnv! ∅
         (.ifThenElse (.less_than (.intLiteral 3) (.intLiteral 5))
                (.add (.intLiteral 1) (.intLiteral 2))
                (.multiply (.intLiteral 10) (.intLiteral 20)))
#eval interpEnv! ∅ (.lambda "x" (.add (.var "x") (.intLiteral 1)))
#eval interpEnv! ∅ (.apply (.lambda "x" (.add (.var "x") (.intLiteral 1))) (.intLiteral 42))
#eval interpEnv! ∅ (.var "x")  -- unbound variable


/-
  Our interpreter works fine, but is this the semantics we want?
  What we have implemented is known as functions with dynamic scoping, a highly
  fragile programming feature. Consider the following example:
-/

def plus : Expression := (.lambda "x" (.lambda "y" (.add (.var "x") (.var "y"))))
def inc : Expression := (.apply plus (.intLiteral 1))
def prog : Expression :=  (.lambda "x" (.apply inc (.var "x")))

-- Should be 42, but yields 82
#eval interpEnv! ∅ (.apply prog (.intLiteral 41))

/-
  Where did we go wrong?

  The problem is that in dynamic scoping, the variable `x` in `plus` refers to
  the most recent binding of `x` in the call stack, which is the `x` bound in `prog`.
  This leads to unexpected behavior, as the value of `x` can change depending on the
  context in which `plus` is called. Clearly, this is not what we want.

  The solution is to use lexical scoping (also known as static scoping), where
  variables refer to the bindings in the environment where the function was defined,
  not where it is called. To implement lexical scoping, we need to modify our
  interpreter to capture the environment at the time of function definition.
-/

end Interpreter


namespace LexicalScoping

inductive Value where
| int (n : Int)
| bool (b : Bool)
| closure (name : String) (body : Expression) (env : Std.HashMap.Raw String Value)
deriving Inhabited, Repr

instance : ToString Value where
  toString := reprStr


abbrev Environment := Std.HashMap.Raw String Value

/- We define helper functions to extract the underlying data from a `Value`. -/
def Value.asInt! : Value → Int
  | .int n => n
  | v => panic! s!"Expected int value, but got {v}"
def Value.asBool! : Value → Bool
  | .bool b => b
  | v => panic! s!"Expected bool value, but got {v}"
def Value.asClosure : Value → String × Expression × Environment
  | .closure name body env => (name, body, env)
  | v => panic! s!"Expected function value, but got {v}"

partial def interpEnv! (env : Environment) : Expression → Value
| .intLiteral n => .int n
| .boolLiteral b => .bool b
| .add e1 e2 =>
  let v1 := interpEnv! env e1
  let v2 := interpEnv! env e2
  .int (v1.asInt! + v2.asInt!)
| .multiply e1 e2 =>
  let v1 := interpEnv! env e1
  let v2 := interpEnv! env e2
  .int (v1.asInt! * v2.asInt!)
| .less_than e1 e2 =>
  let v1 := interpEnv! env e1
  let v2 := interpEnv! env e2
  .bool (v1.asInt! < v2.asInt!)
| .ifThenElse cond eThen eElse =>
  let vCond := interpEnv! env cond
  if vCond.asBool! then interpEnv! env eThen else interpEnv! env eElse
-- NOTE: here the only changes
| .lambda name body => .closure name body env
| .apply funexp arg =>
  let (name, body, funenv) := (interpEnv! env funexp).asClosure
  let argval := interpEnv! env arg
  let env' := funenv.insert name argval
  interpEnv! env' body
| .var name =>
  match env.get? name with
  | some v => v
  | none => panic! s!"Unbound variable: {name}, env is {env.toList}"



def plus : Expression := (.lambda "x" (.lambda "y" (.add (.var "x") (.var "y"))))
def inc : Expression := (.apply plus (.intLiteral 1))
def prog : Expression :=  (.lambda "x" (.apply inc (.var "x")))

-- now it yields 42
#eval interpEnv! ∅ (.apply prog (.intLiteral 41))
-- and the partial application closes over the free variable `x`:
#eval interpEnv! ∅ inc

end LexicalScoping
