### Values

- all of the values our language can produce

```
inductive Value where
	| int (i: Int)
	| bool (b: Bool)
	| lambda (name: String) (body: Expression)
deriving Repr, BEq, Inhabited

instance : ToString Value where
	toString := reprStr
```

- helper functions which return the value if its correct and else error if not as expected

```
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
```

- to check if a value has free variables we are only interested in the lambda case 
- if either the name or the body contains the lambda name
```
def Value.hasFreeVar (name : String) : Value → Bool
	| .lambda x body => (Expression.lambda name body).freeVariables.contains x
	| _ => false
```

### Substitution

```
def substitute (e : Expression) (name : String) (value : Value) : Expression :=

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
```

### Interpreter

- intepreters are partial because their termination depends on the input (which may not be terminating)
- they also have ! after the name because they may crash

1. Literals evaluate to their value
2. Arithmetics and comparisons evaluate the operands and apply the operator
3. Conditionals evaluate the condition and select the branch
4. Lambdas evaluate to a value with the lambda intact
5. Lambda Application
	- we first evaluate the function expression and then the argument expression
	- the function expression must evaluate to a lambda value, so that we can use substitution to replace name by the argument value
	- NOTE: SUBOPTIMAL PERFORMANCE because substitute traverses the body two times (one with interpreter one without) even if the variable is not even contained in the body
6. Lambda Variables must be eliminated through substitution. Any left-over variable indicates that it was unbound

```
partial def interp! : Expression → Value
	| .intLiteral i => .int i
	| .boolLiteral b => .bool b
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
	| .ifThenElse c t e =>
		let vc := interp! c
		if (vc.asBool!) then
			interp! t
			else
			interp! e
	| .lambda name body => Value.lambda name body
	| .apply funexp arg =>
		let (name, body) := (interp! funexp).asLambda!
		let argval := interp! arg
		-- NOTE: SUBOPTIMAL PERFORMANCE 
		interp! (substitute body name argval)
	| .var name => panic! s!"Unbound variable {name}"
```

### Solution to Suboptimal Performance -> Environment

-  in order to solve the issue with the slow substitute we use Environment
- this is a map of variable_names to variable_values
- now we don't need to substitute the variable through traversing but we can lookup the map

```
abbrev Environment := Std.HashMap String Value
```

- just replace `interp!` with `interp! env`

```
partial def interpEnv! (env : Environment) : Expression → Value
	| .intLiteral i => .int i
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
	| .ifThenElse c t e =>
		let vc := interpEnv! env c
		if (vc.asBool!) then
			interpEnv! env t
		else
			interpEnv! env e
	| .lambda name body => Value.lambda name body
	| .apply funexp arg =>
		let (name, body) := (interpEnv! env funexp).asLambda!
		let argval := interpEnv! env arg
		let env' := env.insert name argval
		interpEnv! env' body
	| .var name =>
		match env.get? name with
		| some v => v
		| none => panic! s!"Unbound variable: {name}, env is {env.toList}"
```

- the big change is in the lambda application
- in the version before we had to substitute and therefore traverse the body 2 times
```
| .apply funexp arg =>
		let (name, body) := (interp! funexp).asLambda!
		let argval := interp! arg
		-- NOTE: SUBOPTIMAL PERFORMANCE 
		interp! (substitute body name argval)
```

- now we put the variable in the environment first `env.insert name argval` and then use the updated environment to traverse the body
```
| .apply funexp arg =>
		let (name, body) := (interpEnv! env funexp).asLambda!
		let argval := interpEnv! env arg
		let env' := env.insert name argval
		interpEnv! env' body
```

- BUT THERE IS A PROBLEM

```
def plus : Expression := (.lambda "x" (.lambda "y" (.add (.var "x") (.var "y"))))
def inc : Expression := (.apply plus (.intLiteral 1))
def prog : Expression := (.lambda "x" (.apply inc (.var "x")))

-- Should be 42, but yields 82
#eval interpEnv! ∅ (.apply prog (.intLiteral 41))
```

- after inc we have set x = 1 in the environment
- however when we run prog we override x = 41
- and we get 41 + 41 = 82 instead of 41 + 1 = 42
- because we didn't do any substitution
- we have to somehow remember that the "x" in plus was already bound to 1 and cannot be overridden

### Lexical Scoping

- we have to change our representation of lambdas

```
inductive Value where
	| int (i: Int)
	| bool (b: Bool)
	| closure (name: String) (body: Expression) (env: Std.HashMap.Raw String Value)
deriving Repr, BEq, Inhabited
```

- closure doesn't have any free variables
- the interpreter is the same except for the lambda cases

```
partial def interpEnv! (env : Environment) : Expression → Value
	...
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
```

`let env' := funenv.insert name argval` we update the local environment not the global one
- the free variables in the lambda body have to be bound at the place where the lambda is defined
- now when we check the inc we see that 1 was bound to x

```
#eval interpEnv! ∅ inc

LexicalScoping.Value.closure
  "y"
  (Expression.add (Expression.var "x") (Expression.var "y"))
  (Std.HashMap.Raw.ofList [("x", LexicalScoping.Value.int 1)])
  
#eval interpEnv! ∅ (.apply prog (.intLiteral 41)) -- 42
```

