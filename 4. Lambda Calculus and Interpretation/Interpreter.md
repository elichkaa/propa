When we have a program saved in a source file, we can compile it. At the end of compilation we again have a source file. How do we run the program now?

Compiling is a multi-step procedure which depends on the PC architecture. For example Lean compiles to C then to sth else etc.

With interpreters, we can stop compiling at an arbitrary step and directly interpret (DIRECTLY RUNNING THE CODE THAT WE HAVE). 

In Java we usually compile before we interpret, because the compiler generates Java Bytecode. JVM interprets the byte code and delivers result. 

We want to interrupt compilation in order to ==improve compile time==. 

Another use case is the pedagogical - we use interpreters to understand the definition of the structures in the language better. 

- this is an abstract syntax of a functional programming language that we want to interpret
```

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
```

- our goal is to write a function that takes this Expression and returns a result
```
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
```

### [[Interpreter for Lambda Calculus]]

