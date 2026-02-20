- the core operation in lambda calculus is function application
	- defined by the process of <mark style="background:#ff4d4f">substitution</mark>
- when a function is applied to an argument the parameter in the function's body is replaced with the argument expression => beta reduction

```
(λx. expr) arg --> expr[x := arg]
```

- Beta reduction states that 
	- applying a lambda function to an argument
	- results in the body of the function to be executed after substituting the
	- argument for the parameter
- here, `expr[x := arg]` denotes the substitution of `arg` for all free occurrences of `x` in `expr`

### Substitution Cases

1. expr == variable
	- `y[x := s]`
	- if `y == x` then return s
	- else return y unchanged
	```
	(λx. x) s  →  x[x := s]  →  s (MATCH)
	(λx. y) s  →  y[x := s]  →  y (NO MATCH)
	```
2. expr == application
	- `(f a)[x := s]`
	- we do simple recursion
	- substitute in both f and a
	```
	(λx. f x) s  →  (f x)[x := s]  →  (f[x:=s]) (x[x:=s])  →  f s
	(λx. x x) s  →  (x x)[x := s]  →  s s
	```
3. expr == lambda
	- `(λy. body)[x := s]`
	- if `y == x` then lambda <mark style="background:#ff4d4f">rebinds</mark> x, so x is no longer free inside => return unchanged
		- `(λx. λx. x) s  →  (λx. x)[x := s]  →  λx. x`
	- if y is not free in s -> we can recurse into body
		- `(λx. λy. x) s  →  (λy. x)[x := s]  →  λy. (x[x := s])  →  λy. s`
	- if y is free in s -> variable capture
		- `(λy. body)[x := y]` returns `(λy. y)` (WRONG - identity function), the y got captured
			- `(λx. λy. x) y  →  (λy. x)[x := y]  →  λy. y   ← WRONG!`
		- so you rename y to a fresh name first then substitute
			- `(λy. x)  →  rename y to z  →  (λz. x)`
			- `(λz. x)[x := y]  →  λz. y   ← CORRECT ✅`

partial def Expr.subst (exp : Expr) (name : String) (replacement : Expr) : Expr := match exp with
	| var vn => if vn == name then replacement else var vn
	| lambda ln lb =>
		if ln == name then lambda ln lb
		else if ln ∈ freeVariables replacement then
			let ln' := generateFreshName ln (freeVariables replacement)
			let lb' := lb.subst ln (var ln')
			lambda ln' (lb'.subst name replacement)
		else
			lambda ln (lb.subst name replacement)
	| apply func arg => apply (func.subst name replacement) (arg.subst name replacement)
``

- further reading - Chapter 5 "The Untyped Lambda-Calculus" of "Types and Programming Languages" by Benjamin C. Pierce

![[substitution cases.png]]