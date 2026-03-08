
- formal system for expression computation based on function abstraction and application
- it is as powerful as the Turing Machine
	- both are Turing Complete
	- they can visualize arbitrary big natural numbers


### Syntax

```
expr ::= x (variables)
		| λ x. expr (function abstraction)
		| expr expr (function application)
```

- variable references - `x`, `y`, `z` etc.
- function abstraction - `λ x. expr`
	- defines a function with parameter `x` and body `expr`
- function application `expr1 expr2` 
	- applies the function represented by `expr1` to the argument `expr2`

### [[Beta Reduction]]

### [[Church Encoding]]