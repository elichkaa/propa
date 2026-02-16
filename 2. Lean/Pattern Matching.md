### What you can match ON

- Literals: `0`, `1`, `true`, `false`, `"hello"`
- Constructors: `nil`, `cons`, `some`, `none`
- Destructuring: `(a, b)`, `x :: xs`, `Node left val right`
- Wildcards: `_` (ignore), named variables
- Nested patterns: `some (x :: xs)`, `(a, (b, c))`

### What you can return

ANY expression of the correct return type:

- Values: `5`, `true`, `"result"`
- Variables from patterns: `x`, `xs`, `a + b`
- Function calls: `f x`, `take xs n`
- Constructors: `some x`, `x :: xs`, `(a, b)`
- Conditionals: `if x > 0 then x else -x` 
	- `(. > 5)` this is short for `if x > 5 then True else False`
- Another match expression
- Lambda: `fun y => x + y`

### Conditionals

`| x :: xs => if x > 0 then x :: xs else xs`

### Match on multiple values simultaneously

```
match xs, ys with
  | [], [] => "both empty"
  | [], _ => "first empty"
  | _, [] => "second empty"
  | x::xs', y::ys' => "both non-empty"
```

### Nested matching

```
| some value => match value with 
	| 0 => "zero" 
	| _ => "nonzero"
```

Children:: [[Pattern Matching Definitions]]