Instead of representing the data (the functions) itself, we will model them with a function, which behaves like the fold function in lean.

<mark style="background:#ff4d4f">FOLD IS ALSO A VALUE</mark>
### [[Bool Church]]

- `Bool` — holds a two-way choice, you supply the branches
- fold [ftrue, ffalse] 
	- 2 constructors: true, false
	- each takes no extra args
### [[Pair Church]]

- `Pair` — holds two values, you supply the handler
- fold [f] 
	- 1 constructor: pair a b
	- f receives both values a and b
### [[Nat Church]]

- `Nat` — holds a count, you supply the base and the step
- fold [fzero, fsucc] 
	- 2 constructors: zero, succ 
	- fzero takes no args
	- fsucc takes the accumulated result so far