- slow implementation
```
def List.len (xs : List α) : Nat :=
	match xs with
	| [] => 0	
	| _ :: xs => 1 + len xs
```

- we have to use an additional parameter - <mark style="background:#ff4d4f">accumulator</mark>
	- it accumulates the length of the list that we have already processed
	- as we process further `cons` cells, we increment the accumulator to reflect the increased length
- in this way we do not store the recursive result in the call stack but in Nat
	- <mark style="background:#d3f8b6">IMPROVED SPACE COMPLEXITY</mark>
- time complexity
	- `List.lenTRAux` traverses the input list `xs` exactly once
	- for each element in the list it performs a pattern match (1 unit of work) and an addition operation to increment the accumulator (1 unit of work)
	- -> overall complexity O(n)
- however key difference is that `List.lenTRAux` is <mark style="background:#ff4d4f">tail-recursive</mark>
	- the recursive call to `lenTRAux` is the last operation performed in the function
	- when a <mark style="background:#ff4d4f">tail call</mark> occurs, the compiler can do tail-call optimization
		- a tail call is a call to another function that doesn't change anymore after this call
		- `def foo x := x + 2`
		- `def bar y := foo (y + 2)`
		- here the call to foo is a tail call because after we call foo we don't do anything anymore (bar just returns the value) -> we do not need to remember an address (here address of bar) to jump back to
		- however if `def bar y := foo (y + 2) + 3` after foo is ready we need to also run bar and add 3 to the result of foo
	- <mark style="background:#ff4d4f">tail-call optimization</mark>
		1. we do not need to keep track of where the call came from, since there are no further operations to perform after the call (tail calls do not grow the call stack, saving memory and allowing arbitrarily deep recursion without stack overflow)
		2. when a function makes a tail call to itself (recursive tail call), the compiler can reuse the caller's stack frame for the callee (<mark style="background:#d3f8b6">THE SAME STACK FRAME, WE REUSE IT</mark>) -> no more overhead of creating a new stack frame for tail calls entirely, which saves execution time
		3. when all recursive calls of a function are tail calls, we call the function tail-recursive -> we can optimize the entire function into a simple loop -> no overhead of function calls at all -> significant performance improvements
- due to this, `List.lenTR` have the same time complexity of O(n) but it benefits from  tail-call optimization -> better performance and stack usage

```
def List.lenTRAux (xs : List α) (acc : Nat) : Nat :=
	match xs with
	| [] => acc
	| _ :: xs => xs.lenTRAux (acc + 1)

def List.lenTR (xs : List α) : Nat := xs.lenTRAux 0
```