```
def List.lenTRAux (xs : List α) (acc : Nat) : Nat :=
	match xs with
	| [] => acc
	| _ :: xs => xs.lenTRAux (acc + 1)

def List.lenTR (xs : List α) : Nat := xs.lenTRAux 0
```

- in`List.lenTR`, with the `acc` parameter
	- we keep track of the length of the list that has already been processed
	- by incrementing the accumulator we traverse the list and can compute the total length without needing to perform additional operations after the recursive calls
	- -> the functions turns tail-recursive

### General Structure of Accumulators

- at any given time, an accumulator-based function has seen a prefix of the input data and has built up some intermediate result in the accumulator
- it the processes the remaining input data, updating the accumulator as it goes
- `fTRAux` is the accumulator-based version of the function `f`
- the result of `fTR` must <mark style="background:#fdbfff">combine (⊕)</mark> the intermediate result `acc` with the result of the remaining input `f xs`

```lean
fTRAux xs acc = acc ⊕ f xs
```

- we can then define (init is the initial value of the accumulator)

```lean
fTR xs = fTRAux xs init
```

-  we require `fTR xs = f xs`, so that we can use the tail-recursive version as a drop-in replacement for the original function
- from these equations, we can derive

```lean
f xs = fTR xs = fTRAux xs init = init ⊕ f xs
```

- this means that `init` must be chosen such that `init ⊕ f xs = f xs`
- in other words, `init` must be the left-neutral element of the operator `⊕`

### List.lenTR Thinking Process

- init
	- we want to keep track of the length of the seen prefix
	- initially, we have not seen any elements -> `init = 0` makes sense
- ⊕
	- to combine the length of the seen prefix with the length of the remaining list, we can use addition, hence `⊕ = +`
	- Indeed, addition has `0` as its left-neutral, so this choice is consistent with our earlier observation
- helper `List.lenTRAux`
	- it has to satisfy the identity `lenTRAux xs acc = acc + len xs`
	- base case `xs = []`
		- acc + len []  (identity)
		- = acc + 0  (def of len)
		- = acc
	- recursive case `xs = x :: xs'`
		- acc + len \( x :: xs' \) (identity)
		- = acc + (1 + len xs') (def of len)
		- = (acc + 1) + len xs' (associativity of +) 
		- = lenTRAux xs' (acc + 1) (identity)
- so at the end
```
def List.lenTRAux (xs : List α) (acc : Nat) : Nat :=
	match xs with
	| [] => acc
	| _ :: xs => xs.lenTRAux (acc + 1)

def List.lenTR (xs : List α) : Nat := xs.lenTRAux 0
```
