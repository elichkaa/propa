- how to determine the asymptotic complexity of a functional program?
### Structural recursion

- when using structural recursion, the number of recursive calls coincides with the <mark style="background:#ff4d4f">size of the input data structure </mark>
	- for a list this is n
	- for a tree this is n
- -> we obtain an asymptotic bound O(n * k), where k is the cost of work done per element
- in case of `List.len`, k is a constant (k = 1)
	- the function performs a pattern match and an addition (constant work) for each element
	- we visit each element once
```
def List.len (xs : List α) : Nat :=
	match xs with
	| [] => 0
	| _ :: xs => 1 + len xs
```

- in case of `List.app`, k = 2
	- because we have 1 work unit for the pattern match and 1 work unit for the cons operator
	- so the complexity is O(2n) = O(n)
```
def List.app (xs ys : List α) : List α :=
	match xs with
	| [] => ys
	| x :: xs' => x :: app xs' ys
```

- in case of `List.rev`, k = O(n) because of append
	- so in total we have O(n^2)
```
def List.rev (xs : List α) : List α :=
	match xs with
	| [] => []
	| x :: xs => (rev xs).app [x]
```
