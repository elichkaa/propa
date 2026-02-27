- unit tests (#assert)
	- can only cover a finite number of cases
	- may miss edge cases or bugs that only appear under certain conditions
- without a specification we cannot even say what "correctness" means for this function

```
def reverse (xs : List α) : List α :=
	match xs with
		| [] => []
		| x :: xs => reverse xs ++ [x]
```

-  a <mark style="background:#ff4d4f">specification</mark> describes the **intended behavior of a function** in a precise way

### State Properties Specification

-  describe the state properties that the function should satisfy

- for any list xs, length (reverse xs) = length xs
```lean
abbrev reverse_same_length (xs : List α) : Prop :=
	(reverse xs).length = xs.length
```

- for any list xs, reverse (reverse xs) = xs
```lean
abbrev reverse_involution (xs : List α) : Prop :=
	reverse (reverse xs) = xs
```

- for any value x, reverse [x] = [x]
```lean
abbrev reverse_singleton_id (x : α) : Prop :=
	reverse [x] = [x]
```

- for any lists xs and ys, reverse (xs ++ ys) = reverse ys ++ reverse xs
```lean
abbrev reverse_append (xs ys : List α) : Prop :=
	reverse (xs ++ ys) = reverse ys ++ reverse xs
```

these propositions can be evaluated as booleans

```
#eval reverse_same_length [1,2,3]
#eval reverse_same_length [4,5,6]

#eval reverse_involution ([] : List Int)
#eval reverse_involution [1,2,3]

#eval reverse_singleton_id 42
#eval reverse_singleton_id "hello"

#eval reverse_append [1,2,3] []
#eval reverse_append [] [1,2,3]
```

### Reference Properties

- we can compare our implementation to a reference implementation to check if it does the same thing
- for all inputs we have to have the same outputs

```
example (xs : List α) : reverseTR xs = reverse xs := 
	by plausible
```
