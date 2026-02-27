- we want to validate our quicksort implementation

```
partial def List.quicksort [Ord α] (list : List α) : List α :=
	match list with
		| .nil => .nil -- Base case: an empty list is already sorted
		| pivot :: tail =>
			let left := tail.filter (λ x => Ord.compare x pivot |>.isLE)
			let right := tail.filter (λ x => Ord.compare x pivot |>.isGT)
			(quicksort left) ++ (pivot :: (quicksort right))
```

- we define our specification that following elements in the sorted list should be smaller than one another

```
abbrev List.isSorted [Ord α] [Inhabited α] (l : List α) (i j : Nat) : Prop :=
	i < j ∧ j < l.length → (Ord.compare l[i]? l[j]? |>.isLE)
```

- then we test with plausible

```
example (xs : List Int) (i j : Nat) : xs.quicksort.isSorted i j := 
	by plausible
```

- however plausible generates a lot of test cases where j < i which are invalid and our specification doesn't get triggered
-> we have to write our own generator

- Bounded is a wrapper type that indicates we want numbers between `lower` and `upper`
```
structure Bounded (lower upper : Nat) where
	num : Nat
	deriving Repr
```

```
open Plausible

instance : Arbitrary (Bounded lower upper) where
	arbitrary := do
	let size ← Gen.getSize -- the target size
	if h: size == 0 || (upper - lower) <= 0
	then Except.error (GenError.genError "impossible to generate value")
	else Bounded.mk <$> Gen.chooseNatLt lower upper (by grind)

instance : Shrinkable (Bounded lower upper) where
	#sample Bounded 0 0
	#sample Bounded 0 1
	#sample Bounded 0 2
	#sample Bounded 0 4
	#sample Bounded 0 8
	#sample Bounded 1 2
	#sample Bounded 2 2
	#sample Bounded 2 4
	#sample Bounded 2 8
	#sample Bounded 4 1
	#sample Bounded 4 2
	#sample Bounded 4 4
	#sample Bounded 4 8
```

- then we can check that the test cases we generate are valid

```
set_option trace.plausible.success true

example (l : List Int) (i : Bounded 0 (l.length - 1)) (j : Bounded (i.num + 1) l.length) :
	l.quicksort.isSorted i.num j.num := by
	plausible
```