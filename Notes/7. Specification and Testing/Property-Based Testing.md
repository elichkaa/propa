- instead of checking the properties on a few specific inputs, we can use a tool to generate many random inputs and check whether the properties hold for all of them
- this can uncover edge cases and bugs that we might not have thought of

- the `plausible` tactic from the Plausible library allows us to perform property-based testing
  
```
example (xs: List α): reverse_same_length xs :=
	by plausible

example (xs: List α): reverse_involution xs :=
	by plausible

example (x: α): reverse_singleton_id x :=
	by plausible
	
example (xs ys: List α): reverse_append xs ys :=
	by plausible
```

### [[PBT for Custom Data Types]]