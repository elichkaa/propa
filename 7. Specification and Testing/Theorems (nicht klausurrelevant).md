- property-based testing is a powerful technique for gaining confidence in the correctness of our programs BUT it does not provide <mark style="background:#ff4d4f">absolute guarantees</mark>
- if we want to be certain that our implementation satisfies the specified properties, we need to prove them formally

```
theorem reverse_same_length : (reverse xs).length = xs.length := by
	induction xs with
		| nil => simp [reverse]
		| cons x xs ih => simp [reverse]; apply ih
```

```
theorem reverse_involution_aux : reverse (xs ++ [x]) = x :: reverse xs := by
	induction xs with
		| nil => simp [reverse]
		| cons y ys ih => simp [reverse]; rw [ih]; trivial
```

```
theorem reverse_involution : reverse (reverse xs) = xs := by
	induction xs with
		| nil => simp [reverse]
		| cons x xs ih => simp [reverse]; rw [reverse_involution_aux, ih]
```

```
theorem reverse_singleton_id : reverse [x] = [x] := by
	simp [reverse]
```

```
theorem reverse_append : reverse (xs ++ ys) = reverse ys ++ reverse xs := by
	induction xs with
		| nil => simp [reverse]
		| cons x xs ih => simp [reverse]; rw [ih]; simp
```