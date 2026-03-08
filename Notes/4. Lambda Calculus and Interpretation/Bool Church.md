### Definition
```
def Bool := def Bool := ∀{α}, α -> α -> α
```

- Bool is a function that takes 2 values and picks 1
	- can be String, Nat, Int etc.

### Fold Definition

```
def Bool.fold {α} (ftrue: α) (ffalse: α) (b: Bool): α :=
	match b with
		| true => ftrue
		| false => ffalse

-- true.fold : β -> β -> β
-- false.fold : β -> β -> β
```

### Constructors
```
def true : Bool := λ ftrue _ => ftrue
def false : Bool := λ _ ffalse => ffalse
```

- true picks the first, false the second one

### AND
```
def and (b1 b2: Bool) := λ ftrue ffalse => b1 (b2 ftrue ffalse) ffalse
OR
def and (b1 b2: Bool) := b1 b2 false
```

- if b1 is true, return b2 (the result depends on b2)
- if b1 is false, return false

### OR

```
def or (b1 b2: Bool) := λ ftrue ffalse => b1 ftrue (b2 ftrue ffalse)
OR
def and (b1 b2: Bool) := b1 true b2
```

- if b1 is true, return true
- if b1 is false, return b2

### NOT

```
def not (b: Bool) := λ ftrue ffalse => b ffalse ftrue
OR
def not (b: Bool) := b false true
```

- swap the branches
- give b the value false as its true branch and true as its false branch

### XOR

```
def xor (b1 b2: Bool) := λ ftrue ffalse => b1 (b2 ffalse ftrue) (b2 ftrue ffalse)
OR
def xor (b1 b2: Bool) := b1 (not b2) b2
```

- if b1 is ftrue => b2 has to be ffalse for the function to be true
- if b1 is false => b2 has to be ftrue for the function to be false