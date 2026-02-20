### Definition

```
Nat := ∀{α}, α -> (α -> α) -> α
```

- a natural number in normal programming is just a value like `3`
-  in Church encoding a number _is_ the act of applying a function `n` times

### Constructors

```
zero = λ z s => z -- apply s 0 times 
one = λ z s => s z -- apply s 1 time 
two = λ z s => s (s z) -- apply s 2 times 
three = λ z s => s (s (s z))-- apply s 3 times
```

- The number `n` literally means "do this step `n` times starting from this base"
-  So `z` is where you start, `s` is what you do each step

```
def zero : Nat := λ fzero fsucc => fzero
def succ (n : Nat) : Nat := λ fzero fsucc => fsucc (n fzero fsucc)
OR
def zero => λ z s => z
def succ n = λ z s => s (n z s)
```
### Addition

```
def plus (m n: Nat): Nat := λ fzero fsucc => 
	m (n fzero fsucc) (fun x => fsucc x)
```

- WE RETURN A NEW NAT
	- `m` is the fold
		- says "apply step `m` times to base"
		- so it applies `fsucc` exactly `m` more times to whatever `n` produce
	- `n fzero fsucc` is the base
		- evaluate `n` completely
		- gives you the result of applying `fsucc` n times to `fzero`
	- `(fun x => fsucc x)` is the step over `x` (accumulated result so far, which at the start is just `n` THE BASE)
		-  use `n fzero fsucc` as the _base_ for `m`, and apply `fsucc` m more times
- You're chaining the two folds 
	- first run `n`
	- then run `m` starting where `n` left off

```
def plus (m n: Nat): Nat := m n succ
```

- `m + n` means (recursively)
	- if `m = 0` → return `n`
	- if `m > 0` → return `succ (plus (pred m) n)`
- `m` takes a base and a step
	- base is `n` (zero additions = just `n`)
	- step is `succ` (each layer of `m` adds one more `succ`)
-  So `m n succ` applies `succ` exactly `m` times to `n`

### Multiplication

```
def mul (m n : Nat) : Nat := λ fzero fsucc =>
	m fzero (fun x => n x fsucc)
```

- WE RETURN A NEW NAT
	- `m` is the fold
		- says "apply step `m` times to base"
	- `fzero` is the base
		- because when m == 0 then we get 0
	- `(fun x => n x fsucc)` is the step over `x` (accumulated result so far, which at the start is just `0` THE BASE)
		-  we then apply (+1) n times to the current accumulated result

```
example
mul 0 3 => 0 (base case)
mul 1 3:
	0. x = 0
	1. n 0 (+1) = 3
	2. result = 3
mul 2 3:
	3. x = 0
	4. n 0 (+1) = 3
	5. n 3 (+1) = 6
	6. result = 6
```

### Power

```
def pow (m n : Nat) : Nat := λ fzero fsucc =>
	n fsucc (fun g x => m x g) fzero
```

- WE RETURN A NEW NAT
	- `n` is the fold
		- says "apply step `n` times to base"
	- `fsucc` is the base
		- because we always do it once (n^0 = 1)
	- `(fun g x => m x g)` is the step over `g` (accumulated result so far, which at the start is just `fsucc = 1` THE BASE

```
pow 3 0 => 1 (base case, n=0, g=fsucc applied to 0 = 1)

pow 3 1:
    0. g = (+1)                -- adds 1
    1. g = 3 _ (+1) = (+3)    -- apply (+1) three times = adds 3
    g(0) = 3
    result = 3

pow 3 2:
    0. g = (+1)                -- adds 1
    1. g = 3 _ (+1) = (+3)    -- apply (+1) three times = adds 3
    2. g = 3 _ (+3) = (+9)    -- apply (+3) three times = adds 9
    g(0) = 9
    result = 9

pow 3 3:
    0. g = (+1)                -- adds 1
    1. g = 3 _ (+1) = (+3)    -- apply (+1) three times = adds 3
    2. g = 3 _ (+3) = (+9)    -- apply (+3) three times = adds 9
    3. g = 3 _ (+9) = (+27)   -- apply (+9) three times = adds 27
    g(0) = 27
    result = 27
```

- In mul, 3 x (+1) means "add 3 to the **number** x".  
- In pow, 3 x g means "apply the **function** g to x, 3 times" — which composes g with itself 3 times, effectively multiplying what g represents by 3

![[recursion levels church.png]]