### Definition

```
def Pair (α β: Type) := ∀ {γ : Type}, (α -> β -> γ) -> γ
```

- a pair doesn't store values, it's a function that hands both values to whoever asks
- The type `∀{γ}, (α -> β -> γ) -> γ` says: "I can produce any type `γ` you want, as long as you tell me how to turn an `α` and a `β` into a `γ`." 
- a pair holds the two values and you decide what to do with them

```
def pair {α β: Type} (a: α) (b: β) : Pair α β := λ f => f a b
```

- A pair of `(1, 2)` in normal programming stores two values
- In Church encoding it's instead a function that _holds_ two values and will give them to you when you ask
- You can think of it like a closed box
	-  The box has `1` and `2` inside
	- To get anything out you pass a function `f` and the box calls `f 1 2`

```
pair 1 2 = λ f => f 1 2
pair 1 2 (λ a b => a) = 1 -- give me first 
pair 1 2 (λ a b => b) = 2 -- give me second 
pair 1 2 (λ a b => a + b) = 3 -- give me sum
```

### Constructors

```
def fst {α β} (p: Pair α β): α := p (λ a _ => a)
def snd {α β} (p: Pair α β): β := p (λ _ b => b)
```

- fst = open the box, take first, ignore second 
- snd = open the box, ignore first, take second
