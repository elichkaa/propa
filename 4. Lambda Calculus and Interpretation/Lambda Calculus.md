### Background

- the lambda calculus is a computational model which is as powerful as the turing machine => both are turing complete (they can vizualise arbitrary big natural numbers)

### Approach in Lean

Instead of representing the data (the functions) itself, we will model them with a function, which behaves like the fold function in lean

```
def Bool.fold {α} (ftrue: α) (ffalse: α) (b: Bool): α :=

match b with

| true => ftrue

| false => ffalse

  

-- true.fold : β -> β -> β

-- false.fold : β -> β -> β
```

We will model the bool values through the signature β -> β -> β. 
So the first β represents true, the second β represents false.

```
def Bool := ∀{α}, α -> α -> α

-- now we want to define true and false from type Bool which will behave like true.fold and false.fold

def true : Bool := λ ftrue => λ ffalse => ftrue

def false : Bool := λ ftrue => λ ffalse => ffalse
```

Essentially true ignores the second arg and returns the first. False ignores the first arg and returns the second.

### Defining Functions

- When we have only one bool, we have to decide what values correspond to the original (true, false)
```
def not (b: Bool): Bool := λ ftrue ffalse => b ffalse ftrue
```
- with the not example we ask ourselves
	- What corresponds to b ftrue to achieve not? -> ffalse
	- What corresponds to b ffalse to achieve not? -> ftrue

- When we have two bools, we have to see what value of the second one corresponds to the first one to achieve the function
```
def and (b1 b2: Bool): Bool := λ ftrue ffalse => b1 (b2 ftrue ffalse) ffalse
```
- if b1 is ftrue => b2 should be ftrue as well to achieve AND => (b2 ftrue ffalse)
- if b1 is ffalse => directly ffalse the whole function no matter what b2 is

```
def or (b1 b2: Bool): Bool := λ ftrue ffalse => b1 ftrue (b2 ftrue ffalse)
```
- if b1 is ftrue => whole function is ftrue no matter what b2 is
- if b1 is false => we check b2 and if its true only then we return true

```
def xor (b1 b2: Bool): Bool := λ ftrue ffalse => b1 (b2 ffalse ftrue) (b2 ftrue ffalse)
```
- if b1 is ftrue => b2 has to be ffalse for the function to be true
- if b1 is false => b2 has to be ftrue for the function to be false