### Explicit Matching

```
def Expression.foo (e : Expression) : Nat :=
  match e with
  | case1 => ...
  | case2 => ...
```

### Short Syntax

```
def Expression.foo : Expression → Nat
  | case1 => ...
  | case2 => ...
```

the biggest difference is the signature which is equivalent

```
[(e : Expression) : Nat] is the same as [Expression → Nat]
```