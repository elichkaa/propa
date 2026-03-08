When referring to constructors of an inductive type, we can use a leading dot to avoid repeating the type name. 

For example, instead of writing `Value.int 42`, we can write `.int 42` when the type is clear from context. This avoids having to `Open` the namespace, which can lead to name clashes.

```
inductive Expression where
| intLiteral (n : Int)
| boolLiteral (b : Bool)
| add (e1 e2 : Expression)
```

```
def Expression.freeVariables : Expression → Std.HashSet String
| .intLiteral _ => {}
| .boolLiteral _ => {}
| .add e1 e2 => e1.freeVariables ∪ e2.freeVariables
```
