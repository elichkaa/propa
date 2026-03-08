- <font color="#2DC26B">curly braces</font> make Lean **infer** the argument automatically

![[implicit explicit arguments.png]]
### Explicitly providing implicit args
-  the first line causes an error because Lean can't determine the expressions type
- can't find neither implicit nor explicit type argument to List.nil
- so we have to provide the type explicitly
- `#eval [].head? (α := Int)` - named syntax
- `#eval ([] : List Int).head?` - type annotation
![[implicit types defined explicitly.png]]