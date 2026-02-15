**Lambda = expression that creates a function value** 
### Syntax variants (all equivalent)
- `fun x => x + 1` 
- `λ x => x + 1`
- In `let`: `let f x := x + 1` 

### Key property: Lambdas are first-class values 

Can be:
- Assigned to variables
- Passed as arguments
-  Returned from functions 
- Stored in data structures

### Use cases: 
- Local helper functions 
- Closures (capture surrounding variables) 
- Avoiding pollution of global namespace 
- **Example**
```
let read := fun a => memory[base + wordSize * a]!
```
-  captures `memory`, `base`, `wordSize` from surrounding scope.