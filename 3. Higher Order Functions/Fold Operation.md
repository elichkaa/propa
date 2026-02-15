**Fold generalizes structural recursion** over algebraic data types. 
### For Lists 
```
fold init combine [x₁, x₂, x₃] = combine x₁ (combine x₂ (combine x₃ init))
```

**Type**: `β → (α → β → β) → List α → β` 
- `init`: base case for empty list 
- `combine`: how to merge head with accumulated tail result
### Examples using fold: 
- Sum: `fold 0 (+)`
- Partition: `fold ([], []) (λ x (ts,fs) => if p x then ...)

## Deriving Fold Systematically 

1. List each constructor's type 
2. Replace the data type with fresh type β 
3. These become fold's parameters

 **Example - List**
```
nil: List α → fnil : β`
cons : α → List α → List α → fcons : α → β → β
Result: fold (fnil : β) (fcons : α → β → β) : List α → β
```

**Example - BinaryTree**

```
empty : BinaryTree α → fempty : β
leaf : α → BinaryTree α → fleaf : α → β
node : BinaryTree α → BinaryTree α → BinaryTree α → fnode : β → β → β
Result: fold (fempty : β) (fleaf : α → β) (fnode : β → β → β) : BinaryTree α → β
```