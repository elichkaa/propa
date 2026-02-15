Functions that 
- take other functions as arguments
-  return functions as results

Advantages
- powerful abstractions
- code reusability

### map
- Applies a function to each element 
- Pattern: `(α → β) → List α → List β` 
- Example: `List.map String.hash ["a", "b"]` → list of hashes

### filter 
- Keeps only elements satisfying a predicate 
- Pattern: `(α → Bool) → List α → List α` 
- Example: `List.filter (·% 2 == 0) [1,2,3,4]` → `[2,4]`

### zipWith
- Combines two lists element-wise using a function 
- Pattern: `(α → β → γ) → List α → List β → List γ`
- Example: `zipWith (+) [1,2] [3,4]` → `[4,6]`

Children :: [[Fold Operation]], [[Lambda Functions]]