### Prod (×)

- `String × Int` = `Prod String Int`
- `Nat × Bool × Char` = `Prod Nat (Prod Bool Char)`
![[prod type.png]]
![[swap function.png]]
### Sum (⊕)
![[sum type definition.png]]
- PetName is a collection for BOTH dog and cat names
	- def PetName := String ⊕ String -- dog name OR cat name
	- Sum.inl "Spot" -- this is a DOG name (left side) 
	- Sum.inr "Tiger" -- this is a CAT name (right side)
![[sum type inl inr.png]]
![[sum type inl inr ex2.png]]

### Unit
- it's a type level placeholder
- we put it when we don't expect a specific type
- can be used a placeholder for in generic types
![[unit definition.png]]

### Option
![[option def.png]]