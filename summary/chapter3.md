# Chapter 3
Functional Data Structures

## Summary
- Functional data structures are by definition immutable. That doesn't mean we have to do a lot of extra copying when using them.
- Nil is a subtype of any List. Nil extends List[Nothing] and Nothing is a subtype of every type in Scala. The type parameter of List[+A] is declared as covariant thus List[Nothing] is a subtype of every List type.
- Variadic function is a function that accepts zero or more arguments of type A.
- Functional data structures are persistent, meaning that existing references are never changed by operations on the data structure.
- To enable type inference for higher order function anonymous function arguments, we move the function parameters to another parameter list. Type information in functions with multiple parameter lists flows from left to right.
- An ADT is just a data type defined by one or more data constructors, each of which may contain zero or more arguments.



## Discussion points
- Are there data structures that are impossible to implement in a functional way?
- ADTs and encapsulation


