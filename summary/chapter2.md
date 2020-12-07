# Chapter 2
Getting started with functional programming in Scala

## Summary
- Functions are values, they can be stored in data structures, passed as arguments and returned from function calls.

- Higher order functions are functions that take other functions as arguments.

- @tailrec will cause a compile error if the tail call in a recursive function cannot be eliminated.

- Anonymous functions are syntactic sugar creating an object with an apply method:
 ```scala
val lessThan = (a: Int, b: Int) => a < b 
//is the same as
val lessThanExpanded = new Function2[Int, Int, Boolean] {
  def apply(a: Int, b: Int): Boolean = a < b
} 

 ```
- compose is defined on Function1

## Discussion points
