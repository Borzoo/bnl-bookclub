#Chapter 1
What is functional programming?
## Summary
- Functional programming is based on a simple premise. We use pure functions to construct programs.

- Pure functions are functions that have no side effect.

- Pure functions are easier to reason about.

- We call functions that have side effects procedures, functions have no side effects.

- If we have to have side effects, it's best to make them *unobservable* by other parts of the code. e.g. writing to a local variable or writing to a file as long as no other enclosing function can *observe* it.

- Referential transparency: An expression is RT if it can be replaced by its evaluated value in a program and not change the meaning of the program. A function is pure if calling it with RT parameters is RT.

- We can use substitution model with (pure) functions. We can replace expressions with their values and vice versa. This makes it easier to reason about the code because we don't need to keep track of the state of the program. This is called local reasoning.

- A function can be treated as a black box. This results in more composable components and easier reuse. The functional programs are more modular in the sense that the meaning of the whole depends on the meaning of the constituent components and rules of composition. Not any (observable) state.   

## Discussion points
- Functional programing makes testing, reuse and parallelization simpler. Compared to what and why?
- What are the downsides?
- The example is kind of useless. It erases the question instead of answering it.
