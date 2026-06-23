# Pressure Todos

## CI

- [ ] Implement CI with tests and build procedure.

## Language

- [x] Add support for conditionals (grammar, interpreter)
- [x] Add support for functions (grammar, interpreter)
- [x] Add support for recursion - Lima
- [x] Add support for assignment operators.
- [x] Add support for loops (grammar, interpreter) - Lima
- [ ] Add support for arrays (grammar, interpreter) - Skeete
- [ ] Add string literals.
- [ ] Add support for pointers (grammar, interpreter)
- [ ] Add support for structs (grammar, interpreter) - Samuel
- [ ] Add support for enums (grammar, interpreter)
- [ ] Add support for pattern matching (grammar, interpreter)
- [ ] Add support for imports (grammar, interpreter)

### Sugar

- [ ] Syntactic sugar for params with the same type in function definitions,
      e.g.:

  ```zig
  add :: fn(a:int,b:int) -> int { ... };

  // Is the same as
  add :: fn(a,b:int) -> int { ... };
  ```

- [ ] Sugar for integer types, e.g.:

  ```odin
  x: u32 = 42;
  x == 42; // <- should be true! But 42 has type i32 while uint has type u32.
  ```

- [ ] Syntactic sugar for `else if`. - Victor

### Std

- [ ] Implement print*.
- [ ] Implement read*.
- [ ] Implement alloc*.
- [ ] Implement free*.

## REPL

- [ ] Add support for control characters in the REPL.
