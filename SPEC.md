# Pressure Programming Language & Interpreter

## Table of Contents

<!--toc:start-->

- [Basic Syntax](#basic-syntax)
  - [Variable binding](#variable-binding)
  - [Conditionals](#conditionals)
  - [Loops](#loops)
  - [structs and tuples](#structs-and-tuples)
  - [enums](#enums)
  - [Functions](#functions)
    - [Higher order functions](#higher-order-functions)
    - [Generics](#generics)
  - [Data structures](#data-structures)

<!--toc:end-->

import MyStruct;

MyStruct.a MyStruct.b MyStruct.c

## Basic Syntax

### Variable binding

```odin
// <ident> : <optional_type> : <value> defines a constant.
age :: 1;
name :: "Lee";
result :: 10 * (20 / 2);

// <ident> : <optional_type> = <value> defines a variable.
age := 1;
age = 42;

// We can also specify the type of the variable if the interpreter cannot infer.
age : u8 : 42;
```

### Conditionals

```odin
// match and if statements can evaluate to a expression.
ok :: if name == "Lee" {true} else {false};
let val = match (result) {
    100 => ok,
    200 => { /* Block with expression in the end. */ },
    _ => ..., // Some expr.
};

// but they also can evaluate to unit type without specifying a var.
if age == 42 and ok {
    ...
} else if age == 42 or val == 100 {
    ...
} else {
    ...
}

match (...) {
    ...
}
```

### Loops

```odin
for val in iterable { ... }

while 42 > 5 { ... }

// Can evaluate to expressions. Will evaluate for 42 if break is never called
// and will evaluate for `val` is break is called.
result :: for val in iterable {
    if (...) continue;
    break val;
} else 42;

// The same thing for while loops.
```

### structs and tuples

The product of types. The structs are also used represent to represent a file
source code file.

```odin
MyStruct :: struct {
    a: i32,
    b: i32,
    c: fn(i32, i32) -> i32,

    // functions and constants in the struct namespace.
    // `pub` establishes that data is accessible by importers.
    MAGIC :: 42;

    // Everything function were the first type is
    // a pointer to the struct or the struct type itself enables the dot syntax.
    add :: fn(self: *MyStruct, other: MyStruct) -> i32 { ... }
    sub :: fn(self: *mut MyStruct, other: MyStruct) -> i32 { ... }
}

my_struct :: MyStruct { a = 42, b = 55, c = sub };
sum :: my_struct.add(myStruct)

// We can build `MyStruct` with this syntax
my_struct : MyStruct = .{ a = 42, b = 55, c = sub };

// Anonymous structs can also be built with.
let anom = .{ a = 42, b = 55, c = 32 };

// Tuples are structs without named fields.
let tuple = (myStruct.a, myStruct.b, myStruct.c);
// tuple values can be accessed with:
tuple.0; // 42
tuple.1; // 55
tuple.3; // sub

// Tuples can also be de-structured
// the `mut` keyword behind the variable establishes that her will be mutable.
a, b, c := tuple;

// We can define a tuple type.
MyTuple :: (myStruct, i32, f64);
```

### enums

The sum of types.

```odin
MyEnum :: enum {
    variant1,
    variant2: u32,
    variant3: MyStruct,
};

// The type should be pattern matched before accessed.
myEnum :: MyEnum{ variant2 = 42 };
match myEnum {
    MyEnum.variant1 => ...,
    MyEnum.variant2: val => ...,
    MyEnum.variant3: structVal => ...,
    _ => ...
};

val :: match myEnum {
    MyEnum.variant1 => ..., // Something of the type of val.
    MyEnum.variant2: val => val,
    _ => ...
};

if myEnum is MyEnum.variant2: val {
    // access to val
}

while iterable.next() is some: val {
    // access to `val`
}
```

### Functions

```odin
add :: fn(a, b) a + b;

// Inferred type
fibonacci :: fn(x) {
  if (x == 0) return 0;
  if (x == 1) return 1;
  fibonacci(x - 1) + fibonacci(x - 2)
};

fibonacci(5);

// Defined type.
// We can also specify if the variable will be mutable inside the function or not.
fibonacci :: fn(x: i32) -> i32 {
    if (x == 0) return 0;
    if (x == 1) return 1;
    fibonacci(x - 1) + fibonacci(x - 2)
};

// The `defer` keyword executes something on the end of the current block.
defer fibonacci(42);
```

#### Higher order functions

Naturally, if types are first class citizens, functions also can receive
functions.

```odin
twice :: fn(f, x) {
  f(f(x))
};

addTwo :: fn(x) {
  x + 2
};

twice(addTwo, 2); // => 6
```

#### Generics

```odin
// T can be inferred or no.
MyGenericStruct :: fn(T) type {
    struct {
        value: T,
        data1: u32,
        data2: u64,
    }
};

MyGenericEnum :: fn(T: type) enum {
    variant1: T,
    variant2,
    variant3,
};
```

### Data structures

```odin
myArray :: [1, 2, 3, 4, 5];
mySlice1 :: myArray[0..2] // [1, 2]
mySlice2 :: myArray[0..=2] // [1, 2, 3]

myArray[0]; // => 1

myArray: []T;
myArray: [][]T;
myArray: []#const []#const T;
```

### Pointers

```odin
x := 24
y : *int : &x

y.* = 42
// x is now 42

z : *#const int : &x
```
