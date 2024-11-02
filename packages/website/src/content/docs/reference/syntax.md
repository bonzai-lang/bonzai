---
title: Bonzai Syntax
description: Dive into the Bonzai language syntax and its design.
---

Bonzai is a simple, yet powerful language that is designed to be easy to read and write. It is designed to be a language that is easy to learn and use, while still being powerful enough to handle complex tasks. This document will provide an overview of the Bonzai language syntax and its design.

## Literals

Bonzai has several types of literals, including strings, numbers, and booleans. Strings are enclosed in double quotes, numbers are written as decimal numbers, and booleans are written as either `true` or `false`.

```bonzai
"Hello, world!"
42
true
```

## Variables

Variables denote many things in Bonzai, such as variable identifiers, constant definitions, live variables, and mutable variables. 

### Variable identifiers

Variable identifiers are used to represent a value behind a name. They follow some naming rules to be valid:

- They must start with a letter or an underscore.
- They can contain letters, numbers, and underscores.
- They are case-sensitive.

```bonzai
let x = 42
let y = "Hello, world!"
let _z = true
```

### Constant definitions

Constant definitions are used to define a value that cannot be changed, once it has been assigned. They are declared using the `let` keyword.

```bonzai
let pi = 3.14159
```

### Live variables

Live variables are used to define a value that can be updated and that will update all references to it. They are declared using the `live` keyword.

```bonzai
live x = 42
let y = x + 1 // y = 43
x = 43
// y = 44
```

### Mutable variables

Mutable variables are used to define a value that can be changed after it has been assigned. They are declared using the `mut` keyword.

```bonzai
mut x = 42
x = 43
```

## Functions and anonymous functions

Functions are used to define a block of code that can be called multiple times with different arguments. They are declared using the `fn` keyword.

```bonzai
fn add(x, y) => {
  x + y
}
```

In Bonzai, there is no return statement. The last expression in a function is automatically returned.

Anonymous functions are functions that are defined without a name. They are declared using the `fn` keyword followed by the arguments and the function body.

```bonzai
fn(x, y) => {
  x + y
}
```

## Control structures

Bonzai has several control structures, including `if`, `else`, `while`, and `match`. 

### If statements

```bonzai
if x > 0 then {
  "Positive"
} else {
  "Negative"
}
```

```bonzai
if x > 0 then "Positive" else "Negative"
```

### While loops

```bonzai
mut i = 0
while i.value < 10 {
  i = i.value + 1
}
```

### Match expressions

```bonzai
let x = [1, 2, 3]

match x {
  case [x, y] => x + y
  case [x, y, z] => x + y + z
  case _ => 0
}
```

## Actors and interfaces

Actors are used to define objects that can communicate with each other. They are declared using the `actor` keyword. Each actor **must** implement an interface that defines the types of the events it can receive.

```bonzai
interface Counter {
  increment()
  decrement()
}

fn Counter() => {
  mut count = 0

  actor < Counter {
    on increment() => {
      count = count.value + 1
    }
    on decrement() => {
      count = count.value - 1
    }
  }
}
```

## Module imports

```bonzai
require "std:math"
require "../path/to/module"
```

## Extern functions

Extern functions are used to define functions that are implemented in another language. They are declared using the `extern` keyword.

```bonzai
extern fn add(x: int, y: int): int
```

## Types

Bonzai supports custom datatype definitions using the `type` keyword.

```bonzai
type List<T> {
  Nil
  Cons(T, List<T>)
}

let list = Cons(1, Cons(2, Cons(3, Nil)))

type unit { unit }
```