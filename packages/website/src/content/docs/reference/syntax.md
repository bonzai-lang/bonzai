---
title: Syntax reference
description: Learn about the syntax of Bonzai, including expressions, statements, and declarations.
---

Bonzai Syntax is designed to be simple and yet expressive. It is inspired by languages like Python and JavaScript, but with its own unique features. This document provides an overview of the syntax used in Bonzai.

## Syntax overall

```haskell
data Expression f t
  = MkExprLiteral Literal
  | MkExprVariable (Annotation (f t))
  | MkExprApplication (Expression f t) [Expression f t]
  | MkExprLambda [Annotation (f t)] (f t) (Expression f t)
  | MkExprTernary (Expression f t) (Expression f t) (Expression f t)
  | MkExprUpdate (Update f t) (Expression f t)
  | MkExprLet (Set Text) (Either (Annotation (f t)) (Pattern f t)) (Expression f t) (Expression f t)
  | MkExprMut (Expression f t)
  | MkExprBlock [Expression f t]
  | MkExprRequire Text (Set Text)
  | MkExprLoc (Expression f t) Position
  | MkExprList [Expression f t]
  | MkExprNative (Annotation [Text]) Ty.Type
  | MkExprWhile (Expression f t) (Expression f t)
  | MkExprIndex (Expression f t) (Expression f t)
  | MkExprData [DataConstructor t]
  | MkExprMatch (Expression f t) [(Pattern f t, Expression f t, Maybe Position)]
  | MkExprPublic (Expression f t)
  | MkExprRecordExtension (Expression f t) Text Bool (Expression f t)
  | MkExprRecordEmpty
  | MkExprRecordAccess (Expression f t) Text
  | MkExprSingleIf (Expression f t) (Expression f t)
  | MkExprBreak
  | MkExprContinue
  | MkExprReturn (Expression f t)
  | MkExprSpawn (Expression f t)
  deriving (Generic)
```

In Bonzai, expressions and statements are indistinguishable in syntax. However, they are semantically different. An expression is a piece of code that evaluates to a value, while a statement is a piece of code that performs an action. In the parser, distinction is made between expressions and statements based on their context.

## Expressions

This section describes the different types of expressions in Bonzai.

### `MkExprLiteral` case

This expression represents a literal value, such as a number, string, or boolean. It is defined as follows:

```haskell
data Literal
  = MkLitInt Integer
  | MkLitFloat Double
  | MkLitChar Char
  | MkLitString Text
  | MkLitBool Bool
  deriving (Eq, Ord, Show, Generic)
```

This parses literals like `42`, `3.14`, `'a'`, and `true`. Note that strings are not handled as literals in the same way as other types because strings may contain expressions between quotes, such as `"Hello, $name!"`. In this case, the string is parsed as an expression that evaluates to a string.


### `MkExprVariable` case
This expression represents a variable, which is defined as an annotation of a type. It is used to refer to a value that has been defined elsewhere in the code. The variable can be annotated with additional information, such as its type or documentation.

```haskell
data Annotation a = MkAnnotation {
  name :: Text,
  value :: a
} deriving (Ord, Show, Generic)
```

A variable is parsed as an identifier, such as `x`, `myVariable`, or `someFunction`. It may includes underscores, but cannot start with a number. It may also includes `::`, used to separate the variable name from its context. Note that this is only a syntactic distinction, and the variable is still treated as an expression.

### `MkExprApplication` case

Application expressions are used to apply a function to zero or more arguments. The function is represented as an expression, and the arguments are a list of expressions. 

During parsing, the given function arguments are separated into two categories: positional and named arguments. Positional arguments are passed in the order they appear, while named arguments are passed with a name and a value.

Leading to the following possibilities:

```bonzai
f(x, y, z)
```

```bonzai
f(x, y, z)
```

```bonzai
f(x, y, z: 42)
```

```bonzai
f(x, y, z: 42, a: "hello")
```

### `MkExprLambda` case

Lambda expressions are used to define anonymous functions. They consist of a list of parameters, a return type, and an expression that defines the function body. The parameters can be annotated with types or other information.

```bonzai
f = fn(x: int, y: int): int => x + y
```

Lambda arguments may contain patterns, which allows for destructuring of complex data types. For example:

```bonzai
f = fn((x, y)): int => x + y
```
This defines a function `f` that takes a tuple of two values and returns their sum. Patterns can also be used to match against specific data structures, such as lists or records.

### `MkExprTernary` case
Ternary expressions are used to evaluate a condition and return one of two values based on the result. They are defined with a condition, a true branch, and a false branch.

```bonzai
x = if condition then trueValue else falseValue
```

### `MkExprUpdate` case
Update expressions are used to modify a value in place. They consist of an update operation and an expression that defines the value to be updated. The update operation can be a simple assignment or a more complex operation, such as incrementing a value.

```bonzai
x = x.value + 1
```

### `MkExprLet` case
Let expressions are used to define local variables within a scope. They consist of a set of variable names, an optional pattern, an expression that defines the value, and the body of the let expression.

```bonzai
let x = 42 in
let y = x + 1 in
y * 2
```
This defines two local variables `x` and `y`, and returns the value of `y * 2`. The pattern can be used to destructure complex data types, such as tuples or records.

Let expression can also be used without any body, in which case it is used to define a variable without immediately using it:

```bonzai
let x = 42
```

### `MkExprMut` case
Mut expressions are used to notify the compiler that a value is mutable. 

```bonzai
let x = mut 42
```

This indicates that the value of `x` can be changed later in the code. You may also use `mut` as a let-in expression:

```bonzai
mut x = 42 in
x.value + 1
```

### `MkExprBlock` case

Block expressions are used to group multiple expressions together. They are defined as a list of expressions, and the last expression in the block is the value returned by the block.

```bonzai
{
  let x = 42
  let y = x + 1
  y * 2
}
```

This defines a block that contains two local variables `x` and `y`, and returns the value of `y * 2`. Blocks can be used to create a scope for local variables, or to group multiple expressions together.

### `MkExprRequire` case

Require expressions are used to import modules or libraries into the current scope. They consist of a module name and an optional set of imports.

```bonzai
require "my_module": 
  myFunction,
  myVariable
```

This imports the `myFunction` and `myVariable` from the `my_module` module into the current scope. The module name is a string, and the imports are a set of identifiers that can be used in the current scope.

The set of imports can be empty, in which case the entire module is imported:

```bonzai
require "my_module"
```

### `MkExprLoc` case
Loc expressions are used to annotate an expression with its position in the source code. This is useful for debugging and error reporting. The position is represented as a `Position` type, which contains information about the line and column numbers.

### `MkExprList` case

List expressions are used to define a list of values. They are defined as a list of expressions, and the values in the list can be of any type.

```bonzai
[1, 2, 3, 4]
```
Lists can also contain expressions, which are evaluated to produce the values in the list:

```bonzai
[x + 1, y * 2, z]
```

### `MkExprNative` case

Native expressions are used to define native functions or types that are implemented in a different language, such as C or Rust. They consist of an annotation and a type.

```bonzai
extern fn myNativeFunction(x: int): int
```

This defines a native function `myNativeFunction` that takes an integer argument and returns an integer. The annotation can be used to provide additional information about the native function, such as its name or documentation.

Native expressions can also have generic parameters, which are defined as a list of type annotations:

```bonzai
extern fn myGenericFunction<T>(x: T): T
```

### `MkExprWhile` case

While expressions are used to define a loop that continues until a condition is no longer true. They consist of a condition and a body that is executed repeatedly as long as the condition is true.

```bonzai
while condition {
  // body of the loop
}
```

This defines a loop that continues until the `condition` is no longer true. The body of the loop can contain any expressions. 

As for now, there is no support for checking for return values in the loop, so the while-loop may be used in combination with another value.

```bonzai
let result = {
  while condition {
    // body of the loop
    x + 1
  }

  x + 2
}
```

### `MkExprIndex` case
Index expressions are used to access elements in a collection, such as a list or a map. They consist of an expression that represents the collection and an expression that represents the index.

```bonzai
myList[0]
```
This accesses the first element of the `myList` collection. The index can be any expression that evaluates to an integer, and the collection can be any expression that represents a collection type.

### `MkExprData` case
Data expressions are used to define data types and their constructors. They consist of a list of data constructors, each of which defines a data type and its associated values.

```bonzai
type {
  User(
    id: int,
    name: string,
    age: int
  )
}
```

This defines an anonymous data type with a single constructor `User`, which has three fields: `id`, `name`, and `age`. The data type can be used to create values of this type, and the fields can be accessed using dot notation.

Note that you can use shorter syntax for data types that have only one field:

```bonzai
type User(id: int, name: string, age: int)
```

And you can also define named data types, which are defined as a type with a name:

```bonzai
type Optional<A> {
  Some(value: A)
  None
}
```

### `MkExprMatch` case

Match expressions are used to pattern match against values. They consist of an expression to match against, a list of patterns and their associated expressions, and an optional position for error reporting.

```bonzai
match myValue {
  case User(id, name, age) => {
    // body of the match
  }
  case _ => {
    // default case
  }
}
```

This matches the `myValue` expression against the `User` data type, and executes the body of the match if the value matches the pattern. The `_` pattern is used as a default case that matches any value that does not match any of the other patterns.

### `MkExprPublic` case

Public expressions are used to mark an expression as public, which means that it can be accessed from outside the current module. This is useful for defining public functions or variables that can be used by other modules.

```bonzai
pub fn myPublicFunction(x: int): int {
  return x + 1
}
```

This defines a public function `myPublicFunction` that takes an integer argument and returns an integer. The `pub` keyword is used to mark the function as public, and it can be accessed from other modules.

### `MkExprRecordExtension` case

Record extension expressions are used to extend an existing record with additional fields. They consist of an expression that represents the existing record, a field name, a boolean indicating whether the field is optional, and an expression that defines the value of the new field.

```bonzai
let myRecord = {
  id: 1,
  name: "John Doe"
}
```

In the following example, there is no record at all at the beginning, so the record is created from scratch:

```hs
MkExprRecordExtension
  (MkExprRecordExtension
    MkExprRecordEmpty
    "id"
    False
    (MkExprLiteral (MkLitInt 1))
  )
  "name"
  False
  (MkExprLiteral (MkLitString "John Doe"))
```

This creates a new record with the fields `id` and `name`, and their corresponding values. The `MkExprRecordEmpty` expression is used to create an empty record, which can then be extended with additional fields.

### `MkExprRecordEmpty` case

Record empty expressions are used to create an empty record. They are defined as a single expression with no fields.

```bonzai
let myRecord = {}
```
This creates an empty record that can be extended with additional fields using the `MkExprRecordExtension` expression.

### `MkExprRecordAccess` case

Record access expressions are used to access fields in a record. They consist of an expression that represents the record and a field name.

```bonzai
let myRecord = {
  id: 1,
  name: "John Doe"
}
let myName = myRecord->name
```

This accesses the `name` field of the `myRecord` record and assigns its value to the `myName` variable. The `->` operator is used to access fields in a record.

### `MkExprSingleIf` case

Single if expressions are used to evaluate a condition and return a value if the condition is true. They consist of a condition and an expression that defines the value to be returned.

```bonzai
if condition then trueValue
```

This evaluates the `condition`, and if it is true, returns the `trueValue`. If the condition is false, the expression does not return anything, which is different from a ternary expression that always returns a value.

### `MkExprBreak` case
Break expressions are used to exit a loop prematurely. They do not take any arguments and simply break out of the nearest enclosing loop.

```bonzai
while true {
  if condition {
    break
  }
  // other code
}
```
This breaks out of the loop when the `condition` is true, allowing the program to continue executing after the loop.

### `MkExprContinue` case

Continue expressions are used to skip the current iteration of a loop and continue with the next iteration. They do not take any arguments and simply continue to the next iteration of the nearest enclosing loop.

```bonzai
while true {
  if condition {
    continue
  }
  // other code
}
```

This skips the rest of the code in the loop when the `condition` is true, allowing the program to continue with the next iteration of the loop.

### `MkExprReturn` case

Return expressions are used to return a value from a function or a block. They consist of an expression that defines the value to be returned.

```bonzai
fn myFunction(x: int): int {
  if x > 0 {
    return x * 2
  }
  return 0
}
```

This defines a function `myFunction` that takes an integer argument `x` and returns an integer. The `return` keyword is used to return a value from the function, and the function can return different values based on the condition.

### `MkExprSpawn` case

Spawn expressions are used to create a new thread or process that runs concurrently with the main program. They consist of an expression that defines the code to be executed in the new thread.

```bonzai
spawn {
  // code to be executed in the new thread
  let x = 42
  println(x)
}
```

This creates a new thread that executes the code inside the `spawn` block concurrently with the main program. The code inside the `spawn` block can access variables and functions defined in the main program, but it runs independently.

## Summary

Bonzai syntax is designed to be simple and expressive, allowing developers to write code that is easy to read and understand. The syntax includes a variety of expressions for defining variables, functions, data types, and control flow constructs. By using these expressions, developers can create complex programs that are both efficient and maintainable.
