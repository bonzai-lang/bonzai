---
title: Writing your first code
description: Start writing your first concurrent code with Bonzai, introducing the basic syntax and features.
---

When it comes to writing your first code, it's always good to have some sort of guideline for what to do. In this guide, we'll show you how to write your first code in Bonzai, introducing the basic syntax and features of the language.

## Hello, world!

As with any programming language, we'll start by writing a program that displays “Hello, world!” on the screen. This is what it would look like in Bonzai:

```bonzai
require "std:natives"

print("Hello, world!")
```

This program is very simple. It first imports the native functions so that it can use the `print` function. Then it calls the `print` function with the text `“Hello, world!”`, which displays this text on the screen.

To run this program, you can use `bpm`, which allows you to execute Bonzai scripts. To run the above program, you can create a `hello.bzi` file with the above contents, then run the following command:

```bash
bpm hello.bzi
```

## Variables

Variables are used to store values in a program. In Bonzai, variables are declared using the `let` keyword. Here's an example of a variable declaration in Bonzai:

```bonzai
let x = 42

print(x)
```

In this example, we declare a variable `x` with the value `42`, then print it on the screen. When you run this program, you should see `42` displayed on the screen.

## Functions

Functions are used to encapsulate and reuse code. In Bonzai, functions are declared using the `fn` keyword. Here's an example of a function declaration in Bonzai :

```bonzai

fn add(x: int, y: int): int => { 
  x + y
}

let result = add(3, 4)

print(result)
```

In this example, we define a function `add` which takes two parameters `x` and `y` of type `int` and returns the sum of these two parameters. We then call the `add` function with arguments `3` and `4` and store the result in a `result` variable, which we then print on the screen.
