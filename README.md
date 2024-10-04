[![GitHub Issues or Pull Requests](https://img.shields.io/github/issues/thomasvergne/bonzai?style=for-the-badge)](https://github.com/thomasvergne/bonzai/issues)
![GitHub License](https://img.shields.io/github/license/thomasvergne/bonzai?style=for-the-badge)
![Bonzai](assets/banner.png)

## Introduction

Bonzai is a programming language that relies on [Actor model](https://en.wikipedia.org/wiki/Actor_model) and on a strong and non-taulerant typechecker to guarantee types and computations in your code. It compiles down to a custom bytecode with relatively good performance.

## Table of Contents

1. [Features](#features)
2. [Example Code](#example-code)
3. [Building Bonzai](#building-bonzai)
4. [Contributing](/CONTRIBUTING.md)
5. [Reporting Issues](#reporting-issues)

---

## Features

- **Strong typechecker**: Throw errors for incompatible types, to ensure security when running your code.
- **Actors as expressions**: Make use of first-class actors to express every code you want to.
- **Bytecode compilation**: Enable multi-platform code-running.

## Example Code

A factorial example :

```lisp
(interface Factorial []
  (defn factorial [(self Factorial) (n int)])  
)

(defn Factorial [] {
  (mut acc 1)

  (actor (implements Factorial)
    (on factorial [self n] 
      (if (== n 0) {
        (print acc)
      } {
        (set acc (* (value acc) n))
        (send self factorial self (- n 1))
      })
    )
  )
})

(let f (spawn (Factorial)))
(send f factorial f 5)
```

Here, let's decompose the code into steps:
- First we declare a function `Factorial` that defines a mutable variable corresponding to an accumulator
- Then, we return an anonymous actor implementing interface Factorial that takes two arguments: itself (anonymous Factorial actor), and an integer `n`.
- For each `n` received, we compare it to zero:
  - If `n` is equal to zero, then we print the value (because an event **never** returns a value)
  - Else we update the accumulator and send another event to the actor itself (used to loop recursively over `n`).
- Finally, we spawn Factorial and store spawning result (the actor value itself) into the variable `f` (spawning create a loop in another thread).
- We send at the end the event to print factorial of 5.

## Building Bonzai

### Prerequisites
- **Haskell compiler**: GHC 2021 is required to compile the code
- **XMake**: to compile the VM

1. **Clone the Repository**: 
  ```sh
  git clone https://github.com/thomasvergne/bonzai.git
  cd bonzai
  ```
2. **Build and install Bonzai**:
  ```sh
  cabal build
  cabal install 
  ```
3. **Build the VM**:
  ```sh
  xmake -P runtime
  # or
  cd runtime && xmake 
  ```

### Reporting Issues

If you find any issues or have suggestions, please use the [Issues page](https://github.com/thomasvergne/bonzai/issues). We appreciate your feedback and contributions!
