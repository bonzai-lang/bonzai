[![GitHub Issues or Pull Requests](https://img.shields.io/github/issues/bonzai-lang/bonzai?style=for-the-badge)](https://github.com/bonzai-lang/bonzai/issues)
![GitHub License](https://img.shields.io/github/license/bonzai-lang/bonzai?style=for-the-badge)
![Bonzai](assets/banner.png)

## Introduction

Bonzai is a generic purpose programming language that relies on a strong and non-taulerant typechecker to guarantee types and computations in your code. It compiles down to a custom bytecode with relatively good performance.

## Table of Contents

1. [Features](#features)
2. [Example Code](#example-code)
3. [Building Bonzai](#building-bonzai)
4. [Contributing](/CONTRIBUTING.md)
5. [Reporting Issues](#reporting-issues)

---

## Features

- **Strong typechecker**: Throw errors for incompatible types, to ensure security when running your code.
- **Datatypes as expressions**: All datatypes are expressions, allowing for more flexibility in your code.
- **Row polymorphism**: Allows for dictionary-like structures with flexible keys.
- **Bytecode compilation**: Suitable for Unix-like systems, enabling almost multi-platform code running.

## Example Code

A parallel HTTP server dispatcher example :

```rs
require "std:http"
require "std:foundation"
require "std:datatypes/json"

let thread = spawn HTTP::create(
  fn(req) => {
    let headers = req->get()

    match Header::from(headers) {
      case Some(Header(top, _, content)) => {
        let json = JSON::parse(content)
        let [method, path .. _] = String::split(top, " ")

        print("[$method] Received request on $path")

        req->send(JSON::stringify(json))

        true
      }

      case None => {
        print("Invalid JSON")
        req->send(JSON::stringify(
          JSON->Object([
            ("error", JSON->String("Invalid JSON")),
            ("status", JSON->String("400 Bad Request"))
          ])
        ))

        false
      }
    }
  },
  8000
)

print("Server started on port 8000")

thread.wait()
```

## Building Bonzai

### Prerequisites
- **Haskell compiler**: GHC 2021 is required to compile the code
- **XMake**: to compile the VM
- **Python**: to run the build script

1. **Clone the Repository**: 
  ```sh
  git clone https://github.com/bonzai-lang/bonzai.git
  cd bonzai
  ```
2. **Build the project**:
  ```sh
  python3 scripts/build.py
  ```
3. **Set the environment variable**:
  ```sh
  export BONZAI_PATH="/path/to/bonzai"
  export PATH="/path/to/bonzai/bin:$PATH"
  ```

### Reporting Issues

If you find any issues or have suggestions, please use the [Issues page](https://github.com/bonzai-lang/bonzai/issues). We appreciate your feedback and contributions!
