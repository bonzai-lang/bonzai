---
title: Write your first program
description: Follow the Getting Started guide to write your first Bonzai program.
---

This getting started is divided into two main sections:
- [Writing your first program as a begginer](#writing-your-first-program-as-a-beginner)
- [Writing your first program as an experienced programmer](#writing-your-first-program-as-an-experienced-programmer)

So you can choose the one that fits you best.

## Writing your first program as a beginner

In this short guide, we will write a simple program that prints "Hello, World!" to the console and then we'll make it more complex by the end of this guide.

### Step 1: Create a new file and include default headers

Create a new file named `hello.bzi` in your preferred text editor. This file will contain our first Bonzai program. 

We'll need to include some packages by default to make our program richer and work. So start with the following required headers:

```bonzai
require "std:foundation"

// ...
```

### Step 2: Write the "Hello, World!" program

Now, let's write a simple program that prints "Hello, World!" to the console. Add the following code to your `hello.bzi` file:

```bonzai
require "std:foundation"

print("Hello, World!")
```

That's it! You have just written your first Bonzai program. And as you can see, it is very similar to other programming languages like Python or JavaScript while keeping everything simple and straightforward.

### Step 3: Run your program

To run your program, open a terminal and navigate to the directory where you saved your `hello.bzi` file. Then, execute the following command:

```bash
bonzai hello.bzi
```

You should see the output:

```
Hello, World!
```

### Step 4: Adding a function

Now, let's make our program a bit more complex by adding a function that prints a greeting message. Modify your `hello.bzi` file to include the following code:

```bonzai
require "std:foundation"

fn greet(name) => print("Hello, $name!")

greet("World")
```
This code defines a function `greet` that takes a parameter `name` and prints a greeting message. When you run the program again, you should see the output:

```
Hello, World!
```

### Step 5: Adding optional parameters to the function

Now, let's enhance our function to accept an optional parameter. If the parameter is not provided, it will default to "Hello". Modify your `hello.bzi` file as follows:

```bonzai
require "std:foundation"

fn greet(name, prefix _?) => {
  let prefix = Optional::unwrap_or(kwargs->prefix, "Hello")
  print("$prefix, $name!")
}

greet("World")
greet("Universe", prefix: "Greetings")
```

We have plenty things to notice here:
- We added an optional parameter `prefix` to the `greet` function:
  - The `_` just means that this parameter is named. 
  - The `?` means that this parameter is optional.
- We used `Optional::unwrap_or` to provide a default value for the `prefix` parameter if it is not provided.

When you run the program again, you should see the output:

```
Hello, World!
Greetings, Universe!
```

### Step 6: Adding a loop

Finally, let's add a loop to print greetings for multiple names. Modify your `hello.bzi` file as follows:

```bonzai
require "std:foundation"

fn greet(name, prefix _?) => {
  let prefix = Optional::unwrap_or(kwargs->prefix, "Hello")
  print("$prefix, $name!")
}

let names = ["Alice", "Bob", "Charlie"]

for name in names {
  greet(name)
}
```

This code defines a list of names and uses a `for` loop to iterate over each name, calling the `greet` function for each one.

When you run the program again, you should see the output:

```
Hello, Alice!
Hello, Bob!
Hello, Charlie!
```

### Step 7: Conclusion

Congratulations! You have successfully written your first Bonzai program and learned some basic concepts such as functions, optional parameters, and loops.
You can now explore more features of Bonzai and start building more complex programs. If you want to learn more about Bonzai, check out the next section of this guide: [Writing your first program as an experienced programmer](#writing-your-first-program-as-an-experienced-programmer).

## Writing your first program as an experienced programmer

In that section, we'll write a simple HTTP server from scratch using Bonzai, its standard library, and its powerful abstraction capabilities.

### Step 1: Create a new file and include default headers

Create a new file named `server.bzi` in your preferred text editor. This file will contain our first Bonzai program.
We'll need to include some packages by default to make our program richer and work. So start with the following required headers:

```bonzai
require "std:foundation"
require "std:http": 
  start_http_server, accept_request, get_buffer, 
  Header::from, send_buffer

// ...
```

### Step 2: Write the HTTP server

We could use the HTTP module functions that are already available in the standard library, but let's write our own HTTP server from scratch to understand how it works.

An HTTP server is a program that listens for incoming HTTP requests and responds to them. It consists typically of the following steps:
1. Create a socket to listen for incoming connections.
2. Accept incoming connections, for instance by using a while loop.
3. Read the request from the socket.
4. Parse the request to extract the method, path, and headers.
5. Generate a response based on the request.
6. Send the response back to the client.

Let's implement these steps in our `server.bzi` file:

```bonzai
require "std:foundation"
require "std:http": 
  start_http_server, accept_request, get_buffer, 
  Header::from, send_buffer

// We have start_http_server function that will create a socket for us.
fn HTTP::createServer(port) => {
  let server = start_http_server(port)
}
```

### Step 3: Accept incoming connections

```bonzai
require "std:foundation"
require "std:http": 
  start_http_server, accept_request, get_buffer, 
  Header::from, send_buffer

fn HTTP::createServer(port) => {
  let server = start_http_server(port)

  while true {
    // Accept incoming connections
    let req = server.accept_request()
    
    print("Received request: $req")
  }
}
```

Let's break down the code as for now:
- We use the `start_http_server` function to create a server that listens on the specified port.
- We enter an infinite loop to continuously accept incoming connections.
- We use the `accept_request` method to accept an incoming request and store it in the `req` variable: This method blocks until a request is received, allowing us to handle each request one by one.
- We print the received request to the console for debugging purposes.


> Note that doing `server.accept_request()`, or `accept_request(server)` results in the same thing as `expr.id` is just a shorthand for `id(expr, ...)` in Bonzai.

### Step 4: Read the request
```bonzai
require "std:foundation"
require "std:http": 
  start_http_server, accept_request, get_buffer, 
  Header::from, send_buffer

fn HTTP::createServer(port) => {
  let server = start_http_server(port)

  while true {
    // Accept incoming connections
    let req = server.accept_request()
    let buf = req.get_buffer()
    let headers = Header::from(buf)

    print("Received request: $headers")
  }
}
```

In this step, we read the request from the socket and parse the headers. We use the `get_buffer` method to get the raw request data and then use the `Header::from` function to parse the headers.

### Step 5: Generate a response

```bonzai
require "std:foundation"
require "std:http": 
  start_http_server, accept_request, get_buffer, 
  Header::from, send_buffer

fn HTTP::createServer(port) => {
  let server = start_http_server(port)

  while true {
    // Accept incoming connections
    let req = server.accept_request()
    let buf = req.get_buffer()
    let headers = Header::from(buf)

    print("Received request: $headers")

    // Generate a response
    req.send_buffer("Hello, World!")
  }
}
```

In this step, we generate a response based on the request. We use the `send_buffer` method to send a response back to the client. In this case, we simply send "Hello, World!" as the response body.

### Step 6: Run the server

Start by adding a `spawn` keyword behind your `HTTP::createServer` function to run it in a separate thread. This allows the server to run concurrently with other tasks, such as handling incoming requests.

```bonzai
require "std:foundation"
require "std:http": 
  start_http_server, accept_request, get_buffer, 
  Header::from, send_buffer

fn HTTP::createServer(port) => {
  let server = start_http_server(port)

  while true {
    // Accept incoming connections
    let req = server.accept_request()
    let buf = req.get_buffer()
    let headers = Header::from(buf)

    print("Received request: $headers")

    // Generate a response
    req.send_buffer("Hello, World!")
  }
}

spawn HTTP::createServer(8080)

print("Server is running on port 8080")
```

Then, to run the server, open a terminal and navigate to the directory where you saved your `server.bzi` file. Then, execute the following command:

```bash
bonzai server.bzi
```

You should see the server starting and listening for incoming requests. You can test it by sending an HTTP request to the server using a tool like `curl` or your web browser.

### Step 7: Test the server
To test the server, you can use `curl` to send a request to the server. Open another terminal and run the following command:

```bash
curl http://localhost:8080
```