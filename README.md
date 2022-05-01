# MiniGo transpiler

`mini-go` is a transpiler from a subset of Go (called MiniGo) to C, written in OCaml.
It has been made as part of the compilation course taught in the 4th year of the Computer Science department at INSA Rennes. It's inspired by the teacher's [MiniJava project](https://github.com/lascar-pacagi/MiniJava).

## Requirements

### Hardware

This project is meant to be run on a **64-bit** CPU.

### Software

This project may only run on a unix-like operating system (Linux, macOS, BSD...).

1. Install the OCaml package manager `opam` by following the [opam documentation](https://opam.ocaml.org/).

2. Install the following `opam` packages with the `opam install` command: 
```text
ocaml  (version 4.x)
dune   (version 3.x)
menhir (version 20220210)
```

3. Install the following system packages with your OS package manager:
```text
gcc or clang
make
```
Make sure that the C compiler can be invoked by the `cc` command.

Additionnaly, you can install the Go compiler to compare the execution result with the one produced by `mini-go`. Check the installation instructions on the [Go website](https://go.dev/doc/install).

### Additional requirements

It's better to know how the Go language has be designed to understand the transpiler. Please, take a quick look at the 
[Go specifications](https://go.dev/ref/spec) first.

## Cloning the repo

Once all dependencies have been installed, clone the project and the associated submodules:

```bash
git clone --recurse-submodules https://github.com/cchavano/mini-go.git
```

## Running the project

In the project directory, run the following commands:

```bash
# Build the project
make

# Transpile a Go file
./mini-go path/to/file.go

# The generated C file is now located at ./target/C/file.c
# The executable file is now located at ./target/exe/file

# Execute the binary
./target/exe/file

# To check the result of the execution by the official Go compiler, run
go run path/to/file.go 
```

You can pass the following options to the `mini-go` executable:

### Print the tokens

```bash
# Only perform the lexical analysis and print the tokens on the standard output
./mini-go --dump-tokens path/to/file.go
```

### Print the AST

```bash
# Only perform the parsing and print the abstract syntax tree on the standard output
./mini-go --dump-ast path/to/file.go
```

## Implementation choices

### String handling

C does not natively support the string type: this can only be done by manipulating char arrays and pointers. When a Go function returns a string value, the equivalent C function must return a pointer to a memory segment allocated in the heap (otherwise, returning a local stack pointer will be problematic because of the local variables lifespan). When transpiling a Go file to C, it's more difficult to know when to free allocated memory, like in this example:

```Go
// Go function that returns a string value
func helloWorld() string {
    return "Hello, World!"
}

// Calling helloWorld() in the main function
func main() {
    fmt.Println(helloWorld())
    // The result of the helloWorld() function call is "anonymous", meaning it is not attached
    // to any variables, so that the allocated memory in the equivalent C program cannot be freed
}
```
Furthermore, to make the transpilation process easier, the handling of string must be standardized.
This means avoiding having pointers both pointing to the stack or to the heap.

Because all of these reasons, the following choices have been made:
- All string values are stored in the heap ;
- To avoid memory leak, a [tiny garbage collector](https://github.com/orangeduck/tgc) is used to free
heap memory segments that are no longer reachable.

If you have better recommendations on how to handle string values efficiently in C -- when dealing with a transpilation process -- feel free to create an issue.

### Variables and functions naming

The transpiled C files use the `complex.h` library to deal with complex numbers, so that some keywords are reserved to this library (such as `I` or `_Complex`). To avoid conflicts between C keywords or builtin functions and user-defined variables or functions, all transpiled variables are prefixed with `v_` and all functions by `def_`.

### Untyped constants

As in earlier versions of Go, `mini-go` untyped values are necessarily constants, so that the "untyped" property of
an expression is considered an expression mode rather than a type on its own.

### Error messages

The error messages are very strongly inspired by the official Go compiler (if not identical for some). This makes it easier to compare the type of errors raised by `mini-go` and the Go compiler.

