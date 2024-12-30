# EduCC Compiler

**EduCC** is a teaching-oriented C99 compiler project, written in C++. It includes:

1. A **Lexer** (converts raw C code into tokens).
2. A **Parser** using **recursive descent** (builds an Abstract Syntax Tree or AST).
3. A **Semantic Analyser** (checks symbols, simple types, etc.).
4. A **Code Generator** to **LLVM IR** (which can be compiled further by `clang` or `llc`).

The compiler aims to be *educational*, with clear organisation and well-documented code, suitable for learning compiler internals.

---

## Features

- **Lexical Analysis**: Identifies keywords, identifiers, literals and operators from raw source.
- **Parsing**: Builds an **AST** supporting a subset of C (variables, functions, statements, expressions, etc.).
- **Local Declarations**: Supports local variable declarations in function bodies (e.g., `int sum = x + y;`).
- **Semantic Analysis**: Symbol table creation, basic checks for undeclared variables, redeclaration, function calls vs. variables, etc.
- **LLVM IR CodeGen**: Produces `.ll` files that you can compile with `clang`.

**Note**: This compiler is **not** a complete C99 implementation yet. Certain advanced features—e.g., pointers, arrays, structs/unions, preprocessor directives—are not fully implemented.

---

## Build & Dependencies

- Requires **C++17** or newer.
- Requires **LLVM** (and `llvm-config`) installed for IR generation.
  - On macOS with Homebrew, for instance, `brew install llvm`.
- Ensure `llvm-config` is in your `$PATH` or update the Makefile variables (`LLVM_CXXFLAGS`, `LLVM_LDFLAGS`).

### Building

1. **Clone** the repo or copy its files.
2. **Run** `make` in the root directory.
3. This produces the `educc` executable.

If successful, you’ll have an `educc` binary in the project root.

### Testing

The Makefile includes a `test` target that:

1. Builds `educc`.
2. Compiles a sample program (`sample.c`) to LLVM IR (`sample.ll`).
3. Compiles `sample.ll` to a native executable (`sample`) with `clang`.
4. Runs `./sample`.

Invoke it with:

```bash
make test
```

---

## Usage

From the command line:

```bash
./educc <input.c> <output.ll>
```

- `educc` will parse and analyse `<input.c>`, producing LLVM IR in `<output.ll>`.
- Then compile it with `clang`:

  ```bash
  clang <output.ll> -o <program>
  ./<program>
  ```

**Example**:

```bash
./educc sample.c sample.ll
clang sample.ll -o sample
./sample
```

---

## Current Status & Limitations

- **Local Declarations**: Basic `int x = 0;` or `int sum = a + b;` is supported.
- **Expressions**: Basic arithmetic (`+ - * / %`), relational operators (`< > <= >= == !=`), unary (`++ --`) and function calls.
- **Semantic Checks**: Minimal symbol table, type checking for redefinitions, undefined variables, function calls vs. variables.
- **Pointers, arrays, structs, unions**: Not yet fully implemented.
- **Preprocessor**: No preprocessor stage is implemented.
- **Error Handling**: Throws `std::runtime_error` on syntax/semantic errors.

---
