# Veritas

[![CI](https://github.com/Athena-E/veritas/actions/workflows/ci.yml/badge.svg)](https://github.com/Athena-E/veritas/actions/workflows/ci.yml)

A programming language project exploring dependent types, and formal verification. This project aims to develop a toolchain for type-preserving compilation targeting a dependently typed assembly language (DTAL), inspired by [Xi, Harper 2001](https://personal.utdallas.edu/~hamlen/Papers/xi2001dtal.pdf).

![Diagram](/veritas_docs/system_diagram.png)

## Quick Start

### Build

```bash
cargo build --release
```

### Compile a program

```bash
cargo run -- src/examples/01_simple.veri
```

This compiles the source file through all pipeline stages (lexing, parsing, type checking, TIR lowering, DTAL code generation) and prints the generated DTAL assembly.

### Run tests

```bash
cargo test
```

## Usage

```
veritas <source_file> [OPTIONS]
```

### Options

| Flag              | Description                                      |
| ----------------- | ------------------------------------------------ |
| `-h`, `--help`    | Show help message                                |
| `-v`, `--verbose` | Show compilation stages                          |
| `--tokens`        | Show lexer tokens                                |
| `--ast`           | Show typed AST                                   |
| `--tir`           | Show TIR (SSA form)                              |
| `--verify`        | Verify DTAL output                               |
| `--verify-only`   | Verify DTAL and exit (no code output)            |
| `-o <file>`       | Output native executable (ELF)                   |
| `--native`        | Print generated x86-64 assembly and machine code |

### Optimization

| Flag               | Description                       |
| ------------------ | --------------------------------- |
| `-O`, `--optimize` | Enable all optimizations          |
| `--copy-prop`      | Enable copy propagation only      |
| `--dce`            | Enable dead code elimination only |

## Veritas Language Overview

Veritas source files use the `.veri` extension. The language supports:

- **Types**: `int`, `bool`, fixed-size arrays (`[int; N]`), references
- **Functions**: with parameters, return types, and multi-function programs
- **Control flow**: `if`/`else`, `for` loops with ranges
- **Mutability**: explicit `let mut` bindings
- **Preconditions**: `requires` clauses for function contracts
- **Postconditions**: `ensures` clause for function contracts
- **Loop invariants**: `invariant` clauses on `for` loops

See [Veritas semantics](/veritas_docs/veritas_semantics.pdf) for formal language specification and typing rules.

### Example program

```
fn bubble_sort(arr: [int; 10]) -> [int; 10] {
    let mut result: [int; 10] = arr;
    for i in 0..10 {
        for j in 0..9 {
            let a: int = result[j];
            let b: int = result[j + 1];
            if a > b {
                result[j] = b;
                result[j + 1] = a;
            } else {
                let dummy: int = 0;
            }
        }
    }
    result
}

fn main() -> int {
    let mut arr: [int; 10] = [0; 10];
    arr[0] = 5;
    arr[1] = 3;
    arr[2] = 8;
    let sorted: [int; 10] = bubble_sort(arr);
    sorted[0]
}
```

## DTAL Overview

DTAL (Dependently Typed Assembly Language) is the compiler's target representation. Inspired by [Xi and Harper (2001)](https://personal.utdallas.edu/~hamlen/Papers/xi2001dtal.pdf), it augments a low-level instruction set with type annotations and constraints that allow independent verification of compiled code.

### Program structure

A DTAL program is a sequence of functions. Each function declares its parameter types, return type, and optional pre/postconditions, followed by a series of labeled basic blocks.

```
.function <name>
.params {<reg>: <type>, ...}
.returns <type>
.precondition <constraint>     ; optional
.postcondition <constraint>    ; optional

<name>:
<label>:
    ; Entry state:
    ;   <reg>: <type>
    <instructions...>
```

### Registers

DTAL uses virtual registers (`v0`, `v1`, ...) during code generation. These are later allocated to physical registers (`r0`–`r15`) plus special registers `sp` (stack pointer), `fp` (frame pointer), and `lr` (link register).

### Instruction set

| Category      | Instructions                  | Syntax                                       |
| ------------- | ----------------------------- | -------------------------------------------- |
| Data movement | `mov`                         | `mov vD, imm` / `mov vD, vS`                 |
| Memory        | `load`, `store`               | `load vD, [vB + vO]` / `store [vB + vO], vS` |
| Arithmetic    | `add`, `sub`, `mul`, `addi`   | `add vD, vL, vR` / `addi vD, vS, imm`        |
| Logical       | `and`, `or`, `not`            | `and vD, vL, vR` / `not vD, vS`              |
| Comparison    | `cmp`, `set<cc>`              | `cmp vL, vR` / `seteq vD`                    |
| Control flow  | `jmp`, `b<cc>`, `call`, `ret` | `jmp label` / `beq label` / `call fn`        |
| Stack         | `push`, `pop`, `alloca`       | `push vS` / `pop vD` / `alloca vD, size`     |

Comparison conditions (`<cc>`): `eq`, `ne`, `lt`, `le`, `gt`, `ge`.

### Type system

Instructions carry type annotations as comments, preserving source-level type information through compilation:

- `int`, `bool`, `unit` — base types
- `int(N)` — singleton integer type (value known at compile time)
- `{a: int | <prop>}` — refinement type with a constraint on the value
- `[T; N]` — fixed-size array
- `&T`, `&mut T` — references

### Constraints

DTAL supports an index-level constraint language used for preconditions, postconditions, and verification annotations:

- Comparisons: `==`, `!=`, `<`, `<=`, `>`, `>=`
- Logical connectives: `&&`, `||`, `!`
- Index expressions: constants, variables, `+`, `-`, `*`

These constraints are checked via the Z3 SMT solver during verification (`--verify`).

### Example output

Compiling `fn add(x: int, y: int) -> int { x + y }` produces:

```
; DTAL Program
; Generated by Veritas Compiler

.function add
.params {v0: int, v1: int}
.returns int

add:
.add_bb0:
    add v2, v0, v1    ; {a: int | _synth_0 }
    mov r0, v2    ; int
    ret
```

See the [DTAL specification](/veritas_docs/dtal_spec.md) for full typing rules.

## Compilation Pipeline

```
Source (.veri) → Lexer → Parser → Type Checker → TIR (SSA) → DTAL Codegen → Emit
                                                                  ↓
                                                             Optimizations
                                                                  ↓
                                                             x86-64 Lowering → ELF Executable
```

## Resources

- [Z3 theorem prover](https://github.com/Z3Prover/z3) (required for SMT-based type checking)
