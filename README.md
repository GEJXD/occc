# occc

`occc` is a simple C-subset compiler based on the C17 standard, implemented in OCaml. It is based on Nora Sandler's book *Write a C Compiler*.

This project is for learning compiler, it is not intended for compiling real C projects.

## supported features

### Type system

Currently only valid type are `int`. But function parameters can be `void`.

### Storage class

support `extern` and `static` keywords. 

support file scope declarations and multi-file compilation.

### Expressions

- Unary Operators: `-`, `~`, `!`, `++`, `--`
- Binary Operators: `+`, `-`, `*`, `/`, `%`, `&`, `|`, `^`, `<<`, `>>`, `&&`, `||`, `==`, `!=`, `<`, `<=`, `>`, `>=`
- Compound Operators: `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`
- Assignment: `=`
- Conditional Operator: `cond ? then_exp : else_exp`
- Function Call.

### Statements

- Sequence statements, null statement (single `;`)
- Branch Statements: `if (cond) stmts`, `if (cond) stmts else stmts`
- Loops: `while (cond) stmts`, `do stmts while (cond)`, `for (init; cond; post) stmts`
- Loop Control: `break`, `continue`
- Return: `return`

### Declaractions
- top-level declaractions / definitions: include variables and functions
- block: { ... }

### unsupported features

`goto` statement, `switch` statement, Floating-Points `struct`, Arrays, Pointers, Characters and Strings, Dynamic Memory Allocation etc.


### Example source 

```C
int putchar(int c);
int print_int(int x);
int print_int_negative(int x);

int lots_of_args(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, int m, int n, int o) {
    return l + o;
}

int print_int(int x) {
    if (x < 0) {
        putchar(45);
        return print_int_negative(x);
    } else {
        return print_int_negative(-x);
    }
}

int print_int_negative(int x) {
    int last_digit;
    int ret;

    if (x > -10) {
        last_digit = -(x % 10);
        return putchar(last_digit + 48);
    }

    ret = print_int_negative(x / 10);

    last_digit = -(x % 10);
    putchar(last_digit + 48);

    return ret;
}

int main(void) {
    int ret = 0;
    for (int i = 0; i < 10000000; i ++) {
        ret = lots_of_args(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, ret, 13, 14, 15);
    }

    print_int(ret);
    putchar(10);

    return ret == 150000000;
}
```

## Build from source

### Prerequisites
```bash
"ocaml" {>= "4.14.0"}
"dune" {>= "3.7"}
"bisect_ppx"
"ppx_deriving"
"ppx_expect"
"ppx_inline_test"
"cmdliner"
"camlp-streams"
"re"
"odoc" {with-doc}
```

you can easily install this by OCaml package manager `opam`.

### Build

1. Clone the repo
```bash
git clone https://github.com/GEJXD/occc.git
cd occc
```

2. Create a OCaml environment by opam

since `opam switch` are pretty like `conda`, it can easily create a virtual environment.
```bash
# Create a new environment named by occc
opam switch create occc ocaml-base-compiler.5.3.0
# Optional: development tools
opam install ocaml-lsp-server odoc ocamlformat utop
# switch to new environment
opam switch occc
# install dependency
opam install . --deps-only
```

3. Build the source
```bash
dune build
```

Now the executable is in `./_build/default/bin/main.exe`. You can also run it by type `dune exec bin/main.exe`.

### Usage

```bash
SYNOPSIS
       occc [OPTION]… files

OPTIONS
       -c  Stop before invoking linker (keep .o file)

       --codegen
           Run through code generation but stop before emitting assembly

       -d  Write out pre- and post-register-allocation assembly and DOT files
           of interference graphs. For developer only.

       --lex
           Run the lexer

       --parse
           Run the lexer and parser

       -s, -S
           Stop before assembling (keep .s file)

       -t VAL, --target=VAL (default=linux)
           Choose target platform

       --tacky
           Run the lexer, parser, and tacky generator

       --validate
           Run the lexer, parser, and semantic analysis 

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.``
```

example:

1. generate executable:
```bash
occc foo.c
```

2. generate assembly code:
```bash
occc -S foo.c
```

3. generate object file:
```bash
occc -c foo.c
```