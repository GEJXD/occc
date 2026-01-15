# occc

`occc` is a simple C compiler based on the C17 standard, implemented in OCaml. It is based on Nora Sandler's book *Write a C Compiler*.

This repo is just used for learn compiler. ~you'd better not use this to compile a real C project.~

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

2. Create a ocaml environment by opam

since `opam switch` are preety like `conda`, it can easily create a virtual environment.
```bash
# Create a new environment named by occc
opam switch create occc ocaml-base-compiler.5.3.0
# Optional: development tools
opam install ocaml-lst-server odoc ocamlformat utop
# switch to new environment
opam switch occc
# install dependency
opam install . --deps-only
```

3. Build the source
```bash
dune build
```

Now the executable is in `_build/default/bin/main.exe`. You can also run it by type `dune exec bin/main.exe`.

### Usage
 
TODO 