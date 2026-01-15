(lang dune 3.7)

(name occc)

(generate_opam_files true)

(map_workspace_root false)

(source
 (github GEJXD/occc))

(authors "Hsin <genesisjxd@gmail.com>")

(maintainers "Hsin <genesisjxd@gmail.com>")

(license MIT)

(package
 (name occc)
 (synopsis "implementation of book 'Write a C Compiler'")
 (description
  "`occc` is a simple C compiler target for x86_64 ISA, adapted from book *Writing a C Compiler* ")
 (depends
  (ocaml
   (>= 4.14.0))
  dune
  bisect_ppx
  ppx_deriving
  ppx_expect
  ppx_inline_test
  cmdliner
  camlp-streams
  re))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
