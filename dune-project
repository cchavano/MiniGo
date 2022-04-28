(lang dune 3.0)

(name mini_go)

(generate_opam_files true)

(source
 (github cchavano/mini-go))

(authors "Clément Chavanon")

; (license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name mini_go)
 (synopsis "Transpiler from a subset of Go to C written in OCaml")
 (depends ocaml dune))

(using menhir 2.1)