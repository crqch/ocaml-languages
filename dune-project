(lang dune 3.21)

(name calc)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Maciej Piróg <mpirog@cs.uni.wroc.pl>")

(license "MIT")

(package
 (name calc)
 (synopsis "Interpreter jezyka CALC")
 (description "Interpreter jezyka CALC (wyrażenia arytmetyczne)")
 (depends ocaml))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

 (using menhir 2.1)
