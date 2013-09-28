SOM: Simple OCaml Mutations
===


Collection of simple tools to mutate OCaml programs.

- `lexer.ml` is a simple lexer for OCaml
  - if you want to compile it to use in a top-level: `ocamlc -c lexer.ml`, then you may use `ocaml lexer.cmo` and write, for instance, `open Lexer;;`
- `lexer_main.ml` implements a simple source-to-source tool using `lexer.ml`
  - to compile it: `ocamlopt -o ocamltoocaml lexer.ml lexer_main.ml` 
- `ocamltohtml.ml` is a simple OCaml to HTML tool, it is based on `lexer.ml`
  - to compile it: `ocamlopt -o ocamltohtml lexer.ml ocamltohtml.ml`



