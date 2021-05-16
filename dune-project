(lang dune 2.7)
(generate_opam_files true)

(source (github rbardou/enumap))
(license MIT)
(authors "Romain Bardou")
(maintainers "Romain Bardou")

(package
 (name enumap)
 (synopsis "Generate code to manage enums and maps of enums")
 (description "This is a code generator to generate enum types and records where fields are enum values, with several helper functions.")
 (depends (ocaml (>= 4.07))))
