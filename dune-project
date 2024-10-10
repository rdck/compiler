(lang dune 3.16)

(name compiler)

(generate_opam_files true)

(source
 (github rdck/compiler))

(authors "River Dillon")

(maintainers "River Dillon")

(license MIT)

(documentation https://github.com/rdck/compiler)

(package
 (name compiler)
 (synopsis "Recurse Center compiler experiments")
 (description "")
 (depends ocaml dune core core_unix (alcotest :with-test))
 (tags
  (compiler "programming language")))
