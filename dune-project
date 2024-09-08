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
 (synopsis "recurse center compiler")
 (description "")
 (depends ocaml dune core)
 (tags
  (compiler "programming language")))
