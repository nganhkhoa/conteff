(lang dune 3.20)

(name conteff)

(generate_opam_files true)

(source
 (github nganhkhoa/conteff))

(authors "Anh-Khoa <nganhkhoa@u.northwestern.edu>")

(maintainers "Anh-Khoa <nganhkhoa@u.northwestern.edu>")

(license LICENSE)

(documentation https://nganhkhoa.github.io/papers/contracts-to-effects.pdf)

(package
 (name conteff)
 (synopsis "Software Contract using Effect Handlers")
 (description "Software Contract using Effect Handlers")
 (depends
   ocaml
   ppx_deriving
   ocaml-compiler-libs
   (alcotest :with-test))
 (tags
  ("software contract" "effect handlers" "theory")))
