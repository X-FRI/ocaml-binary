(lang dune 3.17)

(name ocaml-binary)

(generate_opam_files true)

(source
 (github X-FRI/ocaml-binary))

(authors "Nicolas Trangez <nicolas.trangez@gmail.com>")

(maintainers "Nicolas Trangez <nicolas.trangez@gmail.com>")

(license MPL-2.0)

(documentation https://github.com/X-FRI/ocaml-binary)

(package
 (allow_empty)
 (name ocaml-binary)
 (synopsis "Utilities for binary serialization & deserialization")
 (description "OCaml library for binary serialization & deserialization")
 (depends ocaml ocplib-endian)
 (tags
  ("ocaml" "binary" "serialization" "deserialization")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
