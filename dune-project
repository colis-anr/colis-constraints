(lang dune 2.8)

(name colis-constraints)
(source (github colis-anr/constraints))

(authors "Niols Jeannerod <nicolas.jeannerod@irif.fr>")

(license GPL-3.0-or-later)

(maintainers "Niols Jeannerod <nicolas.jeannerod@irif.fr>")

(generate_opam_files true)

(package
 (name colis-constraints)
 (version 0.1)
 (synopsis "Constraints backend for the CoLiS Symbolic Interpreter")

 (depends
  (batteries (>= 3.3.0))
  yojson
  ppx_deriving_yojson
  (ppx_deriving :build)
  (odoc :with-doc)))
