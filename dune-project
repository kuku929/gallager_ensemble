(lang dune 3.20)
(name Gallager)
(generate_opam_files true)

(source
 (github kuku929/gallager_ensemble))
(authors "Krutarth Patel <krutarthpatel929@gmail.com>")
(maintainers "Krutarth Patel <krutarthpatel929@gmail.com>")
(license LICENSE)

(package
 (name Gallager)
 (synopsis "Gallager ensemble")
 (description "Fast LDPC decoding using gallager ensemble and multithreading")
 (depends ocaml)
 (tags (ldpc decoding gallager ensemble)))
