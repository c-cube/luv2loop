(lang dune 3.0)
(using mdx 0.2)

(name fuseau)
(version 0.1)

(generate_opam_files true)

(source
 (github c-cube/fuseau))

(authors "Simon Cruanes")

(maintainers "Simon Cruanes")

(license MIT)

;(documentation https://url/to/documentation)

(package
 (name fuseau)
 (synopsis "A simple IO and concurrency library for OCaml 5")
 (depends
  (ocaml
   (>= 5.0))
  dune
  (picos (and (>= 0.5) (< 0.6)))
  (mtime
   (>= 2.0))
  (trace (>= 0.6))
  (trace-tef :with-test)
  (mdx :with-test)
  thread-local-storage)
 (tags
  (await effects event-loop thread-local-storage)))

(package
 (name fuseau-lwt)
 (synopsis "Lwt backend for Fuseau")
 (depends
  dune
  (trace (>= 0.6))
  lwt
  base-unix
  (trace-tef :with-test)
  (fuseau (= :version)))
 (tags
  (fuseau lwt)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
