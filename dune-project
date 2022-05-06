(lang dune 2.8)
(name okra)

(generate_opam_files true)

(cram enable)

(license ISC)
(authors "Magnus Skjegstad")
(maintainers "magnus@skjegstad.com")
(homepage "https://github.com/magnuss/okra")
(bug_reports "https://github.com/MagnusS/okra/issues")

(package
 (name okra-bin)
 (synopsis "Report parsing executable")
 (description "An executable to be used for report parsing")
 (depends
  (alcotest :with-test)
  (mdx :with-test)
  logs
  fmt
  okra
  (cmdliner (>= 1.1.0))
  bos
  dune-build-info
  (yaml (>= 3.0))
  (cohttp-lwt-unix (>= 4.0.0))))

(package
 (name okra)
 (synopsis "Report parsing prototypes")
 (description "A library of tools for report parsing")
 (depends
  (ocaml (>= 4.10))
  (alcotest :with-test)
  logs
  fmt
  get-activity
  (gitlab (>= 0.1.3))
  calendar
  csv
  ISO8601
  (omd (>= 2.0))))
(using mdx 0.1)
