(lang dune 3.20)

(name stere)

(generate_opam_files true)

(version 0.1.0)

(authors "mefyl <mefyl@gruntech.org>")

(maintainers "mefyl <mefyl@gruntech.org>")

(license LGPL-3.0-or-later)

(implicit_transitive_deps false)

(package
 (name stere)
 (synopsis "Logging library on top of Logs.")
 (depends fmt logs ocaml)
 (depopts lwt))

(package
 (name stere-otel)
 (synopsis "OpenTelemetry integration for Stere logging")
 (depends
  stere
  opentelemetry
  pbrt
  cohttp
  cohttp-lwt
  cohttp-lwt-unix
  uri
  uuidm
  logs
  fmt
  timmy
  timmy-lwt
  ptime
  schematic
  base
  acid-lwt
  lwt))

(pin
 (url
  git+https://github.com/dbuenzli/bytesrw#47bdebcbc527c264572fb5fbcc5ca606e67bcd32)
 (package
  (name bytesrw)
  (version 0.3.0)))
