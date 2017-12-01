opam_ci:
	ocamlfind ocamlopt -linkpkg -package lwt.ppx,lwt.unix,conduit-lwt,cohttp-lwt,opam-format,opam-solver,opam-state,git-unix,git-http,cohttp-lwt-unix,git-cohttp-lwt-unix,yojson,github-unix -g opam_ci.ml -o opam-ci
