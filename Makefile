opam_ci:
	ocamlfind ocamlopt -linkpkg -package lwt.unix,conduit.lwt,cohttp.lwt,opam-format,opam-solver,opam-state,git.unix,yojson,github.unix opam_ci.ml -o opam-ci
