opam_ci:
	ocamlfind ocamlopt -linkpkg -package lwt.unix,conduit.lwt,cohttp.lwt,opam-lib.format,git.unix,github opam_ci.ml -o opam-ci
