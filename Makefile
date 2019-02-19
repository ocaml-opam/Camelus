.PHONY: opam_ci
opam_ci:
	dune build -p camelus
	cp _build/default/camelus_main.exe opam-ci

clean:
	rm -rf opam-ci _build

install:
	dune install
