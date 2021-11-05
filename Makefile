.PHONY: test

all:
	dune build @install

test:
	dune build test.exe
	./_build/default/test.exe

install: all
	dune install

uninstall:
	ocamlfind -remove vpt

clean:
	rm -rf _build
