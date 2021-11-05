.PHONY: test

all:
	dune build @install

test:
	dune build src/test.exe
	_build/default/src/test.exe

install: all
	dune install

uninstall:
	ocamlfind -remove vpt

clean:
	rm -rf _build
