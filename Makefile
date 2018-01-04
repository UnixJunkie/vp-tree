.PHONY: test

all:
	obuild configure
	obuild build lib-minivpt

test:
	obuild configure
	obuild build exe-test
	./test

install: all
	obuild install

uninstall:
	ocamlfind -remove minivpt

clean:
	obuild clean
