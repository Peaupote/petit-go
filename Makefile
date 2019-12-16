all:
	dune build src/main.exe
	ln -f -s ./_build/default/src/main.exe pgoc

test:
	$(MAKE) -C tests

clean:
	rm pgoc
	dune clean

re: clean all
