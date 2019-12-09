all:
	dune build src/main.exe
	ln -s ./_build/default/src/main.exe pgoc

clean:
	rm pgoc
	dune clean

re: clean all
