all: pgoc

pgoc:
	dune build src/main.exe
	ln -f -s ./_build/default/src/main.exe pgoc

.PHONY: tests
tests: pgoc
	$(MAKE) -C tests

clean:
	rm pgoc
	dune clean

%.s: %.go pgoc
	./pgoc -v $<

%.exe: %.s
	gcc -no-pie -g $< -o $@
	$@

re: clean all
