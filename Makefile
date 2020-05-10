default: build
	dune utop project/lib

build:
	dune build

cli: build
	dune exec project/bin/app.exe "cli"

gui: build
	dune exec project/bin/app.exe "gui"

test:
	dune runtest

bisect:
	BISECT_ENABLE=yes dune runtest --force
	bisect-ppx-report html && bisect-ppx-report summary

docs:
	dune build @doc

zip: clean
	zip -v -r project_src.zip ./project ./resources LOC.txt INSTALL.txt Makefile Solutions.pdf 

clean:
	dune clean
	rm -rf _coverage
	rm -rf project_src.zip