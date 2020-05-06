default: build
	dune utop project/lib

build:
	dune build

run: build
	dune exec project/bin/main.exe

test:
	dune runtest

bisect:
	BISECT_ENABLE=yes dune runtest --force
	bisect-ppx-report html && bisect-ppx-report summary

docs:
	dune build @doc-private

zip:
	zip -v -r project_src.zip ./project ./json_files dune-project project.opam Makefile INSTALL.txt 

clean:
	dune clean
	rm -rf _coverage
	rm -rf project_src.zip