MODULES = lexer parser eval grid state print ast authors
OBJECTS = $(MODULES:=.cmo)
SRC_DIRS = -I src/interpreter -I src/model -I src/view -I src/tests -I src

default: build
	utop

build:
	ocamlbuild -use-ocamlfind $(SRC_DIRS) $(OBJECTS)

run:
	ocamlbuild -use-ocamlfind $(SRC_DIRS) main.byte && ./main.byte

test:
	ocamlbuild -use-ocamlfind -tag 'debug' $(SRC_DIRS) test.byte && ./test.byte

zip:
	zip -v -r project_src.zip ./src ./json_files _tags Makefile INSTALL.txt .ocamlinit .merlin

clean:
	ocamlbuild -clean
	rm -rf project_src.zip