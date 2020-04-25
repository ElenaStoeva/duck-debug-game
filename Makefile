MODULES = lexer parser eval grid state print
OBJECTS = $(MODULES:=.cmo)
SRC_DIRS = -I src/interpreter -I src/model -I src/view -I src/tests -I src

default: build
	utop

build:
	ocamlbuild -use-ocamlfind $(SRC_DIRS) $(OBJECTS)

test:
	ocamlbuild -use-ocamlfind -tag 'debug' $(SRC_DIRS) test.byte && ./test.byte

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private 
