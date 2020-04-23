default: build
	utop

build:
	ocamlbuild -use-ocamlfind eval.byte

test:
	ocamlbuild -use-ocamlfind -tag 'debug' test.byte && ./test.byte

clean:
	ocamlbuild -clean
