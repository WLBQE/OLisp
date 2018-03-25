olisp.native: ast.ml parser.mly scanner.mll olisp.ml
	ocamlbuild -pkg Llvm olisp.native

clean:
	rm -rf _build/ olisp.native
