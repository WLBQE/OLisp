olisp.native: ast.ml scanner.mll parser.mly sast.ml semant.ml codegen.ml olisp.ml 
	ocamlbuild -lib str -pkgs llvm,llvm.analysis olisp.native

.PHONY: clean
clean:
	rm -rf _build/ *.native *.byte tests/*.ll tests/*.s tests/*.exe tests/*.result *.log
