CC = clang

.PHONY: all
all: olisp.native list.o

olisp.native: ast.ml scanner.mll parser.mly sast.ml semant.ml codegen.ml olisp.ml
	rm -f *.o
	ocamlbuild -lib str -pkgs llvm,llvm.analysis olisp.native

list.o: list.c
	$(CC) -c -o list.o list.c

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -f *.native *.byte tests/*.ll tests/*.s tests/*.exe tests/*.result *.log *.o
