
import sys
import os

if __name__ == "__main__":
    os.system("ocamlbuild -clean")
    os.system("ocamlbuild olisp.native")

    print (os.system("./olisp.native test2"))
