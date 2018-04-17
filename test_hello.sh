#!/bin/bash

CC="clang"
LLC="llc"

./olisp.native tests/hello_world.olisp > tests/hello_world.ll

if [[ $? -ne 0 ]]; then
	rm -f tests/hello_world.ll
	printf "\nTest failed!\n"
	exit 1
fi

"$LLC" tests/hello_world.ll > tests/hello_world.s
if [[ $? -ne 0 ]]; then
	rm -f tests/hello_world.ll tests/hello_world.s
	printf "\nTest failed!\n"
	exit 1
fi

"$CC" -o tests/hello_world.exe tests/hello_world.s

if [[ $? -ne 0 ]]; then
	rm -f tests/hello_world.ll tests/hello_world.s tests/hello_world.exe
	printf "\nTest failed!\n"
	exit 1
fi

tests/hello_world.exe > tests/hello_world.result
diff tests/hello_world.result tests/hello_world.out

if [[ $? -ne 0 ]]; then
	rm -f tests/hello_world.ll tests/hello_world.s tests/hello_world.exe tests/hello_world.result
	printf "\nTest failed!\n"
	exit 1
fi

rm -f tests/hello_world.ll tests/hello_world.s tests/hello_world.exe tests/hello_world.result
printf "Test passed!\n"
exit 0
