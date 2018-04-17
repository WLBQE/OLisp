#!/bin/bash

CC="clang"
LLC="llc"

./olisp.native tests/test_print_1.olisp > tests/test_print_1.ll

if [[ $? -ne 0 ]]; then
	rm -f tests/test_print_1.ll
	printf "\nTest failed!\n"
	exit 1
fi

"$LLC" tests/test_print_1.ll > tests/test_print_1.s
if [[ $? -ne 0 ]]; then
	rm -f tests/test_print_1.ll tests/test_print_1.s
	printf "\nTest failed!\n"
	exit 1
fi

"$CC" -o tests/test_print_1.exe tests/test_print_1.s

if [[ $? -ne 0 ]]; then
	rm -f tests/test_print_1.ll tests/test_print_1.s tests/test_print_1.exe
	printf "\nTest failed!\n"
	exit 1
fi

tests/test_print_1.exe > tests/test_print_1.result
diff tests/test_print_1.result tests/test_print_1.out

if [[ $? -ne 0 ]]; then
	rm -f tests/test_print_1.ll tests/test_print_1.s tests/test_print_1.exe tests/test_print_1.result
	printf "\nTest failed!\n"
	exit 1
fi

rm -f tests/test_print_1.ll tests/test_print_1.s tests/test_print_1.exe tests/test_print_1.result
printf "Test passed!\n"
exit 0
