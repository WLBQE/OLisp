#!/bin/bash

LLI="lli"

./olisp.native tests/hello_world.olisp > tests/hello_world.ll

if [[ $? -ne 0 ]]; then
	rm -f tests/hello_world.ll
	printf "\nTest failed!\n"
	exit 1
fi

"$LLI" tests/hello_world.ll > tests/hello_world.result

if [[ $? -ne 0 ]]; then
	rm -f tests/hello_world.result tests/hello_world.ll
	printf "\nTest failed!\n"
	exit 1
fi

diff tests/hello_world.result tests/hello_world.out

if [[ $? -ne 0 ]]; then
	rm -f tests/hello_world.result tests/hello_world.ll
	printf "\nTest failed!\n"
	exit 1
fi

rm -f tests/hello_world.result tests/hello_world.ll
printf "Test passed!\n"
exit 0
