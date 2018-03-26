#!/bin/bash

failed=0

./olisp.native -s tests/hello_world.olisp

if [[ $? -eq 0 ]]; then
	printf "Test passed!\n"
else
	printf "Test failed!\n"
	((failed++))
fi

exit $((failed))
